"""Offline routing tests; mock network tools, retain real host-key parsing."""

import os
from pathlib import Path
import shlex
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / ".local/bin/devserver-ssh-connect"
# Public host key, not a credential. It matches the helper's pinned fingerprint.
TRUSTED_KEY = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN9+6tK2o07pIe82OPPys+Fg9lyAldwYn9GmSPnKDfUb"


class RoutingTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="devserver-helper-test-")
        self.addCleanup(self.tmp.cleanup)
        self.directory = Path(self.tmp.name)
        self.scan_output = self.directory / "scan-output"
        self.calls = self.directory / "calls"
        self.scan_output.write_text("")
        self.env = dict(os.environ, MOCK_SCAN_OUTPUT=str(self.scan_output),
                        MOCK_CALL_LOG=str(self.calls), MOCK_SCAN_EXIT="0", MOCK_NC_EXIT="0")

        # Replace absolute network-tool paths only in an isolated test copy.
        scan = self.directory / "ssh-keyscan"
        scan.write_text("""#!/bin/sh
printf 'scan|%s\\n' "$*" >> "$MOCK_CALL_LOG"
/bin/cat "$MOCK_SCAN_OUTPUT"
exit "$MOCK_SCAN_EXIT"
""")
        relay = self.directory / "nc"
        relay.write_text("""#!/bin/sh
printf 'relay|%s\\n' "$*" >> "$MOCK_CALL_LOG"
/bin/cat
exit "$MOCK_NC_EXIT"
""")
        scan.chmod(0o755)
        relay.chmod(0o755)
        self.helper = self.directory / "helper"
        self.helper.write_text(HELPER.read_text()
                               .replace("/usr/bin/ssh-keyscan", shlex.quote(str(scan)))
                               .replace("/usr/bin/nc", shlex.quote(str(relay))))

    def run_helper(self, *args):
        payload = b"SSH-2.0-test\r\n\x00\xffopaque-transport"
        result = subprocess.run(["/bin/sh", str(self.helper), *args], input=payload,
                                capture_output=True, env=self.env, timeout=5)
        return result, payload

    def assert_route(self, route, address):
        result, payload = self.run_helper()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, payload, "Never mix routing diagnostics into the SSH stream")
        self.assertEqual(result.stderr.decode(), f"devserver: {route} ({address})\n")
        self.assertEqual(self.calls.read_text().splitlines(), [
            "scan|-4 -T 2 -p 22 -t ed25519 192.168.1.159",
            f"relay|-n -G 2 {address} 22",
        ])

    def test_verified_lan_is_preferred(self):
        self.scan_output.write_text(f"192.168.1.159 {TRUSTED_KEY}\n")
        self.assert_route("LAN", "192.168.1.159")

    def test_unreachable_lan_uses_tailnet(self):
        self.env["MOCK_SCAN_EXIT"] = "1"
        self.assert_route("Tailscale", "100.103.159.2")

    def test_empty_scan_uses_tailnet(self):
        self.assert_route("Tailscale", "100.103.159.2")

    def test_unrelated_lan_host_uses_tailnet(self):
        key = self.directory / "unrelated-key"
        subprocess.run(["/usr/bin/ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(key)],
                       check=True, capture_output=True)
        public_key = key.with_suffix(".pub").read_text()
        self.scan_output.write_text(f"192.168.1.159 {public_key}")
        self.assert_route("Tailscale", "100.103.159.2")

    def test_malformed_scan_uses_tailnet(self):
        self.scan_output.write_text("not an SSH public key\n")
        self.assert_route("Tailscale", "100.103.159.2")

    def test_malformed_scan_with_inherited_pipefail_uses_tailnet(self):
        self.env["SHELLOPTS"] = "pipefail"
        self.scan_output.write_text("not an SSH public key\n")
        self.assert_route("Tailscale", "100.103.159.2")

    def test_failed_scan_with_partial_output_uses_tailnet(self):
        self.scan_output.write_text(f"192.168.1.159 {TRUSTED_KEY}\n")
        self.env["MOCK_SCAN_EXIT"] = "1"
        self.assert_route("Tailscale", "100.103.159.2")

    def test_relay_failure_is_returned_without_retrying_authentication(self):
        self.env["MOCK_NC_EXIT"] = "1"
        result, _ = self.run_helper()
        self.assertEqual(result.returncode, 1)
        self.assertEqual(len(self.calls.read_text().splitlines()), 2)

    def test_arguments_rejected_before_network_access(self):
        result, _ = self.run_helper("unexpected-argument")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(result.stdout, b"")
        self.assertFalse(self.calls.exists())


class SSHConfigTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="devserver-config-test-")
        self.addCleanup(self.tmp.cleanup)
        self.directory = Path(self.tmp.name)
        self.fragment = ROOT / ".ssh/config.d/devserver.conf"

    def options(self, alias, identity):
        config = self.directory / "config"
        config.write_text(
            f'Include "{self.fragment}"\n\n'
            "Host devserver devserver-lan devserver-ts\n"
            f"    IdentityFile {identity}\n\n"
            "Host unrelated\n"
            "    HostName other.invalid\n"
            "    User another-user\n\n"
            "Host *\n"
            "    ProxyJump bastion.invalid\n"
            "    ProxyCommand /usr/bin/false\n"
            "    StrictHostKeyChecking no\n"
            "    HostKeyAlias wrong-host\n"
        )
        result = subprocess.run(["/usr/bin/ssh", "-G", "-F", str(config), alias],
                                capture_output=True, text=True, check=True, timeout=5)
        return dict(line.split(" ", 1) for line in result.stdout.splitlines() if " " in line)

    def test_all_routes_share_trusted_identity_without_global_proxy(self):
        for alias, hostname in (("devserver", "100.103.159.2"),
                                ("devserver-lan", "192.168.1.159"),
                                ("devserver-ts", "100.103.159.2")):
            with self.subTest(alias=alias):
                options = self.options(alias, "/tmp/client-only-key")
                self.assertEqual(options["hostname"], hostname)
                self.assertEqual(options["hostkeyalias"], "100.103.159.2")
                self.assertEqual(options["hostkeyalgorithms"], "ssh-ed25519")
                self.assertIn(options["stricthostkeychecking"], ("yes", "true"))
                self.assertEqual(options.get("proxyjump", "none"), "none")
                self.assertEqual(options["port"], "22")
                self.assertEqual(options["identityfile"], "/tmp/client-only-key")
                self.assertEqual(options["user"], "vr-osxs")
                if alias == "devserver":
                    self.assertEqual(options["proxycommand"], "~/.local/bin/devserver-ssh-connect")
                else:
                    self.assertEqual(options.get("proxycommand", "none"), "none")

    def test_identity_path_remains_client_local(self):
        for identity in ("/tmp/first-client-key", "/tmp/second-client-key"):
            with self.subTest(identity=identity):
                self.assertEqual(self.options("devserver", identity)["identityfile"], identity)

    def test_unrelated_host_keeps_its_own_configuration(self):
        options = self.options("unrelated", "/tmp/client-only-key")
        self.assertEqual(options["hostname"], "other.invalid")
        self.assertEqual(options["user"], "another-user")
        self.assertNotIn("/tmp/client-only-key", options["identityfile"])
        self.assertNotEqual(options["hostkeyalias"], "100.103.159.2")


if __name__ == "__main__":
    unittest.main()
