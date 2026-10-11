# Portable Pi configuration

Mise deploys the reviewed settings, prompt templates, Herdr extension, and
non-skl skills from this repository. Skill directories use `symlink-each` so
`skl` can install its own skills alongside them without writing into the repo.
The tuicr skill is intentionally excluded.

After bootstrap, reconcile the packages declared in settings:

```sh
pi update --extensions
pi
```

Use `/login` on each machine. Authentication, sessions, trust decisions, caches,
and downloaded packages stay local; `.pi/agent/.gitignore` only allows the
portable resource categories. Review new files for secrets before committing.

Install skl and let it generate its skills and prompt adapters separately.
The retained custom prompts require skl and its ledger resources to be set up.
The tldraw skill additionally requires tldraw Desktop, Node.js, and curl.
The Herdr extension is active only when Pi is launched in a Herdr pane.

If existing files conflict during deployment, back up the conflicting settings,
prompts, extensions, or skill directories before retrying. Do not replace the
whole `~/.pi/agent` directory: it also holds credentials and session history.

Edits to deployed symlinks change their repository sources. Other files created
under the real agent and skill directories remain machine-local.
