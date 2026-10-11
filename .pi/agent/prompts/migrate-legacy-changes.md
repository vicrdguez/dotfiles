---
description: One-time cutover of a repository's legacy .changes/<slug> work into the private Workflow Ledger
argument-hint: "[repo-path] [remote]"
---

You are the administrative agent for a one-time cutover. Move one Consumer Repository's legacy workflow work into the private Workflow Ledger:

- The legacy model is forge-authoritative: `.changes/<slug>` contracts at an Artifact Baseline, GitHub labels as state, and PR bodies and comments as reports.
- The repository is `${1:-the current working directory}`, and its GitHub remote is `${2:-origin}`.

This migration runs once per repository. It applies only while the repository has no Project in the ledger. Once a repository has been migrated, this prompt no longer applies to it.

Your job is **fidelity**:

- Every accepted obligation, known state, issue/PR attachment, report and actual code revision arrives in the ledger exactly as the legacy record shows it.
- Every artifact you choose not to carry over is accounted for with a reason.
- Anything ambiguous, missing or contradictory goes to the human as a question. The human's answer is the only authority for resolving it.

You are the one sanctioned exception to "only `skl` writes the ledger". Use native `skl` commands wherever one exists. Write records directly only where no command expresses the fact. All writes go to a disposable rehearsal ledger until the human approves the apply in Phase 7.

Keep every working file in one scratch directory: `<TMPDIR>/skl-migration/<repo>-<UTC timestamp>/`, referred to below as `<scratch>`.

## Phase 0 — Preconditions

1. Resolve these values:
   - `<repo-root>`: `git rev-parse --show-toplevel` in the repository.
   - `<owner>/<name>`: from the selected remote's GitHub URL.
   - `<project>` = `<name>`.
2. Confirm `skl ledger` lists `accept`, `publish`, `show` and `present`, and that `skl decision` lists `inbox` and `apply`.
3. Read the ledger path from `skl/config.json` under `XDG_CONFIG_HOME`, or under `~/.config` when `XDG_CONFIG_HOME` is unset. Call it `<ledger>`.
4. Check that `<ledger>` is clean. Then `git -C <ledger> fetch` and confirm that local and upstream point at the same commit.
   - Behind: the human decides whether to fast-forward first.
   - Diverged: stop.
5. **Already-migrated guard.** If `projects/<project>/` exists at `<ledger>` HEAD, with or without `project.json`, stop now:
   - Report that `<project>` is already on the private ledger and this one-time migration does not apply to it.
   - Write nothing.
6. Ask the human to confirm that no implement or watchdog worker, loop or session is running for this repository. Show every issue and PR carrying `wip` as evidence.
7. Record `<ledger>` HEAD as `<start>`.

Check: the guard passed, the human confirmed workers are stopped, and `<start>` is recorded.

## Phase 1 — Inventory

Collect every legacy artifact into `<scratch>/inventory.md`, one row per artifact, with its source location.

**Forge.** Use `gh` with pagination:

- Every issue and PR, open or closed, that carries a legacy workflow label: `ready`, `wip`, `review`, `rework`, `sync`, `needs-human` or `done`.
- Parent issues and their sub-issues.
- Native blocked-by relationships, and "Blocked by" lines in bodies.
- The Branch / Artifact Baseline / Implementation Ledger lines in issue bodies.
- For each such PR: head and base refs, head SHA, merge state and merge commit.
- The full comment timeline of each such PR:
  - `## Watchdog verdict: …` comments with their `Reviewed head:`
  - `Rework: <a>...<b>` comments
  - human dispositions (`W<n> waive/block/…`)
  - body edits, via GraphQL `userContentEdits`

**Proposal membership.** Include every sibling issue and PR of a parent that has at least one open legacy item, including merged and closed siblings.

**Git**, in `<repo-root>`:

- `.changes/*` and `docs/loom/changes/*` on main, on every local and remote branch, and in every `.worktrees/*`.
- `.changes/archive/*`.
- `[baseline] <slug>` and `[completion] <slug>` commits.
- Each `.worktrees/<branch>` git dir's `.watchdog` file (`<count>:<reviewed-sha>`).

Give each row exactly one disposition:

- **migrate**
- **skip**, with the reason. Examples: an intent-only draft that was never published; a proposal whose items are all merged or closed; an archive.
- **needs human**, with the question.

If nothing is marked **migrate**, stop. Report "nothing to migrate: the next Propose adopts this repository" and write nothing.

Check: every legacy label, `.changes` path, worktree and PR in scope appears in exactly one row with a disposition.

## Phase 2 — Classify each slice

Map each **migrate** slice from its legacy evidence:

| Legacy evidence | Ledger state | Records to carry |
|---|---|---|
| issue `ready`, no branch progress | `ready_for_implementation` | Contract |
| `ready` + `wip`, or branch commits past the baseline and no PR | `ready_for_implementation`, no Claim | Contract. The planned branch is the existing branch, whose progress `skl implement prepare` reuses. |
| PR `review` | `awaiting_review` | Contract, submission, review chain |
| PR `rework` | `rework` | Contract, submission, review chain ending in a `rework` watchdog report |
| PR or issue `needs-human` | `needs_human` | Contract, any submission, review chain ending in the blocking `needs_human` report |
| PR `done` | `ready_for_merge` | Contract, submission, implement report and `pass` watchdog report |
| sibling PR merged, or closed unmerged | observed by `skl status` | Contract, submission |
| sibling issue closed, no PR | `superseded` | Contract |

These cases go to the human:

- `sync`
- contradictory or unknown labels
- a `needs-human` item with no identifiable blocking request
- an integration target other than the PR base

**Review chain.** Carry the latest result only, as the minimal reference-consistent chain behind the current state, in chronological order:

1. **I₁**: the implementation result that the latest completed review inspected. Normally this is the PR body, or the `Rework:` comment of that round.
2. **W**: the latest completed review. Its `round` is the legacy Review Count: completed reviews, including Needs Human; interrupted attempts and repeated publication don't count.
   - Take the count from `.watchdog`, cross-checked against the verdict comments. A disagreement goes to the human.
3. **D**: the human's answer to **W**, when **W** is `needs_human` and the human answered it.
4. **I₂**: the implementation result produced after **W**, when one exists.

With no completed review, the chain is the latest implementation result alone. Earlier rounds are cited by URL only.

**Revisions.** Every source revision must be a full 40-character SHA; expand short ones with `git rev-parse` in `<repo-root>`. **W**'s reviewed head must equal **I₁**'s head, and for a non-`pass` **W** it must also equal **W**'s head. Take each target from the observed Integration Target the legacy report states. Unresolvable or mismatched revisions go to the human.

**Dependencies.**

- An edge to an item that is migrated in this run becomes `proposals/<proposal>/<slice>`.
- An edge to already-merged work outside this run is satisfied, so drop it. Record the dropped edge with its issue number, slug and merge commit.
- An edge to unmerged work outside this run goes to the human.

Check: each **migrate** slice has a target state, a complete chain with full SHAs, and resolved dependencies.

## Phase 3 — Recover each Contract byte-exactly

1. Take the Artifact Baseline, in priority order:
   1. the issue body's Artifact Baseline line;
   2. the `[baseline] <slug>` commit;
   3. a SHA the human confirms.
2. Read each file with `git show <baseline>:<dir>/<slug>/<file>`, where `<dir>` is `.changes` or `docs/loom/changes` as the history shows. Keep the bytes exactly.
   - The Contract is `intent.md`, `behavior.md`, and `plan.md` / `tasks.md` when present.
   - `acceptance.md` is review output. It belongs verbatim in the `pass` watchdog report body.
3. Diff the baseline against every later version: the `[completion]` snapshot, main, and the branch. Differences limited to `[ ]`→`[x]` ticks are expected. Any other difference goes to the human, because a changed Contract calls for a renewed proposal, not migration.

Keep the labels exactly as written. Legacy `tasks.md` uses `B/C/D<n>` for behaviour, chore and doc tasks. Record that convention in the provenance of `proposal.md`.

Check: each slice has its Contract files at a named baseline SHA, and every non-tick difference is resolved by the human.

## Phase 4 — Migration plan and approval

Write `<scratch>/plan.md` and present it.

**Proposals.**

- A single-slice proposal is named after its slug.
- A multi-slice proposal gets a kebab-case name derived from its parent issue title, which the human confirms. Its `parent_title` is the parent issue's current title.
- Names must be unused under `projects/<project>/proposals/` and `archive/`.
- `accepted` is the earliest legacy issue `createdAt` in the proposal, in RFC3339 UTC.

**Slices.**

- The title is the issue's current title.
- The planned branch is the legacy branch. Branches are unique within the Project.
- Dependencies kept, and dependencies dropped.
- Target state, submission PR, and integration target (the PR base).
- Each chain record, with its source URL and SHAs.

**Human answers (D).**

- Quote each answer verbatim with its URL.
- The human confirms each one explicitly as their authorized direction, and chooses its route: `implement`, `watchdog` or `supersede`. A forge comment carries no authority until then.

**Other items.**

- Skipped items, with reasons.
- Open questions.
- The forge actions (Phase 8) and source actions (Phase 9) that will follow the apply.

Check: the human has approved the plan, and every **needs human** row is resolved or explicitly excluded.

## Phase 5 — Rehearse in a disposable ledger

### Set up the rehearsal ledger

1. `git clone <ledger> <scratch>/ledger`
2. `git init --bare <scratch>/remote.git`
3. Point the rehearsal clone's `origin` at `<scratch>/remote.git`, push `HEAD`, and set it as upstream.
4. Write `<scratch>/config/skl/config.json` as `{"ledger": "<scratch>/ledger"}`.
5. Export `GH_TOKEN` from `gh auth token`.
6. From here to Phase 7, run every `skl` command with `XDG_CONFIG_HOME=<scratch>/config`.

### Migrate each proposal

Work through the proposals in dependency order. For each one:

1. **Intake.** Create `<scratch>/intake/<proposal>/` with:
   - `proposal.json`: `{"proposal", "parent_title" (multi-slice only), "slices": [{"name", "title", "branch", "depends"}]}`
   - `proposal.md`: the parent and slice issue descriptions verbatim, then a `## Migration provenance` section. It lists legacy URLs, baselines, dropped dependencies and the label convention.
   - `<slice>/` with the Contract files from Phase 3.
2. **Accept.** Run `skl ledger accept --repo <repo-root> --remote <remote> --proposal-dir <scratch>/intake/<proposal>`, with no `--issue` and no `--parent-body`, so nothing is created on the forge. Expect `accepted`. Record the accept commit.
3. **Attachments.** Write them directly and commit as `migrate <project>/<proposal> attachments`:
   - `proposal.json`: `accepted`, and `parent_issue`.
   - Each `state.json`: `issue`, `submission`, and `integration_target`.
   - List the sources in the commit body.
4. **Chains.** Write each slice's chain as one commit per record, in order. Subjects are `migrate <project>/<proposal>/<slice> <record>`, and each body gives the source URL.
   - **I₁ / I₂ and W** go directly into `implement-report.md` / `watchdog-report.md`:
     - Frontmatter follows `skl skill --resource reference/report-schema.md implement`.
     - `ledger.claim` names the `state.json` at the previous commit.
     - `ledger.contract` names the Contract files at the accept commit.
     - `ledger.implement`, `ledger.watchdog` and `ledger.decision` name the chain records this result consumed.
     - The body opens with `> Migrated from <url>. No ledger Claim existed; the claim reference names the migration record. Earlier rounds: <urls>.` Then comes the legacy text verbatim.
     - Commit the report together with `state.json`, set to the state that result leaves (see **Record shapes**).
   - **D** goes through `skl decision apply --project <project> --item <proposal>/<slice> --request-commit <W commit> --request-path <W path> --route <route> --answer <file>`. The answer file holds the human's answer verbatim.
5. **Terminal siblings.** For a sibling PR that is merged or closed unmerged, run `skl status --repo <repo-root> --remote <remote> --item <proposal>/<slice>`. The engine then records `merged` or `superseded` together with its completion evidence. A sibling issue closed without a PR gets a direct `superseded` state commit.

### Record shapes

Model each direct write on these shapes. Native `claim`, `record` and `observe completion` commits in `<ledger>` history show real examples.

- `projects/<project>/project.json`: `{"repository": "<owner>/<name>"}`. `skl ledger accept` writes it.
- `proposal.json`: `accepted`, optional `parent_title`, and optional `parent_issue` as `{"repository", "number"}`.
- `state.json`:
  - `state`, `title` and `branch`
  - optional `dependencies`
  - `issue` and `submission` as `{"repository", "number"}`
  - `integration_target` as `{"repository", "branch"}`
  - optional `decision: true`
  - Leave `claim`, `completion` and `publication` to the engine.
- States: `ready_for_implementation`, `awaiting_review`, `rework`, `needs_human`, `ready_for_merge`, `merged`, `superseded`.
- What each result leaves:
  - implement `awaiting_review` → `awaiting_review`
  - implement `needs_human` → `needs_human`
  - watchdog `pass` → `ready_for_merge`
  - watchdog `rework` → `rework`, or `needs_human` when its round is 2 or more
  - watchdog `needs_human` → `needs_human`
- The `decision` marker: `skl decision apply` sets it. An implement `awaiting_review` result keeps it. A watchdog result, or an implement `needs_human` result, clears it.
- JSON files are two-space indented and end with a newline.

Check: the rehearsal clone is clean, and every record in `plan.md` exists at its path.

## Phase 6 — Verify the rehearsal

Every check must pass. Fix the rehearsal, or rebuild it from Phase 5, until they all do.

1. **Accounting.** Every **migrate** row in `inventory.md` maps to a ledger path. Every other row keeps its reason.
2. **Contract bytes.** For every Contract file, `cmp` the output of `git -C <scratch>/ledger show <accept commit>:<path>` against `git -C <repo-root> show <baseline>:<legacy path>`.
3. **Readback.**
   - `skl ledger show --repo <repo-root> --remote <remote> --item <proposal>/<slice>` succeeds for every slice.
   - `--phase implement` and `--phase watchdog` return every written report.
4. **Inbox.** `skl decision inbox --project <project>` lists exactly the expected `needs_human` slices, each with its blocking report.
5. **Engine probe.**
   1. Clone `<scratch>/ledger` into `<scratch>/probe`, with its own `<scratch>/probe-config`.
   2. Repeat `skl implement next --repo <repo-root> --remote <remote>` until it reports no work. Then do the same with `skl watchdog next`.
   3. The claimed items must equal the expected eligible sets of each lane.
   4. Delete `<scratch>/probe`. It never feeds the apply.
6. **Independent fidelity review.**
   - Dispatch a fresh-context subagent with `inventory.md`, `plan.md`, access to the legacy sources, and `git -C <scratch>/ledger log -p <start>..HEAD`.
   - It reports every obligation, state, attachment, revision or piece of evidence that was omitted, weakened, strengthened or invented.
   - With no subagent tool available, ask the human to run this review in a fresh Pi session and paste back its findings.
7. **Approval.** Present a verification summary. The human approves the apply.

Check: all seven checks pass and the human has approved the apply.

## Phase 7 — Apply

1. Confirm that `<ledger>` is clean, HEAD equals `<start>`, and a fresh `git fetch` shows upstream unchanged. If anything moved, rebuild the rehearsal from Phase 5.
2. Run `git -C <ledger> fetch <scratch>/ledger HEAD`, then `git -C <ledger> merge --ff-only FETCH_HEAD`, then `git -C <ledger> push`.
3. Using the real configuration, repeat Phase 6 checks 3 and 4.

Check: `<ledger>` HEAD equals the rehearsal HEAD, and the push succeeded or is reported as pending.

## Phase 8 — Refresh the forge

Ask the human to confirm each step before running it.

1. **Issues.** For each proposal:
   1. Read `skl skill --resource reference/issue-publication.md --input proposal=<proposal> --input repo=<repo-root> --input remote=<remote> propose`.
   2. Write fresh issue prose, plus a parent body for a multi-slice proposal.
   3. Run `skl ledger publish --repo <repo-root> --remote <remote> --proposal <proposal> --issue <slice>=<file> … [--parent-body <file>]`.
2. **PRs.** For each slice with a submission:
   1. Run `skl ledger present --repo <repo-root> --remote <remote> --item <proposal>/<slice>` without a body, to get its evidence and guidance.
   2. Write fresh public prose.
   3. Tell the human that `present` pushes the recorded revision and sets draft or ready, then run it with `--public-body <file>`.
3. **Labels.** Remove the legacy workflow labels (`ready`, `wip`, `review`, `rework`, `sync`, `needs-human`, `done`) that are present on migrated issues and PRs, using `gh issue edit` / `gh pr edit --remove-label`. Other labels and the label definitions stay as they are.

Check: each issue, PR and label action is reported with its outcome.

## Phase 9 — Offer the source cleanup commit

1. List every migrated slice whose `.changes/<slug>/` or `docs/loom/changes/<slug>/` is still present on the local main of `<repo-root>`.
2. With the human's confirmation, make one commit on local main that removes exactly those directories: `Retire legacy change artifacts migrated to the Workflow Ledger`. Leave it unpushed; the human pushes it.
3. Leave these untouched, and list them: archives, slice branches, worktrees and unpublished drafts.

Check: the commit, if made, removes only the listed directories.

## Phase 10 — Report

Write `<scratch>/report.md` and summarise it for the human. It covers:

- each slice's legacy identity, ledger path, state and ledger commits;
- dropped dependencies;
- skipped and excluded items, with reasons;
- forge and source actions, both taken and declined;
- replication status;
- next steps: `skl decision inbox --project <project>`, `skl implement next`, `skl watchdog next` and `skl status`.

## Boundaries

Through every phase:

- Legacy evidence is carried verbatim. Anything the record doesn't show is asked of the human, never supplied from inference.
- The real `<ledger>` changes only through the Phase 7 fast-forward. Ledger history only moves forward: no force push, rebase or reset.
- Source branches, worktrees and pushed source history stay as they are. The only source change is the confirmed Phase 9 commit.

When every legacy repository has been migrated, the human deletes this prompt.
