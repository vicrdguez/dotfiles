---
description: Resolve skl slice conflicts while preserving both sides' intended behavior
argument-hint: "<PR-number> [context]"
---

GitHub has reported merge conflicts for this PR. Resolve them in the
associated skl Slice's worktree. The conflict report is the reason for this
invocation, not something to independently establish before proceeding.
When no Git operation is in progress, merge the latest base into the Slice
branch; leave the PR itself open for the human to merge.

PR: $1
Additional context: ${@:2}

Commands below use placeholders; replace them with discovered values.

1. Establish the workspace and operation.
   Accept the PR number with or without a leading `#`; use its numeric part
   as `<number>`. Identify the repository with
   `gh repo view --json nameWithOwner`, then inspect the PR:

   ```sh
   gh pr view <number> --repo <owner/repo> --json number,url,state,title,body,headRefName,headRefOid,headRepository,headRepositoryOwner,isCrossRepository,baseRefName,baseRefOid,closingIssuesReferences
   gh pr diff <number> --repo <owner/repo>
   git worktree list --porcelain
   ```

   Find the `<proposal>/<slice>` identity or bound `skl ledger show`
   command in the PR body. If absent, inspect its linked Slice issue:

   ```sh
   gh issue view <issue-url> --json number,title,body
   ```

   Use issue links from the PR body or closingIssuesReferences. Confirm the
   identity through `skl ledger show` in step 2, matching the recorded branch
   and issue to this PR. If no unambiguous Slice reference is available, ask
   for it; a PR or issue number is not a ledger item identity.

   Match the PR's head branch and repository to the Slice's existing
   worktree; its base branch identifies the target. Confirm the push remote
   matches the PR's head repository, including for a fork.
   Perform all edits, staging, Git operation completion, commits, and pushes
   from that worktree for the Slice's intended branch. Leave other worktrees
   and local branches unchanged.

   Inspect repository instructions and local state:

   ```sh
   git -C <worktree> status
   git -C <worktree> rev-parse HEAD
   ```

   If a merge or rebase is already in progress, identify its exact commits
   and confirm it updates this Slice against the intended target. Preserve
   existing resolution edits; use that operation rather than starting another.
   Ask if the operation's purpose is unclear or unrelated to this PR.

   Otherwise, require an open PR, a clean worktree and index, and local HEAD
   equal to the PR's headRefOid. Ask about any mismatch before changing files
   or history. Fetch the base branch from the confirmed base repository:

   ```sh
   git -C <worktree> fetch <base-repository-url> refs/heads/<baseRefName>
   git -C <worktree> rev-parse FETCH_HEAD
   ```

   Record that SHA as `<target-sha>` and use it throughout this resolution.
   Proceed when the workspace, Slice branch, and exact source and target
   commits are confirmed; name each side by its role rather than ours/theirs.

2. Recover intent.
   Read the Slice's frozen Contract and available implementation/review
   reports using its confirmed worktree path and ledger item:

   ```sh
   skl ledger show --repo <worktree> --item <proposal>/<slice>
   skl ledger show --repo <worktree> --item <proposal>/<slice> --phase implement
   skl ledger show --repo <worktree> --item <proposal>/<slice> --phase watchdog
   ```

   Treat the PR body as context, not a substitute for the frozen Contract.
   Compare the base with each side and inspect the commits behind the
   conflicting changes. Follow relevant target-side
   changes to their Slice Contracts and reports where available.
   Before editing, summarize the behavior each side contributes and any
   intentional removal or replacement. Ask if the requirements conflict
   or the available evidence cannot establish the intended result.

3. Resolve.
   If no operation is in progress, start the merge in the Slice's worktree:

   ```sh
   git -C <worktree> merge --no-ff --no-commit <target-sha>
   ```

   Conflicts are the expected input to resolution, not a reason to ask the
   human to start the merge. If Git reports another failure, diagnose it
   before proceeding. If the merge is clean, proceed to verification; if
   already up to date, report that no update was needed without creating
   an empty commit.

   Make the smallest coherent change that satisfies both sides' applicable
   requirements, including intentional removals. Preserve unrelated work
   and existing resolution edits. Inspect affected callers, tests, and
   related cleanly merged changes, not just conflict markers.
   Finish when every resolution is explained by the recovered intent;
   leave genuinely incompatible choices for the human.

4. Verify.
   Review the result against both sides for lost behavior and accidentally
   restored behavior. Run the repository's relevant checks, including tests
   of the interaction between the two changes; add focused regression
   coverage where needed.
   Proceed when all conflicts are resolved, required checks pass, and no
   human choices remain. Otherwise report the blocker and leave the work
   uncommitted and unpushed.

5. Commit and push.
   Confirm you are in the Slice's worktree on its intended branch. Stage
   only the resolved operation's changes, preserving unrelated work.
   Complete the Git operation and commit the verified result. If continuing
   exposes further conflicts, repeat the resolution and verification steps.
   Push to the Slice's remote branch without force. If pushing requires
   rewriting remote history or the destination is unclear, ask the human.
   Finish by reporting the commit, remote branch, verification results,
   and any follow-up review needed. Distinguish prior review evidence from
   verification of this resolution.

Leave PR merging and skl workflow-state changes to the human.
