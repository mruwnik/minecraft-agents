# tools/

## Process helpers (shared checkout)

### commit-mine

    tools/commit-mine --card <id-or-prefix> -m <msg-file-or-text> --expect-lines L <paths...> [--approved-core <card>]
    tools/commit-mine --card <id> -m <msg> --hunks <patchfile> --expect-hunks N <paths...>

Takes the `.git/commit-lock` (mkdir lock, backoff up to 600 s, `COMMIT_LOCK_TIMEOUT` seconds to change),
stages exactly the given paths (`git add -A -- paths`; `--hunks` adds `git apply --cached <patch>` for
files that hold others' edits), and commits. The lock is always released (trap). The commit message gets the
Co-Authored-By trailer if missing. Never stashes, resets, amends or pushes.

Path mode commits WHOLE files, which would sweep in other agents' uncommitted hunks. So `--expect-lines L` is
required: L = added+deleted lines `git diff --numstat <paths>` shows (an untracked file counts its lines). Without
it, or on a mismatch, the tool prints the per-file counts and exits 5 before staging anything. Check `git diff
<path>`; if all lines are yours rerun with the printed L. If not: `git diff <path> > my.patch`, delete the foreign
hunks from the patch (only hunks you wrote), and commit with `--hunks my.patch`.

Hunks mode commits exactly the patch (the given paths are not staged whole). The paths are required and must
cover every file in the patch, else exit 3 listing the others. `--expect-hunks N` must equal the patch's hunk
count, else exit 5; the tool prints each hunk's path and first changed line so foreign hunks show.

Refuses (exit 1, nothing left staged) when:
- an engine-core path (`engine/src/engine/{core,expr,triggers,takeover,trigger_api}.cljs`, `registry.clj`,
  `registry.cljs`) is given without `--approved-core <card>`;
- a new or largely changed (40+ added lines and at least half the file) `.js/.mjs/.cjs` file has no `Why JavaScript:` line;
- a path under `docs/` or `.claude/` is given without `--owner-said <card>` (committed only when the owner says so);
- an added line or the message holds the owner's first name as a whole word (words like "danger" are fine;
  `/home/<user>/` path segments are ignored).

Foreign staged paths (someone staged outside the lock): in path mode they are left out via `commit --only` and
flagged; in `--hunks` mode the run refuses (exit 3) and unstages its own hunks, so retry later.
Exit 4: the lock could not be taken. Exit 6: a staged `.js/.mjs/.cjs` blob fails `node --check` (nothing committed).

Every commit appends a JSON line to `.git/commit-ledger.jsonl`:
`time, hash, card, paths, insertions, deletions, flags` (flags: hunks-mode, engine-core approvals,
foreign-staged, large-deletion >= 500).

### ledger

    tools/ledger [--since <hash|ISO-time>] [--flagged]

Prints ledger rows (hash `--since` means commits after it on HEAD). `--flagged` keeps rows with any flag.

### card

    tools/card id <prefix|title words>     full card id (errors when none or ambiguous)
    tools/card show <id|prefix>            title, status, worker, last 3 notes
    tools/card note <id> <text>
    tools/card move <id> <status> [note]
    tools/card claim <id> <worker>

Talks to the differ HTTP API (default `http://localhost:8576`, override with `DIFFER_URL`; board is the one
whose repo path is this checkout, override with `CARD_REPO`). `CARD_AUTHOR` sets the note author (default
`agent`). The HTTP API cannot set the worker field, so `claim` sets `in_progress` and adds a note
"claimed by <worker>" authored by the worker. Needs `curl` and `jq`.

### compile / test-engine

    tools/compile engine|dashboard <build>      tools/test-engine <ns>... | --full

- `tools/compile` is the only user of `/tmp/mc-compile.lock`: never `flock` it or wrap it in `flock` (it exits 2 if you do).
- Run tests with `tools/test-engine engine.<ns>-test ...` (queues the compile, then runs node outside the lock; `--full` = whole suite).
- Never hold the lock while testing; a waiting compile is not stuck, a `flock` wrapper is.

## Live testing

### world-test.mjs

    node tools/world-test.mjs [fixture.edn|dir ...] [--tag T] [--match TEXT] [--repeat N] [--body NAME] [--world claude] [--first-plot I] [--card ID] [--results FILE] [--list] [--allow-time --time-log F]

Runs world fixtures (live cases as EDN, `engine/fixtures/world/`, format in its README) on a reserved plot grid
(x/z 20000..20640, y 150) with one probe body (`ProbeFixture` by default): builds each case's plot, starts the body
per register, runs the act, judges expectations from the body's event log plus RCON checks, cleans up, prints PASS/FAIL
with evidence. Logic in cljs (`dashboard/src/world_test/`), compiled ahead of time: `tools/compile dashboard world-test`.
