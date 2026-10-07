# tools/

## Process helpers (shared checkout)

### commit-mine

    tools/commit-mine --card <id-or-prefix> -m <msg-file-or-text> --expect-lines L <paths...> [--approved-core <card>]
    tools/commit-mine --card <id> -m <msg> --hunks <patchfile> --expect-hunks N <paths...>

Takes the `.git/commit-lock` (mkdir lock holding the owner's pid, backoff up to 600 s; a dead owner's lock is reclaimed, `COMMIT_LOCK_TIMEOUT` seconds to change),
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
Exit 8: the message lacks `Card <first 8 chars of --card>` (also `card`, `(card ...)`, `cards`; a longer id matches).
After the commit the stat is printed and posted as a note on the card (`COMMIT_MINE_NOTE_TIMEOUT` s, default 15); a board failure only warns.

Every commit appends a JSON line to `.git/commit-ledger.jsonl`:
`time, hash, card, paths, insertions, deletions, stat, flags` (flags: hunks-mode, engine-core approvals,
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

    tools/compile engine|dashboard <build>      tools/test-engine <ns>... | --full | --golden | --changed [<git-rev>]

- `tools/compile` is the only user of `/tmp/mc-compile.lock`: never `flock` it or wrap it in `flock` (it exits 2 if you do).
- Run tests with `tools/test-engine engine.<ns>-test ...` (queues the compile, then runs node outside the lock; `--full` = whole suite in 4 shards, at most as many node processes at once machine-wide as free memory allows (flock slots in /tmp/mc-res; `--shards N --slots M --slowest K`); per-test ms in `engine/out/test-timings.jsonl`).
- A targeted run (`tools/test-run.mjs`) uses a private copy of the bundle (removed on every exit; copies of dead pids swept at start), needs `900 + 60 x namespaces` MB, and is killed (exit 124, `TIMEOUT ... last finished test`) after `60 s + 5 x` its prior timing (180-1200 s; `MC_TEST_TIMEOUT_S` overrides). Shards are timed out the same way.
- `tools/test-engine --golden` runs the opt-in planner pins (`*-golden` namespaces, not in `--full`).
- `tools/test-engine --changed [<git-rev>]` runs only the cljs test namespaces that (transitively) require, or name by quoted symbol, the changed files, plus own/changed test files and the JS tests covering changed JS; changed = working tree + staged + untracked vs HEAD (or vs `<git-rev>`). Prints the list and count first. Build config changes (`shadow-cljs.edn`, `package.json`) run the full suite.
- `tools/test-bisect <ns>... [--good <rev>] [--bad <rev>]` finds the first commit where the namespaces fail (bad = HEAD, good = HEAD~20): `git bisect run` in a throwaway worktree, each step compiles and runs that commit's test-engine; a busy compile slot is retried, commits before ef81c1c6 (retired `compile` slot kind) run under main's `server` slot and stop their servers per step, compile failures (error shown in the step line) and missing namespaces are skipped; prints the first bad commit, its failing tests and the step count; removes the worktree on exit, error and kill.
- Never hold the lock while testing; a waiting compile is not stuck, a `flock` wrapper is.

### res-slot

    tools/res-slot <body|tests|browser> [--need MB] -- <cmd...>      tools/res-slot status

- Heavy commands run under a machine-wide slot: waits for a free `/tmp/mc-res/<kind>.<n>` flock AND `MemAvailable - need - (needs of runs granted in the last 60 s) >= floor`; prints a status line every 60 s; after ~9 min exits 75 `busy, retry later` (just run it again). Slot is held by the command's process, so it frees on exit or crash; a `{start, pid, kind, cmd}` line goes to `/tmp/mc-res/log.jsonl` when the slot is taken.
- Kinds, need, max and the floor: `tools/res-slot.json` (body 500 MB x10, tests 2800 MB x5 with `--full` shards on at most 3, browser 600 MB x2). `status` lists holders (pid, command, age); every finished run appends `{kind, needMb, waitedS, ranS, code}` to `/tmp/mc-res/log.jsonl`.
- Already wrapped: `world-test.mjs` (one body slot for the whole run; `--allow-time` needs `--time-log` and shares the phase time lock (every case holds it; with `--allow-time` time-independent cases lock as :day, without it they join the current phase; joiners wait until the first holder has set and confirmed the phase), so day and night runs may overlap by phase; hand-run `time set` goes through `tools/time-set.mjs`), `test-engine` (tests), `tools/view/headless.mjs` (browser). Wrap other manual body starts and headless browsers yourself.

## Live testing

### world-test.mjs

    node tools/world-test.mjs [fixture.edn|dir ...] [--tag T] [--match TEXT] [--repeat N] [--body NAME] [--world claude] [--first-plot I] [--card ID] [--results FILE] [--list] [--check] [--allow-time --time-log F]

Runs world fixtures (live cases as EDN, `engine/fixtures/world/`, format in its README) on a reserved plot grid
(x/z 20000..20640, y 150) with one probe body (`ProbeFixture` by default): builds each case's plot, starts the body
per register, runs the act, judges expectations from the body's event log plus RCON checks, cleans up, prints PASS/FAIL
with evidence. `--check` only loads and validates the fixtures (no body, server or slot): one result per case, exit 1 on a parse error or problem. Logic in cljs (`dashboard/src/world_test/`), compiled ahead of time: `tools/compile dashboard world-test`.

### cart-sample.mjs

    node tools/cart-sample.mjs (--uuid U | --rider PLAYER | --near X Y Z) [--secs 30] [--until-stop] [--window 10] [--cell X Z --threshold 0.3] [--out F.jsonl]

Follows one minecart over RCON (game time, Pos, Motion each tick; `--near` pins the nearest minecart once, `--rider` the cart
the player rides). JSON lines to `--out` or stdout; prints the minimum windowed speed (blocks/tick, 3D path length over `--window` ticks, so bends read true)
and where, per-cell speeds, and the path distance from `--cell` until the speed is back at `--threshold`. Logic in
`dashboard/src/dashboard/rcon_cart.cljs`; rebuild the bundle with `tools/compile dashboard rcon-tools --release`.
