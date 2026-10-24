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
count, else exit 5; the patch must be zero-context (`git diff -U0`, else exit 9); the tool prints every changed line so foreign lines show. Keep each hunk header as git printed it: the old-side start is checked against HEAD (a mismatch or overlap is exit 10), the new-side numbers are rebuilt.

Refuses (exit 1, nothing left staged) when:
- an engine-core path (`engine/src/engine/{core,expr,triggers,trigger_api}.cljs`, `registry.clj`,
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

### fixer-worktree.sh / land.sh

    tools/fixer-worktree.sh <card8>            # prints the worktree path; branch fix/<card8> from the main checkout's branch
    tools/fixer-worktree.sh --remove <card8>   # abandon: removes worktree and branch
    tools/land.sh [--rebased-ok]               # from the worktree

A fixer's own checkout (`$FIXER_WT_ROOT`, default `~/worktrees/fix-<card8>`; not under `~/.claude`, whose files the Write tool refuses). `node_modules`, `worlds/`, `state/`, `textures/` and the pathfinding fixtures are symlinks to the main checkout's, so tools and tests there write to the shared data; nothing is built until the first `tools/compile` there. `land.sh` takes the commit lock, rebases onto the main checkout's branch and fast-forwards it, then removes the worktree. Exit 3: it had to rebase, so rerun your tests and land again; 1: conflict (aborted) or dirty tree; 4: the main checkout has uncommitted edits to a file the landing changes. Tested by `land.test.mjs`.

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
whose repo path is the main checkout (also from a worktree), override with `CARD_REPO`). `CARD_AUTHOR` sets the note author (default
`agent`). The HTTP API cannot set the worker field, so `claim` sets `in_progress` and adds a note
"claimed by <worker>" authored by the worker. Needs `curl` and `jq`.

### compile / test-engine

    tools/compile engine|dashboard <build>      tools/test-engine <ns>... | --full | --golden | --changed [<git-rev>]

- `tools/compile` is the only user of `/tmp/mc-compile.lock` (and `.1`, `.2`: `MC_COMPILE_SLOTS`, default 3, compiles at once, one per checkout): never `flock` it or wrap it in `flock` (it exits 2 if you do).
- Run tests with `tools/test-engine engine.<ns>-test ...` (queues the compile, then runs node outside the lock; `--full` = whole suite in 4 shards, at most as many node processes at once machine-wide as free memory allows (flock slots in /tmp/mc-res; `--shards N --slots M --slowest K`); per-test ms in `engine/out/test-timings.jsonl`).
- A test run whose event loop drains before the last test ends (a promise that never settles) exits 1 and names that test (`engine.test-events`).
- A targeted run (`tools/test-run.mjs`) uses a private copy of the bundle (removed on every exit; copies of dead pids swept at start), needs `900 + 60 x namespaces` MB, and is killed (exit 124, `STALLED ... last finished test`) when no test has finished for max(90 s (300 s until the first test has finished), 10 x the slowest prior test of its namespaces; `MC_TEST_STALL_S` overrides), also when a test spins; a 3600 s whole-run cap remains (`MC_TEST_TIMEOUT_S`). Shards are watched the same way.
- `tools/test-engine --golden` runs the opt-in planner pins (`*-golden` namespaces, not in `--full`).
- `node tools/changed-tests.mjs [<rev>|<a>..<b>|--since-green] [--suites]` lists the tests affected by changed files, with the same selection as `tools/test-engine --changed [<rev>]`: cljs test namespaces that (transitively) require, or name by quoted symbol, a changed engine file; JS tests of changed `engine/` and `tools/` .mjs files and their importers. Changed = `<rev>` vs the working tree + untracked (default HEAD; on a `fix/*` branch the cut from job-contract/engine-mvp); `a..b` = committed files only. A change under `engine/src/jobs` or `engine/src/triggers` (loaded by directory, not required) also selects the registry, job-fuzz, settings and `*-contract-test` namespaces and the `fuzz` suite; a changed `engine/fixtures/world/*.edn` selects the `fixtures` suite (and tests that name it); changed `dashboard/` files the `dashboard` suite; a reached `*-golden` namespace the `golden` suite. `--suites` prints `engine|engine-js|tools|fixtures <args>` and argless `fuzz|dashboard|golden` for mcp run_tests. `--record-green [<rev>]` writes `.claude/integration-last-green` (rev + time) after a green run; `--since-green` diffs `<that rev>..HEAD`, FULL when the record is missing or not an ancestor. Over 60 namespaces it warns with the widest changed files (pick a subset). Build config changes (`shadow-cljs.edn`, `package.json`) print FULL.
- `tools/test-bisect <ns>... [--good <rev>] [--bad <rev>]` finds the first commit where the namespaces fail (bad = HEAD, good = HEAD~20): `git bisect run` in a throwaway worktree, each step compiles and runs that commit's test-engine; a busy compile slot is retried, commits before ef81c1c6 (retired `compile` slot kind) run under main's `server` slot and stop their servers per step, compile failures (error shown in the step line) and missing namespaces are skipped; prints the first bad commit, its failing tests and the step count; removes the worktree on exit, error and kill.
- Never hold the lock while testing; a waiting compile is not stuck, a `flock` wrapper is.

### res-slot

    tools/res-slot <body|tests|browser> [--need MB] -- <cmd...>      tools/res-slot status

- Heavy commands run under a machine-wide slot: waits for a free `/tmp/mc-res/<kind>.<n>` flock AND `MemAvailable - need - (needs of runs granted in the last 60 s) >= floor`; prints a status line every 60 s; after ~9 min exits 75 `busy, retry later` (just run it again). Slot is held by the command's process, so it frees on exit or crash; a `{start, pid, kind, cmd}` line goes to `/tmp/mc-res/log.jsonl` when the slot is taken.
- Kinds, need, max and the floor: `tools/res-slot.json` (body 400 MB x20, waits while the 1-minute load average plus 1 core per start still ramping is at 1.25 x cores (`maxLoadPerCore`, `loadCores`) (measured about 270 MB per body slot: body 150-190, harness and wrapper about 95), tests 1700 MB x8 (node heap capped at `heapMb` 1024) with `--full` shards on at most 6, browser 600 MB x2). Heavy runs (`--heavy`, `RES_SLOT_HEAVY=1`, or a `.res-slot-heavy` file in the cwd or a parent) may take the `reserve` slots of a kind (server 1, tests 1) and, while they wait, light runs of that kind leave the slots they try alone. `status` lists holders (pid, command, age); every finished run appends `{kind, needMb, waitedS, ranS, code}` to `/tmp/mc-res/log.jsonl`.
- `kit-trial.mjs`: repeatable fresh-body iron-kit trials (`spots`, `plan SPOT [--give ITEM[:N],...]`, `start SPOT [--give ...] --build DIR --go`, `judge RUN`): one new body name per run, one goal (`jobs.items.get-kit`), a clean criterion (kit within 3 in-game days, no death, intervention or crash, every give-up with a reason). `--give` hands items after the clear (recorded as `:given` in run.edn and the report). Runs live in `worlds/<world>/kit-trials/<run>/run.edn`; the body starts under `res-slot body`. Spots without coordinates must be set in `dashboard/src/world_test/kit_trial.cljs` first.
- `fresh-drop.mjs`: the fresh-drop soak (`plan|drop BODY --build DIR [--go]`, `report BODY... [--md]`). `drop` starts BODY with `--fresh` (state and memory wiped), clears it and sets it on the surface of a random spot at least 5000 blocks from every base other bodies recorded, with its spawn point there; only a body you created. `report` reads the event logs (start: the last `worlds/<world>/fresh-drops/` record, else the first event; `--from-ms` overrides) and gives per body: time to base (bed and home place), farm (20 wheat cut and replanted, a baby bred), kit (8 iron pieces), deaths, deaths carrying kit pieces, lead teleports (position jumps no respawn explains), jobs the agent queued, tokens (`--tokens`). Door, walls, pen and chest contents leave no event, so they are not checked.
- `event-counts.mjs`: recounts the measured combat priors from the trial bodies' event logs (`fold|creeper-exchange|flight-starts|notice-rate|drowned-meets|flee-pace|spot-returns|water-lags [--agents DIR] [--counts FILE] [--since-ms MS]`; spot-returns reads the logs alone, for `threat/spot-occupied`; water-lags the recorded chunks, for `answer/unseen-water-lags`) and folds each run's row into `worlds/test/priors-counts.edn` by run id, so counts grow as logs rotate (the debug logs, which hold attack and position actions, rotate in hours: run `fold` at least that often); the settings `threat/creeper-exchange-hp|swings` `danger/flight-starts` (per mob kind, censored starts kept, each kind shrunk toward the other melee kinds; its `:setting`) `death/notice-rate` `answer/drowned-meet-per-s|other-per-s`, `fight/drowned-wet-hits-per-swing` and `danger/flee-walk-speed|flee-stop-s` quote its k and n. drowned-meets also reads the recorded chunks (water class at fold time), gives the posterior (mode, mean, 90% interval, Gamma k and n) of `fight/drowned-wet-hits-per-swing` over the deaths the drowned dealt in sword fights swum in, and prices each first meet in water by cost.answer against its outcome (`:wet-death :rows`). Refuses a world-test bundle older than its sources.
- `sweep-triage.mjs --dir SWEEP_DIR --previous OLD_RESULTS... [--rev R] [--repo PATH] [--plans F.edn] [--out report.edn]`: sorts a sweep's failures into known (same failing check as the previous sweep) / new / flaky and reads the live cards (board, read-only) to list which to tick, send to pending (with their bug cards) or hold, and the new failures needing a bug card. A card whose plan names no fixture is `no-plan`: map it by hand in `--plans`.
- Already wrapped: `world-test.mjs` (one body slot for the whole run; `--allow-time` needs `--time-log` and shares the phase time lock (every case holds it; with `--allow-time` time-independent cases lock as :day, without it they join the current phase; joiners wait until the first holder has set and confirmed the phase; the first holder also sets `weather clear`, and a case with `:weather :rain|:thunder` holds the lock alone, sets that weather and clears it after, skipped without `--allow-time`; both logged in `--time-log`), so day and night runs may overlap by phase; hand-run `time set` goes through `tools/time-set.mjs`; `tools/world-sweep.mjs` runs the day, night and night-exclusive pool passes, then time-set day, and writes `DIR/sweep-summary.edn`), `test-engine` (tests), `tools/view/headless.mjs` (browser). Wrap other manual body starts and headless browsers yourself.

## Live testing

### world-test.mjs

    node tools/world-test.mjs [fixture.edn|dir ...] [--tag T] [--match TEXT] [--repeat N] [--body NAME] [--world test] [--first-plot I] [--card ID] [--results FILE] [--list] [--check] [--allow-time --time-log F]

Runs world fixtures (live cases as EDN, `engine/fixtures/world/`, format in its README) on a reserved plot grid
(x/z 20000..20640, y 150) with one probe body (`ProbeFixture` by default): builds each case's plot, starts the body
per register, runs the act, judges expectations from the body's event log plus RCON checks, cleans up, prints PASS/FAIL
with evidence. `--bodies N` (with `--body PREFIX`, bodies PREFIXA, PREFIXB, ...; `--durations RESULTS.edn` repeatable) runs the fixture files across up to N bodies (capped at the body slot max minus one and by the memory above the res-slot floor / the body's `needMb`, not by cores; `--max-parallel M` overrides; new children wait only while load > cores, longest first, one plot block of 20 per body), reruns each failed case on another body (`--retry-failed N`, default 1, owned by the pool; exact `--match-id`, one run; a pass is `:flaky`); with `TEST_EVENTS=1` the pool prints the `@@test` plan (all cases, from `--list`, fixed up front), one result per case with its final status and progress `{done total retries}`, children silent, and writes one merged `--results`; other flags (`--phase`, `--allow-time`) go to every child, so one call is one time phase (`tools/world-test-pool.mjs`); a body that dies at boot twice (child exit 6) is retired and its cases go to the others (all gone: exit 6; night batches too); with `--results FILE` each failed, errored or flaky case's trace (body events from its setup, `body.log` tail, capped) is written to `<dir of FILE>/cases/` and named `:trace` in its entry. `--match-id ID` is repeatable and `--match-ids FILE` (both also on `world-test.mjs`) takes ids one per line or from a `--results` .edn (its non-pass ids); only those cases run and `--list` counts only them. `--check` only loads and validates the fixtures (no body, server or slot): one result per case, exit 1 on a parse error or problem. Each result and its log line carry `:body-cpu-s` (`/proc` CPU of all the body's threads during the case: `:main`, `:v8` workers, `:other`, `:total`) and `:load1`; `--cpu-prof` sees only `:main`. Logic in cljs (`dashboard/src/world_test/`), compiled ahead of time: `tools/compile dashboard world-test`.

### cart-sample.mjs

    node tools/cart-sample.mjs (--uuid U | --rider PLAYER | --near X Y Z) [--secs 30] [--until-stop] [--window 10] [--cell X Z --threshold 0.3] [--out F.jsonl]

Follows one minecart over RCON (game time, Pos, Motion each tick; `--near` pins the nearest minecart once, `--rider` the cart
the player rides). JSON lines to `--out` or stdout; prints the minimum windowed speed (blocks/tick, 3D path length over `--window` ticks, so bends read true)
and where, per-cell speeds, and the path distance from `--cell` until the speed is back at `--threshold`. Logic in
`dashboard/src/dashboard/rcon_cart.cljs`; rebuild the bundle with `tools/compile dashboard rcon-tools --release`.

### base-rates.mjs

    node tools/base-rates.mjs /path/to/mojang_<version>.jar

Writes `engine/src/jobs/lib/cost/base_rates.json` (ore placements per biome, read by `jobs.lib.cost.base-rates`) from the
worldgen JSON in Mojang's server jar. Run it when the game version changes; nothing else needs the jar. A placement
step it cannot price stops it.
