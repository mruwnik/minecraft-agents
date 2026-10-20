#!/usr/bin/env bash
# Tests for tools/compile lock handling and tools/test-engine, with a temp lock/queue and a stub npx (never the real lock).
set -uo pipefail
TOOLS="$(cd "$(dirname "$0")" && pwd)"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
export MC_COMPILE_LOCK="$T/lock" MC_COMPILE_QUEUE="$T/queue" MC_COMPILE_POOL_SIZE=0   # the pool has its own section below
fail=0
# gone PID [DEADLINE_S]: poll until PID is dead (rc 0) or the deadline passes (rc 1); a bounded wait, not a fixed sleep
gone() { local i; for ((i=0; i<${2:-20}*10; i++)); do kill -0 "$1" 2>/dev/null || return 0; sleep 0.1; done; return 1; }
# seen FILE PATTERN [DEADLINE_S]: poll until FILE holds PATTERN
seen() { local i; for ((i=0; i<${3:-20}*10; i++)); do grep -q "$2" "$1" 2>/dev/null && return 0; sleep 0.1; done; return 1; }
check() { if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: want [$3] got [$2]"; fail=1; fi; }

# Fake repo: tools copied in, stub npx, a live "server" pid so no server start.
mkdir -p "$T/repo/tools" "$T/repo/dashboard/.shadow-cljs" "$T/repo/engine/out/test/cljs-runtime" "$T/bin" "$T/res"
cp "$TOOLS/compile" "$TOOLS/test-engine" "$TOOLS/test-run.mjs" "$TOOLS/res-slot" "$TOOLS/res-slot.mjs" "$TOOLS/res-slot.json" "$TOOLS/compile-idle-watch" "$TOOLS/bundle-jobs-check" "$T/repo/tools/" 2>/dev/null
printf '{"floorMb":0,"kinds":{"tests":{"needMb":1,"max":1}}}' > "$T/res/cfg.json"
export RES_SLOT_DIR="$T/res" RES_SLOT_CONFIG="$T/res/cfg.json"
sleep 300 & SRV=$!; trap 'kill $SRV 2>/dev/null; rm -rf "$T"' EXIT
echo $SRV > "$T/repo/dashboard/.shadow-cljs/server.pid"
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\n' "$T" > "$T/bin/npx"; chmod +x "$T/bin/npx"
printf 'function SHADOW_IMPORT() {} console.log("NODE", process.argv.slice(1).join(" "), process.execArgv.join(" ")); if (process.argv.join(" ").includes("hang-test")) setInterval(() => {}, 1000); if (process.argv.join(" ").includes("stall-test")) { require("fs").writeFileSync(process.env.STALL_PID_FILE, String(process.pid)); require("fs").appendFileSync(process.env.MC_TEST_TIMINGS, JSON.stringify({started: 1}) + "\\n"); for (;;); }\n' > "$T/repo/engine/out/test.cjs"
export PATH="$T/bin:$PATH"
C="$T/repo/tools/compile"

# plain compile works and takes the lock itself
timeout 20 "$C" engine test >/dev/null 2>&1; check "plain compile rc" "$?" 0
check "stub npx ran" "$(grep -c 'shadow-cljs compile test' "$T/npx.log")" 1
timeout 20 "$C" dashboard test >/dev/null 2>&1; check "dashboard test is the :dashboard-test build" "$(grep -c "shadow-cljs compile dashboard-test" "$T/npx.log")" 1
timeout 20 "$C" dashboard ui --output-dir out/public/js.new >/dev/null 2>&1; check "--output-dir passes a :output-dir config merge" "$(grep -c 'shadow-cljs compile ui --config-merge {:output-dir "out/public/js.new"}' "$T/npx.log")" 1
timeout 20 "$C" dashboard agent-tools >/dev/null 2>&1; check "agent-tools is a release build (the CLIs load one file)" "$(grep -c 'shadow-cljs release agent-tools' "$T/npx.log")" 1

# an undeclared-var / undeclared-ns / fn-arity warning fails the build; other warnings do not
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\necho "$WARN"\n' "$T" > "$T/bin/npx"
WARN='------ WARNING #1 - :undeclared-var ---' timeout 20 "$C" engine test >/dev/null 2>&1; check "undeclared-var fails" "$?" 1
WARN='------ WARNING #1 - :undeclared-ns ---' timeout 20 "$C" engine test >/dev/null 2>&1; check "undeclared-ns fails" "$?" 1
WARN='------ WARNING #1 - :fn-arity ---' timeout 20 "$C" engine test >/dev/null 2>&1; check "fn-arity fails" "$?" 1
WARN='------ WARNING #1 - :redef ---' timeout 20 "$C" engine test >/dev/null 2>&1; check "other warning passes" "$?" 0
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\n' "$T" > "$T/bin/npx"

# body-release is built to a temp file, checked for every jobs/ file, then renamed; a bundle lacking a job leaves the old one in place
mkdir -p "$T/repo/engine/src/jobs/movement"; echo '(ns jobs.movement.go-to)' > "$T/repo/engine/src/jobs/movement/go_to.cljs"
echo old > "$T/repo/engine/out/body-release.cjs"
bundle_stub() { printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\n[ "$BUNDLE" = none ] || echo "$BUNDLE" > ../engine/out/body-release.new.cjs\n' "$T" > "$T/bin/npx"; }
bundle_stub
mkdir -p "$T/repo/engine/out/compile-cache/v"; touch -d '5 days ago' "$T/repo/engine/out/compile-cache/v/old"; touch "$T/repo/engine/out/compile-cache/v/new"
BUNDLE='jobs.movement.go_to.round=1' timeout 20 "$C" engine body-release >/dev/null 2>&1; check "complete body-release bundle is renamed into place" "$(cat "$T/repo/engine/out/body-release.cjs")" 'jobs.movement.go_to.round=1'
check "body-release prunes only stale compile-cache entries" "$(ls "$T/repo/engine/out/compile-cache/v")" new
check "no temp bundle left" "$(ls "$T/repo/engine/out" | grep -c 'body-release.new')" 0
BUNDLE='jobs.movement.other.x=1' timeout 20 "$C" engine body-release >/dev/null 2>&1; check "bundle missing a job fails the build" "$?" 1
check "failed bundle leaves the old one" "$(cat "$T/repo/engine/out/body-release.cjs")" 'jobs.movement.go_to.round=1'
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\n' "$T" > "$T/bin/npx"

# TERM during a compile leaves no temp file behind
mkdir -p "$T/tmpd"
printf '#!/bin/sh\necho $$ > "%s/npx.pid"\nexec sleep 30\n' "$T" > "$T/bin/npx"
TMPDIR="$T/tmpd" "$C" engine test >/dev/null 2>&1 & CP=$!
sleep 1.5; kill -TERM $CP; wait $CP 2>/dev/null
kill "$(cat "$T/npx.pid")" 2>/dev/null # the orphaned stub
check "TERM leaves no temp file" "$(ls "$T/tmpd" | wc -l)" 0
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\n' "$T" > "$T/bin/npx"

# wrapped in flock: exits at once with a clear message (would poll forever otherwise)
out=$(timeout 10 flock "$MC_COMPILE_LOCK" "$C" engine test 2>&1); rc=$?
check "flock-wrapped exit" "$rc" 2
check "flock-wrapped message" "$(grep -c "takes the lock itself" <<<"$out")" 1

# the holder further up the tree (flock -o: child drops the fd, flock itself holds it) is caught through intermediate shells
out=$(timeout 10 flock -o "$MC_COMPILE_LOCK" bash -c "bash -c '\"$C\" engine test; exit \$?'" 2>&1); rc=$?
check "ancestor-holder exit" "$rc" 2

# an intermediate process whose comm has a space ("npm start") must not break the parent walk
ln -s "$(command -v bash)" "$T/sp ace"
out=$(timeout 10 flock -o "$MC_COMPILE_LOCK" "$T/sp ace" -c "\"$C\" engine test; exit \$?" 2>&1); rc=$?
check "ancestor-holder through spaced comm exit" "$rc" 2

# another unrelated holder is waited for, not refused
( flock "$MC_COMPILE_LOCK" sleep 1 ) & sleep 0.3
timeout 20 "$C" engine test >/dev/null 2>&1; check "waits for unrelated holder" "$?" 0

# fair against a blocking flock waiter: holder releases, blocking waiter and compile both queued; both finish
( flock "$MC_COMPILE_LOCK" sleep 1 ) & sleep 0.3
( flock "$MC_COMPILE_LOCK" true; echo done > "$T/waiter" ) &
timeout 20 "$C" engine test >/dev/null 2>&1; check "compile with blocking waiter" "$?" 0
wait %+ 2>/dev/null

# stub bundle files so the namespaces count as built (test-run.mjs refuses a namespace missing from the bundle)
for n in engine.a-test engine.b-test engine.hang-test engine.stall-test; do : > "$T/repo/engine/out/test/cljs-runtime/${n//-/_}.js"; echo "SHADOW_IMPORT(\"${n//-/_}.js\");" >> "$T/repo/engine/out/test.cjs"; done
# test-engine: compile for build test, then node outside the lock on a private copy, namespaces comma-joined
out=$(timeout 30 "$T/repo/tools/test-engine" engine.a-test engine.b-test 2>&1); check "test-engine rc" "$?" 0
check "test-engine ns args" "$(grep -c -- '--test=engine.a-test,engine.b-test' <<<"$out")" 1
check "test-engine heap flag" "$(grep -c -- '--max-old-space-size=[0-9][0-9]*\b' <<<"$out")" 1
check "test-engine private copy" "$(grep -c -- '/tmp/mc-test-run-[0-9]*/out/test.cjs' <<<"$out")" 1
# a hung run is killed after the timeout (exit 124) and says so
out=$(MC_TEST_TIMEOUT_S=2 timeout 30 "$T/repo/tools/test-engine" engine.hang-test 2>&1); check "hung run rc" "$?" 124
check "hung run message" "$(grep -c 'TIMEOUT, killed after 2 s' <<<"$out")" 1
# a run that started but finishes no test (a spinning test) is killed by the stall watchdog, not the whole-run cap
# output goes to a file, not $(...): a spinner the watchdog missed would hold the pipe open and hang the run
STALL_PID_FILE="$T/stall.pid" MC_TEST_STALL_S=3 MC_TEST_FIRST_S=3 timeout 30 "$T/repo/tools/test-engine" engine.stall-test > "$T/stall.out" 2>&1; check "stalled run rc" "$?" 124
# reaped by pid (timeout(1) leaves the process group) in case the watchdog missed it
[ -s "$T/stall.pid" ] && kill -KILL "$(cat "$T/stall.pid")" 2>/dev/null
out=$(cat "$T/stall.out")
check "stalled run message" "$(grep -c 'STALLED, no test finished for 3 s' <<<"$out")" 1
out=$(timeout 30 "$T/repo/tools/test-engine" engine.nope-test 2>&1); check "missing ns rc" "$?" 2
check "missing ns message" "$(grep -q "not in the :test bundle" <<<"$out" && echo 1)" 1
# a bundle rebuilt by another run for other namespaces lacks ours: the second compile (the stub writes the file on its 2nd call) brings it back
rm -f "$T/repo/engine/out/test/cljs-runtime/engine.late_test.js"
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\n[ "$(wc -l < "%s/npx.log")" -ge 2 ] && { : > "%s/repo/engine/out/test/cljs-runtime/engine.late_test.js"; echo "SHADOW_IMPORT(\\"engine.late_test.js\\");" >> "%s/repo/engine/out/test.cjs"; }\nexit 0\n' "$T" "$T" "$T" "$T" > "$T/bin/npx"
: > "$T/npx.log"; out=$(timeout 30 "$T/repo/tools/test-engine" engine.late-test 2>&1); check "bundle swapped under the run: recompiled once, rc" "$?" 0
check "bundle swapped under the run: compiled twice" "$(wc -l < "$T/npx.log")" 2
# a failed compile that printed an ERROR / undeclared warning is retried (up to 2x); other failures, usage and busy exits are not
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\nn=$(wc -l < "%s/npx.log")\n[ "$n" -ge "$OK_AT" ] && exit 0\necho "------ WARNING #1 - :undeclared-var ---"\n' "$T" "$T" > "$T/bin/npx"
: > "$T/npx.log"; out=$(OK_AT=3 MC_COMPILE_RETRY_S=0 timeout 30 "$T/repo/tools/test-engine" engine.a-test 2>&1); check "retry: clears on 3rd try rc" "$?" 0
check "retry: compiled 3 times" "$(wc -l < "$T/npx.log")" 3
check "retry: says why" "$(grep -c 'foreign edit in flight' <<<"$out")" 2
: > "$T/npx.log"; OK_AT=99 MC_COMPILE_RETRY_S=0 timeout 30 "$T/repo/tools/test-engine" engine.a-test >/dev/null 2>&1; check "retry: gives up rc" "$?" 1
check "retry: gave up after 3 tries" "$(wc -l < "$T/npx.log")" 3
# a named failing file ("File: path:l:c"): changed during the wait -> retried at once; unchanged -> fails after the wait, no retry
: > "$T/bad.cljs"
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\nn=$(wc -l < "%s/npx.log")\n[ "$n" -ge "$OK_AT" ] && exit 0\necho "------ WARNING #1 - :undeclared-var ---"\necho " File: %s/bad.cljs:3:4"\n' "$T" "$T" "$T" > "$T/bin/npx"
: > "$T/npx.log"; : > "$T/te.out"; ( seen "$T/te.out" 'foreign edit in flight'; sleep 0.2; echo x >> "$T/bad.cljs" ) &
OK_AT=2 MC_COMPILE_RETRY_S=20 MC_COMPILE_POLL_S=1 timeout 30 "$T/repo/tools/test-engine" engine.a-test > "$T/te.out" 2>&1; check "named file changed: retried rc" "$?" 0
check "named file changed: compiled twice" "$(wc -l < "$T/npx.log")" 2
: > "$T/npx.log"; out=$(OK_AT=99 MC_COMPILE_RETRY_S=2 MC_COMPILE_POLL_S=1 timeout 30 "$T/repo/tools/test-engine" engine.a-test 2>&1); check "named file unchanged: rc" "$?" 1
check "named file unchanged: no retry" "$(wc -l < "$T/npx.log")" 1
check "named file unchanged: says so" "$(grep -c 'did not change; not a foreign edit' <<<"$out")" 1
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\necho "plain failure"\nexit 1\n' "$T" > "$T/bin/npx"
: > "$T/npx.log"; MC_COMPILE_RETRY_S=0 timeout 30 "$T/repo/tools/test-engine" engine.a-test >/dev/null 2>&1; check "no retry without ERROR/undeclared output" "$(wc -l < "$T/npx.log")" 1
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\n' "$T" > "$T/bin/npx"
# --golden whose run fails exits with that failure, never falling through to the normal path
out=$(timeout 30 "$T/repo/tools/test-engine" --golden 2>&1); rc=$?
check "golden failure no fall-through" "$(grep -c 'not in the :test bundle' <<<"$out")" 0
# --golden asks res-slot for the golden need from res-slot.json (goldenMb), not the shard need
mv "$T/repo/tools/res-slot" "$T/repo/tools/res-slot.real"
printf '#!/bin/sh\necho "RES-SLOT $*"\n' > "$T/repo/tools/res-slot"; chmod +x "$T/repo/tools/res-slot"
out=$(timeout 30 "$T/repo/tools/test-engine" --golden 2>&1)
gmb=$(node -e 'console.log(require(process.argv[1]).kinds.tests.goldenMb)' "$T/repo/tools/res-slot.json")
check "golden need from goldenMb" "$(grep -cF -e "RES-SLOT tests --need $gmb --" <<<"$out")" 1
mv "$T/repo/tools/res-slot.real" "$T/repo/tools/res-slot"
timeout 5 "$T/repo/tools/test-engine" >/dev/null 2>&1; check "no args usage exit" "$?" 2

# a worktree runs no server: each compile is a one-shot JVM under a res-slot `server` slot, taken before the compile lock
git init -q "$T/main" && git -C "$T/main" -c user.name=t -c user.email=t@t commit -q --allow-empty -m x && git -C "$T/main" worktree add -q "$T/wt" 2>/dev/null
mkdir -p "$T/wt/tools" "$T/wt/dashboard/.shadow-cljs"
cp "$TOOLS/compile" "$TOOLS/compile-idle-watch" "$T/wt/tools/"
printf '#!/bin/sh\nsleep 4\necho "res-slot: busy"\nexit 75\n' > "$T/wt/tools/res-slot"; chmod +x "$T/wt/tools/res-slot"
"$T/wt/tools/compile" engine test >/dev/null 2>&1 & WP=$!
sleep 1.5
flock -n "$MC_COMPILE_LOCK" true; check "lock free while a worktree waits for a slot" "$?" 0
wait $WP; check "worktree without a slot exits 75" "$?" 75

# the slot wait is long by default, res-slot's own wait lines reach the caller, and the compile runs inside the slot once one frees
printf '#!/bin/sh\n[ "$1 $2" = "server --" ] || exit 9\nshift 2\necho "$RES_SLOT_MAX_WAIT_MS" > "%s/maxwait"\necho "res-slot: waiting for server (6/6 slots in use)" >&2\nwhile [ ! -e "%s/release" ]; do sleep 0.1; done\necho held > "%s/slot"\n"$@"; rc=$?\nrm "%s/slot"\nexit $rc\n' "$T" "$T" "$T" "$T" > "$T/wt/tools/res-slot"; chmod +x "$T/wt/tools/res-slot"
rm -f "$T/wt/dashboard/.shadow-cljs/"*
printf '#!/bin/sh\necho "npx $* slot=$(cat %s/slot 2>/dev/null)" >> "%s/wt-npx.log"\n' "$T" "$T" > "$T/bin/npx"
timeout 60 "$T/wt/tools/compile" engine test >"$T/q.out" 2>&1 & QP=$!
for _ in $(seq 1 100); do grep -q 'res-slot: waiting' "$T/q.out" && break; sleep 0.1; done
grep -q 'res-slot: waiting for server' "$T/q.out"; check "queued worktree compile shows the slot wait" "$?" 0
touch "$T/release"; wait $QP; check "queued compile succeeds once a slot frees" "$?" 0
check "worktree compile waits 45 min for a slot by default" "$(cat "$T/maxwait")" 2700000
check "worktree compile runs once, inside the slot" "$(cat "$T/wt-npx.log")" "npx shadow-cljs compile test slot=held"
check "worktree compile starts no server" "$(ls "$T/wt/dashboard/.shadow-cljs" | grep -c -E 'server.pid|launch.pid')" 0
check "worktree slot freed after the compile" "$(ls "$T/slot" 2>/dev/null | wc -l)" 0
rm -f "$T/release" "$T/wt-npx.log"
printf '#!/bin/sh\nshift 2\nexec "$@"\n' > "$T/wt/tools/res-slot"; chmod +x "$T/wt/tools/res-slot"

# the main checkout's server stops after its own (longer) idle time too, with no server slot
mkdir -p "$T/main/tools" "$T/main/dashboard/.shadow-cljs"
cp "$TOOLS/compile" "$TOOLS/compile-idle-watch" "$T/main/tools/"
export MC_COMPILE_MIN_START_MB=0 MC_COMPILE_MAIN_IDLE_S=3 MC_COMPILE_IDLE_POLL_S=1
printf '#!/bin/sh\ncase "$1 $2" in "shadow-cljs server") echo $$ > .shadow-cljs/server.pid; echo 1 > .shadow-cljs/nrepl.port; echo 1 > .shadow-cljs/http.port; echo "ppid=$(ps -o ppid= -p $$ | tr -d " ") label=${LIVE_TESTS_AGENT_ID:-none} shared=${COMPILE_SHARED:-0}" > .shadow-cljs/launch.env; exec sleep 300;; esac\n' > "$T/bin/npx"
LIVE_TESTS_AGENT_ID=fixer-x timeout 30 "$T/main/tools/compile" engine test >/dev/null 2>&1; check "main compile rc" "$?" 0
# the shared server is detached from the caller: no agent label, not a child of the compile script, marked shared
read -r LPPID LLABEL LSHARED < <(tr ' ' '\n' < "$T/main/dashboard/.shadow-cljs/launch.env" | cut -d= -f2 | paste -sd' ')
check "shared server has no agent label" "$LLABEL" none
check "shared server is marked shared" "$LSHARED" 1
WPID=$(pgrep -f "$T/main/tools/compile-idle-watch" | head -1)
check "idle watcher cwd is the checkout's dashboard dir, not the caller's" "$(readlink /proc/$WPID/cwd)" "$T/main/dashboard"
check "shared server is not under the compile script" "$(cat /proc/$LPPID/cmdline 2>/dev/null | tr '\0' ' ' | grep -c 'tools/compile')" 0
MSRV=$(cat "$T/main/dashboard/.shadow-cljs/server.pid")
sleep 1; kill -0 "$MSRV" 2>/dev/null; check "main server alive right after a compile" "$?" 0
gone "$MSRV"; check "main server gone after the main idle time" "$?" 0
timeout 30 "$T/main/tools/compile" engine test >/dev/null 2>&1; check "main compile restarts the server" "$?" 0
# a build-time .clj changed after the server started: the next compile restarts the server (once); a .cljs change does not
mkdir -p "$T/main/engine/src/engine"
OLD=$(cat "$T/main/dashboard/.shadow-cljs/server.pid")
sleep 1; echo x > "$T/main/engine/src/engine/ui.cljs"
timeout 30 "$T/main/tools/compile" engine test >/dev/null 2>&1
check "cljs change keeps the server" "$(cat "$T/main/dashboard/.shadow-cljs/server.pid")" "$OLD"
echo '(ns m)' > "$T/main/engine/src/engine/macros.clj"
timeout 30 "$T/main/tools/compile" engine test >/dev/null 2>&1; check "clj change compile rc" "$?" 0
NEW=$(cat "$T/main/dashboard/.shadow-cljs/server.pid")
kill -0 "$OLD" 2>/dev/null; check "clj change stops the old server" "$?" 1
kill -0 "$NEW" 2>/dev/null; check "clj change starts a new server" "$?" 0
timeout 30 "$T/main/tools/compile" engine test >/dev/null 2>&1
check "no second restart without a new change" "$(cat "$T/main/dashboard/.shadow-cljs/server.pid")" "$NEW"
kill "$(cat "$T/main/dashboard/.shadow-cljs/server.pid")" 2>/dev/null
unset MC_COMPILE_MIN_START_MB MC_COMPILE_MAIN_IDLE_S MC_COMPILE_IDLE_POLL_S

# N compiles run at once (MC_COMPILE_SLOTS), one per checkout. The stub compile logs start/end <checkout> and holds until go-<checkout> exists.
printf '#!/bin/sh\ncase "$1 $2" in "shadow-cljs server") echo $$ > .shadow-cljs/server.pid; echo 1 > .shadow-cljs/nrepl.port; echo 1 > .shadow-cljs/http.port; exec sleep 300;; esac\nk=$(basename "$(dirname "$PWD")")\necho "start $k" >> "%s/par.log"\nwhile [ ! -e "%s/go-$k" ]; do sleep 0.1; done\necho "end $k" >> "%s/par.log"\n' "$T" "$T" "$T" > "$T/bin/npx"
export MC_COMPILE_MIN_START_MB=0 MC_COMPILE_SLOTS=2
: > "$T/par.log"
timeout 30 "$C" engine test >/dev/null 2>&1 & P1=$!
seen "$T/par.log" "start repo"
timeout 30 "$T/wt/tools/compile" engine test >/dev/null 2>&1 & P2=$!
seen "$T/par.log" "start wt"; check "two checkouts compile at once" "$?" 0
"$C" engine test >/dev/null 2>&1 & P3=$!  # no timeout wrapper: the ticket is named after this pid
touch "$T/go-wt"; wait $P2; check "second checkout compile rc" "$?" 0
for _ in $(seq 1 100); do ls "$MC_COMPILE_QUEUE" | grep -q -- "-$P3\$" && break; sleep 0.1; done; sleep 1
check "a second compile of a busy checkout waits for it" "$(grep -c "start repo" "$T/par.log")" 1
touch "$T/go-repo"; wait $P1; check "first compile rc" "$?" 0
wait $P3; check "queued same-checkout compile rc" "$?" 0
check "queued same-checkout compile ran after" "$(grep -c "start repo" "$T/par.log")" 2
rm -f "$T/go-repo" "$T/go-wt" "$T/main/dashboard/.shadow-cljs/server.pid"

# a main compile waiting for a slot starts its server first (never while holding a slot) and keeps it alive past the idle time
export MC_COMPILE_SLOTS=1 MC_COMPILE_MAIN_IDLE_S=1 MC_COMPILE_IDLE_POLL_S=1
: > "$T/par.log"
timeout 60 "$C" engine test >/dev/null 2>&1 & P1=$!
seen "$T/par.log" "start repo"
timeout 60 "$T/main/tools/compile" engine test >/dev/null 2>&1 & P2=$!
timeout 60 "$T/wt/tools/compile" engine test >/dev/null 2>&1 & P3=$!
seen "$T/main/dashboard/.shadow-cljs/server.pid" .; check "a queued compile starts its server before it gets a slot" "$?" 0
MS=$(cat "$T/main/dashboard/.shadow-cljs/server.pid" 2>/dev/null)
sleep 3
kill -0 "$MS" 2>/dev/null; check "a queued main compile's server outlives the idle time" "$?" 0
touch "$T/go-repo" "$T/go-main" "$T/go-wt"
wait $P1; check "slot holder rc" "$?" 0
wait $P2; check "queued main compile rc" "$?" 0
wait $P3; check "queued worktree compile rc" "$?" 0
kill "$MS" 2>/dev/null
unset MC_COMPILE_MIN_START_MB MC_COMPILE_SLOTS MC_COMPILE_MAIN_IDLE_S MC_COMPILE_IDLE_POLL_S

# pool: a worktree compile runs in a slot copy (sources rsynced in, output rsynced back), one compile per slot, one-shot fallback
export MC_COMPILE_POOL_DIR="$T/pool" MC_COMPILE_POOL_SIZE=1 MC_COMPILE_MIN_START_MB=0 MC_COMPILE_POOL_WAIT_S=1
printf '{:builds\n {:test {:target :node-test\n         :output-to "../engine/out/test.cjs"\n         :output-dir "../engine/out/test"}}}\n' > "$T/wt/dashboard/shadow-cljs.edn"
mkdir -p "$T/wt/engine/src" "$T/wt/engine/out"
echo one > "$T/wt/engine/src/a.cljs"; echo two > "$T/wt/engine/src/gone.cljs"
printf '#!/bin/sh\ncase "$1 $2" in "shadow-cljs server") [ -z "$NO_SERVER" ] || exit 1; echo $$ > .shadow-cljs/server.pid; echo 1 > .shadow-cljs/nrepl.port; echo 1 > .shadow-cljs/http.port; exec sleep 300;; esac\necho "$PWD $(cat ../engine/src/a.cljs) $(ls ../engine/src | tr "\\n" " ")" >> "%s/pool-npx.log"\nmkdir -p ../engine/out/test; echo "built $PWD" > ../engine/out/test/x.js; echo "$PWD" > ../engine/out/test.cjs\ncase $PWD in */pool/*) while [ -e "%s/hold" ]; do sleep 0.1; done;; esac\n' "$T" "$T" > "$T/bin/npx"
timeout 60 "$T/wt/tools/compile" engine test >"$T/pool.out" 2>&1; check "pool compile rc" "$?" 0
check "pool: compiled in the slot with the worktree's sources" "$(cat "$T/pool-npx.log")" "$T/pool/0/dashboard one a.cljs gone.cljs "
check "pool: output dir copied back" "$(cat "$T/wt/engine/out/test/x.js")" "built $T/pool/0/dashboard"
check "pool: output-to copied back, slot path replaced" "$(cat "$T/wt/engine/out/test.cjs")" "$T/wt/dashboard"
rm "$T/wt/engine/src/gone.cljs"; echo three > "$T/wt/engine/src/a.cljs"; : > "$T/pool-npx.log"
timeout 60 "$T/wt/tools/compile" engine test >/dev/null 2>&1
check "pool: changed and deleted sources follow the worktree" "$(cat "$T/pool-npx.log")" "$T/pool/0/dashboard three a.cljs "
PSRV=$(cat "$T/pool/0/dashboard/.shadow-cljs/server.pid"); kill -0 "$PSRV" 2>/dev/null; check "pool: slot server stays warm" "$?" 0
# one compile per slot: while one holds it, a second finds no slot within the wait and compiles one-shot
flock "$T/pool/0/.pool.lock" sleep 4 & H=$!; sleep 0.5
printf '#!/bin/sh\nshift 2\nexec "$@"\n' > "$T/wt/tools/res-slot"
out=$(timeout 60 "$T/wt/tools/compile" engine test 2>&1); check "pool busy: fallback rc" "$?" 0
check "pool busy: says one-shot" "$(grep -c 'compiling one-shot' <<<"$out")" 1
check "pool busy: log row names build and fallback cause" "$(tail -n1 "$T/res/log.jsonl" | grep -c '"build":"test".*"fallback":"no free slot in 1s"')" 1
wait $H
# a test macro (.clj on the source paths) changed: the warm slot server restarts before compiling
mkdir -p "$T/wt/engine/test"; echo macro > "$T/wt/engine/test/m.clj"
out=$(timeout 60 "$T/wt/tools/compile" engine test 2>&1); check "pool macro: rc" "$?" 0
check "pool macro: server restarted" "$(grep -c 'build-time .clj file changed' <<<"$out")" 1
check "pool macro: log row names the worktree and the restart" "$(tail -n1 "$T/res/log.jsonl" | grep -c "\"build\":\"test\",\"from\":\"$T/wt\".*\"restarted\":1")" 1
rm "$T/wt/engine/test/m.clj"
# the worktree's own lock serializes its compiles
: > "$T/pool-npx.log"
flock "$T/wt/dashboard/.shadow-cljs/compile.lock" sleep 3 & L=$!; sleep 0.5
timeout 60 "$T/wt/tools/compile" engine test >/dev/null 2>&1 & H=$!
sleep 1.5; check "pool: same-worktree compile waits for the worktree lock" "$(wc -c < "$T/pool-npx.log")" 0
wait $L; wait $H; check "pool: same-worktree compile ran after the lock" "$(grep -c . "$T/pool-npx.log")" 1
kill "$(cat "$T/pool/0/dashboard/.shadow-cljs/server.pid")" 2>/dev/null
# slot server fails to start: one-shot
rm -rf "$T/pool"
out=$(NO_SERVER=1 timeout 60 "$T/wt/tools/compile" engine test 2>&1); check "pool server start failure: fallback rc" "$?" 0
check "pool server start failure: says one-shot" "$(grep -c 'compiling one-shot' <<<"$out")" 1
# no server in the slot and too little memory to start one: one-shot
rm -rf "$T/pool"
out=$(MC_COMPILE_MIN_START_MB=99999999 timeout 60 "$T/wt/tools/compile" engine test 2>&1); check "pool low memory: fallback rc" "$?" 0
check "pool low memory: says one-shot" "$(grep -c 'compiling one-shot' <<<"$out")" 1
# the free slot nearest the worktree is taken, not the first free one or the last used one
rm -rf "$T/pool"; mkdir -p "$T/pool/0"
flock "$T/pool/0/.pool.lock" sleep 6 & H=$!; sleep 0.5
MC_COMPILE_POOL_SIZE=2 timeout 60 "$T/wt/tools/compile" engine test >/dev/null 2>&1
echo elsewhere > "$T/pool/1/.last"; kill "$(cat "$T/pool/1/dashboard/.shadow-cljs/server.pid")" 2>/dev/null; wait $H
: > "$T/pool-npx.log"
MC_COMPILE_POOL_SIZE=2 timeout 60 "$T/wt/tools/compile" engine test >/dev/null 2>&1
check "pool pick: nearest free slot wins over first and last-used" "$(cut -d' ' -f1 "$T/pool-npx.log")" "$T/pool/1/dashboard"
kill "$(cat "$T/pool/1/dashboard/.shadow-cljs/server.pid")" 2>/dev/null
export MC_COMPILE_POOL_SIZE=0; unset MC_COMPILE_POOL_DIR MC_COMPILE_MIN_START_MB MC_COMPILE_POOL_WAIT_S

# the idle watcher kills only the server it was started for (pid + start time), and never while a compile runs
W="$TOOLS/compile-idle-watch"
starttime() { awk '{print $22}' "/proc/$1/stat"; }
touch -d '1 hour ago' "$T/lu"
sleep 300 & V=$!
timeout 10 "$W" "$V" "$(( $(starttime "$V") + 1 ))" "$T/lu" 3 1; kill -0 "$V" 2>/dev/null; check "watcher spares a pid with another start time" "$?" 0
sleep 300 & BUSY=$!; echo $BUSY > "$T/lu.compiling"
"$W" "$V" "$(starttime "$V")" "$T/lu" 3 1 & WW=$!
sleep 3; kill -0 "$V" 2>/dev/null; check "watcher spares the server while a compile runs" "$?" 0
kill $BUSY; wait $BUSY 2>/dev/null; wait $WW; gone "$V"; check "watcher kills the idle server once no compile runs" "$?" 0
rm -f "$T/lu.compiling"; kill $BUSY 2>/dev/null
exit $fail
