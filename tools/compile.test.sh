#!/usr/bin/env bash
# Tests for tools/compile lock handling and tools/test-engine, with a temp lock/queue and a stub npx (never the real lock).
set -uo pipefail
TOOLS="$(cd "$(dirname "$0")" && pwd)"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
export MC_COMPILE_LOCK="$T/lock" MC_COMPILE_QUEUE="$T/queue"
fail=0
check() { if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: want [$3] got [$2]"; fail=1; fi; }

# Fake repo: tools copied in, stub npx, a live "server" pid so no server start.
mkdir -p "$T/repo/tools" "$T/repo/engine/.shadow-cljs" "$T/repo/engine/out/test/cljs-runtime" "$T/bin" "$T/res"
cp "$TOOLS/compile" "$TOOLS/test-engine" "$TOOLS/test-run.mjs" "$TOOLS/res-slot" "$TOOLS/res-slot.mjs" "$TOOLS/res-slot.json" "$T/repo/tools/" 2>/dev/null
printf '{"floorMb":0,"kinds":{"tests":{"needMb":1,"max":1}}}' > "$T/res/cfg.json"
export RES_SLOT_DIR="$T/res" RES_SLOT_CONFIG="$T/res/cfg.json"
sleep 300 & SRV=$!; trap 'kill $SRV 2>/dev/null; rm -rf "$T"' EXIT
echo $SRV > "$T/repo/engine/.shadow-cljs/server.pid"
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\n' "$T" > "$T/bin/npx"; chmod +x "$T/bin/npx"
printf 'console.log("NODE", process.argv.slice(1).join(" "), process.execArgv.join(" ")); if (process.argv.join(" ").includes("hang-test")) setInterval(() => {}, 1000)\n' > "$T/repo/engine/out/test.cjs"
export PATH="$T/bin:$PATH"
C="$T/repo/tools/compile"

# plain compile works and takes the lock itself
timeout 20 "$C" engine test >/dev/null 2>&1; check "plain compile rc" "$?" 0
check "stub npx ran" "$(grep -c 'shadow-cljs compile test' "$T/npx.log")" 1

# wrapped in flock: exits at once with a clear message (would poll forever otherwise)
out=$(timeout 10 flock "$MC_COMPILE_LOCK" "$C" engine test 2>&1); rc=$?
check "flock-wrapped exit" "$rc" 2
check "flock-wrapped message" "$(grep -c "takes the lock itself" <<<"$out")" 1

# the holder further up the tree (flock -o: child drops the fd, flock itself holds it) is caught through intermediate shells
out=$(timeout 10 flock -o "$MC_COMPILE_LOCK" bash -c "bash -c '\"$C\" engine test; exit \$?'" 2>&1); rc=$?
check "ancestor-holder exit" "$rc" 2

# another unrelated holder is waited for, not refused
( flock "$MC_COMPILE_LOCK" sleep 1 ) & sleep 0.3
timeout 20 "$C" engine test >/dev/null 2>&1; check "waits for unrelated holder" "$?" 0

# fair against a blocking flock waiter: holder releases, blocking waiter and compile both queued; both finish
( flock "$MC_COMPILE_LOCK" sleep 1 ) & sleep 0.3
( flock "$MC_COMPILE_LOCK" true; echo done > "$T/waiter" ) &
timeout 20 "$C" engine test >/dev/null 2>&1; check "compile with blocking waiter" "$?" 0
wait %+ 2>/dev/null

# stub bundle files so the namespaces count as built (test-run.mjs refuses a namespace missing from the bundle)
for n in engine.a-test engine.b-test engine.hang-test; do : > "$T/repo/engine/out/test/cljs-runtime/${n//-/_}.js"; done
# test-engine: compile for build test, then node outside the lock on a private copy, namespaces comma-joined
out=$(timeout 30 "$T/repo/tools/test-engine" engine.a-test engine.b-test 2>&1); check "test-engine rc" "$?" 0
check "test-engine ns args" "$(grep -c -- '--test=engine.a-test,engine.b-test' <<<"$out")" 1
check "test-engine heap flag" "$(grep -c -- '--max-old-space-size=4096' <<<"$out")" 1
check "test-engine private copy" "$(grep -c -- '/tmp/mc-test-run-[0-9]*/out/test.cjs' <<<"$out")" 1
# a hung run is killed after the timeout (exit 124) and says so
out=$(MC_TEST_TIMEOUT_S=2 timeout 30 "$T/repo/tools/test-engine" engine.hang-test 2>&1); check "hung run rc" "$?" 124
check "hung run message" "$(grep -c 'TIMEOUT, killed after 2 s' <<<"$out")" 1
out=$(timeout 30 "$T/repo/tools/test-engine" engine.nope-test 2>&1); check "missing ns rc" "$?" 2
check "missing ns message" "$(grep -c 'not in the :test bundle' <<<"$out")" 1
timeout 5 "$T/repo/tools/test-engine" >/dev/null 2>&1; check "no args usage exit" "$?" 2
exit $fail
