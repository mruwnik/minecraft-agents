#!/usr/bin/env bash
# Tests for tools/compile lock handling and tools/test-engine, with a temp lock/queue and a stub npx (never the real lock).
set -uo pipefail
TOOLS="$(cd "$(dirname "$0")" && pwd)"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
export MC_COMPILE_LOCK="$T/lock" MC_COMPILE_QUEUE="$T/queue"
fail=0
check() { if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: want [$3] got [$2]"; fail=1; fi; }

# Fake repo: tools copied in, stub npx, a live "server" pid so no server start.
mkdir -p "$T/repo/tools" "$T/repo/engine/.shadow-cljs" "$T/repo/engine/out" "$T/bin"
cp "$TOOLS/compile" "$TOOLS/test-engine" "$T/repo/tools/" 2>/dev/null
sleep 300 & SRV=$!; trap 'kill $SRV 2>/dev/null; rm -rf "$T"' EXIT
echo $SRV > "$T/repo/engine/.shadow-cljs/server.pid"
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\n' "$T" > "$T/bin/npx"; chmod +x "$T/bin/npx"
printf 'console.log("NODE", process.argv.slice(2).join(" "), process.execArgv.join(" "))\n' > "$T/repo/engine/out/test.cjs"
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

# test-engine: compile for build test, then node outside the lock, namespaces comma-joined
out=$(timeout 30 "$T/repo/tools/test-engine" engine.a-test engine.b-test 2>&1); check "test-engine rc" "$?" 0
check "test-engine ns args" "$(grep -c -- '--test=engine.a-test,engine.b-test' <<<"$out")" 1
out=$(timeout 30 "$T/repo/tools/test-engine" --full 2>&1)
check "full has heap flag" "$(grep -c -- '--max-old-space-size=4096' <<<"$out")" 1
check "full has no --test" "$(grep -c -- '--test=' <<<"$out")" 0
timeout 5 "$T/repo/tools/test-engine" >/dev/null 2>&1; check "no args usage exit" "$?" 2
exit $fail
