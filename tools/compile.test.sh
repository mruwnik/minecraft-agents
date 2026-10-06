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

# an undeclared-var / undeclared-ns warning fails the build; other warnings do not
printf '#!/bin/sh\necho "npx $*" >> "%s/npx.log"\necho "$WARN"\n' "$T" > "$T/bin/npx"
WARN='------ WARNING #1 - :undeclared-var ---' timeout 20 "$C" engine test >/dev/null 2>&1; check "undeclared-var fails" "$?" 1
WARN='------ WARNING #1 - :undeclared-ns ---' timeout 20 "$C" engine test >/dev/null 2>&1; check "undeclared-ns fails" "$?" 1
WARN='------ WARNING #1 - :redef ---' timeout 20 "$C" engine test >/dev/null 2>&1; check "other warning passes" "$?" 0
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

# a worktree's server start waits for a compile slot without holding the global compile lock
git init -q "$T/main" && git -C "$T/main" -c user.name=t -c user.email=t@t commit -q --allow-empty -m x && git -C "$T/main" worktree add -q "$T/wt" 2>/dev/null
mkdir -p "$T/wt/tools" "$T/wt/engine/.shadow-cljs"
cp "$TOOLS/compile" "$T/wt/tools/"
printf '#!/bin/sh\nsleep 4\necho "res-slot: busy"\nexit 75\n' > "$T/wt/tools/res-slot"; chmod +x "$T/wt/tools/res-slot"
MC_COMPILE_MIN_START_MB=0 "$T/wt/tools/compile" engine test >/dev/null 2>&1 & WP=$!
sleep 1.5
flock -n "$MC_COMPILE_LOCK" true; check "lock free while a worktree waits for a slot" "$?" 0
wait $WP; check "worktree without a slot exits 75" "$?" 75
exit $fail
