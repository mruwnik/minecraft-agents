#!/usr/bin/env bash
# Tests for tools/commit-mine trailer handling and exit codes, in a throwaway repo.
set -uo pipefail
SRC="$(cd "$(dirname "$0")" && pwd)/commit-mine"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
cd "$T" && git init -q . && git config user.email t@t && git config user.name t
mkdir tools && cp "$SRC" tools/commit-mine
echo 0 > f.txt && git add f.txt && git commit -q -m init
fail=0
check() { if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: want [$3] got [$2]"; fail=1; fi; }
commit_msg() { echo "$RANDOM$RANDOM" > f.txt; tools/commit-mine --card c -m "$1" --expect-hunks 1 f.txt >/dev/null 2>&1; rc=$?; echo "$rc"; }
trailers() { git log -1 --format=%B | grep -c '^Co-Authored-By:'; }

rc=$(commit_msg "plain"); check "plain rc" "$rc" 0; check "plain gets default trailer" "$(trailers)" 1
rc=$(commit_msg $'other\n\nCo-Authored-By: Claude Opus 4 <noreply@anthropic.com>'); check "other rc" "$rc" 0
check "other model keeps single trailer" "$(trailers)" 1
check "other model trailer is the given one" "$(git log -1 --format=%B | grep -c Opus)" 1
rc=$(commit_msg $'same\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>'); check "same rc" "$rc" 0; check "same stays single" "$(trailers)" 1
echo x > f.txt; tools/commit-mine --card c -m m f.txt >/dev/null 2>&1; check "missing --expect-hunks exit" "$?" 5
git checkout -q f.txt
tools/commit-mine -m m f.txt >/dev/null 2>&1; check "no card exit" "$?" 1
exit $fail
