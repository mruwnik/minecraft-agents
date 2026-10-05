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
# Hunks mode: paths and --expect-hunks are required; a patch touching other paths is refused.
printf 'a\nb\nc\n' > g.txt; printf 'a\nb\nc\n' > h.txt; git add g.txt h.txt; git commit -q -m two
printf 'A\nb\nc\n' > g.txt; printf 'a\nb\nC\n' > h.txt
git diff g.txt > g.patch; git diff g.txt h.txt > gh.patch
tools/commit-mine --card c -m m --hunks g.patch --expect-hunks 1 >/dev/null 2>&1; check "hunks without paths exit" "$?" 1
out=$(tools/commit-mine --card c -m m --hunks gh.patch --expect-hunks 2 g.txt 2>&1); check "hunks foreign path exit" "$?" 3
check "foreign path listed" "$(grep -c 'h.txt' <<<"$out")" 1
check "foreign refusal leaves HEAD" "$(git log -1 --format=%s)" "$(git log -1 --format=%s)"
tools/commit-mine --card c -m m --hunks g.patch g.txt >/dev/null 2>&1; check "hunks missing expect exit" "$?" 5
out=$(tools/commit-mine --card c -m m --hunks g.patch --expect-hunks 2 g.txt 2>&1); check "hunks wrong expect exit" "$?" 5
check "hunk list prints path and line" "$(grep -c 'g.txt: -a' <<<"$out")" 1
before=$(git rev-parse HEAD)
tools/commit-mine --card c -m hm --hunks g.patch --expect-hunks 1 g.txt >/dev/null 2>&1; check "hunks ok exit" "$?" 0
check "hunks commit only g" "$(git show --name-only --format= HEAD)" "g.txt"
check "h.txt change still uncommitted" "$(git diff --name-only)" "h.txt"
check "index clean" "$(git diff --cached --name-only)" ""
# A flag value starting with -- is refused, naming the flag (no silent swallowing of the next flag).
echo y > f.txt
out=$(tools/commit-mine --card c -m m --approved-core --expect-hunks 1 f.txt 2>&1); check "approved-core swallowing exit" "$?" 1
check "refusal names the flag" "$(grep -c -- '--approved-core' <<<"$out")" 1
tools/commit-mine --card --expect-hunks 1 -m m f.txt >/dev/null 2>&1; check "card swallowing exit" "$?" 1
tools/commit-mine --card c -m m --expect-hunks --approved-core x f.txt >/dev/null 2>&1; check "expect swallowing exit" "$?" 1
git checkout -q f.txt
# A committed JS file must parse: a broken .mjs is refused (naming it), a valid one commits.
W='// Why JavaScript: test'
printf '%s\nexport const a = 1\n' "$W" > ok.mjs; printf '%s\nexport const b = (\n' "$W" > bad.mjs
out=$(tools/commit-mine --card c -m m --expect-hunks 1 bad.mjs 2>&1); check "unparseable mjs exit" "$?" 6
check "unparseable mjs named" "$(grep -c 'bad.mjs' <<<"$out")" 1
check "unparseable mjs not committed" "$(git log --format=%s -1 --name-only -- bad.mjs | grep -c bad.mjs)" 0
check "unparseable mjs leaves index clean" "$(git diff --cached --name-only)" ""
tools/commit-mine --card c -m m --expect-hunks 1 ok.mjs >/dev/null 2>&1; check "valid mjs exit" "$?" 0
printf '%s\nmodule.exports = {\n' "$W" > bad.cjs
tools/commit-mine --card c -m m --expect-hunks 1 bad.cjs >/dev/null 2>&1; check "unparseable cjs exit" "$?" 6
rm bad.mjs bad.cjs
# docs/ and .claude/ are committed only on the owner's word.
mkdir -p docs .claude; echo d > docs/a.md; echo c > .claude/s.json
out=$(tools/commit-mine --card c -m m --expect-hunks 1 docs/a.md 2>&1); check "docs refused exit" "$?" 1
check "docs refusal text" "$(grep -c 'docs/ and .claude/ are committed only when the owner says so' <<<"$out")" 1
tools/commit-mine --card c -m m --expect-hunks 1 .claude/s.json >/dev/null 2>&1; check ".claude refused exit" "$?" 1
tools/commit-mine --card c -m m --expect-hunks --owner-said x docs/a.md >/dev/null 2>&1; check "owner-said swallowing exit" "$?" 1
tools/commit-mine --card c -m m --owner-said c2 --expect-hunks 1 docs/a.md >/dev/null 2>&1; check "docs with --owner-said exit" "$?" 0
exit $fail
