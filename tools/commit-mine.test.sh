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
commit_msg() { echo "$RANDOM$RANDOM" > f.txt; tools/commit-mine --card c -m "$1" --expect-lines 2 f.txt >/dev/null 2>&1; rc=$?; echo "$rc"; }
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
out=$(tools/commit-mine --card c -m m --approved-core --expect-lines 2 f.txt 2>&1); check "approved-core swallowing exit" "$?" 1
check "refusal names the flag" "$(grep -c -- '--approved-core' <<<"$out")" 1
tools/commit-mine --card --expect-lines 2 -m m f.txt >/dev/null 2>&1; check "card swallowing exit" "$?" 1
tools/commit-mine --card c -m m --expect-hunks --approved-core x f.txt >/dev/null 2>&1; check "expect swallowing exit" "$?" 1
tools/commit-mine --card c -m m --expect-lines --approved-core x f.txt >/dev/null 2>&1; check "expect-lines swallowing exit" "$?" 1
git checkout -q f.txt
# A committed JS file must parse: a broken .mjs is refused (naming it), a valid one commits.
W='// Why JavaScript: test'
printf '%s\nexport const a = 1\n' "$W" > ok.mjs; printf '%s\nexport const b = (\n' "$W" > bad.mjs
out=$(tools/commit-mine --card c -m m --expect-lines 2 bad.mjs 2>&1); check "unparseable mjs exit" "$?" 6
check "unparseable mjs named" "$(grep -c 'bad.mjs' <<<"$out")" 1
check "unparseable mjs not committed" "$(git log --format=%s -1 --name-only -- bad.mjs | grep -c bad.mjs)" 0
check "unparseable mjs leaves index clean" "$(git diff --cached --name-only)" ""
tools/commit-mine --card c -m m --expect-lines 2 ok.mjs >/dev/null 2>&1; check "valid mjs exit" "$?" 0
printf '%s\nmodule.exports = {\n' "$W" > bad.cjs
tools/commit-mine --card c -m m --expect-lines 2 bad.cjs >/dev/null 2>&1; check "unparseable cjs exit" "$?" 6
rm bad.mjs bad.cjs
# Parse check uses the file's real module type: .js follows the nearest package.json "type" (default CJS).
mkdir -p cjspkg esmpkg; echo '{"type":"commonjs"}' > cjspkg/package.json; echo '{"type":"module"}' > esmpkg/package.json
git add cjspkg/package.json esmpkg/package.json; git commit -q -m pkgs
printf '%s\nimport fs from "fs"\nexport const e = fs\n' "$W" > cjspkg/esm.js
tools/commit-mine --card c -m m --expect-lines 3 cjspkg/esm.js >/dev/null 2>&1; check "esm-only js under cjs package exit" "$?" 6
printf '%s\nimport fs from "fs"\nexport const e = fs\n' "$W" > bare.js
tools/commit-mine --card c -m m --expect-lines 3 bare.js >/dev/null 2>&1; check "esm-only js without package.json exit" "$?" 6
printf '%s\nimport fs from "fs"\nexport const e = fs\n' "$W" > esmpkg/esm.js
tools/commit-mine --card c -m m --expect-lines 3 esmpkg/esm.js >/dev/null 2>&1; check "esm js under esm package exit" "$?" 0
printf '%s\nmodule.exports = { a: 1 }\n' "$W" > cjspkg/ok.js
tools/commit-mine --card c -m m --expect-lines 2 cjspkg/ok.js >/dev/null 2>&1; check "cjs js under cjs package exit" "$?" 0
printf '%s\nimport fs from "fs"\nexport const e = fs\n' "$W" > esm.cjs
tools/commit-mine --card c -m m --expect-lines 3 esm.cjs >/dev/null 2>&1; check "esm syntax in cjs exit" "$?" 6
# Hunks mode parses the patch-applied blob: a patch that leaves a broken file is refused.
printf '%s\nexport const a = 1\n' "$W" > p.mjs; git add p.mjs; git commit -q -m p
printf '%s\nexport const a = (\nexport const z = 2\n' "$W" > p.mjs
git diff p.mjs > p.patch
tools/commit-mine --card c -m m --hunks p.patch --expect-hunks 1 p.mjs >/dev/null 2>&1; check "hunks mode unparseable exit" "$?" 6
check "hunks mode refusal leaves index clean" "$(git diff --cached --name-only)" ""
git checkout -q p.mjs
rm -f bare.js esm.cjs p.patch
# docs/ and .claude/ are committed only on the owner's word.
mkdir -p docs .claude; echo d > docs/a.md; echo c > .claude/s.json
out=$(tools/commit-mine --card c -m m --expect-lines 1 docs/a.md 2>&1); check "docs refused exit" "$?" 1
check "docs refusal text" "$(grep -c 'docs/ and .claude/ are committed only when the owner says so' <<<"$out")" 1
tools/commit-mine --card c -m m --expect-lines 1 .claude/s.json >/dev/null 2>&1; check ".claude refused exit" "$?" 1
tools/commit-mine --card c -m m --expect-hunks --owner-said x docs/a.md >/dev/null 2>&1; check "owner-said swallowing exit" "$?" 1
tools/commit-mine --card c -m m --owner-said c2 --expect-lines 1 docs/a.md >/dev/null 2>&1; check "docs with --owner-said exit" "$?" 0
# Foreign hunks must not ride along. A change already staged in the same file: hunks mode refuses, HEAD unchanged.
printf 'a\nb\nc\nd\ne\nf\ng\nh\ni\nj\n' > k.txt; git add k.txt; git commit -q -m k
printf 'a\nb\nc\nd\ne\nf\ng\nh\ni\nJ\n' > k.txt; git add k.txt   # foreign agent staged this
printf 'A\nb\nc\nd\ne\nf\ng\nh\ni\nJ\n' > k.txt
printf 'diff --git a/k.txt b/k.txt\n--- a/k.txt\n+++ b/k.txt\n@@ -1 +1 @@\n-a\n+A\n' > k.patch
before=$(git rev-parse HEAD)
tools/commit-mine --card c -m m --hunks k.patch --expect-hunks 1 k.txt >/dev/null 2>&1; check "pre-staged foreign hunk exit" "$?" 3
check "pre-staged foreign hunk: HEAD unchanged" "$(git rev-parse HEAD)" "$before"
check "pre-staged foreign hunk left staged" "$(git diff --cached --name-only)" "k.txt"
git restore --staged k.txt; git checkout -q k.txt; rm k.patch
# Path mode: a file edited between the hunk check and staging is refused, nothing committed.
seq 1 20 > r.txt; git add r.txt; git commit -q -m r
seq 1 20 | sed 's/^1$/y/' > r.txt; before=$(git rev-parse HEAD)
COMMIT_MINE_TEST_BEFORE_STAGE='echo q >> r.txt' \
  tools/commit-mine --card c -m m --expect-lines 2 r.txt >/dev/null 2>&1; check "edit during commit exit" "$?" 7
check "edit during commit: HEAD unchanged" "$(git rev-parse HEAD)" "$before"
check "edit during commit: index clean" "$(git diff --cached --name-only)" ""
git checkout -q r.txt
# Adjacent foreign hunk merges with mine into one hunk: the hunk count matches, the line count does not.
seq 1 12 > m.txt; git add m.txt; git commit -q -m m
seq 1 12 | sed 's/^5$/five-foreign/;s/^6$/six-mine/' > m.txt; before=$(git rev-parse HEAD)
check "merged hunks show as one" "$(git diff m.txt | grep -c '^@@')" 1
tools/commit-mine --card c -m m --expect-hunks 1 m.txt >/dev/null 2>&1; check "path mode without --expect-lines exit" "$?" 5
out=$(tools/commit-mine --card c -m m --expect-hunks 1 --expect-lines 2 m.txt 2>&1); check "adjacent foreign hunk exit" "$?" 5
check "adjacent foreign hunk: HEAD unchanged" "$(git rev-parse HEAD)" "$before"
check "adjacent foreign hunk: counts shown" "$(grep -c 'Total: 4 line' <<<"$out")" 1
tools/commit-mine --card c -m m --expect-lines 4 m.txt >/dev/null 2>&1; check "right line count commits" "$?" 0
exit $fail
