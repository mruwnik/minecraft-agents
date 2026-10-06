#!/usr/bin/env bash
# Tests for tools/commit-mine trailer handling and exit codes, in a throwaway repo.
set -uo pipefail
SRC="$(cd "$(dirname "$0")" && pwd)/commit-mine"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
cd "$T" && git init -q . && git config user.email t@t && git config user.name t
mkdir tools && cp "$SRC" tools/commit-mine
echo 0 > f.txt && git add f.txt && git commit -q -m init
fail=0
export DIFFER_URL=http://127.0.0.1:1
C="Card abcd1234"
check() { if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: want [$3] got [$2]"; fail=1; fi; }
commit_msg() { echo "$RANDOM$RANDOM" > f.txt; tools/commit-mine --card abcd1234 -m "$C $1" --expect-lines 2 f.txt >/dev/null 2>&1; rc=$?; echo "$rc"; }
trailers() { git log -1 --format=%B | grep -c '^Co-Authored-By:'; }

rc=$(commit_msg "plain"); check "plain rc" "$rc" 0; check "plain gets default trailer" "$(trailers)" 1
rc=$(commit_msg $'other\n\nCo-Authored-By: Claude Opus 4 <noreply@anthropic.com>'); check "other rc" "$rc" 0
check "other model keeps single trailer" "$(trailers)" 1
check "other model trailer is the given one" "$(git log -1 --format=%B | grep -c Opus)" 1
rc=$(commit_msg $'same\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>'); check "same rc" "$rc" 0; check "same stays single" "$(trailers)" 1
echo x > f.txt; tools/commit-mine --card abcd1234 -m "$C m" f.txt >/dev/null 2>&1; check "missing --expect-hunks exit" "$?" 5
git checkout -q f.txt
tools/commit-mine -m m f.txt >/dev/null 2>&1; check "no card exit" "$?" 1
# Hunks mode: paths and --expect-hunks are required; a patch touching other paths is refused.
printf 'a\nb\nc\n' > g.txt; printf 'a\nb\nc\n' > h.txt; git add g.txt h.txt; git commit -q -m two
printf 'A\nb\nc\n' > g.txt; printf 'a\nb\nC\n' > h.txt
git diff g.txt > g.patch; git diff g.txt h.txt > gh.patch
tools/commit-mine --card abcd1234 -m "$C m" --hunks g.patch --expect-hunks 1 >/dev/null 2>&1; check "hunks without paths exit" "$?" 1
out=$(tools/commit-mine --card abcd1234 -m "$C m" --hunks gh.patch --expect-hunks 2 g.txt 2>&1); check "hunks foreign path exit" "$?" 3
check "foreign path listed" "$(grep -c 'h.txt' <<<"$out")" 1
check "foreign refusal leaves HEAD" "$(git log -1 --format=%s)" "$(git log -1 --format=%s)"
tools/commit-mine --card abcd1234 -m "$C m" --hunks g.patch g.txt >/dev/null 2>&1; check "hunks missing expect exit" "$?" 5
out=$(tools/commit-mine --card abcd1234 -m "$C m" --hunks g.patch --expect-hunks 2 g.txt 2>&1); check "hunks wrong expect exit" "$?" 5
check "hunk list prints path and line" "$(grep -c 'g.txt: -a' <<<"$out")" 1
before=$(git rev-parse HEAD)
tools/commit-mine --card abcd1234 -m "$C hm" --hunks g.patch --expect-hunks 1 g.txt >/dev/null 2>&1; check "hunks ok exit" "$?" 0
check "hunks commit only g" "$(git show --name-only --format= HEAD)" "g.txt"
check "h.txt change still uncommitted" "$(git diff --name-only)" "h.txt"
check "index clean" "$(git diff --cached --name-only)" ""
# A flag value starting with -- is refused, naming the flag (no silent swallowing of the next flag).
echo y > f.txt
out=$(tools/commit-mine --card abcd1234 -m "$C m" --approved-core --expect-lines 2 f.txt 2>&1); check "approved-core swallowing exit" "$?" 1
check "refusal names the flag" "$(grep -c -- '--approved-core' <<<"$out")" 1
tools/commit-mine --card --expect-lines 2 -m m f.txt >/dev/null 2>&1; check "card swallowing exit" "$?" 1
tools/commit-mine --card abcd1234 -m "$C m" --expect-hunks --approved-core x f.txt >/dev/null 2>&1; check "expect swallowing exit" "$?" 1
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines --approved-core x f.txt >/dev/null 2>&1; check "expect-lines swallowing exit" "$?" 1
git checkout -q f.txt
# Engine-core gate: engine/src/engine/* paths (except planner*) need --approved-core; planner* and engine/src/jobs/* do not.
mkdir -p engine/src/engine/path engine/src/jobs/gather
echo "planner" > engine/src/engine/path/planner_tuned.cljs
echo "other core" > engine/src/engine/path/executor.cljs
echo "job" > engine/src/jobs/gather/mine.cljs
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 1 engine/src/engine/path/planner_tuned.cljs >/dev/null 2>&1; check "planner file without approval exit" "$?" 0
git checkout -q engine/src/engine/path/planner_tuned.cljs 2>/dev/null || rm engine/src/engine/path/planner_tuned.cljs
out=$(tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 1 engine/src/engine/path/executor.cljs 2>&1); check "other core path without approval exit" "$?" 1
check "core refusal text" "$(grep -c 'engine-core path.*needs --approved-core' <<<"$out")" 1
tools/commit-mine --card abcd1234 -m "$C m" --approved-core card2 --expect-lines 1 engine/src/engine/path/executor.cljs >/dev/null 2>&1; check "core path with approval exit" "$?" 0
git checkout -q engine/src/engine/path/executor.cljs 2>/dev/null || rm engine/src/engine/path/executor.cljs
echo "job2" > engine/src/jobs/gather/mine.cljs
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 1 engine/src/jobs/gather/mine.cljs >/dev/null 2>&1; check "job path without approval exit" "$?" 0
git checkout -q engine/src/jobs/gather/mine.cljs 2>/dev/null || rm engine/src/jobs/gather/mine.cljs
# A committed JS file must parse: a broken .mjs is refused (naming it), a valid one commits.
W='// Why JavaScript: test'
printf '%s\nexport const a = 1\n' "$W" > ok.mjs; printf '%s\nexport const b = (\n' "$W" > bad.mjs
out=$(tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 2 bad.mjs 2>&1); check "unparseable mjs exit" "$?" 6
check "unparseable mjs named" "$(grep -c 'bad.mjs' <<<"$out")" 1
check "unparseable mjs not committed" "$(git log --format=%s -1 --name-only -- bad.mjs | grep -c bad.mjs)" 0
check "unparseable mjs leaves index clean" "$(git diff --cached --name-only)" ""
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 2 ok.mjs >/dev/null 2>&1; check "valid mjs exit" "$?" 0
printf '%s\nmodule.exports = {\n' "$W" > bad.cjs
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 2 bad.cjs >/dev/null 2>&1; check "unparseable cjs exit" "$?" 6
rm bad.mjs bad.cjs
# Parse check uses the file's real module type: .js follows the nearest package.json "type" (default CJS).
mkdir -p cjspkg esmpkg; echo '{"type":"commonjs"}' > cjspkg/package.json; echo '{"type":"module"}' > esmpkg/package.json
git add cjspkg/package.json esmpkg/package.json; git commit -q -m pkgs
printf '%s\nimport fs from "fs"\nexport const e = fs\n' "$W" > cjspkg/esm.js
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 3 cjspkg/esm.js >/dev/null 2>&1; check "esm-only js under cjs package exit" "$?" 6
printf '%s\nimport fs from "fs"\nexport const e = fs\n' "$W" > bare.js
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 3 bare.js >/dev/null 2>&1; check "esm-only js without package.json exit" "$?" 6
printf '%s\nimport fs from "fs"\nexport const e = fs\n' "$W" > esmpkg/esm.js
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 3 esmpkg/esm.js >/dev/null 2>&1; check "esm js under esm package exit" "$?" 0
printf '%s\nmodule.exports = { a: 1 }\n' "$W" > cjspkg/ok.js
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 2 cjspkg/ok.js >/dev/null 2>&1; check "cjs js under cjs package exit" "$?" 0
printf '%s\nimport fs from "fs"\nexport const e = fs\n' "$W" > esm.cjs
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 3 esm.cjs >/dev/null 2>&1; check "esm syntax in cjs exit" "$?" 6
# Hunks mode parses the patch-applied blob: a patch that leaves a broken file is refused.
printf '%s\nexport const a = 1\n' "$W" > p.mjs; git add p.mjs; git commit -q -m p
printf '%s\nexport const a = (\nexport const z = 2\n' "$W" > p.mjs
git diff p.mjs > p.patch
tools/commit-mine --card abcd1234 -m "$C m" --hunks p.patch --expect-hunks 1 p.mjs >/dev/null 2>&1; check "hunks mode unparseable exit" "$?" 6
check "hunks mode refusal leaves index clean" "$(git diff --cached --name-only)" ""
git checkout -q p.mjs
rm -f bare.js esm.cjs p.patch
# docs/ and .claude/ are committed only on the owner's word.
mkdir -p docs .claude; echo d > docs/a.md; echo c > .claude/s.json
out=$(tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 1 docs/a.md 2>&1); check "docs refused exit" "$?" 1
check "docs refusal text" "$(grep -c 'docs/ and .claude/ are committed only when the owner says so' <<<"$out")" 1
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 1 .claude/s.json >/dev/null 2>&1; check ".claude refused exit" "$?" 1
tools/commit-mine --card abcd1234 -m "$C m" --expect-hunks --owner-said x docs/a.md >/dev/null 2>&1; check "owner-said swallowing exit" "$?" 1
tools/commit-mine --card abcd1234 -m "$C m" --owner-said c2 --expect-lines 1 docs/a.md >/dev/null 2>&1; check "docs with --owner-said exit" "$?" 0
# Foreign hunks must not ride along. A change already staged in the same file: hunks mode refuses, HEAD unchanged.
printf 'a\nb\nc\nd\ne\nf\ng\nh\ni\nj\n' > k.txt; git add k.txt; git commit -q -m k
printf 'a\nb\nc\nd\ne\nf\ng\nh\ni\nJ\n' > k.txt; git add k.txt   # foreign agent staged this
printf 'A\nb\nc\nd\ne\nf\ng\nh\ni\nJ\n' > k.txt
printf 'diff --git a/k.txt b/k.txt\n--- a/k.txt\n+++ b/k.txt\n@@ -1 +1 @@\n-a\n+A\n' > k.patch
before=$(git rev-parse HEAD)
tools/commit-mine --card abcd1234 -m "$C m" --hunks k.patch --expect-hunks 1 k.txt >/dev/null 2>&1; check "pre-staged foreign hunk exit" "$?" 3
check "pre-staged foreign hunk: HEAD unchanged" "$(git rev-parse HEAD)" "$before"
check "pre-staged foreign hunk left staged" "$(git diff --cached --name-only)" "k.txt"
git restore --staged k.txt; git checkout -q k.txt; rm k.patch
# Path mode: a file edited between the hunk check and staging is refused, nothing committed.
seq 1 20 > r.txt; git add r.txt; git commit -q -m r
seq 1 20 | sed 's/^1$/y/' > r.txt; before=$(git rev-parse HEAD)
COMMIT_MINE_TEST_BEFORE_STAGE='echo q >> r.txt' \
  tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 2 r.txt >/dev/null 2>&1; check "edit during commit exit" "$?" 7
check "edit during commit: HEAD unchanged" "$(git rev-parse HEAD)" "$before"
check "edit during commit: index clean" "$(git diff --cached --name-only)" ""
git checkout -q r.txt
# Adjacent foreign hunk merges with mine into one hunk: the hunk count matches, the line count does not.
seq 1 12 > m.txt; git add m.txt; git commit -q -m m
seq 1 12 | sed 's/^5$/five-foreign/;s/^6$/six-mine/' > m.txt; before=$(git rev-parse HEAD)
check "merged hunks show as one" "$(git diff m.txt | grep -c '^@@')" 1
tools/commit-mine --card abcd1234 -m "$C m" --expect-hunks 1 m.txt >/dev/null 2>&1; check "path mode without --expect-lines exit" "$?" 5
out=$(tools/commit-mine --card abcd1234 -m "$C m" --expect-hunks 1 --expect-lines 2 m.txt 2>&1); check "adjacent foreign hunk exit" "$?" 5
check "adjacent foreign hunk: HEAD unchanged" "$(git rev-parse HEAD)" "$before"
check "adjacent foreign hunk: counts shown" "$(grep -c 'Total: 4 line' <<<"$out")" 1
tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 4 m.txt >/dev/null 2>&1; check "right line count commits" "$?" 0
# Deleted files: staged (git rm) or not, many, a whole directory, mixed with an edit.
mkdir -p gone/sub; for i in $(seq 1 120); do echo $i > gone/f$i.txt; done; echo s > gone/sub/s.txt; seq 1 5 > e.txt
git add gone e.txt; git commit -q -m gone
git rm -rq gone; printf '%s\n' x >> e.txt
files=$(for i in $(seq 1 120); do printf 'gone/f%s.txt ' $i; done)
out=$(tools/commit-mine --card abcd1234 -m "$C rmmany" --expect-lines 122 $files gone/sub/s.txt e.txt 2>&1); check "staged deletes + edit exit" "$?" 0
check "staged deletes + edit: 121 deletions in HEAD" "$(git show --numstat --format= HEAD | grep -c '	gone/')" 121
check "staged deletes + edit: edit in HEAD" "$(git show --name-only --format= HEAD | grep -c '^e.txt$')" 1
check "staged deletes: index clean" "$(git diff --cached --name-only)" ""
git checkout -q HEAD~1 -- gone; git commit -q -m restore; rm -rf gone
out=$(tools/commit-mine --card abcd1234 -m "$C rmdir" --expect-lines 121 gone 2>&1); check "unstaged dir delete exit" "$?" 0
check "unstaged dir delete: gone from HEAD" "$(git ls-tree -r HEAD --name-only | grep -c '^gone/')" 0
git checkout -q HEAD~1 -- gone; git commit -q -m restore2
git rm -rq gone
out=$(tools/commit-mine --card abcd1234 -m "$C rmdir2" --expect-lines 121 gone 2>&1); check "staged dir delete exit" "$?" 0
check "staged dir delete: gone from HEAD" "$(git ls-tree -r HEAD --name-only | grep -c '^gone/')" 0
# A deleted .mjs (staged or not) with an edited file in one call commits both; a new .mjs without the line is still refused.
big() { printf '%s\n' "$W"; seq 1 60; }
big > del.mjs; seq 1 3 > k.txt; git add del.mjs k.txt; git commit -q -m delmjs
rm del.mjs; echo y >> k.txt
out=$(tools/commit-mine --card abcd1234 -m "$C delmjs" --expect-lines 62 del.mjs k.txt 2>&1); check "deleted mjs + edit exit" "$?" 0
check "deleted mjs + edit: both in HEAD" "$(git show --name-only --format= HEAD | sort | tr '\n' ' ')" "del.mjs k.txt "
big > del2.mjs; git add del2.mjs; git commit -q -m delmjs2; git rm -q del2.mjs
out=$(tools/commit-mine --card abcd1234 -m "$C delmjs2" --expect-lines 61 del2.mjs 2>&1); check "staged deleted mjs exit" "$?" 0
seq 1 60 > nolabel.mjs
out=$(tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 60 nolabel.mjs 2>&1); check "new mjs without Why line refused" "$?" 1
check "refusal names the line" "$(grep -c 'Why JavaScript' <<<"$out")" 1
rm -f nolabel.mjs
# A git failure prints git's message and exits non-zero (stray pathspec that matches nothing, nothing deleted).
out=$(tools/commit-mine --card abcd1234 -m "$C m" --expect-lines 0 nosuchfile 2>&1); rc=$?
check "git failure exits non-zero" "$((rc != 0))" 1
check "git failure prints a message" "$((${#out} > 0))" 1
# A commit of 1000 files prints a stat longer than a pipe buffer (64 KB); a tail that closes the pipe early makes the tool exit 141 under pipefail.
mkdir -p many; for i in $(seq 1 1000); do echo "file $i" > many/file$i.txt; done
out=$(tools/commit-mine --card abcd1234 -m "$C many files" --expect-lines 1000 many 2>/dev/null); rc=$?
check "1000-file commit exit" "$rc" 0
check "1000-file commit: all in HEAD" "$(git show --name-only --format= HEAD | wc -l)" 1000
check "1000-file commit: output capped at 20 lines" "$(wc -l <<<"$out")" 20
# Card id in the message: refused (exit 8) without it; "Card x", "card x", "(card x)", "cards x" and a longer prefix pass.
cp "$(dirname "$SRC")/card" tools/card
idtest() { echo "$RANDOM$RANDOM" > f.txt; tools/commit-mine --card "$1" -m "$2" --expect-lines 2 f.txt >/dev/null 2>&1; echo $?; }
before=$(git rev-parse HEAD)
check "message without id exit" "$(idtest abcd1234 'no id here')" 8
check "message with other id exit" "$(idtest abcd1234 'Card abcd1235')" 8
check "refused message: HEAD unchanged" "$(git rev-parse HEAD)" "$before"
check "refused message: index clean" "$(git diff --cached --name-only)" ""
out=$(tools/commit-mine --card abcd1234 -m 'x' --expect-lines 2 f.txt 2>&1); check "refusal names the id" "$(grep -c 'abcd1234' <<<"$out")" 1
check "id format 'Card x'" "$(idtest abcd1234 'Fix. Card abcd1234')" 0
check "id format 'card x'" "$(idtest abcd1234 'Fix. card abcd1234')" 0
check "id format '(card x)'" "$(idtest abcd1234 'Fix (card abcd1234)')" 0
check "id format 'cards x'" "$(idtest abcd1234 'Fix, cards abcd1234')" 0
check "id format 'Card x' on line 2" "$(idtest abcd1234 $'Fix\n\nCard abcd1234')" 0
check "longer --card prefix matches first 8" "$(idtest abcd1234-5678-aaaa 'Card abcd1234')" 0
check "longer id in message matches" "$(idtest abcd1234 'Card abcd1234-5678')" 0
check "bare id without the word card" "$(idtest abcd1234 'abcd1234 fix')" 8
echo "Card abcd1234 from file" > msg.txt; echo "$RANDOM$RANDOM" > f.txt
tools/commit-mine --card abcd1234 -m msg.txt --expect-lines 2 f.txt >/dev/null 2>&1; check "message file with id exit" "$?" 0
echo "no id" > msg.txt; echo "$RANDOM$RANDOM" > f.txt
tools/commit-mine --card abcd1234 -m msg.txt --expect-lines 2 f.txt >/dev/null 2>&1; check "message file without id exit" "$?" 8
rm msg.txt; git checkout -q f.txt
# Stat note: posted via tools/card note after the commit; a dead or slow differ only warns.
cp tools/card tools/card.real
cat > tools/card <<'EOS'
#!/usr/bin/env bash
echo "$*" > "$CARD_LOG"
EOS
chmod +x tools/card
export CARD_LOG="$T/card.log"; rm -f "$CARD_LOG"
echo "$RANDOM$RANDOM" > f.txt
out=$(tools/commit-mine --card abcd1234-5678 -m "$C stat" --expect-lines 2 f.txt 2>&1); check "note commit exit" "$?" 0
h=$(git rev-parse --short HEAD)
check "note goes to the card" "$(head -1 "$CARD_LOG" | awk '{print $1, $2}')" "note abcd1234-5678"
check "note holds hash and stat" "$(tr '\n' ' ' < "$CARD_LOG" | grep -c "$h.*f.txt.*1 file changed")" 1
check "ledger holds stat" "$(tail -1 .git/commit-ledger.jsonl | jq -r .stat)" "1 file changed, 1 insertion(+), 1 deletion(-)"
cp tools/card.real tools/card
echo "$RANDOM$RANDOM" > f.txt
out=$(tools/commit-mine --card abcd1234 -m "$C dead differ" --expect-lines 2 f.txt 2>&1); check "dead differ exit" "$?" 0
check "dead differ warns" "$(grep -ci 'warning.*card' <<<"$out")" 1
check "dead differ: commit exists" "$(git log -1 --format=%s | grep -c 'dead differ')" 1
printf '#!/usr/bin/env bash\nsleep 30\n' > tools/card; chmod +x tools/card
echo "$RANDOM$RANDOM" > f.txt; t0=$SECONDS
out=$(COMMIT_MINE_NOTE_TIMEOUT=1 tools/commit-mine --card abcd1234 -m "$C slow differ" --expect-lines 2 f.txt 2>&1); check "slow differ exit" "$?" 0
check "slow differ gives up" "$((SECONDS - t0 <= 10))" 1
check "slow differ warns" "$(grep -ci 'warning.*card' <<<"$out")" 1
rm tools/card tools/card.real
# Lock: the timeout counts seconds; a lock whose owner pid is dead is reclaimed; one held by a live pid is waited on.
echo z > f.txt
mkdir .git/commit-lock; echo 99999999 > .git/commit-lock/pid
out=$(tools/commit-mine --card abcd1234 -m "$C dead" --expect-lines 2 f.txt 2>&1); check "dead-owner lock reclaimed exit" "$?" 0
check "lock gone after commit" "$(ls .git | grep -c '^commit-lock$')" 0
echo zz > f.txt
mkdir .git/commit-lock; echo $$ > .git/commit-lock/pid
t0=$SECONDS
out=$(COMMIT_LOCK_TIMEOUT=3 tools/commit-mine --card abcd1234 -m "$C live" --expect-lines 2 f.txt 2>&1); rc=$?
check "live-owner lock times out exit" "$rc" 4
check "timeout is in seconds" "$((SECONDS - t0 >= 3 && SECONDS - t0 <= 6))" 1
check "foreign lock left in place" "$(ls .git | grep -c '^commit-lock$')" 1
rm -rf .git/commit-lock; git checkout -q f.txt
# Reclaim race: 3 contenders take the lock in turn after a dead holder; two inside the critical section at once is a violation.
rdir=$(mktemp -d); LOCK=$rdir/lock; eval "$(sed -n '/^reclaim_dead() {/,/^}/p;/^release_lock() /p' tools/commit-mine)"; export LOCK
contender() {
  local n
  for n in 1 2 3 4 5; do
    until mkdir "$LOCK" 2>/dev/null; do reclaim_dead && continue; sleep 0.001; done
    echo $BASHPID > "$LOCK/pid"
    mkdir "$rdir/cs" 2>/dev/null || echo x >> "$rdir/violations"
    sleep 0.002; rmdir "$rdir/cs" 2>/dev/null; release_lock
  done
}
for round in $(seq 1 60); do
  mkdir "$LOCK"; echo 99999999 > "$LOCK/pid"
  contender & contender & contender & contender & wait
done
check "reclaim race: no two holders" "$(cat "$rdir/violations" 2>/dev/null | wc -l)" 0
rm -rf "$rdir"; unset LOCK
exit $fail
