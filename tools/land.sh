#!/usr/bin/env bash
# Land the current fix/<card8> worktree's commits on the checkout of the branch it was cut from (the main checkout's, unless --base was used): rebase, then fast-forward, under the commit lock.
# Usage (from the worktree): tools/land.sh [--rebased-ok]
# Exit 0: landed; the worktree and its branch are removed (cd out of it) and the new hashes are printed.
# Docs (*.md) never count: a rebase over commits that changed only docs lands at once, and a docs overlap is no overlap.
# Exit 3: the branch was behind and is now rebased. What you tested is not what would land: rerun your test namespaces, then land.sh again.
#         --rebased-ok lands the rebased branch anyway (use only when the upstream commits it printed touch none of your files).
# Exit 5: the branch was rebased over commits that touch files it changes and no clean test run (a .claude/live-tests/*.log ending
#         "0 failures, 0 errors"; for a change of engine/fixtures/ files only, a fixtures-suite log "0 failed" counts too) is newer than that rebase; the overlapping files are named. Rerun the tests, then land.sh again.
# Exit 1: rebase conflict (aborted; branch unchanged), dirty worktree, or no checkout has the branch you were cut from.
# A landing that changes the agent-tools bundle sources starts a detached rebuild of the main checkout bundle.
# Exit 6: a commit being landed adds a conflict marker (<<<<<<<, =======, >>>>>>>); the file:line is printed.
# Exit 4: the main checkout has uncommitted edits to a file the landing changes (git refused; nothing moved): wait or ask the lead.
set -euo pipefail

usage() { sed -n '2,/^set -/{/^#/s/^# \{0,1\}//p}' "$0"; }
ok=0
case "${1-}" in
  "") ;;
  --rebased-ok) ok=1 ;;
  -h|--help) usage; exit 0 ;;
  *) usage >&2; exit 2 ;;
esac
[ $# -le 1 ] || { usage >&2; exit 2; }
branch=$(git symbolic-ref --short HEAD)
[[ "$branch" == fix/* ]] || { echo "land: not on a fix/* branch ($branch)" >&2; exit 1; }
git diff --quiet HEAD -- || { echo "land: tracked changes not committed" >&2; exit 1; }
common=$(cd "$(git rev-parse --git-common-dir)" && pwd)
repo=$(dirname "$common")
# The branch the fix branch was cut from (fixer-worktree.sh records it), landed on the checkout that has it checked out.
target=$(git config "branch.$branch.fixbase") || { echo "land: no branch.$branch.fixbase recorded; cut the branch with tools/fixer-worktree.sh [--base <branch>]" >&2; exit 1; }
source "$(dirname "$0")/commit-lock.sh"
main=$(checkout_of "$target")
[ -n "$main" ] || { echo "land: no checkout has $target checked out" >&2; exit 1; }
take_lock

marker=$(git rev-parse --absolute-git-dir)/land-overlap
old=$(git rev-parse "$target")
if [ "$(git merge-base HEAD "$target")" != "$old" ]; then
  base=$(git merge-base HEAD "$target")
  upstream=$(git log --oneline "$base..$target")
  git rebase -q "$target" || { git rebase --abort; echo "land: rebase conflict; resolve by hand against $target" >&2; exit 1; }
  echo "land: rebased onto:"; echo "$upstream"
  # Docs (*.md) cannot break a test: they never count as an overlap, and a rebase over docs alone lands at once.
  upstream_code=$(git diff --name-only "$base" "$old" | grep -v '\.md$' | sort || true)
  overlap=$(comm -12 <(printf '%s\n' "$upstream_code") <(git diff --name-only "$old" HEAD | sort))
  [ -z "$overlap" ] || { printf '%s\n' "$overlap" >> "$marker"; touch "$marker"; }
  [ "$ok" = 1 ] || [ -z "$upstream_code" ] || { echo "land: rerun your tests, then land.sh again" >&2; exit 3; }
fi

# Only conflict markers are taken from --check: its whitespace warnings do not block.
markers=$(git diff --check "$target" HEAD | grep 'leftover conflict marker' || true)
[ -z "$markers" ] || { echo "land: commits add conflict markers; remove them:" >&2; echo "$markers" >&2; exit 6; }

# A rebase over commits that touch this branch's files must be followed by a clean test run before it lands.
if [ -s "$marker" ]; then
  top=$(git rev-parse --show-toplevel)
  pass='(^|[^0-9])0 failures, 0 errors'
  # A fixture-only change is tested by the offline fixtures check, whose log says "0 failed".
  [ -z "$(git diff --name-only "$target" HEAD | grep -v '^engine/fixtures/' || true)" ] && pass="$pass|world-test --check: [0-9]+ cases, 0 failed"
  clean=$(find "$top/.claude/live-tests" -name '*.log' -newer "$marker" -exec grep -lE "$pass" {} + 2>/dev/null | head -1 || true)
  [ -n "$clean" ] || { echo "land: rebased over commits that touch your files; no clean test run since. Run the tests, then land.sh again. Overlap:" >&2; sort -u "$marker" >&2; exit 5; }
fi

git -C "$main" merge --ff-only -q "$branch" || { echo "land: ff-only merge refused (see above)" >&2; exit 4; }
git -C "$main" log --oneline "$old..$target"
release; trap - EXIT
card8=${branch#fix/}
cd "$main"
"$repo/tools/fixer-worktree.sh" --remove "$card8"
# The agent tools run from the main checkout's compiled bundle: rebuild it (detached; the compile queues) when this landing
# changed its sources (engine/tools/agent-tools-bundle-check.mjs bundleSources), so its stale-bundle warning stays true.
if git diff --name-only "$old" "$target" | grep -qE '^(dashboard/src/agent_tools/|dashboard/src/dashboard/(agent_plan_tools|plan_compare)\.cljs$|engine/src/plan/|engine/src/engine/bodies\.cljs$)'; then
  nohup tools/compile dashboard agent-tools >/dev/null 2>&1 &
fi
