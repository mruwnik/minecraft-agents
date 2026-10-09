#!/usr/bin/env bash
# Land the current fix/<card8> worktree's commits on the main checkout's branch: rebase, then fast-forward, under the commit lock.
# Usage (from the worktree): tools/land.sh [--rebased-ok]
# Exit 0: landed; the worktree and its branch are removed (cd out of it) and the new hashes are printed.
# Exit 3: the branch was behind and is now rebased. What you tested is not what would land: rerun your test namespaces, then land.sh again.
#         --rebased-ok lands the rebased branch anyway (use only when the upstream commits it printed touch none of your files).
# Exit 1: rebase conflict (aborted; branch unchanged), dirty worktree, or the main checkout is not on the branch you were cut from.
# Exit 4: the main checkout has uncommitted edits to a file the landing changes (git refused; nothing moved): wait or ask the lead.
set -euo pipefail

ok=0; [ "${1-}" != --rebased-ok ] || ok=1
branch=$(git symbolic-ref --short HEAD)
[[ "$branch" == fix/* ]] || { echo "land: not on a fix/* branch ($branch)" >&2; exit 1; }
git diff --quiet HEAD -- || { echo "land: tracked changes not committed" >&2; exit 1; }
common=$(cd "$(git rev-parse --git-common-dir)" && pwd)
main=$(dirname "$common")
target=$(git -C "$main" symbolic-ref --short HEAD)
lock=$common/commit-lock

release() { ( flock 8; rm -rf "$lock" ) 8>"$lock.guard"; }
reclaim_dead() {
  (
    flock 8
    owner=$(cat "$lock/pid" 2>/dev/null) || exit 1
    [[ "$owner" =~ ^[0-9]+$ ]] && ! kill -0 "$owner" 2>/dev/null || exit 1
    rm -rf "$lock"
  ) 8>"$lock.guard"
}
start=$SECONDS
until mkdir "$lock" 2>/dev/null; do
  reclaim_dead && continue
  [ $((SECONDS - start)) -lt "${COMMIT_LOCK_TIMEOUT:-600}" ] || { echo "land: could not take $lock" >&2; exit 1; }
  sleep 1
done
echo $$ > "$lock/pid"
trap release EXIT

old=$(git rev-parse "$target")
if [ "$(git merge-base HEAD "$target")" != "$old" ]; then
  upstream=$(git log --oneline "$(git merge-base HEAD "$target")..$target")
  git rebase -q "$target" || { git rebase --abort; echo "land: rebase conflict; resolve by hand against $target" >&2; exit 1; }
  echo "land: rebased onto:"; echo "$upstream"
  [ "$ok" = 1 ] || { echo "land: rerun your tests, then land.sh again" >&2; exit 3; }
fi

git -C "$main" merge --ff-only -q "$branch" || { echo "land: ff-only merge refused (see above)" >&2; exit 4; }
git -C "$main" log --oneline "$old..$target"
release; trap - EXIT
card8=${branch#fix/}
cd "$main"
"$main/tools/fixer-worktree.sh" --remove "$card8"
