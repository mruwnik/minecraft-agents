#!/usr/bin/env bash
# A fixer's own worktree on branch fix/<card8>, cut from the main checkout's current branch.
# Usage: tools/fixer-worktree.sh <card8> | --remove <card8>
# Prints the worktree path (last line). Dir: $FIXER_WT_ROOT (default ~/.claude/jobs/worktrees)/fix-<card8>.
# Shared with the main checkout by symlink, so tools and tests there write to the same place: node_modules, worlds/ (bodies, plans, zones),
# state/, textures/ and the frozen pathfinding fixtures. Blueprints stay per checkout (tracked files): bodies started from the worktree
# need --worlds <main>/worlds --blueprints <main>/blueprints. No builds here: the first tools/compile or run_tests there starts the
# worktree's own shadow-cljs server (1-2 GB, stops itself when idle); --remove stops it by pid.
set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
root=${FIXER_WT_ROOT:-$HOME/.claude/jobs/worktrees}
valid() { [[ "${1-}" =~ ^[0-9a-f]{8}$ ]] || { echo "Usage: $0 <card8> | --remove <card8> (8 hex chars)" >&2; exit 1; }; }

if [ "${1-}" = --remove ]; then
  valid "${2-}"
  wt=$root/fix-$2
  pid=$(cat "$wt/dashboard/.shadow-cljs/server.pid" 2>/dev/null) && kill "$pid" 2>/dev/null || true
  git -C "$repo" worktree remove --force "$wt"
  git -C "$repo" branch -D "fix/$2" >/dev/null
  exit
fi

valid "${1-}"
wt=$root/fix-$1
mkdir -p "$root"
git -C "$repo" worktree add -q -b "fix/$1" "$wt" "$(git -C "$repo" symbolic-ref --short HEAD)"

for d in . engine dashboard; do
  ln -s "$repo/$d/node_modules" "$wt/$d/node_modules"
done
for d in worlds state textures; do
  [ ! -e "$repo/$d" ] || ln -s "$repo/$d" "$wt/$d"
done
mkdir -p "$wt/dashboard/.shadow-cljs" "$wt/engine/test/fixtures" "$wt/engine/out"
ln -s "$repo/engine/test/fixtures/pathfinding" "$wt/engine/test/fixtures/pathfinding"
[ ! -f "$repo/engine/out/test-ns-ms.json" ] || cp "$repo/engine/out/test-ns-ms.json" "$wt/engine/out/"
# .gitignore lists these as directories (trailing slash), which does not match the symlinks.
exclude=$(git -C "$repo" rev-parse --git-path info/exclude)
for p in /node_modules /dashboard/node_modules /engine/node_modules /worlds /state /textures /engine/test/fixtures/pathfinding; do
  grep -qxF "$p" "$exclude" 2>/dev/null || echo "$p" >> "$exclude"
done
echo "$wt"
