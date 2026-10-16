#!/usr/bin/env bash
# A fixer's own worktree on branch fix/<card8>, cut from --base <branch> (default: the main checkout's current branch);
# tools/land.sh lands it back on the checkout of that branch.
# Usage: tools/fixer-worktree.sh [--base <branch>] <card8> | --remove <card8> | --checkout <existing-branch> <dir>
# --checkout makes a long-lived checkout (e.g. ../minecraft-agents-jc on job-contract) with the same shared links.
# Prints the worktree path (last line). Dir: $FIXER_WT_ROOT (default ~/worktrees)/fix-<card8>; not under ~/.claude, whose files the Write tool refuses.
# Shared with the main checkout by symlink, so tools and tests there write to the same place: node_modules, worlds/ (bodies, plans, zones),
# state/, textures/ and the frozen pathfinding fixtures. Blueprints stay per checkout (tracked files): bodies started from the worktree
# need --worlds <main>/worlds --blueprints <main>/blueprints. No builds here; a worktree runs no shadow-cljs server (tools/compile).
# The :test build's analyzer cache is copied from the most recently compiled checkout (tools/seed-shadow-cache.sh), so the first compile is warm.
set -euo pipefail

# The main checkout (source of the shared links), whichever checkout this script runs from.
repo=$(dirname "$(cd "$(dirname "$0")" && git rev-parse --path-format=absolute --git-common-dir)")
root=${FIXER_WT_ROOT:-$HOME/worktrees}
usage="Usage: $0 [--base <branch>] <card8> | --remove <card8> | --checkout <branch> <dir> (card8 = 8 hex chars)"
valid() { [[ "${1-}" =~ ^[0-9a-f]{8}$ ]] || { echo "$usage" >&2; exit 1; }; }
case "${1-}" in
  -h|--help) echo "$usage"; exit 0 ;;
  -*) [[ "$1" =~ ^--(remove|checkout|base)$ ]] || { echo "$usage" >&2; exit 2; } ;;
esac

if [ "${1-}" = --remove ]; then
  valid "${2-}"
  # Found by its branch, so a worktree cut under an older root is removed too.
  wt=$(git -C "$repo" worktree list --porcelain | awk -v b="refs/heads/fix/$2" '/^worktree /{p=substr($0,10)} $0=="branch "b{print p}')
  git -C "$repo" worktree remove --force "${wt:-$root/fix-$2}"
  git -C "$repo" branch -D "fix/$2" >/dev/null
  exit
fi

base=
if [ "${1-}" = --checkout ]; then
  [ -n "${2-}" ] && [ -n "${3-}" ] || valid x
  wt=$3
  git -C "$repo" worktree add -q "$wt" "$2"
else
  [ "${1-}" != --base ] || { base=${2-}; shift 2; }
  valid "${1-}"
  wt=$root/fix-$1
  mkdir -p "$root"
  [ -n "$base" ] || base=$(git -C "$repo" symbolic-ref --short HEAD)
  git -C "$repo" worktree add -q -b "fix/$1" "$wt" "$base"
  git -C "$repo" config "branch.fix/$1.fixbase" "$base"
fi

ln -s "$repo/node_modules" "$wt/node_modules"
for d in engine dashboard; do
  ln -s "$repo/$d/node_modules" "$wt/$d/node_modules"
done
for d in worlds state textures; do
  [ ! -e "$repo/$d" ] || ln -s "$repo/$d" "$wt/$d"
done
mkdir -p "$wt/dashboard/.shadow-cljs" "$wt/engine/test/fixtures" "$wt/engine/out"
ln -s "$repo/engine/test/fixtures/pathfinding" "$wt/engine/test/fixtures/pathfinding"
[ ! -f "$repo/engine/out/test-ns-ms.json" ] || cp "$repo/engine/out/test-ns-ms.json" "$wt/engine/out/"
# .gitignore lists these as directories (trailing slash), which does not match the symlinks.
exclude=$(git -C "$repo" rev-parse --path-format=absolute --git-path info/exclude)
for p in /node_modules /dashboard/node_modules /engine/node_modules /worlds /state /textures /engine/test/fixtures/pathfinding; do
  grep -qxF "$p" "$exclude" 2>/dev/null || echo "$p" >> "$exclude"
done
"$(dirname "$0")/seed-shadow-cache.sh" "$wt"
echo "$wt"
