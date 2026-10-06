#!/usr/bin/env bash
# Set up a fresh git worktree for integration testing.
# Usage: tools/integration-worktree.sh <commit> <dir> | --remove <dir>
# Checks out <commit> at <dir>, then adds what a clean checkout lacks (all git-ignored): node_modules (symlinked to the main
# checkout's), the .shadow-cljs dirs, worlds/claude/biomes.json, textures/, engine/test/fixtures/pathfinding, the built agent-tools.cjs and the viewer cljs.
# Any failing step fails the script. The builds there start shadow servers (each under a res-slot `server` slot, 1-2 GB);
# remove the worktree afterwards with `tools/integration-worktree.sh --remove <dir>`: it kills those servers by PID, then removes it.
set -euo pipefail

if [ "${1:-}" = --remove ] && [ $# -eq 2 ]; then
  for d in engine dashboard; do
    pid=$(cat "$2/$d/.shadow-cljs/server.pid" 2>/dev/null) && kill "$pid" 2>/dev/null || true
  done
  git -C "$(cd "$(dirname "$0")/.." && pwd)" worktree remove --force "$2"
  exit
fi

[ $# -eq 2 ] || { echo "Usage: $0 <commit> <dir>" >&2; exit 1; }
repo=$(cd "$(dirname "$0")/.." && pwd)
git -C "$repo" worktree add --detach "$2" "$1"
wt=$(cd "$2" && pwd)

for d in . engine dashboard; do
  ln -s "$repo/$d/node_modules" "$wt/$d/node_modules"
done
mkdir -p "$wt/engine/.shadow-cljs" "$wt/dashboard/.shadow-cljs" "$wt/worlds/claude"
cp "$repo/worlds/claude/biomes.json" "$wt/worlds/claude/"
cp -r "$repo/textures" "$wt/"
mkdir -p "$wt/engine/test/fixtures"
cp -r "$repo/engine/test/fixtures/pathfinding" "$wt/engine/test/fixtures/"

"$wt/tools/compile" dashboard agent-tools --release
node "$wt/tools/view/build-cljs.mjs"
echo "Worktree ready at $wt"
