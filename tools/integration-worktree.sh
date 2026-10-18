#!/usr/bin/env bash
# Set up a fresh git worktree for integration testing.
# Usage: tools/integration-worktree.sh <commit> <dir> | --remove <dir>
# Checks out <commit> at <dir>, then adds what a clean checkout lacks (all git-ignored): node_modules (symlinked to the main
# checkout's), the .shadow-cljs dir (with the :test cache seeded by tools/seed-shadow-cache.sh), worlds/test (symlinked to the main checkout's: world.json, biomes.json and the bodies; nothing else under worlds/), textures/, engine/test/fixtures/pathfinding, engine/out/test-ns-ms.json (shard timeouts), the built agent-tools.cjs, world-test.cjs and rcon-tools.cjs bundles and the viewer cljs.
# At the end it prints the body flags that point the worktree at the main checkout's worlds and blueprints (untracked blueprints are not in the checkout).
# Any failing step fails the script. Each build there is a one-shot shadow-cljs JVM under a res-slot `server` slot (tools/compile);
# remove the worktree afterwards with `tools/integration-worktree.sh --remove <dir>`.
set -euo pipefail
export RES_SLOT_HEAVY=1 # its compiles may take the reserved server slot (tools/res-slot.json)

if [ "${1:-}" = --remove ] && [ $# -eq 2 ]; then
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
touch "$wt/.res-slot-heavy" # tools/res-slot treats runs from this checkout as heavy (agents' test runs cannot set env)
mkdir -p "$wt/dashboard/.shadow-cljs" "$wt/worlds"
"$repo/tools/seed-shadow-cache.sh" "$wt"
ln -s "$repo/worlds/test" "$wt/worlds/test"
cp -r "$repo/textures" "$wt/"
mkdir -p "$wt/engine/test/fixtures"
cp -r "$repo/engine/test/fixtures/pathfinding" "$wt/engine/test/fixtures/"
mkdir -p "$wt/engine/out"
[ -f "$repo/engine/out/test-ns-ms.json" ] && cp "$repo/engine/out/test-ns-ms.json" "$wt/engine/out/" || true

"$wt/tools/compile" dashboard agent-tools --release
"$wt/tools/compile" dashboard world-test --release # the world suites and tools/world-sweep.mjs load it
"$wt/tools/compile" dashboard rcon-tools --release
node "$wt/tools/view/build-cljs.mjs"
echo "Worktree ready at $wt"
echo "Start a body from it with: --worlds $repo/worlds --blueprints $repo/blueprints"
