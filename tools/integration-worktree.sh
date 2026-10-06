#!/bin/bash
# Set up a fresh git worktree for integration testing.
# Usage: tools/integration-worktree.sh <commit> <dir>
# Creates a worktree at <dir> checked out to <commit>, then populates it with
# necessary ignored files and builds required outputs.

set -e

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <commit> <dir>" >&2
  exit 1
fi

commit="$1"
worktree_dir="$2"

# Get the repo root (directory containing this script)
repo_root=$(cd "$(dirname "$0")/.." && pwd)

# Create worktree at the given commit
git -C "$repo_root" worktree add "$worktree_dir" "$commit"

# Copy ignored files and directories needed for tests to run
# These files are .gitignored but required for the test environment

# Create directories if they don't exist
mkdir -p "$worktree_dir/worlds/claude"

# Copy biomes.json (needed by world setup)
if [[ -f "$repo_root/worlds/claude/biomes.json" ]]; then
  cp "$repo_root/worlds/claude/biomes.json" "$worktree_dir/worlds/claude/"
fi

# Copy textures directory (needed for rendering)
if [[ -d "$repo_root/textures" ]]; then
  cp -r "$repo_root/textures" "$worktree_dir/"
fi

# Build dashboard agent-tools.cjs
# This is needed for integration tests
if [[ -f "$worktree_dir/dashboard/package.json" ]]; then
  (cd "$worktree_dir/dashboard" && npm run build-agent-tools 2>/dev/null || true)
fi

# Build viewer cljs
# Create .shadow-cljs directories needed by shadow-cljs
mkdir -p "$worktree_dir/.shadow-cljs"

# Ensure out/ directory exists for build outputs
mkdir -p "$worktree_dir/out"

echo "Worktree setup complete at $worktree_dir"
