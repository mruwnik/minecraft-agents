#!/usr/bin/env bash
# Merge master into job-contract (or the branch given) in the checkout that has it, under the commit lock.
# Usage: tools/merge-master.sh [branch]   Exit 0: merged (or up to date). Exit 1: conflict (merge aborted, branch unchanged, files named),
# dirty checkout, or no checkout has the branch. Exit 4: uncommitted edits in the checkout block the merge.
set -euo pipefail

branch=${1-job-contract}
common=$(cd "$(git rev-parse --git-common-dir)" && pwd)
source "$(dirname "$0")/commit-lock.sh"
dest=$(checkout_of "$branch")
[ -n "$dest" ] || { echo "merge-master: no checkout has $branch checked out" >&2; exit 1; }
take_lock
git -C "$dest" diff --quiet HEAD -- || { echo "merge-master: $dest has uncommitted tracked changes" >&2; exit 4; }

old=$(git -C "$dest" rev-parse HEAD)
if ! out=$(git -C "$dest" merge --no-edit -q master 2>&1); then
  files=$(git -C "$dest" diff --name-only --diff-filter=U)
  if [ -z "$files" ]; then
    printf 'merge-master: merge failed:\n%s\n' "$out" >&2
    exit 1
  fi
  git -C "$dest" merge --abort
  printf 'merge-master: conflict, merge aborted; conflicting files:\n%s\n' "$files" >&2
  exit 1
fi
git -C "$dest" log --oneline "$old..HEAD"
echo "merge-master: $branch at $(git -C "$dest" rev-parse --short HEAD)"
