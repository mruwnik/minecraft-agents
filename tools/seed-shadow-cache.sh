#!/usr/bin/env bash
# tools/seed-shadow-cache.sh <worktree> [build...]: copy shadow-cljs's analyzer cache (dashboard/.shadow-cljs/builds/<build>, default: test)
# from the checkout that compiled most recently into a fresh worktree, so its first compile reuses it (about 35 s instead of 65 s).
# Source: the main checkout or any of its worktrees, whichever has the newest cache; env SEED_CACHE_FROM=<checkout> forces one.
# The cache is per file, keyed by content and dependencies: a cache from another revision only costs recompiles, never stale output.
# Plain copy (shadow rewrites cache files in place, so hardlinks would corrupt the source). A checkout whose compile.lock is held
# (a compile is writing its cache) is skipped. Never copies server/port/pid/lock files; outputs nothing and succeeds when no source exists.
set -euo pipefail
wt=${1:?usage: $0 <worktree> [build...]}; shift
builds=("$@"); [ ${#builds[@]} -gt 0 ] || builds=(test)
repo=$(dirname "$(cd "$(dirname "$0")" && git rev-parse --path-format=absolute --git-common-dir)")
wt=$(cd "$wt" && pwd)

best=; best_t=0
if [ -n "${SEED_CACHE_FROM:-}" ]; then
  best=$SEED_CACHE_FROM
else
  for c in $(git -C "$repo" worktree list --porcelain | sed -n 's/^worktree //p'); do
    [ "$c" != "$wt" ] && [ -d "$c/dashboard/.shadow-cljs/builds/${builds[0]}" ] || continue
    t=$(find "$c/dashboard/.shadow-cljs/builds/${builds[0]}" -type f -printf '%T@\n' | sort -n | tail -1)
    [ -n "$t" ] || continue
    if [ -z "$best" ] || [ "${t%.*}" -gt "$best_t" ]; then best=$c; best_t=${t%.*}; fi
  done
fi
[ -n "$best" ] || exit 0
src=$best/dashboard/.shadow-cljs
if [ -e "$src/compile.lock" ]; then
  exec 9<"$src/compile.lock"
  flock -n 9 || exit 0
fi
for b in "${builds[@]}"; do
  [ -d "$src/builds/$b" ] || continue
  mkdir -p "$wt/dashboard/.shadow-cljs/builds"
  rm -rf "$wt/dashboard/.shadow-cljs/builds/$b"
  cp -r "$src/builds/$b" "$wt/dashboard/.shadow-cljs/builds/$b"
done
