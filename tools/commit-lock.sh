# Sourced: take_lock / release for the shared commit lock ($common = the repo's git common dir). Reclaims a lock whose owner is dead.
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
take_lock() {
  local start=$SECONDS
  until mkdir "$lock" 2>/dev/null; do
    reclaim_dead && continue
    [ $((SECONDS - start)) -lt "${COMMIT_LOCK_TIMEOUT:-600}" ] || { echo "could not take $lock" >&2; exit 1; }
    sleep 1
  done
  echo $$ > "$lock/pid"
  trap release EXIT
}
# checkout_of <branch>: the path of the worktree that has the branch checked out (empty if none).
checkout_of() { git worktree list --porcelain | awk -v b="refs/heads/$1" '/^worktree /{w=substr($0,10)} $0=="branch " b{print w}'; }
