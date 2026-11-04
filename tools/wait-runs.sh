#!/bin/sh
# Wait in the foreground for test runs: checks every 30 s and returns as soon as any run has exited, or after
# 4 min so the agent can read messages and call again. Background waits rely on a wake notification that
# often never arrives, leaving runners idle for hours.
# Usage: tools/wait-runs.sh <run.events>...   Prints one line per run: done|running <file>.
[ $# -gt 0 ] || { echo "usage: $0 <run.events>..." >&2; exit 2; }
finished() { tail -n 1 "$1" 2>/dev/null | grep -q '"event":"exit"'; }
end=$(( $(date +%s) + 240 ))
while :; do
  for f in "$@"; do finished "$f" && break 2; done
  [ "$(date +%s)" -ge "$end" ] && break
  sleep 30
done
for f in "$@"; do
  if finished "$f"; then echo "done $f"; else echo "running $f"; fi
done
