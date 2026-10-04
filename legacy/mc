#!/bin/bash
# Usage: ./mc <action> [key=value ...] [-v]   e.g.  ./mc goto x=10 y=64 z=-3   |   ./mc chat message="hello there"
# Values are parsed as JSON when possible (numbers, arrays, objects), otherwise sent as strings.
# Output is one terse line per result (it is read by an LLM that pays per token); -v prints the full JSON.
exec node "$(dirname "$(readlink -f "$0")")/tools/mc.mjs" "$@"
