#!/bin/sh
# Why shell: thin wrapper so run_tests args (files, --test-name-pattern) reach node --test instead of being added to the full glob.
cd "$(dirname "$0")" || exit 1
files=""
for a in "$@"; do
  case "$a" in -*) ;; *) files=1 ;; esac
done
if [ -n "$files" ]; then
  exec node --test "$@"
fi
exec node --test "$@" 'js/**/*.test.mjs' 'test/tools/**/*.test.mjs'
