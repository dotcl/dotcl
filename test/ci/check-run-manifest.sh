#!/usr/bin/env bash
# check-run-manifest -- shape of the LOAD manifest in test/regression/run.lisp.
#
#   test/ci/check-run-manifest.sh
#
# Flags:
#   1. a test file named by LOAD forms (load, load-skip-under-interp, load-full-tier-only) more than
#      once: it would run twice. A hand edit that goes wrong (a line added after every line, two
#      branches that both add the same file) shows up here at once.
#   2. a LOAD of a test/regression/ file on a line before the one that loads test/framework.lisp:
#      the file reads before DEFTEST exists.
# The count of lines run.lisp gains is not checked: a legitimate change adds as many as it needs.
set -u
cd "$(git rev-parse --show-toplevel)" || exit 2
f=test/regression/run.lisp
bad=0
dups=$(grep -oE '^\((load|load-skip-under-interp|load-full-tier-only) "test/regression/[^"]*"' "$f" \
       | sed -E 's/^[^"]*"//; s/"$//' | sort | uniq -d)
if [ -n "$dups" ]; then
  echo "check-run-manifest: files loaded more than once by $f:"; echo "$dups"; bad=1
fi
fw=$(grep -nE '^\(load "test/framework.lisp"\)' "$f" | head -1 | cut -d: -f1)
if [ -z "$fw" ]; then
  echo "check-run-manifest: no (load \"test/framework.lisp\") line in $f"; bad=1
else
  early=$(grep -nE '^\((load|load-skip-under-interp|load-full-tier-only) "test/regression/' "$f" | awk -F: -v fw="$fw" '$1 < fw')
  if [ -n "$early" ]; then
    echo "check-run-manifest: test files loaded before test/framework.lisp (line $fw):"; echo "$early"; bad=1
  fi
fi
exit $bad
