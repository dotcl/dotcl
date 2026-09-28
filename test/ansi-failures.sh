#!/bin/sh
# The names of the ansi-test tests that failed, one per line, from the
# per-category outputs `make test-ansi-all` leaves in out/ansi/ansi-<category>.txt.
#
# Two callers need exactly this list and must agree on it: `make
# update-ansi-state`, which records it as the known-failure set, and
# test/ansi-gate.sh, which compares a run against that set. Reading the outputs in
# one place is what keeps them from drifting apart.
#
# Usage: ansi-failures.sh <category>...
#        ANSI_OUT_DIR=<dir> ansi-failures.sh <category>...   (default out/ansi)
#
# A category with no output file is skipped silently: "the suite did not run
# there" is not "nothing failed there", and it is the caller that knows which of
# those it is looking at (update-ansi-state records it as untested; the gate
# refuses to pass a run with no outputs at all).
set -eu

dir="${ANSI_OUT_DIR:-$(dirname "$0")/../out/ansi}"

for cat in "$@"; do
  f="$dir/ansi-$cat.txt"
  [ -f "$f" ] || continue
  # The runner prints "N out of M total tests failed:" and then the names, one
  # per line, wrapped in a single pair of parens. -a: the outputs can carry
  # stray bytes from the runtime, and grep would otherwise call them binary.
  grep -a '' "$f" | awk '
    /out of [0-9]+ total tests failed/ { collecting = 1; next }
    collecting {
      raw = $0
      # A GC stats line (or anything else) means the list is over and the
      # closing paren never came: stop rather than swallow the rest of the file.
      if (raw ~ /^;/) { collecting = 0; next }
      line = raw
      gsub(/[()]/, "", line)
      gsub(/^[ \t]+/, "", line)
      gsub(/[ \t\r]+$/, "", line)
      if (line != "") print line
      if (raw ~ /\)/) collecting = 0
    }'
done | sort -u
