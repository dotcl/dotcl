#!/bin/sh
# Compare an ansi-test run against the failures ansi-state.json records, and
# fail the build on any difference.
#
# Why a set and not a count: ansi-test is an upstream clone, so the number of
# tests moves on its own. "One more failure than last time" is therefore not a
# signal, and "the same number of failures" is not a reassurance -- a real
# regression can arrive in the same run that an unrelated test is added or
# removed. The names are the invariant.
#
# Both directions are an error:
#
#   unexpected  a test failed that ansi-state.json does not know about. This is
#               the regression this gate exists to catch.
#   fixed       a recorded known failure passed. Good news, but the record is
#               now wrong, and leaving it wrong is what lets the next unexpected
#               failure hide behind a stale file. `make update-ansi-state`
#               rewrites it from the run.
#
# A run that produced no category output at all is also an error: a suite that
# did not run must not read as a suite with nothing to report (the same reason
# test/host-api/check.sh treats a skip under DOTCL_CI=1 as a failure).
#
# Usage: ansi-gate.sh <ansi-state.json> <category>...
#        ANSI_OUT_DIR=<dir> ansi-gate.sh ...   (where the run left its outputs;
#                                              default out/ansi, as the Makefile)
set -eu

state="$1"
shift

dir="${ANSI_OUT_DIR:-$(dirname "$0")/../out/ansi}"
here=$(dirname "$0")

[ -f "$state" ] || { echo "ansi-gate: no state file at $state" >&2; exit 1; }

produced=0
for cat in "$@"; do
  [ -f "$dir/ansi-$cat.txt" ] && produced=1
done
if [ "$produced" -eq 0 ]; then
  echo "ansi-gate: no category outputs in $dir -- the suite did not run" >&2
  exit 1
fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

ANSI_OUT_DIR="$dir" sh "$here/ansi-failures.sh" "$@" > "$tmp/actual"

# The recorded set: the one-line "known-failures": ["A", "B"] the generator
# writes. Names only -- the reasons live in known-failure-notes beside it.
sed -n 's/.*"known-failures"[[:space:]]*:[[:space:]]*\[\(.*\)\].*/\1/p' "$state" \
  | tr ',' '\n' \
  | sed 's/[][" ]//g' \
  | grep -v '^$' \
  | sort -u > "$tmp/known" || true

unexpected=$(comm -23 "$tmp/actual" "$tmp/known")
fixed=$(comm -13 "$tmp/actual" "$tmp/known")

printf 'ansi-gate: %s failing, %s recorded\n' \
  "$(grep -c '' < "$tmp/actual")" "$(grep -c '' < "$tmp/known")"

status=0

if [ -n "$unexpected" ]; then
  echo "" >&2
  echo "ansi-gate: FAILURES NOT IN ansi-state.json:" >&2
  printf '  %s\n' $unexpected >&2
  echo "  These are regressions unless they are deliberate. If deliberate, record" >&2
  echo "  them with the reason (known-failure-notes) and say so in docs/deviations.md." >&2
  status=1
fi

if [ -n "$fixed" ]; then
  echo "" >&2
  echo "ansi-gate: RECORDED FAILURES THAT NOW PASS:" >&2
  printf '  %s\n' $fixed >&2
  echo "  Refresh the record with: make update-ansi-state" >&2
  status=1
fi

if [ "$status" -eq 0 ]; then
  echo "ansi-gate: ok -- the failing set is exactly what ansi-state.json records"
fi

exit "$status"
