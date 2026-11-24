#!/bin/sh
# test/ansi-gate.sh fails a category that ran 0 tests, and still passes a normal one, a recorded failure
# set that matches, and (as before) an unfinished category and a run with no outputs at all.
set -u
ROOT="$(cd "${1%/}" && pwd)"
W="$(mktemp -d)"; trap 'rm -rf "$W"' EXIT
fail=0; note() { echo "ansi-gate-empty-category: FAIL: $1"; fail=1; }

printf '{\n  "known-failures": []\n}\n' > "$W/state.json"
good() { printf 'Doing 12 pending tests of 12 tests total.\nNo tests failed.\n' > "$W/ansi-$1.txt"; }
empty() { printf 'Doing 0 pending tests of 0 tests total.\nNo tests failed.\n' > "$W/ansi-$1.txt"; }
gate() { ANSI_OUT_DIR="$W" sh "$ROOT/test/ansi-gate.sh" "$W/state.json" "$@" > "$W/out.txt" 2>&1; }

good a; good b
gate a b || note "two normal categories were refused"

empty c
gate a c && note "a category of 0 tests was accepted"
grep -q "ran 0 tests" "$W/out.txt" || note "the refusal does not say a category ran 0 tests"
gate c && note "a run of only an empty category was accepted"

printf 'Doing 5 pending tests of 5 tests total.\n' > "$W/ansi-d.txt"
gate a d && note "an unfinished category was accepted"

for c in a b c d; do rm -f "$W/ansi-$c.txt"; done
gate a b && note "a run with no outputs was accepted"

[ "$fail" = 0 ] && echo "ansi-gate-empty-category: OK"
exit "$fail"
