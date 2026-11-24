#!/bin/sh
# ci/gate/run-target.sh: a failing target prints the end of its log into the step output and
# still records its summary line; a passing one prints no log tail.
# usage: sh test/ci/run-target.sh [ROOT]
set -eu
root=${1:-.}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$root"
fail=0
out=$(GATE_OUT=$tmp sh ci/gate/run-target.sh no-such-gate-target < /dev/null 2>&1) && rc=0 || rc=$?
[ "$rc" != 0 ] || { echo "run-target: a failing target exited 0"; fail=1; }
echo "$out" | grep -q '^mac no-such-gate-target exit=[1-9]' || { echo "run-target: no summary line"; fail=1; }
echo "$out" | grep -q '^::group::mac no-such-gate-target: last 80 lines' || { echo "run-target: no log tail for a failed target"; fail=1; }
echo "$out" | grep -q 'No rule to make target' || { echo "run-target: the target's own error is not in the step output"; fail=1; }
grep -q '^mac no-such-gate-target exit=[1-9]' "$tmp/summary.txt" || { echo "run-target: summary.txt not written"; fail=1; }
[ -f "$tmp/mac-no-such-gate-target.log" ] || { echo "run-target: log file not kept"; fail=1; }
[ "$fail" = 0 ] && echo "run-target: OK"
exit $fail
