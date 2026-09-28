#!/bin/bash
# Self-test for the split rule in bench/modes.awk, on fixed per-process values.
#
# Usage: test-modes.sh
#
# The first six cases are per-process times (ms, one value per process) of one
# kernel, taken from an A/B on a Linux x64 machine (Intel i5-8350U) where the
# same binary ran some kernels at two speeds depending on the process. They are
# here so the rule is held to the data it was written against: the three split
# cases must stay split and the three noisy one-group cases must stay one group.
# Runs in milliseconds and needs only awk, so `make bench-parity` runs it first:
# a detector that has stopped working would otherwise just print "one group".

set -u

here=$(cd "$(dirname "$0")" && pwd)
modes_awk="$here/modes.awk"

# expected<TAB>label<TAB>values
cases='split 1/6	array-walk, one fast process	57.839 58.000 57.916 40.097 57.790 57.190 57.837
split 3/4	array-walk, three fast processes	57.516 39.422 57.473 39.585 57.845 57.700 39.453
split 1/6	string-walk, one fast process	175.795 176.078 176.120 175.988 103.283 175.908 177.105
one	array-walk, one group with a 6% high value	38.778 38.667 38.194 39.370 38.598 41.161 38.576
one	struct-slots, one slow process (interference)	113.612 129.409 113.507 113.195 113.636 113.492 113.980
one	fib, wide spread without a gap	255.327 272.114 282.445 255.577 276.052 256.695 256.074
one	identical values	10.000 10.000 10.000 10.000
split 2/2	two clean groups of two	10.0 10.1 15.0 15.1
one	one slow process of four is not a group	10.0 10.1 10.2 15.0
unchecked	two values are not checked	10.0 20.0'

out=$(echo "$cases" | awk -F'\t' "$(cat "$modes_awk")"'
    {
        n = split($3, v, " ")
        for (i = 1; i <= n; i++) v[i] = v[i] + 0
        split_modes(v, n)
        if (!M_checked)   got = "unchecked"
        else if (M_split) got = "split " M_fast_n "/" M_slow_n
        else              got = "one"
        status = (got == $1) ? "ok  " : "FAIL"
        if (got != $1) bad++
        printf "  %s %-48s expected %-10s got %s\n", status, $2, $1, got
    }
    END { exit bad ? 1 : 0 }')
rc=$?

echo "=== split detection self-test (bench/modes.awk) ==="
echo "$out"
if [ "$rc" -ne 0 ]; then
    echo "test-modes: the split rule no longer classifies the recorded cases as expected." >&2
    exit 1
fi
echo "test-modes: ok"
