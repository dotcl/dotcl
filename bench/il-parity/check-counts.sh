#!/usr/bin/env bash
# Gate the IL parity instruction counts against the recorded baseline.
#
# Usage: check-counts.sh <baseline.tsv> <current.tsv>
#
# Only INCREASES fail. The numbers are what dotcl emits for a method body, so a
# category going up means the compiler started paying for something it did not
# pay for before -- a box, a cast, a call where there was none. Going down is
# the point of the exercise and needs no permission; refresh the baseline with
# `make il-parity-accept` when it does.
#
# Unlike a time ratio this is deterministic: the same compiler on the same
# source gives the same counts on any machine, so a difference is a real
# difference and the threshold is zero.
#
# One class of rise is NOT the compiler paying for something new: moving work
# out of a runtime helper makes previously-invisible instructions visible. The
# counter reads a method body and does not descend into the helpers it calls,
# so lowering a call that took a boxed argument into one that takes a raw
# argument relocates the unbox from inside the helper to the call site, where
# it shows up as a castclass and a call that were not counted before. The
# category rises while the same work -- or less of it -- is done. Before
# rejecting a rise, read what the old helper did on that path: if every gained
# instruction has a counterpart that used to run inside it, the rise is
# bookkeeping and the baseline can be refreshed.

set -u

baseline="$1"
current="$2"

if [ ! -f "$baseline" ]; then
    echo "check-counts: no baseline at $baseline -- record one with:"
    echo "    cp $current $baseline"
    exit 1
fi

categories="box unbox castclass isinst newobj call callvirt field element total"

fail=0
new=0

# Read the baseline into a lookup keyed by "case<TAB>method".
while IFS=$'\t' read -r case method rest; do
    [ -n "${case:-}" ] || continue
    eval "base_$(echo "${case}_${method}" | tr -c 'A-Za-z0-9' '_')=\$rest"
done < "$baseline"

while IFS=$'\t' read -r case method rest; do
    [ -n "${case:-}" ] || continue
    key="base_$(echo "${case}_${method}" | tr -c 'A-Za-z0-9' '_')"
    old="${!key:-}"
    if [ -z "$old" ]; then
        echo "  $case/$method  NEW (not in baseline)"
        new=$((new + 1))
        continue
    fi
    i=1
    for cat in $categories; do
        a=$(echo "$old" | cut -f$i)
        b=$(echo "$rest" | cut -f$i)
        if [ "${b:-0}" -gt "${a:-0}" ]; then
            echo "  $case/$method  $cat  $a -> $b   WORSE"
            fail=$((fail + 1))
        elif [ "${b:-0}" -lt "${a:-0}" ]; then
            echo "  $case/$method  $cat  $a -> $b   better"
        fi
        i=$((i + 1))
    done
done < "$current"

if [ "$new" -gt 0 ]; then
    echo "check-counts: $new pair(s) not in the baseline -- accept them with make il-parity-accept"
fi
if [ "$fail" -gt 0 ]; then
    echo "check-counts: $fail count(s) went up. Something started emitting more than it did."
    exit 1
fi
echo "check-counts: ok"
exit 0
