#!/bin/bash
# Show how per-process benchmark times are distributed, and say when they
# fall into two groups.
#
# Usage: process-modes.sh [--no-compare] [label=]file [[label=]file ...]
#
# Each file is one side of a measurement: the stdout of one or more processes
# of the benchmark, concatenated, one "name<TAB>milliseconds" line per kernel
# per process (the format of bench/csharp-parity). The k-th line for a kernel
# is its value in the k-th process. With one file this describes the file;
# with two or more, every later side is also set against the first, which is
# the A/B case. --no-compare leaves the comparison lines out, for sides that
# are not two builds of the same thing (dotcl and C# in make bench-parity).
#
# The split rule is in bench/modes.awk. When a side is split, its median only
# says how many processes drew each group, so the comparison line reports the
# share of fast processes and the fast-group medians instead of a median
# difference, and does not call the change a difference or a non-difference.
# Nothing here decides "better" or "worse": it prints numbers, and says which
# of them can be compared.

set -e

compare=1
if [ "${1:-}" = "--no-compare" ]; then compare=0; shift; fi

if [ $# -lt 1 ]; then
    echo "Usage: $0 [--no-compare] [label=]file [[label=]file ...]" >&2
    exit 2
fi

here=$(cd "$(dirname "$0")" && pwd)
modes_awk="$here/modes.awk"

labels=()
files=()
for arg in "$@"; do
    case "$arg" in
        *=*) labels+=("${arg%%=*}"); files+=("${arg#*=}") ;;
        *)   labels+=("$(basename "$arg" .txt)"); files+=("$arg") ;;
    esac
done
for f in "${files[@]}"; do
    if [ ! -s "$f" ]; then
        echo "$0: $f is missing or empty" >&2
        exit 1
    fi
done

label_list=$(IFS=,; echo "${labels[*]}")

awk -F'\t' -v labels="$label_list" -v compare="$compare" "$(cat "$modes_awk")"'
FNR == 1 { side++ }
NF >= 2 && $2 + 0 == $2 {
    if (!($1 in seen)) { seen[$1] = 1; order[++nk] = $1 }
    key = side SUBSEP $1
    cnt[key]++
    val[key, cnt[key]] = $2 + 0
}
function describe(s, k,    key, i, v, n) {
    key = s SUBSEP k
    n = cnt[key]
    if (n == 0) { printf "  %-12s no result\n", lab[s]; has[s] = 0; return }
    for (i = 1; i <= n; i++) v[i] = val[key, i]
    split_modes(v, n)
    has[s] = 1; sp[s] = M_split; nn[s] = n; mn[s] = M_min; md[s] = M_med
    fn_[s] = M_split ? M_fast_n : n
    fm[s] = M_split ? M_fast_med : M_med
    printf "  %-12s n=%-3d min %9.3f  median %9.3f  ", lab[s], n, M_min, M_med
    if (!M_checked)   printf "not checked (fewer than 3 processes)\n"
    else if (M_split) printf "SPLIT: %d fast at %.3f, %d slow at %.3f (slow/fast %.2f)\n", \
                             M_fast_n, M_fast_med, M_slow_n, M_slow_med, M_slow_med / M_fast_med
    else              printf "one group\n"
}
function pct(a, b) { return (a > 0) ? (b - a) / a * 100 : 0 }
END {
    ns = split(labels, lab, ",")
    for (i = 1; i <= nk; i++) {
        k = order[i]
        print k
        for (s = 1; s <= ns; s++) describe(s, k)
        for (s = 2; compare && s <= ns; s++) {
            if (!has[1] || !has[s]) continue
            if (sp[1] || sp[s]) {
                printf "  %s -> %s: split (%s); compare shares and groups, not medians: fast %d/%d -> %d/%d, fast group %.3f -> %.3f (%+.1f%%)\n", \
                    lab[1], lab[s], \
                    (sp[1] && sp[s]) ? "both" : (sp[1] ? lab[1] : lab[s]), \
                    fn_[1], nn[1], fn_[s], nn[s], fm[1], fm[s], pct(fm[1], fm[s])
            } else {
                printf "  %s -> %s: median %.3f -> %.3f (%+.1f%%), min %.3f -> %.3f (%+.1f%%)\n", \
                    lab[1], lab[s], md[1], md[s], pct(md[1], md[s]), mn[1], mn[s], pct(mn[1], mn[s])
            }
        }
    }
}' "${files[@]}"
