# modes.awk -- do per-process timings fall into two groups?
#
# Not a program on its own: bench scripts prepend it to their own awk text, as
#
#     awk -F'\t' "$(cat bench/modes.awk)"'  ...program using split_modes...'
#
# so it is POSIX awk only (no asort, no gensub): CI runs mawk.
#
# Why this exists. The same binary can run a kernel at two distinct speeds
# depending on the process it lands in: where the JIT places the method decides
# whether a hot loop straddles an alignment boundary, and that changes from one
# process to the next. One such kernel measured 39-40 ms in some processes and
# 57-58 ms in the others, with the instructions of the loop identical. A median
# of such values tells you the share of processes in each group, not the speed
# of the code, and a spread figure (CV) computed across them is inflated to the
# point where a real difference hides inside it. So the first thing to know
# about a set of per-process values is whether it is one group or two.
#
# The rule, kept simple enough to state in one breath:
#
#   Sort the values. Among the gaps between neighbours that leave at least two
#   values above them, take the widest. Call everything below it the fast
#   group and everything above it the slow group. The values are split when
#   that gap is at least MODES_REL (10%) of the fast group's median AND at
#   least MODES_SPREAD (3) times the wider of the two groups' own ranges.
#
# The slow group needs two members, the fast group only one. Interference from
# the rest of the machine only ever adds time, so a single slow process is the
# ordinary outlier every benchmark has; a single fast process is not something
# noise can produce. Fewer than 3 values are not checked at all.
#
# One large outlier on top of a real split widens the slow group's range and
# can hide the split. The rule errs toward "not split" in that case; it never
# invents one out of spread alone.
#
# split_modes(v, n) sorts v[1..n] in place and sets:
#   M_n         n
#   M_min       smallest value
#   M_med       median of all values
#   M_checked   1 when n >= 3, else 0
#   M_split     1 when the values are split as above, else 0
#   M_fast_n, M_fast_med, M_slow_n, M_slow_med   the two groups, when split

function modes_sort(v, n,    i, j, t) {
    for (i = 2; i <= n; i++) {
        t = v[i]
        for (j = i - 1; j >= 1 && v[j] > t; j--) v[j + 1] = v[j]
        v[j + 1] = t
    }
}

# Median of the sorted slice v[lo..hi].
function modes_median(v, lo, hi,    k, mid) {
    k = hi - lo + 1
    mid = lo + int((k - 1) / 2)
    if (k % 2) return v[mid]
    return (v[mid] + v[mid + 1]) / 2
}

function split_modes(v, n,    i, gap, best, at, lo_range, hi_range, wide, fast_med) {
    MODES_REL = 0.10
    MODES_SPREAD = 3
    modes_sort(v, n)
    M_n = n
    M_min = v[1]
    M_med = modes_median(v, 1, n)
    M_split = 0
    M_fast_n = M_fast_med = M_slow_n = M_slow_med = 0
    M_checked = (n >= 3)
    if (!M_checked) return 0

    best = -1
    for (i = 1; i <= n - 2; i++) {
        gap = v[i + 1] - v[i]
        if (gap > best) { best = gap; at = i }
    }
    lo_range = v[at] - v[1]
    hi_range = v[n] - v[at + 1]
    wide = (lo_range > hi_range) ? lo_range : hi_range
    fast_med = modes_median(v, 1, at)
    if (best >= MODES_REL * fast_med && best >= MODES_SPREAD * wide) {
        M_split = 1
        M_fast_n = at
        M_fast_med = fast_med
        M_slow_n = n - at
        M_slow_med = modes_median(v, at + 1, n)
    }
    return M_split
}
