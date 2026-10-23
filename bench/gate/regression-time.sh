#!/bin/bash
# Time the compiled regression suite file by file and judge each file's time
# against a recorded table of per-file times.
#
# Usage: regression-time.sh <dotcl-root> <bench-state.json> <baseline.tsv> <platform> <log>
#        regression-time.sh --record <dotcl-root> <bench-state.json> <baseline.tsv> <platform> <log>
#
# The suite is run as `make test-regression' runs it (dotnet run of the runtime
# project, --asm compiler/cil-out.sil), but through
# bench/gate/regression-file-times.lisp, which loads test/regression/run.lisp
# form by form and prints a FILE-TIME line per form: milliseconds, the test
# file it loads, and how many tests it ran. Output goes to <log>; the per-file
# seconds and test counts, largest first, go to <log>.files. The JIT profile
# stays on (DOTCL_NO_JIT_PROFILE unset): one test checks that the profile is in
# use, and the suite must pass for its time to count.
#
# Why per file and not one wall time: tests are added every day, so a single
# number goes stale; the machine's speed drifts from day to day by more than a
# whole-suite margin can absorb; and one existing test becoming a hundred times
# slower is a few percent of the whole and disappears in it.
#
# <baseline.tsv> holds, per platform, the seconds and test count of each file:
#
#     <platform> TAB <file> TAB <seconds> TAB <tests>
#
# The judgement, with the limits from the variables below:
#   (a) FAIL when a file in the baseline now takes more than its baseline plus
#       FILE_SLACK seconds and more than FILE_RATIO times its baseline.
#   (b) FAIL when the whole suite, per test, is over TOTAL_RATIO times what the
#       baseline predicts per test. The prediction is the baseline seconds of
#       the files still present plus the measured seconds of new files, over
#       their tests; so new files do not count against the suite, and tests
#       added to an existing file are paid for at that file's own rate.
#   (c) A warning (not a failure) for a new file that takes over NEW_FILE_WARN
#       seconds on its own.
#
# --record runs the suite twice and writes, for <platform>, the smaller of the
# two times of each file into <baseline.tsv> (rows of other platforms are kept).
# It does not judge. Run it on the judging machine after a landing.
#
# Writes one row into <bench-state.json>, carrying every other row through
# verbatim:
#
#     "regression/compiled": {"seconds": 652, "tests": 7867, "passed": true}
#
# Exit status and the last line of output, as in check-ratios.sh:
#   0  "regression-time: ok -- ..."
#   1  "regression-time: FAIL -- ..."   a limit is exceeded, or the suite failed
#   2  usage error or a missing input file
#   3  "regression-time: UNJUDGED -- ..."  no baseline rows for this platform
#        (exit 0 with CHECK_RATIOS_ALLOW_UNJUDGED=1)

set -u

FILE_SLACK=2
FILE_RATIO=1.5
TOTAL_RATIO=1.15
NEW_FILE_WARN=10

record=0
if [ "${1:-}" = "--record" ]; then record=1; shift; fi

root="${1:-}"
state_file="${2:-}"
baseline_file="${3:-}"
platform="${4:-}"
log="${5:-}"

if [ -z "$root" ] || [ -z "$state_file" ] || [ -z "$baseline_file" ] || [ -z "$platform" ] || [ -z "$log" ]; then
    echo "Usage: $0 [--record] <dotcl-root> <bench-state.json> <baseline.tsv> <platform> <log>" >&2
    exit 2
fi
if [ "$record" = 0 ] && [ ! -f "$baseline_file" ]; then
    echo "regression-time: $baseline_file not found" >&2
    exit 2
fi

setsid_cmd=""
command -v setsid > /dev/null 2>&1 && setsid_cmd="setsid"

# run_suite <log>: sets seconds, status, tests, passed; writes <log>.files as
# "<seconds> <file> <tests>" lines, largest first.
run_suite() {
    echo "=== Timing the compiled regression suite (output: $1) ==="
    local start end
    start=$(date +%s)
    (cd "$root" && $setsid_cmd dotnet run --project runtime/runtime.csproj -- \
        --asm compiler/cil-out.sil bench/gate/regression-file-times.lisp) < /dev/null > "$1" 2>&1
    status=$?
    end=$(date +%s)
    seconds=$((end - start))
    awk '/^FILE-TIME / { s[$3] += $2; n[$3] += $4 } END { for (k in s) printf "%.1f %s %d\n", s[k] / 1000, k, n[k] }' "$1" \
        | sort -g -r > "$1.files"
    tests=$(sed -n 's/^(\([0-9]*\) PASSED [0-9]* FAILED OF [0-9]* TOTAL).*/\1/p' "$1" | tail -1)
    passed=false
    if [ "$status" -eq 0 ] && grep -q '^ALL-TESTS-PASSED' "$1"; then
        passed=true
    fi
}

record_state() {
    local row name tmp
    row="{\"seconds\": $seconds, \"tests\": ${tests:-null}, \"passed\": $passed}"
    name="regression/compiled"
    tmp="$state_file.new"
    {
        echo "{"
        echo "  \"updated\": \"$(date +%Y-%m-%d)\","
        echo "  \"benchmarks\": {"
        if [ -f "$state_file" ]; then
            # Every entry line except an old row for this name, verbatim. The
            # closing brace is required so the "benchmarks" header is not read
            # as an entry.
            sed -n 's/^ *\("[^"]*": {.*}\),\{0,1\}$/\1/p' "$state_file" \
                | grep -v "^\"$name\":" \
                | sed 's/^/    /; s/$/,/'
        fi
        echo "    \"$name\": $row"
        echo "  }"
        echo "}"
    } > "$tmp" && mv "$tmp" "$state_file"
}

# --- record a new baseline ---------------------------------------------

if [ "$record" = 1 ]; then
    for i in 1 2; do
        run_suite "$log.$i"
        if [ "$passed" != true ]; then
            echo "regression-time: FAIL -- run $i of the suite failed (exit $status), baseline not written; see $log.$i"
            exit 1
        fi
        echo "  run $i: $seconds s, $tests tests"
    done
    commit=$(git -C "$root" rev-parse --short HEAD 2>/dev/null || echo unknown)
    tmp="$baseline_file.new"
    {
        if [ -f "$baseline_file" ]; then
            awk -F '\t' -v p="$platform" '$1 != p' "$baseline_file" \
                | grep -v "^# recorded $platform "
        else
            printf '# Per-file seconds of the compiled regression suite, the smaller of two\n'
            printf '# runs; written by make regression-time-baseline, read by make regression-time\n'
            printf '# (bench/gate/regression-time.sh).\n'
            printf '# platform\tfile\tseconds\ttests\n'
        fi
        echo "# recorded $platform $(date +%Y-%m-%d) at $commit"
        awk -v p="$platform" '
            FNR == 1 { run++ }
            { if (!($2 in t) || $1 < t[$2]) t[$2] = $1; if (run == 1) n[$2] = $3 }
            END { for (k in t) printf "%s\t%s\t%.1f\t%d\n", p, k, t[k], n[k] }' \
            "$log.1.files" "$log.2.files" | sort -t "$(printf '\t')" -k 2,2
    } > "$tmp" && mv "$tmp" "$baseline_file"
    total=$(awk -F '\t' -v p="$platform" '$1 == p { s += $3 } END { printf "%.0f", s }' "$baseline_file")
    echo "regression-time: recorded -- $platform, $total s over the files, $tests tests, at $commit in $baseline_file"
    exit 0
fi

# --- run and judge -----------------------------------------------------

run_suite "$log"
echo "Per-file seconds: $log.files"
record_state

echo "=== regression time (platform: $platform) ==="

if [ "$passed" != true ]; then
    echo "  regression/compiled: ${seconds} s, but the suite did not pass (exit $status); see $log"
    echo "regression-time: FAIL -- the suite failed, so its time is not judged"
    exit 1
fi

if ! awk -F '\t' -v p="$platform" '$1 == p { found = 1 } END { exit !found }' "$baseline_file"; then
    echo "  regression/compiled: ${seconds} s, $tests tests   (no baseline)"
    echo ""
    echo "To start gating it, record a baseline on this machine: make regression-time-baseline"
    echo ""
    verdict="regression-time: UNJUDGED -- no $platform rows in $baseline_file; measured $seconds s"
    if [ "${CHECK_RATIOS_ALLOW_UNJUDGED:-}" = "1" ]; then
        echo "$verdict (exit 0 because CHECK_RATIOS_ALLOW_UNJUDGED=1)"
        exit 0
    fi
    echo "$verdict (exit 3; set CHECK_RATIOS_ALLOW_UNJUDGED=1 to accept)"
    exit 3
fi

awk -v p="$platform" -v slack="$FILE_SLACK" -v fratio="$FILE_RATIO" \
    -v tratio="$TOTAL_RATIO" -v warn="$NEW_FILE_WARN" -v wall="$seconds" '
    FNR == 1 { part++ }
    part == 1 {
        split($0, f, "\t")
        if (f[1] == p) { bs[f[2]] = f[3]; bt[f[2]] = f[4] }
        next
    }
    { cs[$2] = $1; ct[$2] = $3; order[++m] = $2 }
    END {
        for (i = 1; i <= m; i++) {
            k = order[i]; cur += cs[k]; curt += ct[k]
            if (k in bs) {
                pred += bs[k]; predt += bt[k]
                if (cs[k] > bs[k] + slack && cs[k] > bs[k] * fratio) {
                    slow = slow sprintf("%s%s %.1f -> %.1f s", (slow == "" ? "" : ", "), k, bs[k], cs[k])
                    printf "  SLOW  %s: %.1f s, baseline %.1f s (limit: +%s s and %sx)\n", k, cs[k], bs[k], slack, fratio
                }
            } else {
                pred += cs[k]; predt += ct[k]; nnew++
                if (cs[k] > warn)
                    printf "  WARNING  new file %s takes %.1f s (over %s s)\n", k, cs[k], warn
            }
        }
        ratio = (predt > 0 && curt > 0 && pred > 0) ? (cur / curt) / (pred / predt) : 1
        printf "  regression/compiled: %d s wall, %.1f s over %d files (%d new), %d tests\n", wall, cur, m, nnew, curt
        printf "  per test: %.4f s, baseline predicts %.4f s; ratio %.3f (limit %s)\n", cur / curt, pred / predt, ratio, tratio
        if (ratio > tratio)
            total = sprintf("the suite per test is %.3fx the baseline, over the limit %s", ratio, tratio)
        if (slow != "" && total != "")
            msg = "files over the limit: " slow "; " total
        else if (slow != "")
            msg = "files over the limit (+" slack " s and " fratio "x): " slow
        else
            msg = total
        if (msg != "") {
            printf "regression-time: FAIL -- %s\n", msg
            exit 1
        }
        printf "regression-time: ok -- %d s, per test %.3fx the baseline (limit %s), no file over +%s s and %sx\n", wall, ratio, tratio, slack, fratio
    }' "$baseline_file" "$log.files"
