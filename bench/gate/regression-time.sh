#!/bin/bash
# Time the compiled regression suite and judge the time against a recorded
# baseline, the way bench/gate/check-ratios.sh judges the benchmark ratios.
#
# Usage: regression-time.sh <dotcl-root> <bench-state.json> <ratio-baseline.json> <platform> <log>
#
# The suite is run exactly as `make test-regression' runs it (dotnet run of the
# runtime project, --asm compiler/cil-out.sil test/regression/run.lisp). Its
# output goes to <log>. The JIT profile stays on (DOTCL_NO_JIT_PROFILE unset):
# one test checks that the profile is in use, and the suite must pass for its
# time to count.
#
# Why a wall time and not a ratio: the suite compiles thousands of small forms
# and runs them, so its time is mostly compile time, and nothing else in the
# bench record watches compile time. It is not a ratio against anything, so it
# is only comparable on one machine: the baseline is keyed by platform, like the
# ratios, and a platform with no baseline is reported as UNJUDGED.
#
# Writes one row into <bench-state.json>, carrying every other row through
# verbatim:
#
#     "regression/compiled": {"seconds": 652, "tests": 7867, "passed": true}
#
# The row has no "ratio" field, so check-ratios.sh does not read it.
#
# Baseline: "<platform>/regression/compiled-seconds" in ratio-baseline.json.
# The limit is baseline * "regression_threshold" (or "threshold" when that is
# absent).
#
# Exit status and the last line of output, as in check-ratios.sh:
#   0  "regression-time: ok -- ..."
#   1  "regression-time: FAIL -- ..."   over the limit, or the suite failed
#   2  usage error or a missing input file
#   3  "regression-time: UNJUDGED -- ..."  no baseline for this platform
#        (exit 0 with CHECK_RATIOS_ALLOW_UNJUDGED=1)

set -u

root="${1:-}"
state_file="${2:-}"
baseline_file="${3:-}"
platform="${4:-}"
log="${5:-}"

if [ -z "$root" ] || [ -z "$state_file" ] || [ -z "$baseline_file" ] || [ -z "$platform" ] || [ -z "$log" ]; then
    echo "Usage: $0 <dotcl-root> <bench-state.json> <ratio-baseline.json> <platform> <log>" >&2
    exit 2
fi
if [ ! -f "$baseline_file" ]; then
    echo "regression-time: $baseline_file not found" >&2
    exit 2
fi

# --- run ---------------------------------------------------------------

setsid_cmd=""
command -v setsid > /dev/null 2>&1 && setsid_cmd="setsid"

echo "=== Timing the compiled regression suite (output: $log) ==="
start=$(date +%s)
$setsid_cmd dotnet run --project "$root/runtime/runtime.csproj" -- \
    --asm "$root/compiler/cil-out.sil" "$root/test/regression/run.lisp" < /dev/null > "$log" 2>&1
status=$?
end=$(date +%s)
seconds=$((end - start))

tests=$(sed -n 's/^(\([0-9]*\) PASSED [0-9]* FAILED OF [0-9]* TOTAL).*/\1/p' "$log" | tail -1)
passed=false
if [ "$status" -eq 0 ] && grep -q '^ALL-TESTS-PASSED' "$log"; then
    passed=true
fi

# --- record ------------------------------------------------------------

row="{\"seconds\": $seconds, \"tests\": ${tests:-null}, \"passed\": $passed}"
name="regression/compiled"
tmp="$state_file.new"
{
    echo "{"
    echo "  \"updated\": \"$(date +%Y-%m-%d)\","
    echo "  \"benchmarks\": {"
    if [ -f "$state_file" ]; then
        # Every entry line except an old row for this name, verbatim. The
        # closing brace is required so the "benchmarks" header is not read as
        # an entry.
        sed -n 's/^ *\("[^"]*": {.*}\),\{0,1\}$/\1/p' "$state_file" \
            | grep -v "^\"$name\":" \
            | sed 's/^/    /; s/$/,/'
    fi
    echo "    \"$name\": $row"
    echo "  }"
    echo "}"
} > "$tmp" && mv "$tmp" "$state_file"

# --- judge -------------------------------------------------------------

echo "=== regression time (platform: $platform) ==="

if [ "$passed" != true ]; then
    echo "  $name: ${seconds} s, but the suite did not pass (exit $status); see $log"
    echo "regression-time: FAIL -- the suite failed, so its time is not judged"
    exit 1
fi

threshold=$(sed -n 's/^ *"regression_threshold": *\([0-9.]*\).*/\1/p' "$baseline_file" | head -1)
[ -z "$threshold" ] && threshold=$(sed -n 's/^ *"threshold": *\([0-9.]*\).*/\1/p' "$baseline_file" | head -1)
base=$(sed -n "s|^ *\"$platform/regression/compiled-seconds\": *\([0-9.]*\),\{0,1\}\$|\1|p" "$baseline_file" | head -1)

if [ -z "$base" ]; then
    echo "  $name: ${seconds} s, $tests tests   (no baseline)"
    echo ""
    echo "To start gating it, add this to the \"ratios\" object in bench/gate/ratio-baseline.json"
    echo "and describe the machine under \"recorded\":"
    echo ""
    echo "    \"$platform/regression/compiled-seconds\": $seconds,"
    echo ""
    verdict="regression-time: UNJUDGED -- no \"$platform/regression/compiled-seconds\" baseline; measured $seconds s"
    if [ "${CHECK_RATIOS_ALLOW_UNJUDGED:-}" = "1" ]; then
        echo "$verdict (exit 0 because CHECK_RATIOS_ALLOW_UNJUDGED=1)"
        exit 0
    fi
    echo "$verdict (exit 3; set CHECK_RATIOS_ALLOW_UNJUDGED=1 to accept)"
    exit 3
fi

limit=$(awk "BEGIN { printf \"%.0f\", $base * $threshold }")
if [ "$seconds" -gt "$limit" ]; then
    echo "  $name: ${seconds} s, $tests tests   FAIL  baseline $base s, limit $limit s"
    echo "regression-time: FAIL -- $seconds s is over ${threshold}x the baseline $base s"
    exit 1
fi
echo "  $name: ${seconds} s, $tests tests   ok    baseline $base s, limit $limit s"
echo "regression-time: ok -- $seconds s (baseline $base s, limit $limit s)"
