#!/bin/sh
# The bench gate scripts (bench/gate/*.lisp, run with dotcl) give, on the sample inputs under
# fixtures/, the output the awk scripts they replaced gave: the expected files were produced
# by those scripts. Needs a built runtime and compiler/dotcl.core (make build compile-core-fasl).
set -u
ROOT="$(cd "${1%/}" && pwd)"
RT="${DOTCL_RUNTIME:-$ROOT/runtime/bin/Debug/net10.0/runtime.dll}"
F="$ROOT/test/bench-gate/fixtures"
T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
fail=0
run() { dotnet "$RT" --core "$ROOT/compiler/dotcl.core" "$@" < /dev/null 2>&1; }
check() { # name expected-file actual-file
    if diff "$2" "$3" > "$T/diff"; then echo "  ok    $1"; else echo "  FAIL  $1"; head -10 "$T/diff"; fail=1; fi
}
echo "=== bench gate scripts against recorded output ==="

run "$ROOT/bench/gate/process-modes.lisp" dotcl="$F/dotcl.tsv" csharp="$F/csharp.tsv" dotcl2="$F/dotcl2.tsv" > "$T/a"
check "process-modes, with comparison" "$F/process-modes-compare.out" "$T/a"
run "$ROOT/bench/gate/process-modes.lisp" --no-compare dotcl="$F/dotcl.tsv" csharp="$F/csharp.tsv" > "$T/b"
check "process-modes --no-compare" "$F/process-modes-nocompare.out" "$T/b"
run "$ROOT/bench/gate/test-modes.lisp" > "$T/c"
check "test-modes" "$F/test-modes.out" "$T/c"

BENCH_RUNTIME="" DOTCL_RUNTIME_UNUSED=1 bash "$ROOT/bench/csharp-parity/make-parity-state.sh" "$F/csharp-full.tsv" "$F/dotcl.tsv" 2>&1 | grep -v '"updated"' > "$T/d"
check "make-parity-state" "$F/parity-state.out" "$T/d"
{ bash "$ROOT/bench/csharp-parity/make-parity-state.sh" "$F/csharp.tsv" "$F/dotcl.tsv" 2>&1; echo "rc=$?"; } \
    | sed "s|$ROOT/bench/csharp-parity/||" > "$T/e"
sed "s|bench/csharp-parity/||" "$F/parity-state-missing.out" > "$T/e-expected"
check "make-parity-state, a kernel missing from one side" "$T/e-expected" "$T/e"

run "$ROOT/bench/gate/process-modes.lisp" "$T/no-such-file.tsv" > "$T/f"; rc=$?
[ "$rc" -ne 0 ] && grep -q "is missing or empty" "$T/f" && echo "  ok    process-modes, a missing file" || { echo "  FAIL  process-modes, a missing file"; fail=1; }

[ "$fail" -eq 0 ] && echo "bench-gate: ok"
exit "$fail"
