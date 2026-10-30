#!/bin/sh
# Which methods named by generated code (CALL / CALLVIRT in the compiler's
# output) the regression suite actually runs. With DOTCL_CALL_COVERAGE set, the
# assembler counts every such call it emits and puts a counter in front of it;
# each dotcl process writes its counts on exit. report.sh then joins them with
# the names the compiler sources mention.
#
# Usage: run.sh <repo-root>   (results in <repo-root>/out/call-coverage/)
set -eu
ROOT="$(cd "${1%/}" && pwd)"
OUT="$ROOT/out/call-coverage"
RAW="$OUT/raw"
mkdir -p "$RAW"
find "$RAW" -name 'calls-*.tsv' -type f -exec rm -f {} +

echo "=== regression (compiled) with call counting ==="
set +e
DOTCL_CALL_COVERAGE="$RAW" dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
  --asm "$ROOT/compiler/cil-out.sil" "$ROOT/test/regression/run.lisp" \
  < /dev/null > "$OUT/regression.log" 2>&1
status=$?
set -e
tail -n 3 "$OUT/regression.log"
echo "regression exit=$status"

sh "$ROOT/test/call-coverage/report.sh" "$ROOT" "$RAW" "$OUT"
