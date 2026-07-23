#!/bin/sh
# Debug-path check: compile the corpus with DOTCL_EMIT_PDB both unset and set,
# and assert (1) the results are identical (the debug codegen path — extra
# markers + disabled slot-merge — must not miscompile) and (2) the emitted PDB
# reads back cleanly with a source hash and well-formed sequence points/scopes.
#
# Usage: check.sh <repo-root>
set -eu
ROOT="${1%/}"
RT="$ROOT/runtime/runtime.csproj"
SIL="$ROOT/compiler/cil-out.sil"
DRIVER="$ROOT/test/debug-pdb/driver.lisp"
CORPUS="$ROOT/test/debug-pdb/corpus.lisp"
CHECK="$ROOT/test/debug-pdb/pdbcheck"

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
cp "$CORPUS" "$WORK/corpus.lisp"

run() { # $1 = output file; env (DOTCL_EMIT_PDB) inherited from caller
  CORPUS_SRC="$WORK/corpus.lisp" dotnet run --project "$RT" -- --asm "$SIL" "$DRIVER" 2>/dev/null \
    | sed -n '/===CORPUS-BEGIN===/,/===CORPUS-END===/p' > "$1"
}

echo "=== compiling corpus (debug OFF) ==="
run "$WORK/out-off.txt"
rm -f "$WORK/corpus.pdb"

echo "=== compiling corpus (debug ON: DOTCL_EMIT_PDB=1) ==="
DOTCL_EMIT_PDB=1 run "$WORK/out-on.txt"

# 1. Results must match between the two codegen paths.
if ! diff -u "$WORK/out-off.txt" "$WORK/out-on.txt"; then
  echo "FAIL: debug-on output differs from debug-off — the debug codegen path miscompiled"
  exit 1
fi
if ! grep -q "===CORPUS-END===" "$WORK/out-on.txt"; then
  echo "FAIL: corpus did not run to completion"
  exit 1
fi
echo "PASS: debug-on and debug-off produce identical results:"
sed '/===CORPUS/d' "$WORK/out-on.txt" | sed 's/^/    /'

# 2. The PDB must exist and validate.
test -f "$WORK/corpus.pdb" || { echo "FAIL: no corpus.pdb was produced under DOTCL_EMIT_PDB"; exit 1; }
echo "=== validating corpus.pdb ==="
dotnet run --project "$CHECK" -c Release -- "$WORK/corpus.pdb"

echo "ALL-DEBUG-PDB-CHECKS-PASSED"
