#!/bin/sh
# Line coverage for .lisp, using an off-the-shelf .NET coverage collector and no
# coverage-specific code of our own: COMPILE-FILE writes a PE plus a PDB whose
# document table names the .lisp, and the collector reports against that.
#
# Two things have to hold, and this script asserts both:
#   1. the report names lib.lisp at all — it only does when the fasl is loaded
#      file-backed, hence DOTCL_FASL_LOADFROM=1
#   2. a function the driver never calls shows as uncovered — otherwise the
#      numbers are not measuring anything
#
# Needs the dotnet-coverage tool: dotnet tool install -g dotnet-coverage
#
# Usage: check.sh <repo-root>
set -eu
# Absolute: the collected run happens from a temp directory, so every path handed
# to it has to survive the cd.
ROOT="$(cd "${1%/}" && pwd)"
RT="$ROOT/runtime/runtime.csproj"
SIL="$ROOT/compiler/cil-out.sil"

if ! command -v dotnet-coverage > /dev/null 2>&1; then
  echo "SKIP: dotnet-coverage not installed (dotnet tool install -g dotnet-coverage)"
  exit 0
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
cp "$ROOT/test/coverage/lib.lisp" "$ROOT/test/coverage/driver.lisp" "$WORK/"
REPORT="$WORK/coverage.cobertura.xml"

echo "=== collecting coverage ==="
( cd "$WORK" \
  && DOTCL_EMIT_PDB=1 DOTCL_FASL_LOADFROM=1 COVERAGE_SRC=lib.lisp \
     dotnet-coverage collect --output "$REPORT" --output-format cobertura -- \
     dotnet run --project "$RT" -- --asm "$SIL" driver.lisp ) > "$WORK/run.log" 2>&1 \
  || { echo "FAIL: the collected run itself failed"; cat "$WORK/run.log"; exit 1; }

grep -q ";;COVERAGE result=(:NEGATIVE :NON-NEGATIVE 10)" "$WORK/run.log" \
  || { echo "FAIL: driver did not produce the expected result"; cat "$WORK/run.log"; exit 1; }

test -f "$REPORT" || { echo "FAIL: no coverage report was written"; exit 1; }

# 1. The .lisp has to appear as a source file.
grep -q 'filename="[^"]*lib\.lisp"' "$REPORT" \
  || { echo "FAIL: lib.lisp is absent from the report — was the fasl loaded file-backed?"; exit 1; }
echo "PASS: report names lib.lisp"

# 2. The uncalled function has to be uncovered, and a called one covered.
#    Read the <class> block for lib.lisp only.
block=$(awk '/lib\.lisp/{f=1} f{print} f&&/<\/class>/{exit}' "$REPORT")

lines_for() { # $1 = method name prefix; prints "number:hits" per line
  printf '%s\n' "$block" \
    | tr '<' '\n' \
    | awk -v m="$1" '
        /^method /   { inm = ($0 ~ "name=\"" m) }
        inm && /^line / {
          num = $0; sub(/.*number="/, "", num); sub(/".*/, "", num)
          hit = $0; sub(/.*hits="/, "", hit);   sub(/".*/, "", hit)
          print num ":" hit
        }'
}

untouched=$(lines_for "COV_UNTOUCHED")
classify=$(lines_for "COV_CLASSIFY")

test -n "$untouched" || { echo "FAIL: COV-UNTOUCHED has no lines in the report"; exit 1; }
test -n "$classify"  || { echo "FAIL: COV-CLASSIFY has no lines in the report"; exit 1; }

if printf '%s\n' "$untouched" | grep -qv ':0$'; then
  echo "FAIL: COV-UNTOUCHED is never called but reports hits: $untouched"
  exit 1
fi
if ! printf '%s\n' "$classify" | grep -q ':[1-9]'; then
  echo "FAIL: COV-CLASSIFY is called but reports no hits: $classify"
  exit 1
fi

echo "PASS: uncalled function is uncovered, called function is covered"
echo "    COV-UNTOUCHED $untouched"
echo "    COV-CLASSIFY  $classify"
echo "ALL-COVERAGE-CHECKS-PASSED"
