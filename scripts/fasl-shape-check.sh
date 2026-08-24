#!/usr/bin/env bash
# fasl-shape-check.sh — assert dotcl's emitted fasls keep a loadable SHAPE.
#
# A fasl is a .NET assembly, and what decides whether loading it is cheap or
# fatal is not its size but its shape: how many IL bytes sit in ONE method, how
# many fields sit on ONE type, how much string data sits in the module-wide #US
# heap. Those bounds have been crossed four times in this repo. Each time the
# compile side looked perfectly healthy — total IL, fasl bytes and compile time
# barely moved — and the failure landed at LOAD, as one of:
#
#   "Internal limitation: too many fields."   (one type past 65,535 fields)
#   InvalidProgramException                   (one method too large to JIT)
#   the process killed while JITting          (RSS grows superlinearly in
#                                              per-method IL: a 17 MB method
#                                              cost over 3.6 GB)
#
# None of it names the file or the cause, and every instance so far was found by
# a user's out-of-memory rather than by CI. This gate exists so the next one is
# found here.
#
# What is checked, and why these three targets:
#   shape   a fixture whose every top level form must become its own method
#           (test/fasl-shape/shape.lisp). It carries 400 definitions inside each
#           of PROGN / EVAL-WHEN / LOCALLY / MACROLET / SYMBOL-MACROLET, so if
#           the flattener stops descending any one of them, that body collapses
#           into a single method of roughly 46 KB — well past this target's
#           tight threshold, while everything else about the fasl stays put.
#   core    the compiler and standard library compiled by themselves: the
#           largest thing dotcl emits from its own sources.
#   asdf    15k lines of ordinary Lisp nobody wrote for this check. Used when a
#           fresh contrib/asdf/asdf.fasl is available.
#
# Thresholds are per target because they mean different things: tight on the
# fixture (it is built so that correct output stays small), loose on real
# libraries (they are allowed to grow, just not to collapse into one method).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT="$ROOT/build/fasl-shape"
mkdir -p "$OUT"

TOOL="dotnet run $ROOT/bench/fasl-il.cs --"

echo "=== Generating the shape fixture and a fresh core ==="
dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
  --asm "$ROOT/compiler/cil-out.sil" \
  --eval "(progn
            (compile-file \"test/fasl-shape/shape.lisp\" :output-file \"build/fasl-shape/shape.dll\" :module-name \"shapefx\")
            (load \"build/fasl-shape/shape.dll\")
            (unless (funcall (intern \"SHAPE-FIXTURE-OK-P\" :dotcl-fasl-shape))
              (format *error-output* \"~&shape fixture did not load correctly~%\")
              (dotcl:quit 1))
            (dotcl:sil-to-fasl \"compiler/cil-out.sil\" \"build/fasl-shape/core.dll\" \"shapecore\")
            (dotcl:quit 0))"

rc=0

# Fixture: 2,000 top level forms, each its own method. The largest method is
# ModuleInit, which legitimately grows with the number of forms (~12 KB here);
# one collapsed wrapper would be ~46 KB. 24 KB sits between the two.
$TOOL "$OUT/shape.dll" limits il=24576 fields=4096 methods=32768 us=1048576 || rc=1

# Real fasls: the point here is a collapse or an unbounded per-symbol growth
# path, not ordinary size. Defaults, which sit far below the hard limits and far
# above what dotcl emits today.
$TOOL "$OUT/core.dll" limits || rc=1

ASDF_FASL="$ROOT/contrib/asdf/asdf.fasl"
if [ ! -f "$ASDF_FASL" ]; then
  echo "note: contrib/asdf/asdf.fasl absent — skipping the real-library check"
  echo "      (make setup-asdf && make compile-asdf-fasl)"
elif [ "$ASDF_FASL" -ot "$ROOT/compiler/cil-out.sil" ]; then
  # A fasl carries the code generation of the compiler that built it, so an old
  # one reports on old codegen. Skip rather than judge. (CI builds it fresh.)
  echo "note: contrib/asdf/asdf.fasl is older than compiler/cil-out.sil —"
  echo "      skipping the real-library check (it would measure stale codegen)."
  echo "      Refresh it with: rm contrib/asdf/asdf.fasl && make compile-asdf-fasl"
else
  cp "$ASDF_FASL" "$OUT/asdf.dll"
  $TOOL "$OUT/asdf.dll" limits || rc=1
fi

if [ "$rc" -ne 0 ]; then
  echo "FASL SHAPE CHECK FAILED — the offending method or type is named above." >&2
  echo "Do not raise the threshold to make this pass: the bound stands for a" >&2
  echo "load-time failure that no compile-side measurement can see." >&2
fi
exit $rc
