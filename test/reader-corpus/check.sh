#!/bin/sh
# Reads the sources of the Quicklisp libraries (the dist that bench/library-status
# installs) with dotcl's reader, and compares the result with the previous run.
# It is a net for changes to the reader: run it before and after, and the number
# of files that got worse says whether the change broke the reading of any
# library. Not part of the CI run.
#
# Usage: check.sh <repo-root> [files]
#   files   how many files to read (default 500), or "all". The libraries come in
#           the order of bench/library-status/targets.txt, most depended upon
#           first, so a small number covers the libraries most code uses.
#
# Environment:
#   LIBRARY_STATUS_QL_HOME   the Quicklisp home whose dists/quicklisp/software is read
#                            (default ~/quicklisp, else the home of the dotcl client)
#   READER_BASE              a result file to compare with instead of the previous run
#   READER_FEATURES          *features* for the run, comma separated
#                            (default: dotcl's own)
#
# Output: out/reader-corpus.tsv (file, forms read, error type, first error). The
# result of the run before is kept as out/reader-corpus.prev.tsv, and the two are
# compared: a file is worse when it reads fewer forms or newly stops at an error.
# Exits 1 when any file got worse.
set -eu

ROOT="$(cd "${1%/}" && pwd)"
FILES="${2:-500}"
exe="$ROOT/runtime/bin/Debug/net10.0/runtime.exe"
[ -x "$exe" ] || exe="$ROOT/runtime/bin/Debug/net10.0/runtime"
core="$ROOT/compiler/cil-out.sil"
script="$ROOT/test/reader-corpus/reader-corpus.lisp"

if [ ! -x "$exe" ] || [ ! -f "$core" ]; then
  echo "reader-corpus: no built runtime or compiler/cil-out.sil (make build cross-compile)" >&2
  exit 1
fi

qlhome="${LIBRARY_STATUS_QL_HOME:-}"
if [ -z "$qlhome" ]; then
  if [ -d "$HOME/quicklisp/dists/quicklisp/software" ]; then
    qlhome="$HOME/quicklisp"
  else
    qlhome="${XDG_DATA_HOME:-$HOME/.local/share}/dotcl/quicklisp"
  fi
fi
software="$qlhome/dists/quicklisp/software"
if [ ! -d "$software" ]; then
  echo "reader-corpus: no library sources: $software does not exist."
  echo "  Install the dists first (see bench/library-status/README.md), or set"
  echo "  LIBRARY_STATUS_QL_HOME to a Quicklisp home that has them."
  exit 0
fi

mkdir -p "$ROOT/out"
out="$ROOT/out/reader-corpus.tsv"
prev="$ROOT/out/reader-corpus.prev.tsv"
[ -f "$out" ] && cp "$out" "$prev"

features=""
[ -n "${READER_FEATURES:-}" ] && features="$READER_FEATURES"

echo "=== Reading library sources from $software (files: $FILES) ==="
READER_REPO="$ROOT" READER_FILES="$FILES" READER_OUT="$out" \
  READER_FEATURES="$features" LIBRARY_STATUS_QL_HOME="$qlhome" \
  "$exe" --asm "$core" "$script" < /dev/null

base="${READER_BASE:-$prev}"
if [ ! -f "$base" ]; then
  echo "reader-corpus: no earlier result to compare with; this run is the first."
  echo "  Change the reader, run this again, and the files that got worse are listed."
  exit 0
fi

# A file is worse when it reads fewer forms, or stops at an error it did not
# stop at before. Files only in one of the two runs (the number of files differs)
# are not compared.
worse=$(awk -F'\t' '
  FNR == NR { if ($0 !~ /^#/) { forms[$1] = $2; err[$1] = $3 }; next }
  $0 ~ /^#/ { next }
  ($1 in forms) {
    compared++
    if ($2 < forms[$1] || (err[$1] == "" && $3 != "")) {
      worse++
      printf "  WORSE %s: %d -> %d forms, %s %s\n", $1, forms[$1], $2, $3, $4 > "/dev/stderr"
    } else if ($2 > forms[$1] || (err[$1] != "" && $3 == "")) better++
  }
  END { printf "%d %d %d\n", compared, worse + 0, better + 0 }
' "$base" "$out")
set -- $worse
echo "reader-corpus: compared $1 files with $(basename "$base"): $2 got worse, $3 got better"
[ "$2" -eq 0 ] || exit 1
