#!/bin/sh
# Joins the per-process counts written under DOTCL_CALL_COVERAGE with the method
# names the compiler sources mention, and writes:
#   calls.tsv     name, compiler reference, emitted (core / later), run (core / later)
#   never-run.txt names the compiler refers to that no generated code ran
#   summary.txt   the counts
#   compiler-unresolved.txt  "X.Y" strings in the compiler that name no method
# "core" is code assembled while the core loads; "later" is everything compiled
# afterwards (the tests' own code). A compiler reference is "literal" when the
# full name is a string in compiler/*.lisp, "prefix:P" when only a leading part
# P is, as a FORMAT control string's head or a CONCATENATE argument (the name
# is built at compile time), "none" otherwise (the table entry is stale, or the
# name reaches the assembler some other way).
#
# Usage: report.sh <repo-root> <raw-dir> <out-dir>
set -eu
ROOT="$(cd "${1%/}" && pwd)"
RAW="$2"
OUT="$3"
mkdir -p "$OUT"

files=$(find "$RAW" -name 'calls-*.tsv' -type f | sort)
if [ -z "$files" ]; then
  echo "FAIL: no counts under $RAW (was DOTCL_CALL_COVERAGE honoured?)"
  exit 1
fi
nproc=$(echo "$files" | wc -l | tr -d ' ')

# shellcheck disable=SC2086
awk -F'\t' '{ e0[$1]+=$2; e1[$1]+=$3; r0[$1]+=$4; r1[$1]+=$5 }
  END { for (k in e0) printf "%s\t%d\t%d\t%d\t%d\n", k, e0[k], e1[k], r0[k], r1[k] }' \
  $files | sort > "$OUT/sums.tsv"

find "$ROOT/compiler" -maxdepth 1 -name '*.lisp' -type f -exec cat {} + \
  | grep -oE '"[A-Z][A-Za-z0-9_]*\.[A-Za-z0-9_]+' | tr -d '"' | sort | uniq -c \
  | awk '{ print $2 "\t" $1 }' > "$OUT/compiler-names.tsv"
find "$ROOT/compiler" -maxdepth 1 -name '*.lisp' -type f -exec cat {} + \
  | grep -oE '"[A-Z][A-Za-z0-9_]*\.[A-Za-z0-9_]*~|concatenate .string +"[A-Z][A-Za-z0-9_]*\.[A-Za-z0-9_]*"' \
  | sed -e 's/^concatenate .string *//' | tr -d '"~' | sort -u > "$OUT/compiler-prefixes.txt"

awk -F'\t' -v OFS='\t' '
  FILENAME == ARGV[1] { pre[$1] = 1; next }
  FILENAME == ARGV[2] { lit[$1] = $2; next }
  {
    ref = "none"
    if ($1 in lit) ref = "literal"
    else {
      best = ""
      for (p in pre)
        if (length(p) > length(best) && length(p) < length($1) \
            && substr($1, 1, length(p)) == p)
          best = p
      if (best != "") ref = "prefix:" best
    }
    print $1, ref, $2, $3, $4, $5
  }' "$OUT/compiler-prefixes.txt" "$OUT/compiler-names.tsv" "$OUT/sums.tsv" > "$OUT/calls.body"
{
  printf 'name\tcompiler-ref\temitted-core\temitted-later\trun-core\trun-later\n'
  cat "$OUT/calls.body"
} > "$OUT/calls.tsv"

awk -F'\t' '$2 != "none" && $5 + $6 == 0 { print $1 "\t" $2 "\temitted=" $3 + $4 }' \
  "$OUT/calls.body" > "$OUT/never-run.txt"

# Names the compiler spells out that the assembler cannot resolve as a method
# (a field, a type, a docstring, or a stale name).
awk -F'\t' 'FILENAME == ARGV[1] { pre[$1] = 1; next }
  FILENAME == ARGV[2] { known[$1] = 1; next }
  !($1 in known) && !($1 in pre) { print $1 }' \
  "$OUT/compiler-prefixes.txt" "$OUT/sums.tsv" "$OUT/compiler-names.tsv" > "$OUT/compiler-unresolved.txt"

awk -F'\t' -v nproc="$nproc" '
  { n++
    if ($2 != "none") { ref++
      if ($5 + $6 == 0) { dead++; if ($3 + $4 == 0) noemit++ }
      else if ($6 == 0) coreonly++ }
    else if ($5 + $6 == 0) unref_dead++ }
  END {
    printf "processes counted:                         %d\n", nproc
    printf "names the assembler resolves:              %d\n", n
    printf "  referred to by compiler/*.lisp:          %d\n", ref
    printf "    never run by any generated code:       %d  (never-run.txt)\n", dead
    printf "      of which never even emitted:         %d\n", noemit
    printf "    run only from code in the core:        %d\n", coreonly
    printf "  not referred to, and never run:          %d\n", unref_dead
  }' "$OUT/calls.body" > "$OUT/summary.txt"
rm -f "$OUT/calls.body"

cat "$OUT/summary.txt"
echo "compiler strings naming no method: $(wc -l < "$OUT/compiler-unresolved.txt" | tr -d ' ') (compiler-unresolved.txt)"
echo "details: $OUT/calls.tsv, $OUT/never-run.txt"
