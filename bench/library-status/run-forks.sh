#!/bin/sh
# Stage 2c of the library-status pipeline: which rows a fork that is not in
# the dotcl dist yet would change, and how.
#
# The table measures what a user gets from the dists: Quicklisp plus the dotcl
# overlay. Some libraries already have a dotcl fork with the fix, waiting to be
# added to the overlay. A row that fails only for want of that fork looks the
# same in the table as one that fails for a dotcl bug, which is the wrong
# place to look. This stage tells the two apart, without changing the status
# column (that stays what the dists give):
#
#   1. Every subdirectory of LIBRARY_STATUS_FORKS is a fork checkout. A fork
#      whose HEAD is the commit the dotcl dist already ships is left out; the
#      rest are "pending". Their systems are the DEFSYSTEM names in their .asd
#      files.
#   2. Candidates are the rows that are not ok with the dists alone (they do
#      not load, or their test suite fails, errs, times out or its test system
#      does not load) and whose dependency tree reaches a pending fork's
#      system. The tree is the depends-on lists the dists record in
#      systems.txt (the dotcl dist replaces a Quicklisp project it carries),
#      from the row's system and its test systems.
#   3. Only the candidates are measured again, by run-quickload.sh and
#      run-tests.sh, with the pending forks put ahead of the dists through
#      CL_SOURCE_REGISTRY (ASDF's source registry is searched before
#      Quicklisp's).
#   4. forks.json gets one entry per candidate: the pending forks its tree
#      reaches, what the second run saw, and a judgement comparing the two
#      runs (see step 4 below for the order):
#        pending-fork   loads, or passes, only with the forks: the row waits on them
#        improved       better with the forks, but still not ok
#        same           no better: the forks are not what this row waits on
#        worse          worse with the forks
#
# render.lisp shows it as the "with forks" column.
#
# Usage:  run-forks.sh [repo-root]
#   LIBRARY_STATUS_FORKS        directory of fork checkouts (required)
#   LIBRARY_STATUS_QL_HOME      the Quicklisp home whose dists are read; by
#                               default the one the bundled client picks
#   LIBRARY_STATUS_JSON         dist-only load results (default results.json)
#   LIBRARY_STATUS_TESTS_JSON   dist-only test verdicts (default tests.json)
#   LIBRARY_STATUS_FORKS_JSON   output (default forks.json)
#   LIBRARY_STATUS_LOGDIR       logs (default out/library-status-forks)
#   LIBRARY_STATUS_CANDIDATES   only list the candidates, measure nothing, when 1
set -eu

root="${1:-.}"
here="$root/bench/library-status"
forks="${LIBRARY_STATUS_FORKS:?set LIBRARY_STATUS_FORKS to the directory of fork checkouts}"
results="${LIBRARY_STATUS_JSON:-$here/results.json}"
tests="${LIBRARY_STATUS_TESTS_JSON:-$here/tests.json}"
out="${LIBRARY_STATUS_FORKS_JSON:-$here/forks.json}"
logdir="${LIBRARY_STATUS_LOGDIR:-$root/out/library-status-forks}"
only_list="${LIBRARY_STATUS_CANDIDATES:-0}"

# The home the bundled Quicklisp client uses (its prelude): an existing
# ~/quicklisp, else $XDG_DATA_HOME/dotcl/quicklisp.
if [ -n "${LIBRARY_STATUS_QL_HOME:-}" ]; then qlhome="$LIBRARY_STATUS_QL_HOME"
elif [ -d "$HOME/quicklisp" ]; then qlhome="$HOME/quicklisp"
else qlhome="${XDG_DATA_HOME:-$HOME/.local/share}/dotcl/quicklisp"
fi
dists="$qlhome/dists"
for d in quicklisp dotcl; do
  [ -f "$dists/$d/systems.txt" ] || { echo "run-forks: no $dists/$d/systems.txt" >&2; exit 1; }
done

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$logdir"

# 1. The pending forks: "FORK DIR SYSTEM..." per line.
: > "$work/forks"
for dir in "$forks"/*/; do
  dir=${dir%/}
  name=$(basename "$dir")
  [ -d "$dir/.git" ] || [ -f "$dir/.git" ] || continue
  head=$(git -C "$dir" rev-parse --short=7 HEAD)
  # The dotcl dist names a release PROJECT-DATE-COMMIT.
  if awk -v p="$name" -v h="$head" '$1 == p && $6 ~ ("-" h "$") { found = 1 } END { exit !found }' \
       "$dists/dotcl/releases.txt"; then
    continue
  fi
  systems=$(find -H "$dir" -name '*.asd' -exec cat {} + 2>/dev/null \
    | tr 'A-Z' 'a-z' \
    | sed -En 's/.*\([[:space:]]*(asdf:|asdf\/[a-z-]*:)?defsystem[[:space:]]+([^[:space:])]+).*/\2/p' \
    | sed 's/^#://; s/^://; s/"//g' | sort -u | tr '\n' ' ')
  [ -n "$systems" ] || continue
  echo "$name $dir $head $systems" >> "$work/forks"
done

# 2. The candidates: "SYSTEM FORK,FORK..." per line, in results order.
#    Not ok with the dists alone: a load that failed, or a test verdict that
#    says something went wrong.
sed -En 's/^  [{]"system": "([^"]*)".*"status": "([^"]*)".*/\1 \2/p' "$results" > "$work/status"
sed -En 's/^  [{]"system": "([^"]*)".*"verdict": "([^"]*)".*"failed": ([0-9]*).*"test-systems": "([^"]*)".*/\1 \2 \3 \4/p' "$tests" > "$work/verdicts"
awk '
  FILENAME == ARGV[1] { if ($0 !~ /^#/ && NF >= 3) { proj[$3] = $1; for (i = 4; i <= NF; i++) qdeps[$3] = qdeps[$3] " " $i } next }
  FILENAME == ARGV[2] { if ($0 !~ /^#/ && NF >= 3) { over[$1] = 1; dproj[$3] = $1; for (i = 4; i <= NF; i++) ddeps[$3] = ddeps[$3] " " $i } next }
  FILENAME == ARGV[3] { for (i = 4; i <= NF; i++) forkof[$i] = (forkof[$i] == "" ? $1 : forkof[$i]); next }
  FILENAME == ARGV[4] { tsys[$1] = ""; verdict[$1] = $2; nfail[$1] = $3; for (i = 4; i <= NF; i++) tsys[$1] = tsys[$1] " " $i; next }
  FILENAME == ARGV[5] {
    sys = $1; st = $2
    bad = (st == "fail") || (verdict[sys] ~ /^(fail|error|timeout|load-fail)$/)
    if (!bad) next
    # The dependency closure. A Quicklisp project the dotcl dist carries is
    # read from the dotcl dist only.
    n = 0; delete seen; delete hit
    stack[++n] = sys
    split(tsys[sys], extra, " ")
    for (k in extra) stack[++n] = extra[k]
    stack[++n] = sys "/test"; stack[++n] = sys "/tests"; stack[++n] = sys "-test"; stack[++n] = sys "-tests"
    while (n > 0) {
      s = stack[n--]
      if (s in seen) continue
      seen[s] = 1
      if (s in forkof) hit[forkof[s]] = 1
      if (s in dproj) d = ddeps[s]
      else if ((s in proj) && !(proj[s] in over)) d = qdeps[s]
      else d = ""
      m = split(d, w, " ")
      for (k = 1; k <= m; k++) if (!(w[k] in seen)) stack[++n] = w[k]
    }
    names = ""
    for (f in hit) names = names (names == "" ? "" : ",") f
    if (names != "") print sys, names, st, (sys in verdict ? verdict[sys] : "-"), (sys in nfail ? nfail[sys] : 0)
  }' "$dists/quicklisp/systems.txt" "$dists/dotcl/systems.txt" "$work/forks" "$work/verdicts" "$work/status" \
  > "$work/candidates"

echo "run-forks: pending forks:"
awk '{ printf "  %-28s %s\n", $1, $3 }' "$work/forks"
echo "run-forks: $(wc -l < "$work/candidates" | tr -d ' ') candidate rows"
if [ "$only_list" = "1" ]; then
  awk '{ printf "  %-28s %-10s %-10s %s\n", $1, $3, $4, $2 }' "$work/candidates"
  exit 0
fi

# 3. Measure the candidates with the pending forks ahead of the dists.
reg="(:source-registry"
while read -r name dir rest; do
  reg="$reg (:tree \"$dir/\")"
done < "$work/forks"
CL_SOURCE_REGISTRY="$reg :inherit-configuration)"
export CL_SOURCE_REGISTRY

awk '$3 == "fail" { print $1 }' "$work/candidates" > "$work/load-targets"
LIBRARY_STATUS_TARGETS="$work/load-targets" LIBRARY_STATUS_JSON="$work/results.json" \
LIBRARY_STATUS_LOGDIR="$logdir/load" LIBRARY_STATUS_RESUME=0 LIMIT=0 \
  sh "$here/run-quickload.sh" "$root"
# Test every candidate that loads: those that loaded before, and those the
# forks made load.
sed -En 's/^  [{]"system": "([^"]*)".*"status": "(load-only|patched)".*/\1/p' "$work/results.json" > "$work/newly"
{ awk '$3 != "fail" { print $1 }' "$work/candidates"; cat "$work/newly"; } > "$work/test-targets"
LIBRARY_STATUS_TEST_TARGETS="$work/test-targets" LIBRARY_STATUS_TESTS_JSON="$work/tests.json" \
LIBRARY_STATUS_LOGDIR="$logdir/tests" LIBRARY_STATUS_RESUME=0 LIMIT=0 \
  sh "$here/run-tests.sh" "$root"

# 4. forks.json. Two runs are compared by load first (does not load < loads),
#    then by test verdict (timeout, load-fail < error < no-result, no-tests <
#    fail < pass), then, both failing, by the number of failures.
#      pending-fork  did not load and now loads, or did not pass and now passes
#      improved      better, short of that (fewer failures, or error -> fail)
#      same / worse
sed -En 's/^  [{]"system": "([^"]*)".*"status": "([^"]*)".*/\1 \2/p' "$work/results.json" > "$work/b-status"
sed -En 's/^  [{]"system": "([^"]*)".*"verdict": "([^"]*)", "framework": "([^"]*)", "passed": ([0-9]*), "failed": ([0-9]*).*/\1 \2 \3 \4 \5/p' \
  "$work/tests.json" > "$work/b-verdicts"
checked=$(date +%Y-%m-%d)
awk -v checked="$checked" '
  function trank(v) {
    if (v == "pass") return 4; if (v == "fail") return 3
    if (v == "no-result" || v == "no-tests" || v == "-") return 2
    if (v == "error") return 1; return 0 }
  FILENAME == ARGV[1] { bload[$1] = $2; next }
  FILENAME == ARGV[2] { bv[$1] = $2; bfw[$1] = $3; bp[$1] = $4; bf[$1] = $5; next }
  {
    sys = $1; forks = $2; aload = $3; av = $4; af = $5
    gsub(/,/, " ", forks)
    bl = (sys in bload) ? bload[sys] : aload
    b = (sys in bv) ? bv[sys] : "-"
    al = (aload == "fail") ? 0 : 1; blr = (bl == "fail") ? 0 : 1
    if (blr > al || (al == 1 && av != "pass" && b == "pass")) j = "pending-fork"
    else if (blr < al) j = "worse"
    else if (al == 0) j = "same"
    else if (trank(b) > trank(av)) j = "improved"
    else if (trank(b) < trank(av)) j = "worse"
    else if (b == "fail" && bf[sys] < af) j = "improved"
    else if (b == "fail" && bf[sys] > af) j = "worse"
    else j = "same"
    v = (sys in bv) ? bv[sys] : "-"
    printf "  {\"system\": \"%s\", \"checked\": \"%s\", \"forks\": \"%s\", \"judgement\": \"%s\", \"load\": \"%s\", \"verdict\": \"%s\", \"framework\": \"%s\", \"passed\": %d, \"failed\": %d}\n", \
      sys, checked, forks, j, (bl == "fail" ? "fail" : "loads"), v, (sys in bfw ? bfw[sys] : "-"), bp[sys], bf[sys]
  }' "$work/b-status" "$work/b-verdicts" "$work/candidates" > "$work/entries"
{
  echo '['
  sed '$!s/$/,/' "$work/entries"
  echo ']'
} > "$out"

echo ""
echo "run-forks: $(wc -l < "$work/entries" | tr -d ' ') candidates -> $out"
for j in pending-fork improved same worse; do
  printf '  %-13s %s\n' "$j" "$(grep -c "\"judgement\": \"$j\"" "$work/entries" || true)"
done
