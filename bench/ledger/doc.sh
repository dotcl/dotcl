#!/bin/bash
# Regenerate the cl-bench tables of docs/benchmarks.md from one ledger run.
#
#   bench/ledger/doc.sh [LEDGER_OUTPUT_DIR] [DOC]
#
# LEDGER_OUTPUT_DIR is what run.sh / remote.sh wrote (results.md and
# results.tsv; default out/ledger-sv7), DOC defaults to docs/benchmarks.md.
# Only the text between "<!-- ledger:begin NAME -->" and "<!-- ledger:end NAME -->"
# is replaced; everything else in DOC is left as it is. Every block is rebuilt
# from the one run, so no value from an earlier run or another machine stays.
#
# Blocks: setup (machine, date, versions) and cl-bench (summary, table, counts).
# Groups and run counts come from bench/cl-bench-runner/run.lisp.
#
# LEDGER_DOC_VERSION   text for the dotcl line instead of the one derived from git
set -eu

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
in=${1:-$root/out/ledger-sv7}
doc=${2:-$root/docs/benchmarks.md}
md=$in/results.md
tsv=$in/results.tsv
[ -s "$md" ] && [ -s "$tsv" ] || { echo "doc.sh: no results.md / results.tsv in $in" >&2; exit 1; }
[ -f "$doc" ] || { echo "doc.sh: no $doc" >&2; exit 1; }

hdr() { sed -n "s/^- $1: //p" "$md" | head -1; }
tree=$(hdr tree | awk '{ print $1 }')
date=$(hdr date | sed 's/,.*//')
cpu=$(hdr machine | sed 's/^[^,]*, *//; s/([RT]M*)//g; s/  */ /g')
host=$(hdr host)
sbcl_line=$(hdr SBCL)
sbcl_ver=$(echo "$sbcl_line" | awk '{ print $2 }')
sbcl_flags=$(echo "$sbcl_line" | sed -n 's/.*(flags: *\(.*\))$/\1/p')
clbench_rev=$(hdr Coalton | sed -n 's/.*cl-bench: *//p')
[ -n "$tree" ] && [ -n "$date" ] || { echo "doc.sh: results.md has no tree or date line" >&2; exit 1; }

ver_of() { git -C "$root" show "$1:runtime/Directory.Build.props" 2>/dev/null | sed -n 's:.*<Version>\(.*\)</Version>.*:\1:p' | head -1; }
if [ -n "${LEDGER_DOC_VERSION:-}" ]; then
  dotcl=$LEDGER_DOC_VERSION
else
  git -C "$root" merge-base --is-ancestor "$tree" HEAD 2>/dev/null ||
    { echo "doc.sh: measured commit $tree is not an ancestor of HEAD" >&2; exit 1; }
  head_ver=$(ver_of HEAD); tree_ver=$(ver_of "$tree")
  if [ -n "$head_ver" ] && [ "$head_ver" != "$tree_ver" ]; then
    dotcl="$head_ver, measured at commit $tree (the release commit on top of it changes only the version number and documents)"
  else
    dotcl="the development sources at commit $tree (after v$tree_ver)"
  fi
fi
case $sbcl_ver in
  *.roswell) sbcl="SBCL ${sbcl_ver%.roswell} (installed through Roswell)";;
  *) sbcl="SBCL $sbcl_ver";;
esac

tmp=$root/out/ledger-doc
mkdir -p "$tmp"

{
  if [ -n "$host" ]; then
    echo "- Machine and date: $cpu ($host), measured on $date."
  else
    echo "- Machine and date: $cpu, measured on $date."
  fi
  echo "- dotcl: $dotcl. Release build, with \`DOTCL_NO_JIT_PROFILE=1\` and \`DOTNET_gcServer=0\`."
  if [ -n "$sbcl_flags" ]; then
    echo "  $sbcl, started with \`$sbcl_flags\`."
  else
    echo "  $sbcl."
  fi
  [ -z "$clbench_rev" ] || echo "- cl-bench revision $clbench_rev."
} > "$tmp/setup.md"

awk '
  /\(with-suite \(:/ { g = $0; sub(/.*\(with-suite \(:/, "", g); sub(/[ )].*/, "", g) }
  /^[ \t]*\(bench "/ { n = $0; sub(/^[ \t]*\(bench "/, "", n); r = n; sub(/".*/, "", n)
                       sub(/^[^"]*" */, "", r); sub(/[ \t].*/, "", r); print g "\t" n "\t" r }
' "$root/bench/cl-bench-runner/run.lisp" > "$tmp/clbench-order.tsv"

awk -F'\t' '
  function num(v) { return v ~ /^[0-9.]+$/ }
  function ms(v) { if (!num(v)) return v; return (v == int(v)) ? sprintf("%d", v) : sprintf("%.1f", v) }
  function code(s) { return "`" s "`" }
  function join(arr, n,   i, s) { s = ""; for (i = 1; i <= n; i++) s = s (i == 1 ? "" : (i == n ? " and " : ", ")) arr[i]; return s }
  function stats(r, n, td, ts,   i, j, t, lg, a, med) {
    for (i = 1; i <= n; i++) a[i] = r[i] + 0
    for (i = 2; i <= n; i++) { t = a[i]; for (j = i - 1; j >= 1 && a[j] > t; j--) a[j + 1] = a[j]; a[j + 1] = t }
    lg = 0; for (i = 1; i <= n; i++) lg += log(a[i])
    med = (n % 2) ? a[(n + 1) / 2] : (a[n / 2] + a[n / 2 + 1]) / 2
    return sprintf("%.2f\t%.2f\t%.2f", exp(lg / n), med, td / ts)
  }
  FNR == 1 { fi++ }
  fi == 1 { ng++; grp[ng] = $1; name[ng] = $2; runs[ng] = $3; known[$2] = 1; next }
  fi == 2 {
    if ($1 ~ /^cl-bench\//)       { b = substr($1, 10); cd[b] = $2; cs[b] = $3; cr[b] = $4; have[b] = 1 }
    else if ($1 ~ /^cl-bench-suite\//) { b = substr($1, 16); sd[b] = $2; ss[b] = $3; sr[b] = $4 }
    next
  }
  END {
    for (b in have) if (!(b in known)) { ng++; grp[ng] = "other"; name[ng] = b; runs[ng] = "-" }
    for (i = 1; i <= ng; i++) {
      b = name[i]; if (!(b in have)) continue
      nrow++; ord[nrow] = b; og[nrow] = grp[i]; orr[nrow] = runs[i]
      if (num(cr[b])) { nc++; crs[nc] = cd[b] / cs[b]; ctd += cd[b]; cts += cs[b] }
      else nratio[++nn] = b
      if (num(sr[b])) { ns++; srs[ns] = sd[b] / ss[b]; std += sd[b]; sts += ss[b] }
    }
    if (nc == 0) { print "doc.sh: no cl-bench row with a ratio" > "/dev/stderr"; exit 1 }
    split(stats(crs, nc, ctd, cts), C, "\t")
    if (ns > 0) split(stats(srs, ns, std, sts), S, "\t"); else { S[1] = S[2] = S[3] = "-" }
    print "| statistic | cold | suite |"
    print "| --- | ---: | ---: |"
    printf "| benchmarks with a ratio | %d | %d |\n", nc, ns
    printf "| geometric mean of the ratios | %s | %s |\n", C[1], S[1]
    printf "| median of the ratios | %s | %s |\n", C[2], S[2]
    printf "| total dotcl time / total SBCL time | %s | %s |\n", C[3], S[3]
    print ""
    if (nn > 0) {
      for (i = 1; i <= nn; i++) {
        b = nratio[i]
        if (cs[b] == "0.0" || cs[b] == "0") why = "SBCL reported 0 ms"
        else if (!num(cd[b])) why = "the dotcl process failed"
        else if (!num(cs[b])) why = "the SBCL process failed"
        else why = "no value"
        txt[i] = code(b) " (" why ")"
      }
      print "Without a cold ratio: " join(txt, nn) "."
      print ""
    }
    # the longest benchmarks on dotcl and their share of the dotcl total
    for (i = 1; i <= nrow; i++) { b = ord[i]; if (num(cr[b])) { k++; lb[k] = b } }
    for (i = 2; i <= k; i++) { t = lb[i]; for (j = i - 1; j >= 1 && cd[lb[j]] + 0 < cd[t] + 0; j--) lb[j + 1] = lb[j]; lb[j + 1] = t }
    top = (k < 4) ? k : 4; part = 0; lo = ""; hi = ""
    for (i = 1; i <= top; i++) {
      part += cd[lb[i]]; tl[i] = code(lb[i])
      if (lo == "" || cr[lb[i]] + 0 < lo + 0) lo = cr[lb[i]]
      if (hi == "" || cr[lb[i]] + 0 > hi + 0) hi = cr[lb[i]]
    }
    printf "The %d longest benchmarks on dotcl (%s) are %.0f%% of the dotcl total in the cold column, at %sx to %sx of the SBCL time.\n", top, join(tl, top), part * 100 / ctd, lo, hi
    print ""
    print "| group | benchmark | runs | dotcl ms | SBCL ms | ratio (cold) | ratio (suite) |"
    print "| --- | --- | ---: | ---: | ---: | ---: | ---: |"
    for (i = 1; i <= nrow; i++) {
      b = ord[i]
      printf "| %s | %s | %s | %s | %s | %s | %s |\n", og[i], b, orr[i], ms(cd[b]), ms(cs[b]), cr[b], ((b in sr) ? sr[b] : "-")
    }
    print ""
    # counts by band, cold
    for (i = 1; i <= nrow; i++) {
      b = ord[i]; if (!num(cr[b])) continue
      r = cr[b] + 0
      if (r < 1) { nf++; fl[nf] = b } else if (r < 2) n12++; else if (r <= 10) n210++; else { nh++; hl[nh] = b }
    }
    for (i = 2; i <= nf; i++) { t = fl[i]; for (j = i - 1; j >= 1 && cr[fl[j]] + 0 > cr[t] + 0; j--) fl[j + 1] = fl[j]; fl[j + 1] = t }
    for (i = 2; i <= nh; i++) { t = hl[i]; for (j = i - 1; j >= 1 && cr[hl[j]] + 0 < cr[t] + 0; j--) hl[j + 1] = hl[j]; hl[j + 1] = t }
    for (i = 1; i <= nf; i++) ft[i] = code(fl[i]) " " cr[fl[i]]
    for (i = 1; i <= nh; i++) ht[i] = code(hl[i]) " " cr[hl[i]]
    printf "- Cold, %d of the %d benchmarks came out faster on dotcl", nf + 0, nc
    if (nf > 0) printf " (%s)", join(ft, nf)
    printf ", %d are between 1x and 2x, %d between 2x and 10x, and %d above 10x", n12 + 0, n210 + 0, nh + 0
    if (nh > 0) printf " (%s)", join(ht, nh)
    print "."
    # the largest cold ratios
    for (i = 1; i <= k; i++) la[i] = lb[i]
    for (i = 2; i <= k; i++) { t = la[i]; for (j = i - 1; j >= 1 && cr[la[j]] + 0 < cr[t] + 0; j--) la[j + 1] = la[j]; la[j + 1] = t }
    top = (k < 6) ? k : 6
    for (i = 1; i <= top; i++) lt[i] = code(la[i]) " " cr[la[i]]
    print "- The largest cold ratios: " join(lt, top) "."
    # short on SBCL
    for (i = 1; i <= nrow; i++) { b = ord[i]; if (num(cr[b]) && cs[b] + 0 < 10) { nsh++; sh[nsh] = code(b) " (" ms(cs[b]) " ms)" } }
    if (nsh > 0) printf "- %d benchmarks have an SBCL time under 10 ms: %s.\n", nsh, join(sh, nsh)
    # suite against cold
    for (i = 1; i <= nrow; i++) { b = ord[i]; if (num(cr[b]) && num(sr[b])) { nb++; if (sr[b] + 0 < cr[b] + 0) nlow++ } }
    if (nb > 0) printf "- The suite ratio is lower than the cold ratio for %d of the %d benchmarks that have both.\n", nlow + 0, nb
  }
' "$tmp/clbench-order.tsv" "$tsv" > "$tmp/cl-bench.md"

awk -v dir="$tmp" '
  /^<!-- ledger:begin [a-z-]+ -->$/ {
    if (cur != "") { print "doc.sh: nested ledger block at line " NR > "/dev/stderr"; bad = 1 }
    cur = $3; f = dir "/" cur ".md"
    r = (getline line < f)
    if (r < 0) { print "doc.sh: no generator for block " cur > "/dev/stderr"; bad = 1 }
    else { print; if (r > 0) print line; while ((getline line < f) > 0) print line; close(f) }
    seen[cur] = 1; next
  }
  /^<!-- ledger:end [a-z-]+ -->$/ {
    if ($3 != cur) { print "doc.sh: unmatched end of " $3 " at line " NR > "/dev/stderr"; bad = 1 }
    cur = ""; print; next
  }
  cur == "" { print }
  END {
    if (cur != "") { print "doc.sh: block " cur " is not closed" > "/dev/stderr"; bad = 1 }
    if (!("setup" in seen) || !("cl-bench" in seen)) { print "doc.sh: the setup and cl-bench markers are both required" > "/dev/stderr"; bad = 1 }
    exit bad
  }
' "$doc" > "$tmp/doc.new"
cp "$tmp/doc.new" "$doc"
echo "doc.sh: $doc from $in (commit $tree, $date)" >&2
