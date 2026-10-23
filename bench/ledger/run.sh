#!/bin/bash
# Re-measure every dotcl-versus-SBCL ruler in items.tsv and write one table.
#
#   bench/ledger/run.sh [setup|measure|render|all]      (default: all)
#
# For each row of items.tsv, in its own process per side, dotcl and SBCL are run
# alternately ROUNDS times (default 3); the table keeps the fastest value of each
# side. Every process runs under flock when LEDGER_LOCK is set, one at a time, so
# other jobs on the machine can take the lock between two measurements.
#
# Output (in $LEDGER_OUT, default $LEDGER_WORK/out):
#   raw.tsv          one line per value: metric side round value unit note
#   results.tsv      metric, dotcl, SBCL, ratio, unit, how (failed rows say "failed")
#   results.md       the same as a Markdown table with the run header
#   results-detail.tsv   per-test lines for the ironclad rows
#
# Environment:
#   LEDGER_TREE      dotcl tree with a Release runtime and compiler/dotcl.core (default: this tree)
#   LEDGER_WORK      scratch directory for clones, caches and logs (default: $LEDGER_TREE/out/ledger)
#   LEDGER_MACHINE   name of the machine, selects the baseline file (default: hostname)
#   LEDGER_PREV      previous results.tsv to compare with (default: bench/ledger/baseline/<machine>.tsv)
#   LEDGER_SAVE=1    copy this run's results.tsv over LEDGER_PREV after rendering
#   LEDGER_REG_PCT   a row whose ratio is this many percent above the previous run is listed first (default 10)
#   LEDGER_APPEND=1  keep raw.tsv and replace only the rows being measured (see LEDGER_ONLY)
#   LEDGER_PUBLISH=1 after "all", replace the body of issue LEDGER_ISSUE in LEDGER_ISSUE_REPO (both
#                    required, no defaults); the old body goes into a comment first
#   LEDGER_ROUNDS    alternating rounds per row (default 3)
#   LEDGER_ONLY      extended regex; only rows whose id matches are measured
#   LEDGER_ITEMS     rows file (default: items.tsv next to this script)
#   LEDGER_NOTES     id<TAB>note file shown in the note column (default: notes.tsv next to this script)
#   LEDGER_LOCK      lock file taken around each process (default ~/measure.lock; empty = none)
#   SBCL_CMD         how to start SBCL (default: ros -L sbcl-bin run -- --dynamic-space-size 4096)
#   COALTON_DIR      Coalton checkout (default: $LEDGER_WORK/coalton, cloned by setup)
#   LEDGER_COALTON_REV   Coalton revision setup checks out (default 024d0a17)
#   DOTCL_QL_HOME    dotcl's Quicklisp home (default ~/.local/share/dotcl/quicklisp)
#   ROS_QL_SOFTWARE  Roswell Quicklisp software directory
#                    (default ~/.roswell/lisp/quicklisp/dists/quicklisp/software)
set -u

here=$(cd "$(dirname "$0")" && pwd)
TREE=${LEDGER_TREE:-$(cd "$here/../.." && pwd)}
WORK=${LEDGER_WORK:-$TREE/out/ledger}
OUT=${LEDGER_OUT:-$WORK/out}
ROUNDS=${LEDGER_ROUNDS:-3}
ITEMS=${LEDGER_ITEMS:-$here/items.tsv}
NOTES=${LEDGER_NOTES:-$here/notes.tsv}
ONLY=${LEDGER_ONLY:-}
LOCK=${LEDGER_LOCK-$HOME/measure.lock}
SBCL_CMD=${SBCL_CMD:-"ros -L sbcl-bin run -- --dynamic-space-size 4096"}
COALTON_REV=${LEDGER_COALTON_REV:-024d0a17}
COALTON_DIR=${COALTON_DIR:-$WORK/coalton}
QLH=${DOTCL_QL_HOME:-$HOME/.local/share/dotcl/quicklisp}
ROSQL=${ROS_QL_SOFTWARE:-$HOME/.roswell/lisp/quicklisp/dists/quicklisp/software}
RUNTIME=$TREE/runtime/bin/Release/net10.0/runtime
CORE=$TREE/compiler/dotcl.core
IC_DOTCL=$WORK/ic-dotcl
IC_SBCL=$WORK/ic-sbcl

MACHINE=${LEDGER_MACHINE:-$(hostname)}
PREV=${LEDGER_PREV:-$here/baseline/$MACHINE.tsv}
REG_PCT=${LEDGER_REG_PCT:-10}
ISSUE=${LEDGER_ISSUE:-}
ISSUE_REPO=${LEDGER_ISSUE_REPO:-}
mkdir -p "$WORK" "$OUT"

log() { echo "[$(date +%H:%M:%S)] $*" >&2; }

# ---------------------------------------------------------------- setup

# newest directory under the given software directories whose name starts with $1-
find_software() {
  local name=$1 d s
  shift
  for s in "$@"; do
    for d in $(ls "$s" 2>/dev/null | sort -r -V); do
      case $d in "$name"-[0-9v]*|"$name"-git*) echo "$s/$d"; return 0;; esac
    done
  done
  return 1
}

setup_ironclad() {
  local ov=$QLH/dists/dotcl/software st=$QLH/dists/quicklisp/software p src
  local pkg
  for side in dotcl sbcl; do
    [ $side = dotcl ] && dst=$IC_DOTCL || dst=$IC_SBCL
    [ -f "$dst/.ready" ] && continue
    rm -rf "$dst"; mkdir -p "$dst"
    for p in alexandria bordeaux-threads global-vars ironclad rt trivial-features trivial-garbage; do
      src=$(find_software $p "$ov" "$st" "$ROSQL") || { log "ironclad setup: $p not found"; return 1; }
      cp -R "$src" "$dst/"
    done
    if [ $side = dotcl ]; then
      # ironclad's assembly path is SBCL-only code; dotcl is compared with the
      # portable Lisp code, as the ironclad rows have always been measured
      pkg=$(ls -d "$dst"/ironclad-*/src/package.lisp)
      sed -i.bak 's/^(pushnew :ironclad-assembly \*features\*)/(progn)/' "$pkg"
      rm -f "$pkg.bak"
      grep -q '^(progn)$' "$pkg" || { log "ironclad setup: assembly line not found in $pkg"; return 1; }
    fi
    touch "$dst/.ready"
  done
}

setup() {
  [ -x "$RUNTIME" ] && [ -f "$CORE" ] || { log "no Release runtime / dotcl.core in $TREE (build first)"; return 1; }
  if [ ! -d "$WORK/cl-bench" ]; then
    git clone -q https://github.com/benkard/cl-bench.git "$WORK/cl-bench" || return 1
  fi
  if [ ! -d "$COALTON_DIR" ]; then
    git clone -q https://github.com/coalton-lang/coalton.git "$COALTON_DIR" || return 1
    git -C "$COALTON_DIR" checkout -q "$COALTON_REV" || return 1
  fi
  setup_ironclad || return 1
  log "setup done"
}

# CL_SOURCE_REGISTRY for dotcl: the Coalton checkout, then the newest dotcl
# overlay release of each project, then dotcl's stock dist, then Roswell's
registry_dotcl() {
  local ov=$QLH/dists/dotcl/software out d p src
  declare -A seen
  seen[coalton]=1
  out="(:source-registry (:tree \"$COALTON_DIR/\")"
  for src in "$ov" "$QLH/dists/quicklisp/software" "$ROSQL"; do
    for d in $(ls "$src" 2>/dev/null | sort -r -V); do
      p=$(echo "$d" | sed -E 's/-(v?[0-9][0-9.]*|release|git|darcs).*$//')
      [ -n "${seen[$p]:-}" ] && continue
      seen[$p]=1
      out="$out (:tree \"$src/$d/\")"
    done
  done
  echo "$out :ignore-inherited-configuration)"
}

# SBCL uses Roswell's Quicklisp; its named-readtables is too old for Coalton,
# so the one from dotcl's stock dist is added
registry_sbcl() {
  local nr
  nr=$(find_software named-readtables "$QLH/dists/quicklisp/software") || nr=
  echo "(:source-registry (:tree \"$COALTON_DIR/\")${nr:+ (:tree \"$nr/\")} :inherit-configuration)"
}

# ---------------------------------------------------------------- running

# lisp_args SIDE EVAL... -- FILE : the Lisp command line for SIDE (sets ARGV)
# The first word of ARGV is the command; env assignments are passed by "env".
build_argv() {
  local side=$1 kind=$2 file=$3; shift 3
  local evals=("$@") e
  ARGV=()
  if [ "$side" = dotcl ]; then
    ARGV=("$RUNTIME" --core "$CORE")
    for e in "${evals[@]}"; do ARGV+=(--eval "$e"); done
    ARGV+=("$file")
  else
    read -ra ARGV <<< "$SBCL_CMD"
    ARGV+=(--non-interactive)
    [ "$kind" = coalton ] || ARGV+=(--no-userinit)
    for e in "${evals[@]}"; do ARGV+=(--eval "$e"); done
    ARGV+=(--load "$file")
    case $kind in clbench*) ARGV+=(--eval '(quit)');; esac
  fi
}

# "a,b" (benchmarks that need an earlier one to define their classes) -> '("a" "b")
bench_names() {
  case $1 in
    *,*) printf "'(%s)" "$(echo "$1" | sed 's/[^,]*/"&"/g; s/,/ /g')";;
    *) printf '"%s"' "$1";;
  esac
}

# run_one KIND ARG SIDE TIMEOUT STDOUT STDERR : sets RC
run_one() {
  local kind=$1 arg=$2 side=$3 to=$4 so=$5 se=$6
  local envs=(DOTCL_NO_JIT_PROFILE=1 DOTNET_gcServer=0) file evals=() dir=$WORK
  case $kind in
    clbench)
      file=$TREE/bench/cl-bench-runner/run.lisp
      evals=("(setq *bench-name* $(bench_names "$arg"))");;
    clbench-suite)
      file=$TREE/bench/cl-bench-runner/run.lisp
      evals=("(setq *bench-suite* :$arg)");;
    clbench-warm)
      file=$TREE/bench/cl-bench-runner/run.lisp
      evals=("(setq *bench-name* $(bench_names "$arg"))" '(setq *bench-runs* 3)' '(setq *bench-warmup* 1)');;
    coalton)
      file=$TREE/bench/coalton/run.lisp
      envs+=(COALTON_ENV=release "XDG_CACHE_HOME=$WORK/xdg-$side" "COALTON_BENCH_OUT=$WORK/fasl-$side/"
             "COALTON_BENCH_ONLY=$arg")
      if [ $side = dotcl ]; then envs+=("CL_SOURCE_REGISTRY=$(registry_dotcl)")
      else envs+=("CL_SOURCE_REGISTRY=$(registry_sbcl)"); fi;;
    parity)
      file=$TREE/bench/csharp-parity/kernels.lisp;;
    ironclad-cold|ironclad-warm)
      file=$here/ironclad-drv.lisp
      [ $side = dotcl ] && envs+=("IC_DIR=$IC_DOTCL") || envs+=("IC_DIR=$IC_SBCL")
      envs+=("XDG_CACHE_HOME=$WORK/xdg-ic-$side")
      if [ $kind = ironclad-warm ]; then envs+=(IC_REPS=4); fi
      [ "$arg" = all ] || envs+=("IC_CATS=$arg");;
    imago)
      file=$here/bic2-run.lisp
      envs+=("LEDGER_FASL=$WORK/bic2-$side.fasl");;
    *) echo "unknown kind $kind" >&2; RC=99; return;;
  esac
  build_argv "$side" "$kind" "$file" "${evals[@]}"
  local cmd=(timeout -k 10 "$to" env "${envs[@]}" "${ARGV[@]}")
  if [ -n "$LOCK" ]; then
    (cd "$dir" && flock "$LOCK" "${cmd[@]}") < /dev/null > "$so" 2> "$se"
  else
    (cd "$dir" && "${cmd[@]}") < /dev/null > "$so" 2> "$se"
  fi
  RC=$?
}

# parse KIND ARG ID SO SE : prints "metric<TAB>value<TAB>unit<TAB>note" lines
parse() {
  local kind=$1 arg=$2 id=$3 so=$4 se=$5
  case $kind in
    clbench|clbench-warm)
      awk -v key="\"${arg##*,}\": " -v id="$id" '
        { s = $0; sub(/^ +/, "", s)
          if (index(s, key) != 1) next
          v = substr(s, length(key) + 1)
          gsub(/[\[\], ]+$/, "", v); gsub(/^\[/, "", v)
          n = split(v, a, /, */)
          for (i = 1; i <= n; i++) if (a[i] ~ /^[0-9.]+$/ && (best == "" || a[i] + 0 < best + 0)) best = a[i]
        }
        END { if (best != "") printf "%s\t%.1f\tms\t\n", id, best * 1000 }' "$se";;
    clbench-suite)
      awk '
        { s = $0; sub(/^ +/, "", s)
          if (s !~ /^"[^"]+": [0-9.]+,?$/ || s ~ /\/consed"/) next
          split(s, a, "\": "); name = substr(a[1], 2); v = a[2]; sub(/,$/, "", v)
          printf "cl-bench-suite/%s\t%.1f\tms\t\n", name, v * 1000 }' "$se";;
    coalton)
      awk -v k="$arg" -v id="$id" '$1 == "RESULT" && $2 == k { printf "%s\t%s\tms\t%s\n", id, $3, $4 }' "$so";;
    parity)
      awk -F'\t' -v id="$id" 'NF == 2 && $1 ~ /^[a-z][a-z-]*$/ && $2 ~ /^[0-9.]+$/ { printf "%s/%s\t%s\tms\t\n", id, $1, $2 }' "$so";;
    ironclad-cold|ironclad-warm)
      awk -v id="$id" '$1 == "TIME" && $5 == "ok" { printf "%s/%s/%s\t%.1f\tms\t\n", id, $2, $3, $4 * 1000 }' "$so";;
    imago)
      awk -v id="$id" '$1 == "BIC2" { printf "%s\t%s\tns\t%s\n", id, $2, $6 }' "$so";;
  esac
}

measure() {
  [ -x "$RUNTIME" ] && [ -f "$CORE" ] || { log "no Release runtime / dotcl.core in $TREE"; return 1; }
  if [ "${LEDGER_APPEND:-0}" = 1 ] && [ -f "$OUT/raw.tsv" ]; then
    :
  else
    : > "$OUT/raw.tsv"
    date +%s > "$OUT/start.epoch"
  fi
  local id kind arg to side round so se n failed primed=" "
  local ids=0
  while IFS=$'\t' read -r id kind arg to _note; do
    case $id in ''|'#'*) continue;; esac
    if [ -n "$ONLY" ] && ! echo "$id" | grep -Eq "$ONLY"; then continue; fi
    ids=$((ids + 1))
    if [ "${LEDGER_APPEND:-0}" = 1 ]; then
      awk -F'\t' -v id="$id" '$1 != id && index($1, id "/") != 1' "$OUT/raw.tsv" > "$OUT/raw.tmp" && mv "$OUT/raw.tmp" "$OUT/raw.tsv"
    fi
    for side in dotcl sbcl; do
      case $kind in coalton|ironclad-cold|ironclad-warm|imago)
        # the first process of a kind fills the fasl cache; it is not measured
        case $primed in *" $kind-$side "*) ;; *)
          primed="$primed$kind-$side "
          log "prime $kind $side"
          run_one "$kind" "$arg" "$side" 3600 "$OUT/prime.out" "$OUT/prime.err";;
        esac;;
      esac
    done
    failed_dotcl=0; failed_sbcl=0
    for round in $(seq 1 "$ROUNDS"); do
      for side in dotcl sbcl; do
        eval "failed=\$failed_$side"
        [ "$failed" = 1 ] && continue
        so=$OUT/last-$side.out; se=$OUT/last-$side.err
        log "$id $side round $round"
        run_one "$kind" "$arg" "$side" "$to" "$so" "$se"
        parse "$kind" "$arg" "$id" "$so" "$se" > "$OUT/last.parsed"
        n=$(wc -l < "$OUT/last.parsed" | tr -d ' ')
        awk -F'\t' -v s="$side" -v r="$round" '{ printf "%s\t%s\t%s\t%s\t%s\t%s\n", $1, s, r, $2, $3, $4 }' \
          "$OUT/last.parsed" >> "$OUT/raw.tsv"
        if [ "$RC" != 0 ] || [ "$n" = 0 ]; then
          printf '%s\t%s\t%s\t\t\tFAIL exit=%s values=%s\n' "$id" "$side" "$round" "$RC" "$n" >> "$OUT/raw.tsv"
          mkdir -p "$OUT/fail"
          cp "$so" "$OUT/fail/$(echo "$id" | tr '/+' '__')-$side.out" 2>/dev/null
          cp "$se" "$OUT/fail/$(echo "$id" | tr '/+' '__')-$side.err" 2>/dev/null
          log "$id $side: failed (exit $RC, $n values)"
          [ "$n" = 0 ] && eval "failed_$side=1"
        fi
      done
    done
  done < "$ITEMS"
  date +%s > "$OUT/end.epoch"
  log "measured $ids rows"
}

# ---------------------------------------------------------------- table

render() {
  [ -s "$OUT/raw.tsv" ] || { log "no raw.tsv in $OUT"; return 1; }
  { printf -- '-\t-\n'; grep -v '^#' "$NOTES" 2>/dev/null | awk -F'\t' 'NF >= 2 { print $1 "\t" $2 }'; } > "$OUT/notes.tsv"
  local prev=$PREV
  [ -f "$prev" ] || { prev=$OUT/no-previous.tsv; : > "$prev"; }
 { printf -- '-\t-\t-\t-\n'; cat "$prev"; } > "$OUT/prev-in.tsv"
  : > "$OUT/regressions.tsv"; : > "$OUT/results-detail.tsv"
  awk -F'\t' -v OFS='\t' -v regfile="$OUT/regressions.tsv" -v detailfile="$OUT/results-detail.tsv" -v thr="$REG_PCT" '
    function how(m) {
      if (m ~ /^cl-bench-warm\//) return "cl-bench, warm (1 warm-up + 3 runs in the process, min)"
      if (m ~ /^cl-bench-suite\//) return "cl-bench, whole suite in one process (cold, JIT included, earlier benchmarks have run)"
      if (m ~ /^cl-bench\//) return "cl-bench, cold (1 scalar run, JIT included)"
      if (m ~ /^coalton\//) return "Coalton kernel, warm (1 warm-up in the process, min of 5)"
      if (m ~ /^parity\//) return "csharp-parity kernel, warm (1 warm-up block, min of 5)"
      if (m ~ /^ironclad-cold\//) return "ironclad rt suite, 1 rep per test (cache warm, JIT included); sum of tests"
      if (m ~ /^ironclad-warm\//) return "ironclad rt suite, min of 4 reps per test; sum of tests"
      if (m ~ /^imago\//) return "imago bicubic, warm (min of 5 in the process), ns per pixel"
      return ""
    }
    FNR == 1 { fi++ }
    fi == 1 { hnote[$1] = $2; next }
    fi == 2 { pratio[$1] = $4; next }
    $6 ~ /^FAIL/ { fail[$1, $2] = 1; failnote[$1] = failnote[$1] " " $2 ":" $6; seen[$1] = 1; next }
    $4 != "" {
      k = $1 SUBSEP $2
      if (!(k in best) || $4 + 0 < best[k] + 0) best[k] = $4 + 0
      unit[$1] = $5
      if ($6 != "") sumnote[$1, $2] = $6
      seen[$1] = 1
    }
    END {
      for (m in seen) {
        d = ((m SUBSEP "dotcl") in best) ? best[m, "dotcl"] : ""
        s = ((m SUBSEP "sbcl") in best) ? best[m, "sbcl"] : ""
        if (m ~ /^ironclad-(cold|warm)\/[^\/]+\/./) {
          split(m, p, "/")
          detail[m] = d OFS s
          if (d != "" && s != "") {
            cat = p[1] "/" p[2]
            cd[cat] += d; cs[cat] += s; cseen[cat] = 1
            td[p[1]] += d; ts[p[1]] += s; cseen[p[1] "/TOTAL"] = 1
          }
          continue
        }
        row[m] = 1
        rd[m] = d; rs[m] = s
      }
      for (c in cseen) {
        if (c ~ /\/TOTAL$/) { split(c, q, "/"); rd[c] = td[q[1]]; rs[c] = ts[q[1]]; unit[c] = "ms" }
        else { rd[c] = cd[c]; rs[c] = cs[c]; unit[c] = "ms" }
        row[c] = 1
      }
      for (m in row) {
        d = rd[m]; s = rs[m]
        note = ""
        split(m, g, "/"); hn = (m in hnote) ? hnote[m] : ((g[1] in hnote) ? hnote[g[1]] : "")
        if (d != "" && s != "" && s + 0 > 0) ratio = sprintf("%.2f", d / s); else ratio = ""
        if ((m SUBSEP "dotcl") in sumnote && (m SUBSEP "sbcl") in sumnote && sumnote[m, "dotcl"] != sumnote[m, "sbcl"]) note = "checksum differs"
        if (hn != "") note = (note == "" ? hn : note "; " hn)
        if (ratio != "" && (m in pratio) && pratio[m] ~ /^[0-9.]+$/ && ratio + 0 > pratio[m] * (1 + thr / 100)) {
          printf "%s\t%s\t%s\t%+.0f%%\n", m, pratio[m], ratio, (ratio / pratio[m] - 1) * 100 > regfile
        }
        ds = (d == "") ? "failed" : sprintf("%.1f", d)
        ss = (s == "") ? "failed" : sprintf("%.1f", s)
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\n", m, ds, ss, (ratio == "" ? "-" : ratio), (unit[m] == "" ? "ms" : unit[m]), how(m), note > "/dev/stdout"
      }
      for (m in detail) print m, detail[m] > detailfile
    }' "$OUT/notes.tsv" "$OUT/prev-in.tsv" "$OUT/raw.tsv" | sort -t$'\t' -k4,4gr -k1,1 > "$OUT/results.body"
  # rows without a ratio (failed) sort first with -g; move them to the end
  { awk -F'\t' '$4 != "-"' "$OUT/results.body"; awk -F'\t' '$4 == "-"' "$OUT/results.body"; } > "$OUT/results.tsv"
  rm -f "$OUT/results.body"
  sort -o "$OUT/results-detail.tsv" "$OUT/results-detail.tsv" 2>/dev/null

  local start end elapsed
  start=$(cat "$OUT/start.epoch" 2>/dev/null || echo 0); end=$(cat "$OUT/end.epoch" 2>/dev/null || echo 0)
  elapsed=$(( end - start ))
  {
    echo "# dotcl versus SBCL: current values"
    echo
    echo "- tree: $(git -C "$TREE" rev-parse --short=8 HEAD 2>/dev/null) (Release, compiler/dotcl.core, DOTCL_NO_JIT_PROFILE=1, DOTNET_gcServer=0)"
    echo "- SBCL: $(cd "$WORK" && $SBCL_CMD --version 2>/dev/null </dev/null | head -1) (flags: ${SBCL_CMD#*--})"
    echo "- Coalton: $(git -C "$COALTON_DIR" rev-parse --short=8 HEAD 2>/dev/null); cl-bench: $(git -C "$WORK/cl-bench" rev-parse --short=8 HEAD 2>/dev/null)"
    echo "- machine: $(hostname), $(grep -m1 'model name' /proc/cpuinfo 2>/dev/null | sed 's/.*: //')"
    echo "- method: each row is its own process per side; dotcl and SBCL alternate $ROUNDS times; the fastest value of each side is shown"
    echo "- date: $(date +%Y-%m-%d), wall time of the run $((elapsed / 60)) min (lock waits included)"
    echo "- ratio = dotcl / SBCL; times in ms (imago in ns per pixel); \"failed\" = the process died or printed no value on that side in every round"
    echo
    if [ ! -s "$prev" ]; then echo "Regressions: no previous run to compare with ($PREV)."
    elif [ -s "$OUT/regressions.tsv" ]; then
      echo "## Regressions: ratio ${REG_PCT}% or more above the previous run"
      echo
      echo "| item | previous ratio | ratio now | change |"
      echo "|---|---:|---:|---:|"
      sort "$OUT/regressions.tsv" | awk -F'\t' '{ printf "| %s | %s | %s | %s |\n", $1, $2, $3, $4 }'
    else echo "Regressions: none (nothing is ${REG_PCT}% or more above the previous run, $PREV)."
    fi
    echo
    echo "| item | dotcl | SBCL | ratio | unit | how | note |"
    echo "|---|---:|---:|---:|---|---|---|"
    awk -F'\t' '{ printf "| %s | %s | %s | %s | %s | %s | %s |\n", $1, $2, $3, $4, $5, $6, $7 }' "$OUT/results.tsv"
    echo
    echo "Per-test lines of the ironclad rows are in results-detail.tsv (metric, dotcl ms, SBCL ms)."
  } > "$OUT/results.md"
  if [ "${LEDGER_SAVE:-0}" = 1 ]; then mkdir -p "$(dirname "$PREV")"; cp "$OUT/results.tsv" "$PREV"; log "saved baseline $PREV"; fi
  log "wrote $OUT/results.md ($(wc -l < "$OUT/results.tsv" | tr -d ' ') rows)"
}

# Replaces the body of the ledger issue with results.md; the previous body goes
# into a comment first. Runs only when asked for explicitly.
publish() {
  [ -s "$OUT/results.md" ] || { log "no results.md"; return 1; }
  [ -n "$ISSUE" ] && [ -n "$ISSUE_REPO" ] || { log "LEDGER_PUBLISH needs LEDGER_ISSUE and LEDGER_ISSUE_REPO"; return 1; }
  local old=$OUT/issue-body-before.md
  gh issue view "$ISSUE" -R "$ISSUE_REPO" --json body -q .body > "$old" || return 1
  { echo "Table replaced on $(date +%Y-%m-%d); the previous body:"; echo; cat "$old"; } > "$OUT/issue-comment.md"
  gh issue comment "$ISSUE" -R "$ISSUE_REPO" --body-file "$OUT/issue-comment.md" || return 1
  gh issue edit "$ISSUE" -R "$ISSUE_REPO" --body-file "$OUT/results.md" || return 1
  log "issue $ISSUE body replaced"
}

case ${1:-all} in
  setup) setup;;
  measure) measure;;
  render) render;;
  publish) publish;;
  all) setup && measure && render && { [ "${LEDGER_PUBLISH:-0}" != 1 ] || publish; };;
  *) echo "usage: $0 [setup|measure|render|publish|all]" >&2; exit 2;;
esac
