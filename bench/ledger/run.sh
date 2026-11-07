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
#   raw.tsv          one line per value: metric mode side round value unit note runs
#   results.tsv      metric, cold dotcl, cold SBCL, cold ratio, steady dotcl,
#                    steady SBCL, steady ratio, runs, mark, unit, how, note
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
#   LEDGER_STARTUP_COUNT  fresh processes per startup measurement, median taken (default 10)
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
#   LEDGER_COLD_MAX       cold ratio that gets a mark (default 10)
#   LEDGER_STEADY_MAX     steady ratio that gets a mark (default 2)
#   LEDGER_STEADY_STRONG  steady ratio that gets the strong mark (default 3)
#   LEDGER_STEADY_TOLERANCE  consecutive-run spread considered stable (default 0.05)
#   LEDGER_STEADY_WINDOW     consecutive stable runs required (default 3)
#   LEDGER_STEADY_MIN_RUNS   runs before a stable window is accepted (default 10)
#   LEDGER_STEADY_MIN_SECONDS  seconds before a stable window may start (default 3)
#   LEDGER_STEADY_MAX_RUNS   runs after the minimum time before stopping (default 20)
#   LEDGER_STEADY_MAX_SECONDS  cap on steady seconds per benchmark (default 120)
set -u

here=$(cd "$(dirname "$0")" && pwd)
TREE=${LEDGER_TREE:-$(cd "$here/../.." && pwd)}
WORK=${LEDGER_WORK:-$TREE/out/ledger}
OUT=${LEDGER_OUT:-$WORK/out}
ROUNDS=${LEDGER_ROUNDS:-3}
STARTUP_COUNT=${LEDGER_STARTUP_COUNT:-10}
COLD_MAX=${LEDGER_COLD_MAX:-10}
STEADY_MAX=${LEDGER_STEADY_MAX:-2}
STEADY_STRONG=${LEDGER_STEADY_STRONG:-3}
STEADY_TOLERANCE=${LEDGER_STEADY_TOLERANCE:-0.05}
STEADY_WINDOW=${LEDGER_STEADY_WINDOW:-3}
STEADY_MIN_RUNS=${LEDGER_STEADY_MIN_RUNS:-10}
STEADY_MIN_SECONDS=${LEDGER_STEADY_MIN_SECONDS:-3}
STEADY_MAX_RUNS=${LEDGER_STEADY_MAX_RUNS:-20}
STEADY_MAX_SECONDS=${LEDGER_STEADY_MAX_SECONDS:-120}
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
    [ "$kind" = startup ] && ARGV+=(--no-init)
    for e in ${evals[@]+"${evals[@]}"}; do ARGV+=(--eval "$e"); done
    ARGV+=("$file")
  elif [ "$kind" = startup ]; then
    ARGV=("${SBCL_DIRECT[@]}")
    [ -n "${SBCL_DIRECT_CORE:-}" ] && ARGV+=(--core "$SBCL_DIRECT_CORE")
    ARGV+=(--non-interactive --no-userinit --load "$file")
  else
    read -ra ARGV <<< "$SBCL_CMD"
    ARGV+=(--non-interactive)
    [ "$kind" = coalton ] || ARGV+=(--no-userinit)
    for e in ${evals[@]+"${evals[@]}"}; do ARGV+=(--eval "$e"); done
    ARGV+=(--load "$file")
    case $kind in clbench*) ARGV+=(--eval '(quit)');; esac
  fi
}

# The SBCL binary that ros starts, found without ros so that a startup measurement does not
# include ros's own launch: the newest <roswell home>/impls/*/*/sbcl-bin/<version>/bin/sbcl
# (the version comes from the SBCL_CMD banner, else any), then sbcl on PATH. Prints the
# binary and, when it sits in a roswell tree, its core as two lines; prints nothing if none.
sbcl_direct() {
  local ver root bin d
  ver=$(cd "$WORK" && $SBCL_CMD --version 2>/dev/null </dev/null | head -1 | sed -E 's/^SBCL ([0-9][0-9.]*[0-9]).*/\1/')
  for root in "${LEDGER_ROSWELL_HOME:-$HOME/.roswell}"; do
    for d in $(ls -d "$root"/impls/*/*/sbcl-bin/${ver:-*} 2>/dev/null | sort -r -V); do
      if [ -x "$d/bin/sbcl" ]; then
        echo "$d/bin/sbcl"
        [ -f "$d/lib/sbcl/sbcl.core" ] && echo "$d/lib/sbcl/sbcl.core"
        return 0
      fi
    done
  done
  bin=$(command -v sbcl) && echo "$bin"
  return 0
}

# "a,b" (benchmarks that need an earlier one to define their classes) -> '("a" "b")
bench_names() {
  case $1 in
    *,*) printf "'(%s)" "$(echo "$1" | sed 's/[^,]*/"&"/g; s/,/ /g')";;
    *) printf '"%s"' "$1";;
  esac
}

# run_one KIND ARG SIDE TIMEOUT STDOUT STDERR MODE : sets RC
run_one() {
  local kind=$1 arg=$2 side=$3 to=$4 so=$5 se=$6 mode=${7:-cold}
  local envs=(DOTCL_NO_JIT_PROFILE=1 DOTNET_gcServer=0
              "LEDGER_STEADY_TOLERANCE=$STEADY_TOLERANCE"
              "LEDGER_STEADY_WINDOW=$STEADY_WINDOW"
              "LEDGER_STEADY_MIN_RUNS=$STEADY_MIN_RUNS"
              "LEDGER_STEADY_MIN_SECONDS=$STEADY_MIN_SECONDS"
              "LEDGER_STEADY_MAX_RUNS=$STEADY_MAX_RUNS"
              "LEDGER_STEADY_MAX_SECONDS=$STEADY_MAX_SECONDS")
  local file evals=() dir=$WORK wrap=()
  [ "$mode" = steady ] && envs+=(LEDGER_STEADY=1)
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
    startup)
      # median wall time of STARTUP_COUNT fresh processes running a tiny script
      file=$here/startup-$arg.lisp
      [ "$arg" = hello ] && envs+=(STARTUP_EXPECT=hello)
      wrap=("$RUNTIME" --core "$CORE" --no-init "$here/startup-time.lisp" "$STARTUP_COUNT");;
    *) echo "unknown kind $kind" >&2; RC=99; return;;
  esac
  build_argv "$side" "$kind" "$file" ${evals[@]+"${evals[@]}"}
  ARGV=(${wrap[@]+"${wrap[@]}"} "${ARGV[@]}")
  # GNU timeout is coreutils' gtimeout on macOS, and absent on a stock runner: without
  # either, run unbounded rather than failing every item with exit 127.
  local cmd
  if command -v timeout >/dev/null 2>&1; then cmd=(timeout -k 10 "$to" env "${envs[@]}" "${ARGV[@]}")
  elif command -v gtimeout >/dev/null 2>&1; then cmd=(gtimeout -k 10 "$to" env "${envs[@]}" "${ARGV[@]}")
  else
    echo "ledger: no timeout or gtimeout on PATH; running $kind without a time limit" >&2
    cmd=(env "${envs[@]}" "${ARGV[@]}")
  fi
  if [ -n "$LOCK" ]; then
    (cd "$dir" && flock "$LOCK" "${cmd[@]}") < /dev/null > "$so" 2> "$se"
  else
    (cd "$dir" && "${cmd[@]}") < /dev/null > "$so" 2> "$se"
  fi
  RC=$?
}

# parse KIND ARG ID SO SE MODE : prints "metric<TAB>value<TAB>unit<TAB>note<TAB>runs"
parse() {
  local kind=$1 arg=$2 id=$3 so=$4 se=$5 mode=${6:-cold}
  if [ "$mode" = steady ]; then
    case $kind in
      clbench|clbench-warm)
        awk -v key="${arg##*,}" -v id="$id" '
          $1 == "STEADY" && $2 == key {
            printf "%s\t%.1f\tms\t%s\t%s\n", id, $3 * 1000,
                   ($5 == "stable" ? "" : "steady: not stable"), $4 }' "$so";;
      clbench-suite)
        awk '
          $1 == "STEADY" {
            printf "cl-bench-suite/%s\t%.1f\tms\t%s\t%s\n", $2, $3 * 1000,
                   ($5 == "stable" ? "" : "steady: not stable"), $4 }' "$so";;
      coalton)
        awk -v k="$arg" -v id="$id" '
          $1 == "STEADY" && $2 == k {
            printf "%s\t%s\tms\t%s%s\t%s\n", id, $3, $6,
                   ($5 == "stable" ? "" : " steady: not stable"), $4 }' "$so";;
      parity)
        awk -v id="$id" '
          $1 == "STEADY" && $2 ~ /^[a-z][a-z-]*$/ && $3 ~ /^[0-9.]+$/ {
            printf "%s/%s\t%s\tms\t%s\t%s\n", id, $2, $3,
                   ($5 == "stable" ? "" : "steady: not stable"), $4 }' "$so";;
      ironclad-cold|ironclad-warm)
        awk -v id="$id" '
          $1 == "TIME" && $5 == "ok" {
            printf "%s/%s/%s\t%.1f\tms\t%s\t%s\n", id, $2, $3, $4 * 1000,
                   ($8 == "stable" ? "" : "steady: not stable"), $7 }' "$so";;
      imago)
        awk -v id="$id" '
          $1 == "BIC2" {
            printf "%s\t%s\tns\t%s\t%s\n", id, $2,
                   ($8 == "stable" ? "" : "steady: not stable"), $7 }' "$so";;
    esac
  else
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
      startup)
        awk -v id="$id" '$1 == "STARTUP" { printf "%s\t%s\tms\t\n", id, $2 }' "$so";;
    esac
  fi
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
  SBCL_DIRECT=(); SBCL_DIRECT_CORE=
  { read -r sb; read -r SBCL_DIRECT_CORE; } < <(sbcl_direct)
  [ -n "${sb:-}" ] && SBCL_DIRECT=("$sb")
  log "startup SBCL: ${SBCL_DIRECT[*]:-none}${SBCL_DIRECT_CORE:+ --core $SBCL_DIRECT_CORE}"
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
    if [ "$kind" = startup ] && [ ${#SBCL_DIRECT[@]} = 0 ]; then
      log "$id: no sbcl binary found without ros; recording dotcl only"
      failed_sbcl=1
    fi
    for round in $(seq 1 "$ROUNDS"); do
      for side in dotcl sbcl; do
        eval "failed=\$failed_$side"
        [ "$failed" = 1 ] && continue
        so=$OUT/last-$side.out; se=$OUT/last-$side.err
        log "$id $side round $round"
        run_one "$kind" "$arg" "$side" "$to" "$so" "$se" cold
        parse "$kind" "$arg" "$id" "$so" "$se" cold > "$OUT/last.parsed"
        n=$(wc -l < "$OUT/last.parsed" | tr -d ' ')
        awk -F'\t' -v m=cold -v s="$side" -v r="$round" \
          '{ printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", $1, m, s, r, $2, $3, $4, $5 }' \
          "$OUT/last.parsed" >> "$OUT/raw.tsv"
        if [ "$RC" != 0 ] || [ "$n" = 0 ]; then
          printf '%s\t%s\t%s\t%s\t\t\tFAIL exit=%s values=%s\t\n' "$id" cold "$side" "$round" "$RC" "$n" >> "$OUT/raw.tsv"
          mkdir -p "$OUT/fail"
          cp "$so" "$OUT/fail/$(echo "$id" | tr '/+' '__')-$side.out" 2>/dev/null
          cp "$se" "$OUT/fail/$(echo "$id" | tr '/+' '__')-$side.err" 2>/dev/null
          log "$id $side: failed (exit $RC, $n values)"
          [ "$n" = 0 ] && eval "failed_$side=1"
        fi
      done
    done
    # startup has no steady state: the steady columns stay empty
    [ "$kind" = startup ] && continue
    for side in dotcl sbcl; do
      so=$OUT/steady-$side.out; se=$OUT/steady-$side.err
      log "$id $side steady"
      run_one "$kind" "$arg" "$side" "$to" "$so" "$se" steady
      parse "$kind" "$arg" "$id" "$so" "$se" steady > "$OUT/last.parsed"
      n=$(wc -l < "$OUT/last.parsed" | tr -d ' ')
      awk -F'\t' -v m=steady -v s="$side" -v r=- \
        '{ printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", $1, m, s, r, $2, $3, $4, $5 }' \
        "$OUT/last.parsed" >> "$OUT/raw.tsv"
      if [ "$RC" != 0 ] || [ "$n" = 0 ]; then
        printf '%s\t%s\t%s\t%s\t\t\tFAIL exit=%s values=%s\t\n' "$id" steady "$side" - "$RC" "$n" >> "$OUT/raw.tsv"
        mkdir -p "$OUT/fail"
        cp "$so" "$OUT/fail/$(echo "$id" | tr '/+' '__')-$side-steady.out" 2>/dev/null
        cp "$se" "$OUT/fail/$(echo "$id" | tr '/+' '__')-$side-steady.err" 2>/dev/null
        log "$id $side steady: failed (exit $RC, $n values)"
      fi
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
  awk -F'\t' -v OFS='\t' -v regfile="$OUT/regressions.tsv" -v detailfile="$OUT/results-detail.tsv" \
    -v thr="$REG_PCT" -v coldmax="$COLD_MAX" -v steadymax="$STEADY_MAX" -v strong="$STEADY_STRONG" '
    function how(m) {
      if (m ~ /^cl-bench-warm\//) return "cl-bench: existing warm form in the cold column; steady repeats in-process until stable"
      if (m ~ /^cl-bench-suite\//) return "cl-bench whole suite in one process; steady repeats each benchmark until stable"
      if (m ~ /^cl-bench\//) return "cl-bench cold (1 scalar run, JIT included); steady repeats until stable"
      if (m ~ /^coalton\//) return "Coalton kernel cold (1 warm-up + 5 runs, min); steady repeats until stable"
      if (m ~ /^parity\//) return "csharp-parity kernel cold (1 warm-up + 5 runs, min); steady repeats until stable"
      if (m ~ /^ironclad-cold\//) return "ironclad rt suite cold (1 rep per test); steady repeats each test until stable; sum of tests"
      if (m ~ /^ironclad-warm\//) return "ironclad rt suite warm (min of 4 reps per test); steady repeats each test until stable; sum of tests"
      if (m ~ /^startup\//) return "wall time of a fresh process running a tiny script (median of several); no steady state; the ledger runs a non-ReadyToRun build, so startup is larger than the shipped tool; only the trend matters; the cold ratio is not marked"
      if (m ~ /^imago\//) return "imago bicubic cold (min of 5, warm); steady repeats until stable; ns per pixel"
      return ""
    }
    function noteadd(a, b) {
      if (a == "") return b
      if (b == "") return a
      return a "; " b
    }
    function runfield(m) {
      d = sruns[m, "dotcl"]; s = sruns[m, "sbcl"]
      if (d == "" && s == "") return "-"
      if (d == s) return d
      return d "/" s
    }
    FNR == 1 { fi++ }
    fi == 1 { hnote[$1] = $2; next }
    fi == 2 { if ($4 ~ /^[0-9.]+$/) pratio[$1] = $4; next }
    fi == 3 {
      m = $1; mode = $2; side = $3; v = $5; u = $6; nt = $7; r = $8
      if (nt ~ /^FAIL/) {
        fail[m, side] = 1
        failnote[m] = noteadd(failnote[m], side ":" nt)
        seen[m] = 1
        next
      }
      if (v == "") next
      k = m SUBSEP side
      if (mode == "cold") {
        if (!(k in cbest) || v + 0 < cbest[k] + 0) cbest[k] = v + 0
        unit[m] = u
        if (nt != "") cnote[m, side] = nt
      } else if (mode == "steady") {
        if (!(k in sbest) || v + 0 < sbest[k] + 0) sbest[k] = v + 0
        sruns[m, side] = r
        sunit[m] = u
        if (nt != "") snote[m, side] = nt
      }
      seen[m] = 1
    }
    END {
      for (m in seen) {
        d = ((m SUBSEP "dotcl") in cbest) ? cbest[m, "dotcl"] : ""
        s = ((m SUBSEP "sbcl") in cbest) ? cbest[m, "sbcl"] : ""
        sd = ((m SUBSEP "dotcl") in sbest) ? sbest[m, "dotcl"] : ""
        ss = ((m SUBSEP "sbcl") in sbest) ? sbest[m, "sbcl"] : ""
        if (m ~ /^ironclad-(cold|warm)\/[^\/]+\/./) {
          split(m, p, "/")
          detail[m] = d OFS s OFS sd OFS ss
          cat = p[1] "/" p[2]
          ctotal[cat]++; ctotal[p[1] "/TOTAL"]++
          if (d != "" && s != "") {
            cd[cat] += d; cs[cat] += s; cseen[cat] = 1
            td[p[1]] += d; ts[p[1]] += s; cseen[p[1] "/TOTAL"] = 1
          }
          if (sd != "" && ss != "") {
            scd[cat] += sd; scs[cat] += ss; sseen[cat] = 1
            std[p[1]] += sd; sts[p[1]] += ss; sseen[p[1] "/TOTAL"] = 1
            csteady[cat]++; csteady[p[1] "/TOTAL"]++
          }
          if (sruns[m, "dotcl"] != "" && sruns[m, "dotcl"] + 0 > rd[cat]) rd[cat] = sruns[m, "dotcl"] + 0
          if (sruns[m, "sbcl"] != "" && sruns[m, "sbcl"] + 0 > rs[cat]) rs[cat] = sruns[m, "sbcl"] + 0
          if (sruns[m, "dotcl"] != "" && sruns[m, "dotcl"] + 0 > rd[p[1] "/TOTAL"]) rd[p[1] "/TOTAL"] = sruns[m, "dotcl"] + 0
          if (sruns[m, "sbcl"] != "" && sruns[m, "sbcl"] + 0 > rs[p[1] "/TOTAL"]) rs[p[1] "/TOTAL"] = sruns[m, "sbcl"] + 0
          continue
        }
        row[m] = 1
        coldd[m] = d; colds[m] = s; steadyd[m] = sd; steadys[m] = ss
        if (unit[m] == "") unit[m] = sunit[m]
      }
      for (c in cseen) {
        if (c ~ /\/TOTAL$/) { split(c, q, "/"); coldd[c] = td[q[1]]; colds[c] = ts[q[1]] }
        else { coldd[c] = cd[c]; colds[c] = cs[c] }
        unit[c] = "ms"; row[c] = 1
      }
      for (c in sseen) {
        if (c ~ /\/TOTAL$/) { split(c, q, "/"); steadyd[c] = std[q[1]]; steadys[c] = sts[q[1]] }
        else { steadyd[c] = scd[c]; steadys[c] = scs[c] }
        unit[c] = "ms"; row[c] = 1
      }
      for (m in row) {
        d = coldd[m]; s = colds[m]; sd = steadyd[m]; ss = steadys[m]
        cr = (d != "" && s != "" && s + 0 > 0) ? sprintf("%.2f", d / s) : "-"
        sr = (sd != "" && ss != "" && ss + 0 > 0) ? sprintf("%.2f", sd / ss) : "-"
        mark = ""
        if (m ~ /^startup\//) mark = ""
        else if (sr != "-" && sr + 0 > strong) mark = "!!"
        else if ((cr != "-" && cr + 0 > coldmax) || (sr != "-" && sr + 0 > steadymax)) mark = "!"
        note = ""
        split(m, g, "/"); hn = (m in hnote) ? hnote[m] : ((g[1] in hnote) ? hnote[g[1]] : "")
        if ((m SUBSEP "dotcl") in cnote && (m SUBSEP "sbcl") in cnote && cnote[m, "dotcl"] != cnote[m, "sbcl"]) note = "checksum differs"
        if (((m SUBSEP "dotcl") in snote && snote[m, "dotcl"] ~ /not stable/) || ((m SUBSEP "sbcl") in snote && snote[m, "sbcl"] ~ /not stable/)) note = noteadd(note, "steady: not stable")
        if (m ~ /^ironclad-/ && (m in ctotal) && csteady[m] < ctotal[m]) note = noteadd(note, "steady incomplete")
        if (hn != "") note = noteadd(note, hn)
        if (cr != "-" && (m in pratio) && pratio[m] ~ /^[0-9.]+$/ && cr + 0 > pratio[m] * (1 + thr / 100)) {
          printf "%s\t%s\t%s\t%+.0f%%\n", m, pratio[m], cr, (cr / pratio[m] - 1) * 100 > regfile
        }
        ds = (d == "") ? "failed" : sprintf("%.1f", d)
        ssx = (s == "") ? ((m ~ /^startup\// && !fail[m, "sbcl"]) ? "-" : "failed") : sprintf("%.1f", s)
        sds = (sd == "") ? "failed" : sprintf("%.1f", sd)
        sss = (ss == "") ? "failed" : sprintf("%.1f", ss)
        if (m ~ /^startup\//) { sds = "-"; sss = "-" }
        if (m ~ /^ironclad-/) {
          if (rd[m] == "" && rs[m] == "") runs = "-"
          else if (rd[m] == rs[m]) runs = rd[m]
          else runs = rd[m] "/" rs[m]
        } else runs = runfield(m)
        printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
          m, ds, ssx, cr, sds, sss, sr, runs, mark,
          (unit[m] == "" ? "ms" : unit[m]), how(m), note > "/dev/stdout"
      }
      for (m in detail) print m, detail[m] > detailfile
    }' "$OUT/notes.tsv" "$OUT/prev-in.tsv" "$OUT/raw.tsv" | sort -t$'\t' -k4,4gr -k1,1 > "$OUT/results.body"
  # rows without a ratio (failed) sort first with -g; move them to the end
  { awk -F'\t' '$4 != "-"' "$OUT/results.body"; awk -F'\t' '$4 == "-"' "$OUT/results.body"; } > "$OUT/results.tsv"
  rm -f "$OUT/results.body"
  sort -o "$OUT/results-detail.tsv" "$OUT/results-detail.tsv" 2>/dev/null

  local start end elapsed tol
  start=$(cat "$OUT/start.epoch" 2>/dev/null || echo 0); end=$(cat "$OUT/end.epoch" 2>/dev/null || echo 0)
  elapsed=$(( end - start ))
  tol=$(awk -v t="$STEADY_TOLERANCE" 'BEGIN { printf "%.0f", t * 100 }')
  {
    echo "# dotcl versus SBCL: current values"
    echo
    echo "- tree: $(git -C "$TREE" rev-parse --short=8 HEAD 2>/dev/null) (Release, compiler/dotcl.core, DOTCL_NO_JIT_PROFILE=1, DOTNET_gcServer=0)"
    echo "- SBCL: $(cd "$WORK" && $SBCL_CMD --version 2>/dev/null </dev/null | head -1) (flags: ${SBCL_CMD#*--})"
    echo "- Coalton: $(git -C "$COALTON_DIR" rev-parse --short=8 HEAD 2>/dev/null); cl-bench: $(git -C "$WORK/cl-bench" rev-parse --short=8 HEAD 2>/dev/null)"
    echo "- machine: $(hostname), $(grep -m1 'model name' /proc/cpuinfo 2>/dev/null | sed 's/.*: //')"
    echo "- host: $(nproc 2>/dev/null || sysctl -n hw.logicalcpu) logical CPUs, $(uname -sm), .NET $(dotnet --list-runtimes 2>/dev/null | awk '$1 == "Microsoft.NETCore.App" { v = $2 } END { print v }')"
    echo "- cold: the row's existing fresh-process measurement; dotcl and SBCL alternate $ROUNDS times and the fastest value of each side is shown"
    echo "- steady: repeated in one process until $STEADY_WINDOW consecutive runs are within ${tol}% of each other, after at least $STEADY_MIN_RUNS runs and $STEADY_MIN_SECONDS s (at most $STEADY_MAX_RUNS runs after those seconds or $STEADY_MAX_SECONDS s)"
    echo "- mark: ! means cold is over ${COLD_MAX}x or steady is over ${STEADY_MAX}x; !! means steady is over ${STEADY_STRONG}x"
    echo "- runs = steady runs to stability (dotcl/SBCL when they differ); ratio = dotcl / SBCL; times in ms (imago in ns per pixel)"
    echo "- date: $(date +%Y-%m-%d), wall time of the run $((elapsed / 60)) min (lock waits included)"
    echo "- \"failed\" = the process died or printed no value on that side"
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
    echo "| item | cold dotcl | cold SBCL | cold ratio | steady dotcl | steady SBCL | steady ratio | runs | mark | unit | how | note |"
    echo "|---|---:|---:|---:|---:|---:|---:|---:|---:|---|---|---|"
    awk -F'\t' '{ printf "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n", $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12 }' "$OUT/results.tsv"
    echo
    echo "Per-test lines of the ironclad rows are in results-detail.tsv (metric, cold dotcl, cold SBCL, steady dotcl, steady SBCL)."
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
