#!/bin/bash
# Alternate tiered-JIT settings on one machine and record the times.
#
#   bench/jit-tiering/run.sh                 # measure, then summarize
#   bench/jit-tiering/run.sh summarize
#
# Every sample is one fresh process under an exclusive flock. The settings are
# alternated within each round, so slow drift of the machine hits all of them.
#
# Environment:
#   JT_TREE      dotcl tree (default: this tree)
#   JT_RUNTIME   runtime executable (default: $JT_TREE/runtime/bin/Release/net10.0/runtime)
#   JT_CORE      core file (default: $JT_TREE/compiler/dotcl.core)
#   JT_WORK      directory holding the cl-bench checkout as cl-bench/ (default: $JT_TREE/out/ledger)
#   JT_OUT       result directory (default: $JT_TREE/out/jit-tiering)
#   JT_LAYOUT    label written into the result lines (default: il)
#   JT_ROUNDS    rounds (default 5)
#   JT_VARIANTS  "name=ENV=VAL[,ENV=VAL] ..." (default: the three below)
#   JT_ITEMS     extended regex; only matching items are measured
#   JT_LOCK      lock file (default ~/measure.lock, empty = none)
set -u
here=$(cd "$(dirname "$0")" && pwd)
TREE=${JT_TREE:-$(cd "$here/../.." && pwd)}
RUNTIME=${JT_RUNTIME:-$TREE/runtime/bin/Release/net10.0/runtime}
CORE=${JT_CORE:-$TREE/compiler/dotcl.core}
WORK=${JT_WORK:-$TREE/out/ledger}
CLB=$WORK/cl-bench
OUT=${JT_OUT:-$TREE/out/jit-tiering}
LAYOUT=${JT_LAYOUT:-il}
ROUNDS=${JT_ROUNDS:-5}
LOCK=${JT_LOCK-$HOME/measure.lock}
ONLY=${JT_ITEMS:-}
VARIANTS=${JT_VARIANTS:-"default= qjl0=DOTNET_TC_QuickJitForLoops=0 qj0=DOTNET_TC_QuickJit=0 gcc0=DOTNET_gcConcurrent=0"}
mkdir -p "$OUT"
RAW=$OUT/raw.tsv

ITEMS=(
  "startup"
  "repl"
  "compile-file"
  "gcfull"
  "gcstep"
  "ql-ppcre"
  "ql-alexandria"
  "cold ctak" "cold fft" "cold frpoly/bignum" "cold browse" "cold mrg32k3a" "cold frpoly/fixnum"
  "steady ctak" "steady fft" "steady frpoly/bignum" "steady browse" "steady mrg32k3a" "steady frpoly/fixnum"
)

timed() {  # timed STDIN OUTFILE ERRFILE ENV... -- CMD... ; prints wall ms
  local in=$1 so=$2 se=$3; shift 3
  local envs=()
  while [ "$1" != "--" ]; do envs+=("$1"); shift; done
  shift
  local pre=()
  [ -n "$LOCK" ] && pre=(flock -x "$LOCK")
  IN="$in" SO="$so" SE="$se" "${pre[@]}" bash -c 'a=$(date +%s%N); env "$@" < "$IN" > "$SO" 2> "$SE"; rc=$?; b=$(date +%s%N); echo $(( (b - a) / 1000000 )) $rc' \
    _ "${envs[@]}" "$@" 2>/dev/null
}

measure() {
  local round item v name venv kind arg so se res wall rc val
  : > "$OUT/.start"
  for round in $(seq 1 "$ROUNDS"); do
    for item in "${ITEMS[@]}"; do
      if [ -n "$ONLY" ] && ! echo "$item" | grep -Eq "$ONLY"; then continue; fi
      kind=${item%% *}; arg=${item#* }; [ "$arg" = "$item" ] && arg=
      for v in $VARIANTS; do
        name=${v%%=*}; venv=${v#*=}
        so=$OUT/last.out; se=$OUT/last.err
        local envs=(DOTCL_NO_JIT_PROFILE=1 DOTNET_gcServer=0)
        [ -n "$venv" ] && envs+=(${venv//,/ })
        local base=("$RUNTIME" --core "$CORE") in=/dev/null cmd=()
        case $kind in
          startup) cmd=("${base[@]}" --eval '(quit)');;
          gcfull|gcstep) cmd=("${base[@]}" "$here/gcpause.lisp" --eval '(quit)');;
          repl) in=$OUT/repl-input; printf '(+ 1 2)\n(quit)\n' > "$in"; cmd=("${base[@]}");;
          compile-file)
            cmd=("${base[@]}" --eval '(defpackage :cl-bench.gabriel (:use :common-lisp))' --eval "(compile-file \"$CLB/files/gabriel.lisp\" :output-file \"$OUT/gabriel-$name.fasl\")" --eval '(quit)');;
          ql-*)
            local xdg=$OUT/xdg-$name-$round-${kind#ql-}
            mkdir -p "$xdg"
            envs+=("XDG_CACHE_HOME=$xdg")
            local sys=${kind#ql-}; [ "$sys" = ppcre ] && sys=cl-ppcre
            cmd=("${base[@]}" --eval '(require "quicklisp")' --eval "(ql:quickload \"$sys\" :silent t)" --eval '(quit)');;
          cold) cmd=("${base[@]}" --eval "(setq *bench-name* \"$arg\")" "$TREE/bench/cl-bench-runner/run.lisp");;
          steady) envs+=(LEDGER_STEADY=1 LEDGER_STEADY_TOLERANCE=0.05 LEDGER_STEADY_WINDOW=3 LEDGER_STEADY_MIN_RUNS=10 LEDGER_STEADY_MIN_SECONDS=0 LEDGER_STEADY_MAX_RUNS=20 LEDGER_STEADY_MAX_SECONDS=120)
                  cmd=("${base[@]}" --eval "(setq *bench-name* \"$arg\")" "$TREE/bench/cl-bench-runner/run.lisp");;
        esac
        res=$( (cd "$WORK" && timed "$in" "$so" "$se" "${envs[@]}" -- "${cmd[@]}") )
        wall=${res% *}; rc=${res#* }
        val=$wall
        case $kind in
          cold) val=$(awk -v key="\"$arg\": " '{ s=$0; sub(/^ +/,"",s); if (index(s,key)==1) { v=substr(s,length(key)+1); gsub(/[\[\], ]+$/,"",v); print v*1000 } }' "$se" | head -1);;
          gcfull) val=$(awk '$1=="FULLGC"{print $2}' "$so" | head -1);;
          gcstep) val=$(awk '$1=="MAXSTEP"{print $2}' "$so" | head -1);;
          steady) val=$(awk -v k="$arg" '$1=="STEADY" && $2==k { print $3*1000 }' "$so" | head -1);;
        esac
        printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$LAYOUT" "$round" "$name" "${item// /:}" "${val:-failed}" "$wall" "${rc:-?}" >> "$RAW"
      done
    done
  done
}

summarize() {
  awk -F'\t' '
    $5 != "failed" { k = $1 "\t" $4 "\t" $3
      n[k]++; v[k, n[k]] = $5; if (!(k in mn) || $5 + 0 < mn[k] + 0) mn[k] = $5 }
    END {
      for (k in n) {
        m = n[k]; for (i = 1; i <= m; i++) a[i] = v[k, i] + 0
        for (i = 1; i <= m; i++) for (j = i + 1; j <= m; j++) if (a[j] < a[i]) { t = a[i]; a[i] = a[j]; a[j] = t }
        med = (m % 2) ? a[(m + 1) / 2] : (a[m / 2] + a[m / 2 + 1]) / 2
        printf "%s\t%d\t%.1f\t%.1f\n", k, m, mn[k], med
      }
    }' "$RAW" | sort
}

case ${1:-all} in
  summarize) summarize;;
  *) measure; summarize;;
esac
