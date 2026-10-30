#!/bin/bash
# Alternate GC settings on one machine: Coalton kernels, cl-bench rows, the whole
# cl-bench suite, and the large-heap pause (gcpause.lisp).
#
#   bench/jit-tiering/gc-matrix.sh            # measure
#   bench/jit-tiering/gc-matrix.sh summarize  # medians and per-round ratio ranges
#
# Every sample is one fresh process. Settings alternate within each item of each
# round. Load averages before and after each sample are recorded (mac only).
#
# Environment:
#   GM_RUNTIME   runtime executable          GM_CORE   core file
#   GM_WORK      directory with cl-bench/ and coalton/
#   GM_OUT       result directory            GM_MACHINE  label in the result lines
#   GM_ROUNDS    rounds (default 3)          GM_ITEMS    extended regex on item names
#   GM_REGISTRY  CL_SOURCE_REGISTRY value for the Coalton items
#   GM_LOCKCMD   prefix that runs a script under a machine lock; the script path
#                is appended (default: bash)
#   GM_SCRIPT_PATH  how the lock command sees the script path (default: same)
#   GM_FIRST_ROUND first round number (default 1)   GM_GAP seconds to wait after each sample
#   GM_VARIANTS  "name=ENV=VAL ..." (default: default / gen0 / conc0)
set -u
here=$(cd "$(dirname "$0")" && pwd)
TREE=$(cd "$here/../.." && pwd)
RUNTIME=${GM_RUNTIME:?}
CORE=${GM_CORE:?}
WORK=${GM_WORK:?}
OUT=${GM_OUT:?}
MACHINE=${GM_MACHINE:?}
ROUNDS=${GM_ROUNDS:-3}
ONLY=${GM_ITEMS:-}
LOCKCMD=${GM_LOCKCMD:-bash}
VARIANTS=${GM_VARIANTS:-"default= gen0=DOTNET_GCgen0size=0x4000000 conc0=DOTNET_gcConcurrent=0"}
mkdir -p "$OUT"
RAW=$OUT/raw.tsv
UNIT=$OUT/unit.sh
UNITARG=${GM_SCRIPT_PATH:-$UNIT}
CBFILE=$TREE/bench/cl-bench-runner/run.lisp
KERNELS=list-ops,ord-sort,tree-insert-fold

ITEMS=(
  "coalton" "gcfull" "gcstep"
  "cold bitvectors" "cold browse" "cold fib" "cold hash-strings" "cold SUITE"
  "steady bitvectors" "steady browse" "steady fib" "steady hash-strings" "steady SUITE"
)

load() { command -v uptime > /dev/null || return 0; uptime | sed 's/.*load averages*: *//; s/,//g' | awk '{print $1}'; }

# run_unit ENV... -- CMD... : leaves stdout in $OUT/last.out, stderr in last.err
run_unit() {
  {
    echo '#!/bin/bash'
    echo "cd $(printf %q "$WORK")"
    while [ "$1" != "--" ]; do printf 'export %q\n' "$1"; shift; done
    shift
    printf 'exec'; printf ' %q' "$@"; printf ' < /dev/null > %q 2> %q\n' "$OUT/last.out" "$OUT/last.err"
  } > "$UNIT"
  chmod +x "$UNIT"
  : > "$OUT/last.out"; : > "$OUT/last.err"
  if [ "$LOCKCMD" = bash ]; then bash "$UNIT"; else $LOCKCMD "$UNITARG"; fi
  tr -d '\r' < "$OUT/last.out" > "$OUT/last.o2"; tr -d '\r' < "$OUT/last.err" > "$OUT/last.e2"
}

parse() {  # parse KIND ARG -> prints value(s) "label<TAB>ms"
  local kind=$1 arg=$2
  case $kind in
    coalton) awk '$1=="RESULT"{print $2 "\t" $3}' "$OUT/last.o2";;
    gcfull) awk '$1=="FULLGC"{print "gcfull\t" $2}' "$OUT/last.o2";;
    gcstep) awk '$1=="MAXSTEP"{print "gcstep\t" $2}' "$OUT/last.o2";;
    cold)
      if [ "$arg" = SUITE ]; then
        awk '{ s=$0; sub(/^ +/,"",s); if (s !~ /^"[^"]+": [0-9.]+,?$/ || s ~ /\/consed"/) next
               split(s,a,"\": "); v=a[2]; sub(/,$/,"",v); t+=v; n++ }
             END { if (n) printf "suite\t%.1f\n", t*1000 }' "$OUT/last.e2"
      else
        awk -v key="\"$arg\": " '{ s=$0; sub(/^ +/,"",s); if (index(s,key)==1) { v=substr(s,length(key)+1); gsub(/[\[\], ]+$/,"",v); print arg "\t" v*1000 } }' arg="$arg" "$OUT/last.e2" | head -1
      fi;;
    steady)
      if [ "$arg" = SUITE ]; then
        awk '$1=="STEADY"{t+=$3; n++} END{if(n) printf "suite\t%.1f\n", t*1000}' "$OUT/last.o2"
      else
        awk -v k="$arg" '$1=="STEADY" && $2==k {print k "\t" $3*1000}' "$OUT/last.o2" | head -1
      fi;;
  esac
}

measure() {
  local round item v name venv kind arg lb la envs cmd l label val
  for round in $(seq "${GM_FIRST_ROUND:-1}" "$ROUNDS"); do
    for item in "${ITEMS[@]}"; do
      if [ -n "$ONLY" ] && ! echo "$item" | grep -Eq "$ONLY"; then continue; fi
      kind=${item%% *}; arg=${item#* }; [ "$arg" = "$item" ] && arg=
      for v in $VARIANTS; do
        name=${v%%=*}; venv=${v#*=}
        envs=(DOTCL_NO_JIT_PROFILE=1 DOTNET_gcServer=0)
        [ -n "$venv" ] && envs+=("$venv")
        local base=("$RUNTIME" --core "$CORE")
        case $kind in
          coalton)
            envs+=(COALTON_ENV=release "XDG_CACHE_HOME=$WORK/xdg" "COALTON_BENCH_OUT=$WORK/fasl/" "COALTON_BENCH_ONLY=$KERNELS")
            [ -n "${GM_REGISTRY:-}" ] && envs+=("CL_SOURCE_REGISTRY=$GM_REGISTRY")
            cmd=("${base[@]}" "$TREE/bench/coalton/run.lisp");;
          gcfull|gcstep) cmd=("${base[@]}" "$here/gcpause.lisp" --eval '(quit)');;
          cold)
            if [ "$arg" = SUITE ]; then cmd=("${base[@]}" "$CBFILE")
            else cmd=("${base[@]}" --eval "(setq *bench-name* \"$arg\")" "$CBFILE"); fi;;
          steady)
            envs+=(LEDGER_STEADY=1 LEDGER_STEADY_TOLERANCE=0.05 LEDGER_STEADY_WINDOW=3 LEDGER_STEADY_MIN_RUNS=5 LEDGER_STEADY_MIN_SECONDS=0 LEDGER_STEADY_MAX_RUNS=12 LEDGER_STEADY_MAX_SECONDS=30)
            if [ "$arg" = SUITE ]; then
              envs+=(LEDGER_STEADY_MIN_RUNS=3 LEDGER_STEADY_MAX_SECONDS=15)
              cmd=("${base[@]}" "$CBFILE")
            else cmd=("${base[@]}" --eval "(setq *bench-name* \"$arg\")" "$CBFILE"); fi;;
        esac
        lb=$(load)
        run_unit "${envs[@]}" -- "${cmd[@]}"
        la=$(load)
        [ -n "${GM_GAP:-}" ] && sleep "$GM_GAP"
        val=$(parse "$kind" "$arg")
        if [ -z "$val" ]; then val="${arg:-$kind}	failed"; fi
        echo "$val" | while IFS=$'\t' read -r label l; do
          printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$MACHINE" "$round" "$name" "${kind}:${label}" "$l" "$lb" "$la" "$(date +%H:%M:%S)" >> "$RAW"
        done
      done
    done
  done
}

summarize() {
  # per item: median and min per variant, and ratio (variant/default) per round: median, min, max
  awk -F'\t' -v maxload="${GM_MAXLOAD:-1e9}" '
    $5 == "failed" || $6 > maxload || $7 > maxload { next }
    { key = $4; var = $3; r = $2
      v[key, var, r] = $5; items[key] = 1; vars[var] = 1; rounds[r] = 1
      cnt[key, var]++; val[key, var, cnt[key, var]] = $5 }
    function med(key, var,   n, i, j, a, t) {
      n = cnt[key, var]; if (!n) return "-"
      for (i = 1; i <= n; i++) a[i] = val[key, var, i] + 0
      for (i = 1; i <= n; i++) for (j = i + 1; j <= n; j++) if (a[j] < a[i]) { t = a[i]; a[i] = a[j]; a[j] = t }
      return (n % 2) ? a[(n + 1) / 2] : (a[n / 2] + a[n / 2 + 1]) / 2
    }
    END {
      for (k in items) {
        d = med(k, "default")
        printf "%s\tdefault_med=%.1f", k, d
        for (x in vars) { if (x == "default") continue
          m = med(k, x); n = 0; lo = 1e9; hi = 0; rs = ""
          for (r in rounds) if (((k, "default", r) in v) && ((k, x, r) in v)) {
            q = v[k, x, r] / v[k, "default", r]; n++; if (q < lo) lo = q; if (q > hi) hi = q; rs = rs sprintf(" %.2f", q) }
          printf "\t%s_med=%.1f ratio_med=%.2f range=%.2f-%.2f n=%d [%s ]", x, m, (d ? m / d : 0), lo, hi, n, rs
        }
        printf "\n"
      }
    }' "$RAW" | sort
}

case ${1:-all} in
  summarize) summarize;;
  *) measure; summarize;;
esac
