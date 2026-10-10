#!/bin/sh
# Count the methods of compiled libraries that RyuJIT gives up optimizing.
#
# RyuJIT compiles a method with MinOpts (no inlining, no register allocation
# across statements, no loop optimization) instead of full optimization when
# the method is too big. The limits are compile-time constants of the JIT
# (src/coreclr/jit/compiler.h, unchanged from .NET 6 to 10), not settings:
# a release runtime ignores DOTNET_JITMinOpts* and runtimeconfig.json, and
# MethodImplOptions.AggressiveOptimization does not lift them. A method
# switches when any of these is exceeded:
#
#   IL bytes                         > 60000
#   IL instructions                  > 20000
#   basic blocks                     > 2000
#   locals (arguments + IL locals + JIT temporaries) > 2000
#   references to locals and args    > 8000
#
# A MinOpts method typically runs 2x or more slower, and nothing reports it:
# the code is correct, only slow. This gate compiles each target system from
# source with the compiler under test, makes the JIT compile every method of
# the resulting fasls (bench/jit-minopts/prepare.lisp), reads the JIT's own
# summary and counts the methods it switched to MinOpts.
#
# Usage:  bench/jit-minopts/run.sh [--accept] [system ...]
#   systems default to the list in baseline.tsv
#   --accept   rewrite baseline.tsv with this run's counts
#
# Only INCREASES fail (exit 1): a new MinOpts method means the compiler started
# emitting a body past one of the limits. Decreases are reported and need no
# permission; refresh the baseline with --accept.
#
# Environment:
#   JIT_MINOPTS_OUT       work directory (default out/jit-minopts below the root)
#   JIT_MINOPTS_TIMEOUT   seconds per system (default 1800)
#   LIBRARY_STATUS_CONFIG Release (default) or Debug runtime, see
#                         bench/library-status/runtime.sh
set -eu

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
baseline="$here/baseline.tsv"
work="${JIT_MINOPTS_OUT:-$root/out/jit-minopts}"
per="${JIT_MINOPTS_TIMEOUT:-1800}"

accept=0
if [ "${1:-}" = "--accept" ]; then accept=1; shift; fi

if [ $# -gt 0 ]; then systems="$*"
else systems=$(awk -F '\t' 'NR > 1 { print $1 }' "$baseline")
fi

. "$root/bench/library-status/runtime.sh"
select_runtime jit-minopts

if command -v timeout >/dev/null 2>&1; then timeout=timeout
elif command -v gtimeout >/dev/null 2>&1; then timeout=gtimeout
else echo "jit-minopts: needs GNU timeout (coreutils; gtimeout on macOS)" >&2; exit 1
fi

mkdir -p "$work"
tab=$(printf '\t')
current="$work/counts.tsv"
printf 'system\tminopts\tmethods\n' > "$current"

for sys in $systems; do
  d="$work/$sys"
  # A fresh cache every time: a fasl left from an earlier run would hide the
  # compiler under test. Only this script's own directory is removed.
  rm -rf "$d"
  mkdir -p "$d/cache"
  echo "jit-minopts: $sys"
  status=0
  JIT_MINOPTS_SYSTEM="$sys" JIT_MINOPTS_OUT="$d/methods.tsv" \
  XDG_CACHE_HOME="$d/cache" DOTCL_NO_JIT_PROFILE=1 \
  DOTNET_TieredCompilation=0 DOTNET_JitStdOutFile="$d/jit.txt" DOTNET_JitDisasmSummary=1 \
    "$timeout" "$per" "$exe" --core "$core" "$here/prepare.lisp" \
      > "$d/log.txt" 2>&1 < /dev/null || status=$?
  if [ "$status" -ne 0 ] || [ ! -s "$d/methods.tsv" ] || [ ! -s "$d/jit.txt" ]; then
    echo "jit-minopts: $sys failed (exit $status), see $d/log.txt" >&2
    exit 2
  fi
  # Join the JIT summary ("JIT compiled Type:Method(sig) [tier, IL size=N, ...")
  # with the fasl method table on (Type:Method, IL bytes). Every MinOpts line of
  # a fasl method is listed in minopts.tsv with the limit it most likely hit:
  # the IL bytes and the locals are known here; anything else is one of the
  # instruction, basic block or reference counts.
  awk -F '\t' -v OFS='\t' '
    FNR == NR { key = $2 "|" $3; m[key] = $0; loc[key] = $4 + $5; n++; next }
    /JIT compiled / && /MinOpts/ {
      s = $0; sub(/.*JIT compiled /, "", s)
      name = s; sub(/\(.*/, "", name)
      il = s; sub(/.*IL size=/, "", il); sub(/,.*/, "", il)
      key = name "|" il
      if (key in m && !(key in seen)) {
        seen[key] = 1
        why = (il + 0 > 60000) ? "il-bytes" : (loc[key] >= 2000 ? "locals" : "instr/bb/refs")
        print m[key], why
      }
    }
    END { }' "$d/methods.tsv" "$d/jit.txt" | sort -t "$tab" -k3,3nr > "$d/minopts.tsv"
  count=$(wc -l < "$d/minopts.tsv" | tr -d ' ')
  methods=$(wc -l < "$d/methods.tsv" | tr -d ' ')
  printf '%s\t%s\t%s\n' "$sys" "$count" "$methods" >> "$current"
  echo "jit-minopts: $sys: $count of $methods methods in MinOpts ($d/minopts.tsv)"
done

if [ "$accept" = 1 ]; then
  cp "$current" "$baseline"
  echo "jit-minopts: baseline rewritten"
  exit 0
fi

fail=0
for sys in $systems; do
  now=$(awk -F '\t' -v s="$sys" '$1 == s { print $2 }' "$current")
  was=$(awk -F '\t' -v s="$sys" '$1 == s { print $2 }' "$baseline")
  if [ -z "$was" ]; then
    echo "jit-minopts: $sys: $now (no baseline)"
  elif [ "$now" -gt "$was" ]; then
    echo "jit-minopts: FAIL $sys: $was -> $now methods in MinOpts"; fail=1
  elif [ "$now" -lt "$was" ]; then
    echo "jit-minopts: $sys: $was -> $now (fewer; refresh with --accept)"
  else
    echo "jit-minopts: $sys: $now (unchanged)"
  fi
done
exit $fail
