#!/bin/bash
# Run the Coalton kernels (run.lisp) on dotcl and on SBCL with the same Coalton
# tree, then print one table: kernel, dotcl ms, SBCL ms, dotcl/SBCL.
#
#   bench/coalton/compare.sh COALTON_DIR [OUT_DIR]
#
# COALTON_DIR is a checkout of Coalton (its examples/raytrace is used too).
# Each implementation gets its own ASDF cache under OUT_DIR (default out/coalton-bench),
# kept between runs; delete it after switching Coalton commits or compiler settings.
#
# Environment:
#   DOTCL_CMD   how to start dotcl (default: this tree's Release runtime with compiler/dotcl.core)
#   SBCL_CMD    how to start SBCL  (default: ros -L sbcl-bin run -- --dynamic-space-size 4096)
#   COALTON_BENCH_RUNS / COALTON_BENCH_ONLY   passed to run.lisp
#   EXTRA_TREES  space-separated directories added to the ASDF source registry
#                (e.g. a newer named-readtables than the SBCL Quicklisp has)
#   SKIP_DOTCL=1 / SKIP_SBCL=1               reuse the previous log of that side
set -u
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
coalton=${1:?usage: compare.sh COALTON_DIR [OUT_DIR]}
coalton=$(cd "$coalton" && pwd)
out=${2:-$root/out/coalton-bench}
mkdir -p "$out"
out=$(cd "$out" && pwd)

DOTCL_CMD=${DOTCL_CMD:-"$root/runtime/bin/Release/net10.0/runtime --core $root/compiler/dotcl.core"}
SBCL_CMD=${SBCL_CMD:-"ros -L sbcl-bin run -- --dynamic-space-size 4096 --non-interactive --load"}
TIMEOUT=${TIMEOUT:-3600}

export COALTON_ENV=release DOTCL_NO_JIT_PROFILE=1 DOTNET_gcServer=0
trees="(:tree \"$coalton/\")"
for d in ${EXTRA_TREES:-}; do trees="$trees (:tree \"$(cd "$d" && pwd)/\")"; done
export CL_SOURCE_REGISTRY="(:source-registry $trees :inherit-configuration)"

run_side() {
  local name=$1; shift
  echo "=== $name ===" >&2
  XDG_CACHE_HOME="$out/xdg-$name" COALTON_BENCH_OUT="$out/fasl-$name" \
    timeout "$TIMEOUT" "$@" "$here/run.lisp" < /dev/null > "$out/$name.log" 2>&1
  local rc=$?
  grep -E '^(IMPL|COALTON-MODE|LOAD|COMPILE|RESULT)' "$out/$name.log" >&2
  [ $rc -eq 0 ] || echo "$name: exit $rc, see $out/$name.log" >&2
}

[ "${SKIP_DOTCL:-}" = 1 ] || run_side dotcl $DOTCL_CMD
[ "${SKIP_SBCL:-}" = 1 ] || run_side sbcl $SBCL_CMD

awk '
  FNR == 1 { side = (FILENAME ~ /dotcl\.log$/) ? "d" : "s" }
  $1 == "RESULT" {
    if (!($2 in seen)) { seen[$2] = 1; order[++n] = $2 }
    ms[side, $2] = $3; sum[side, $2] = $4
  }
  END {
    printf "%-18s %10s %10s %8s  %s\n", "kernel", "dotcl ms", "SBCL ms", "ratio", "checksum"
    for (i = 1; i <= n; i++) {
      k = order[i]; d = ms["d", k]; s = ms["s", k]
      r = (d != "" && s != "" && s > 0) ? sprintf("%.1f", d / s) : "-"
      c = (sum["d", k] == sum["s", k]) ? "same" : "DIFFERS " sum["d", k] " / " sum["s", k]
      printf "%-18s %10s %10s %8s  %s\n", k, (d == "" ? "-" : d), (s == "" ? "-" : s), r, c
    }
  }' "$out/dotcl.log" "$out/sbcl.log" | tee "$out/table.txt"
