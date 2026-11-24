#!/bin/bash
# Build the tree the ledger measures, and say whether it is fresh.
#
#   bench/ledger/build.sh [build] [TREE]    build and refresh, then check (default)
#   bench/ledger/build.sh check [TREE]      only check
#
# TREE defaults to the tree this script is in.
#
# "build" makes the Release runtime, then refreshes the generated files that
# depend on the runtime source: compiler/dotcl.core, contrib/asdf/asdf.fasl, the
# other contrib fasls, contrib/quicklisp/quicklisp.fasl and the copies the runtime
# loads (the Makefile's refresh-fasls). Those files are not rebuilt just because a
# C# file changed, and a fasl or core built against an older runtime source loads
# but behaves as that source did; without this step a measurement after a
# cherry-pick measures the old core.
#
# "check" fails (exit 1) when the Release runtime is missing, or when one of
# those files is missing or was built against a runtime source other than the one
# the Release runtime was built from. It prints which file, and what to run.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
cmd=build
case ${1:-} in build|check) cmd=$1; shift;; esac
tree=${1:-$(cd "$here/../.." && pwd)}

check() {
  local idfile=$tree/runtime/obj/Release/net10.0/dotcl-runtime-source.id id f bad=0
  if [ ! -x "$tree/runtime/bin/Release/net10.0/runtime" ]; then
    echo "ledger build check: no Release runtime in $tree; run bench/ledger/build.sh" >&2
    return 1
  fi
  if [ ! -f "$idfile" ]; then
    echo "ledger build check: $idfile not found; run bench/ledger/build.sh" >&2
    return 1
  fi
  id=$(head -n 1 "$idfile" | tr -d '\r\n ')
  [ -n "$id" ] || { echo "ledger build check: empty runtime source id in $idfile" >&2; return 1; }
  for f in compiler/dotcl.core contrib/asdf/asdf.fasl contrib/quicklisp/quicklisp.fasl; do
    if [ ! -f "$tree/$f" ]; then
      echo "ledger build check: $f is missing; run bench/ledger/build.sh" >&2; bad=1
    elif ! LC_ALL=C grep -q -a -F -- "-$id" "$tree/$f"; then
      echo "ledger build check: $f was built against another runtime source than the Release runtime ($id); run bench/ledger/build.sh" >&2; bad=1
    fi
  done
  for f in "$tree"/contrib/*/*.fasl; do
    [ -f "$f" ] || continue
    case $f in */contrib/asdf/asdf.fasl|*/contrib/quicklisp/quicklisp.fasl) continue;; esac
    if ! LC_ALL=C grep -q -a -F -- "-$id" "$f"; then
      echo "ledger build check: ${f#$tree/} was built against another runtime source than the Release runtime ($id); run bench/ledger/build.sh" >&2; bad=1
    fi
  done
  return $bad
}

build() {
  cd "$tree"
  export UseSharedCompilation=false MSBUILDDISABLENODEREUSE=1 DOTNET_CLI_USE_MSBUILD_SERVER=0
  make setup-asdf setup-quicklisp cross-compile < /dev/null
  make bench-build < /dev/null
  # drops the fasls and the core whose stamp is not the current runtime source,
  # rebuilds them, and syncs the contrib copies
  make refresh-fasls compile-asdf-fasl compile-quicklisp-fasl compile-core-fasl < /dev/null
}

case $cmd in
  build) build && check;;
  check) check;;
esac
