#!/bin/sh
# The dist check of the library-status scripts (bench/library-status/dists.sh):
# a measurement goes ahead only against the published dotcl dist, and every
# entry is stamped with the versions it ran against.
#
# Needs no network and no built runtime: the published version is read from a
# file:// URL and the Quicklisp home is a directory with two distinfo.txt files.
#
# Usage: check.sh <repo-root>
set -eu

ROOT="$(cd "${1%/}" && pwd)"
DISTS="$ROOT/bench/library-status/dists.sh"

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
fails=0

mkdir -p "$WORK/home/dists/quicklisp" "$WORK/home/dists/dotcl"
echo "version: 2026-01-01" > "$WORK/home/dists/quicklisp/distinfo.txt"

# run_case NAME INSTALLED PUBLISHED ALLOW WANT_CODE [WANT_TEXT]
# INSTALLED / PUBLISHED "-" means none. Runs check_dists in a child shell and
# prints dists_desc on success.
run_case() {
  name=$1; installed=$2; published=$3; allow=$4; want=$5; want_text=${6:-}
  if [ "$installed" = - ]; then rm -f "$WORK/home/dists/dotcl/distinfo.txt"
  else echo "version: $installed" > "$WORK/home/dists/dotcl/distinfo.txt"; fi
  if [ "$published" = - ]; then url="file://$WORK/missing.txt"
  else printf 'name: dotcl\nversion: %s\n' "$published" > "$WORK/dotcl.txt"; url="file://$WORK/dotcl.txt"; fi
  set +e
  out=$(LIBRARY_STATUS_QL_HOME="$WORK/home" LIBRARY_STATUS_DOTCL_DIST_URL="$url" \
        LIBRARY_STATUS_ALLOW_STALE_DIST="$allow" LIBRARY_STATUS_DIST_CHECKED=0 \
        sh -c '. "$1"; check_dists t; echo "desc=$dists_desc"' sh "$DISTS" 2>&1 < /dev/null)
  code=$?
  set -e
  ok=1
  [ "$code" -eq "$want" ] || ok=0
  if [ -n "$want_text" ]; then
    case "$out" in *"$want_text"*) ;; *) ok=0 ;; esac
  fi
  if [ "$ok" = 1 ]; then
    echo "  ok   $name"
  else
    echo "  FAIL $name: exit $code (want $want)${want_text:+, want text '$want_text'}"
    echo "$out" | sed 's/^/       /'
    fails=$((fails + 1))
  fi
}

run_case "current dist goes ahead and is stamped" 2026-10-12 2026-10-12 0 0 "desc=quicklisp=2026-01-01 dotcl=2026-10-12"
run_case "older dist stops"                        2026-10-06 2026-10-12 0 1 "not the published 2026-10-12"
run_case "newer dist than published stops"         2026-10-13 2026-10-12 0 1 "not the published 2026-10-12"
run_case "unreadable published version stops"      2026-10-12 -          0 1 "could not read"
run_case "no dotcl dist stops"                     -          2026-10-12 0 1 "no dotcl dist"
run_case "ALLOW_STALE_DIST=1 goes on, stamped"     2026-10-06 2026-10-12 1 0 "desc=quicklisp=2026-01-01 dotcl=2026-10-06"

if [ "$fails" -gt 0 ]; then
  echo "test-library-status-dists: $fails failed"
  exit 1
fi
echo "test-library-status-dists: OK"
