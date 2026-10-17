# Sourced by run-quickload.sh, run-tests.sh and run-forks.sh.
#
# A measurement is only as current as the dists in the Quicklisp home it ran
# against. A home whose dotcl dist is older than the published one measures
# libraries as they were then, and the table looks like a statement about now.
# So this does two things before anything is measured:
#
#   1. Reads the installed versions of the quicklisp and dotcl dists and sets
#      $dists_desc ("quicklisp=2026-01-01 dotcl=2026-10-04"), which each script
#      writes into every entry it produces.
#   2. Compares the installed dotcl dist with the latest published one and
#      stops unless they are the same, unless told to go on. A home with no
#      dotcl dist, or a published version that cannot be read, also stops.
#
# Environment:
#   LIBRARY_STATUS_QL_HOME            the Quicklisp home to read; by default
#                                     the one the bundled client picks
#   LIBRARY_STATUS_ALLOW_STALE_DIST   1: warn instead of stopping and go on
#   LIBRARY_STATUS_DOTCL_DIST_URL     where the latest version is published
#                                     (default https://dotcl.github.io/dist/dotcl.txt)
#   LIBRARY_STATUS_DIST_CHECKED       1: skip the network check (set by
#                                     run-forks.sh for the scripts it starts)

# dist_installed_version NAME -> the "version:" of an installed dist, or empty.
dist_installed_version() {
  sed -n 's/^version: *//p' "$qlhome/dists/$1/distinfo.txt" 2>/dev/null | head -1 | tr -d '\r '
}

# dist_latest_version -> the published dotcl dist version, or empty when it
# cannot be fetched.
dist_latest_version() {
  command -v curl >/dev/null 2>&1 || return 0
  curl -fsS -m 15 "${LIBRARY_STATUS_DOTCL_DIST_URL:-https://dotcl.github.io/dist/dotcl.txt}" 2>/dev/null \
    | sed -n 's/^version: *//p' | head -1 | tr -d '\r '
}

# dist_refuse CMD MESSAGE...: prints the messages and exits 1, or only warns
# with LIBRARY_STATUS_ALLOW_STALE_DIST=1.
dist_refuse() {
  cmd=$1; shift
  for m in "$@"; do echo "$cmd: $m" >&2; done
  if [ "${LIBRARY_STATUS_ALLOW_STALE_DIST:-0}" = "1" ]; then
    echo "$cmd: LIBRARY_STATUS_ALLOW_STALE_DIST=1: going on with dotcl=${dotcl_v:-none}" >&2
    return 0
  fi
  echo "$cmd: set LIBRARY_STATUS_ALLOW_STALE_DIST=1 to measure anyway" >&2
  exit 1
}

# check_dists CMD: sets dists_desc; exits 1 unless the installed dotcl dist is
# the published one.
check_dists() {
  if [ -n "${LIBRARY_STATUS_QL_HOME:-}" ]; then qlhome="$LIBRARY_STATUS_QL_HOME"
  elif [ -d "$HOME/quicklisp" ]; then qlhome="$HOME/quicklisp"
  else qlhome="${XDG_DATA_HOME:-$HOME/.local/share}/dotcl/quicklisp"
  fi
  ql_v=$(dist_installed_version quicklisp)
  dotcl_v=$(dist_installed_version dotcl)
  dists_desc="quicklisp=${ql_v:-unknown} dotcl=${dotcl_v:-unknown}"
  echo "$1: dists $dists_desc ($qlhome)"
  if [ "${lisp:-dotcl}" != dotcl ] && [ ! -f "$qlhome/setup.lisp" ]; then
    echo "$1: $lisp loads Quicklisp from $qlhome/setup.lisp, which is missing; set LIBRARY_STATUS_QL_HOME" >&2
    exit 1
  fi
  [ "${LIBRARY_STATUS_DIST_CHECKED:-0}" = "1" ] && return 0
  if [ -z "$dotcl_v" ]; then
    dist_refuse "$1" "no dotcl dist installed in $qlhome; rows would be measured without the dotcl overlay"
    return 0
  fi
  latest=$(dist_latest_version)
  if [ -z "$latest" ]; then
    dist_refuse "$1" "could not read the published dotcl dist version, so cannot tell whether $dotcl_v is current"
    return 0
  fi
  if [ "$dotcl_v" != "$latest" ]; then
    dist_refuse "$1" "the dotcl dist in $qlhome is $dotcl_v, not the published $latest." \
      "update it first (in dotcl: (ql:update-dist \"dotcl\" :prompt nil))"
  fi
}
