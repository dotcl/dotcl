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
#      stops when it is older, unless told to go on.
#
# Environment:
#   LIBRARY_STATUS_QL_HOME            the Quicklisp home to read; by default
#                                     the one the bundled client picks
#   LIBRARY_STATUS_ALLOW_STALE_DIST   1: warn about a stale dotcl dist and go on
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

# check_dists CMD: sets dists_desc; exits 1 on a stale dotcl dist.
check_dists() {
  if [ -n "${LIBRARY_STATUS_QL_HOME:-}" ]; then qlhome="$LIBRARY_STATUS_QL_HOME"
  elif [ -d "$HOME/quicklisp" ]; then qlhome="$HOME/quicklisp"
  else qlhome="${XDG_DATA_HOME:-$HOME/.local/share}/dotcl/quicklisp"
  fi
  ql_v=$(dist_installed_version quicklisp)
  dotcl_v=$(dist_installed_version dotcl)
  dists_desc="quicklisp=${ql_v:-unknown} dotcl=${dotcl_v:-unknown}"
  echo "$1: dists $dists_desc ($qlhome)"
  [ "${LIBRARY_STATUS_DIST_CHECKED:-0}" = "1" ] && return 0
  if [ -z "$dotcl_v" ]; then
    echo "$1: warning: no dotcl dist installed in $qlhome; rows are measured without the dotcl overlay" >&2
    return 0
  fi
  latest=$(dist_latest_version)
  if [ -z "$latest" ]; then
    echo "$1: warning: could not read the latest dotcl dist version; not checking that $dotcl_v is current" >&2
    return 0
  fi
  # Versions are dates (YYYY-MM-DD), so the string order is the time order.
  if [ "$dotcl_v" \< "$latest" ]; then
    echo "$1: the dotcl dist in $qlhome is $dotcl_v, older than the published $latest." >&2
    echo "$1: update it first (in dotcl: (ql:update-dist \"dotcl\" :prompt nil))," >&2
    echo "$1: or set LIBRARY_STATUS_ALLOW_STALE_DIST=1 to measure with $dotcl_v anyway." >&2
    if [ "${LIBRARY_STATUS_ALLOW_STALE_DIST:-0}" = "1" ]; then
      echo "$1: LIBRARY_STATUS_ALLOW_STALE_DIST=1: going on with the stale dist" >&2
      return 0
    fi
    exit 1
  fi
}
