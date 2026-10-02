#!/bin/sh
# Check the note filters in scrub.sh, and that nothing they are meant to remove
# has reached the published files. Run after either stage and after render:
#
#   sh bench/library-status/check-scrub.sh [repo-root]
#
# Exits non-zero on the first kind of problem found, printing the lines.
set -eu
root="${1:-.}"
here="$root/bench/library-status"
. "$(dirname "$0")/scrub.sh"

fail=0
expect() {
  got=$(printf '%s' "$1" | scrub_note)
  if [ "$got" != "$2" ]; then
    echo "check-scrub: scrub_note: '$1'" >&2
    echo "  expected '$2'" >&2
    echo "  got      '$got'" >&2
    fail=1
  fi
}

expect "The function get-structure is not yet implemented for dotcl 0.1.29+273.ge515cc0 on Arm64." \
       "The function get-structure is not yet implemented for dotcl <version> on <arch>."
expect "not supported on dotcl 0.1.30 on X64" "not supported on dotcl <version> on <arch>"
expect "built by 0.1.30+12.gabc1234-dirty here" "built by <version> here"
expect "File not found: /Users/me/work/x.lisp" "File not found: <path>"
expect "Component ASDF/USER::FOO-TEST not found" "Component ASDF/USER::FOO-TEST not found"
expect "SB-EXT:*RUNTIME-PATHNAME* 1.2.3" "SB-EXT:*RUNTIME-PATHNAME* 1.2.3"

# What must not appear in a published note: a development build's version, a
# release version after "dotcl ", an absolute path of the measuring machine.
leaks='[0-9]+\.[0-9]+\.[0-9]+\+[0-9]+\.g[0-9a-f]|dotcl [0-9]+\.[0-9]+\.[0-9]|(^|[^A-Za-z0-9<])/(home|Users|tmp|mnt)/'
for f in "$here/results.json" "$here/tests.json" "$here/forks.json" "$root/docs/library-status.md"; do
  [ -f "$f" ] || continue
  if grep -nE "$leaks" "$f" >&2; then
    echo "check-scrub: $f has the lines above" >&2
    fail=1
  fi
done

[ "$fail" -eq 0 ] && echo "check-scrub: OK"
exit "$fail"
