#!/bin/sh
# Usage: sync-contrib-copies.sh ROOT NAME...
# Overwrite the copies of each contrib (runtime/contrib/NAME/ and
# runtime/bin/Debug/net10.0/contrib/NAME/) with contrib/NAME/'s .lisp, .asd and
# .fasl, for the copies that exist. A copy left from another checkout would
# otherwise make the loader ignore its stale .fasl and read the stale .lisp
# next to it.
set -u
root="${1%/}"; shift
for n in "$@"; do
  src="$root/contrib/$n"
  for dst in "$root/runtime/contrib/$n" "$root/runtime/bin/Debug/net10.0/contrib/$n"; do
    [ -d "$dst" ] || continue
    for ext in lisp asd fasl; do
      [ -f "$src/$n.$ext" ] && cp -p "$src/$n.$ext" "$dst/$n.$ext"
    done
  done
done
exit 0
