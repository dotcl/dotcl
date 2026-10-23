#!/bin/sh
# Usage: drop-stale-fasls.sh <runtime-source-id-file> <fasl>...
#
# Remove each existing fasl whose generation stamp does not end with the
# runtime source digest in the id file (written by the DotCL.Runtime build), so
# that make builds it again. A fasl built against the same runtime source is
# left alone, whatever the C# files' timestamps say.
set -e
idfile=$1; shift
if [ ! -f "$idfile" ]; then
  echo "drop-stale-fasls: $idfile not found; not checking fasls"
  exit 0
fi
id=$(head -n 1 "$idfile" | tr -d '\r\n ')
[ -n "$id" ] || exit 0
for f in "$@"; do
  [ -f "$f" ] || continue
  if ! LC_ALL=C grep -q -a -F -- "-$id" "$f"; then
    echo "drop-stale-fasls: $f was built against other runtime source; rebuilding"
    rm -f "$f"
  fi
done
