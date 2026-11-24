#!/bin/sh
# scripts/sync-contrib-copies.sh: a stale copy of a contrib under runtime/contrib/ and
# runtime/bin/Debug/net10.0/contrib/ is overwritten with the contrib's own files, a copy
# directory that does not exist is not made, and a contrib without a copy is left alone.
set -u
ROOT="$(cd "${1%/}" && pwd)"
W="$(mktemp -d)"; trap 'rm -rf "$W"' EXIT
fail=0; note() { echo "sync-contrib-copies: FAIL: $1"; fail=1; }

mkdir -p "$W/contrib/foo" "$W/contrib/bar" "$W/runtime/contrib/foo" "$W/runtime/bin/Debug/net10.0/contrib/foo"
for ext in lisp asd fasl; do echo "new $ext" > "$W/contrib/foo/foo.$ext"; done
echo "new bar" > "$W/contrib/bar/bar.lisp"
for d in "$W/runtime/contrib/foo" "$W/runtime/bin/Debug/net10.0/contrib/foo"; do
  for ext in lisp asd fasl; do echo "old $ext" > "$d/foo.$ext"; done
  touch -t 202001010000 "$d"/foo.*
done

sh "$ROOT/scripts/sync-contrib-copies.sh" "$W" foo bar || note "the script failed"

for d in "$W/runtime/contrib/foo" "$W/runtime/bin/Debug/net10.0/contrib/foo"; do
  for ext in lisp asd fasl; do
    cmp -s "$W/contrib/foo/foo.$ext" "$d/foo.$ext" || note "${d#$W/}/foo.$ext is not the contrib's file"
  done
done
[ -e "$W/runtime/contrib/bar" ] && note "a copy directory was made for bar"
[ -e "$W/runtime/bin/Debug/net10.0/contrib/bar" ] && note "a Debug copy directory was made for bar"
[ -f "$W/contrib/foo/foo.lisp" ] || note "the contrib itself changed"

[ "$fail" = 0 ] && echo "sync-contrib-copies: OK"
exit "$fail"
