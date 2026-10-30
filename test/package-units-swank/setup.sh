#!/bin/sh
# Make two SLIME trees with different versions under $ROOT (default out/package-units-swank):
#   slime-231 = tag v2.31 + swank/dotcl.lisp from NEW + slime-2.31-dotcl.patch  ("2.31")
#   slime-new = NEW (default: HEAD of the clone)                                 ("2.32.git")
# SLIME_SRC is a clone of https://github.com/slime/slime (cloned into $ROOT if unset).
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=${ROOT:-$PWD/out/package-units-swank}
mkdir -p "$ROOT"
SRC=${SLIME_SRC:-$ROOT/slime-src}
[ -d "$SRC" ] || git clone -q https://github.com/slime/slime.git "$SRC"
NEW=$(git -C "$SRC" rev-parse "${NEW:-HEAD}")
cd "$ROOT"
for d in slime-231 slime-new; do
  [ ! -d $d ] || { echo "$ROOT/$d exists; remove it first"; exit 1; }
  mkdir $d
done
git -C "$SRC" archive "$NEW" | tar -x -C slime-new
git -C "$SRC" archive v2.31 | tar -x -C slime-231
cp slime-new/swank/dotcl.lisp slime-231/swank/dotcl.lisp
(cd slime-231 && patch -s -p1 < "$HERE/slime-2.31-dotcl.patch")
echo "new=$NEW"
