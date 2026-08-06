#!/bin/bash
# Show, for every contrib module, which file (require "<name>") will actually
# load — and what that file is hiding.
#
# The question this answers is "why is my edit not taking effect". The module
# loader searches several directories in order and takes the first hit, so an
# older copy earlier in the order silently wins over the file you are editing.
# Reporting "these are older than the compiler" does not help, because after any
# compiler edit everything under contrib/ is older; what matters is which single
# candidate wins and whether something fresher lost.
#
# Search order mirrors ModuleProvideContrib (runtime/Runtime.Misc.cs): for each
# directory in order, try .fasl, then .sil, then .lisp, at <dir>/<name>/<name><ext>.
# With the dev runner's base directory being runtime/bin/<cfg>/<tfm>/, the
# directories are that one, then runtime/, then the project root.
#
# Usage: scripts/contrib-resolve.sh <project-root> [config] [tfm]
set -u
ROOT="${1:?usage: contrib-resolve.sh <project-root> [config] [tfm]}"
CFG="${2:-Debug}"
TFM="${3:-net10.0}"
ROOT="${ROOT%/}"

BASE="$ROOT/runtime/bin/$CFG/$TFM"
DIRS=("$BASE/contrib" "$ROOT/runtime/contrib" "$ROOT/contrib")
EXTS=(fasl sil lisp)

mtime() { stat -c%y "$1" 2>/dev/null | cut -d. -f1; }
# Newer by a margin. Copies made in the same operation differ by fractions of a
# second and are not evidence of anything; a real edit is minutes or days apart.
MARGIN=${CONTRIB_RESOLVE_MARGIN:-60}
newer_than() {
  local a b
  a=$(stat -c%Y "$1" 2>/dev/null) || return 1
  b=$(stat -c%Y "$2" 2>/dev/null) || return 1
  [ $((a - b)) -gt "$MARGIN" ]
}

# Every module name that exists anywhere in the search path.
names=$(for d in "${DIRS[@]}"; do
          [ -d "$d" ] || continue
          for sub in "$d"/*/; do [ -d "$sub" ] && basename "$sub"; done
        done | sort -u)

[ -z "$names" ] && { echo "no contrib modules found under $ROOT"; exit 0; }

problems=0
for n in $names; do
  winner=""
  losers=()
  for d in "${DIRS[@]}"; do
    for e in "${EXTS[@]}"; do
      f="$d/$n/$n.$e"
      [ -f "$f" ] || continue
      if [ -z "$winner" ]; then winner="$f"; else losers+=("$f"); fi
    done
  done
  [ -z "$winner" ] && continue

  # Only a candidate in a DIFFERENT directory counts. Within one directory the
  # order is by extension (.fasl before .lisp) and preferring the compiled one is
  # the intended behaviour, not a stale copy hiding an edit.
  shadowed=()
  wdir=$(dirname "$winner")
  for l in "${losers[@]:-}"; do
    [ -n "$l" ] || continue
    [ "$(dirname "$l")" = "$wdir" ] && continue
    newer_than "$l" "$winner" && shadowed+=("$l")
  done

  if [ ${#shadowed[@]} -gt 0 ]; then
    problems=$((problems + 1))
    echo "$n: loads ${winner#$ROOT/}  ($(mtime "$winner"))"
    for s in "${shadowed[@]}"; do
      echo "    SHADOWS a newer  ${s#$ROOT/}  ($(mtime "$s"))"
    done
  else
    echo "$n: loads ${winner#$ROOT/}"
  fi
done

echo
if [ $problems -gt 0 ]; then
  echo "$problems module(s) load an older file than one available later in the search order."
  echo "Delete the winning copy (or refresh it) so the newer one is reached:"
  echo "  rm -rf $ROOT/runtime/contrib/<name> $ROOT/runtime/bin/*/*/contrib/<name>"
  echo "(runtime/contrib is the pack staging area; make pack recreates it.)"
else
  echo "no module is shadowed by a stale copy."
fi
