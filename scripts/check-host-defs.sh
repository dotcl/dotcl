#!/bin/sh
# Check that nothing the cross-compile host defines for itself reached the core.
#
# The compiler's sources are read with the host's features, so a definition
# written under #-dotcl (or #+sbcl, or any other host-only conditional) is
# compiled into cil-out.sil along with everything else. Loaded into dotcl, it
# replaces the runtime builtin of the same name. Host-only definitions belong in
# compiler/cil-host.lisp, which cil-compile.lisp loads on the host and which is
# not one of the compiled sources.
#
# Two checks:
#   1. No top-level definition in the compiled sources sits behind a read-time
#      feature conditional. That is the shape that leaks into the core.
#   2. No name defined in cil-host.lisp is defined in cil-out.sil. That catches a
#      host stand-in that is also (or again) defined in the compiled sources.
#
# Usage: scripts/check-host-defs.sh <cil-out.sil> <cil-host.lisp> <source>...
set -u
SIL="${1:?usage: check-host-defs.sh <cil-out.sil> <cil-host.lisp> <source>...}"
HOST="${2:?usage: check-host-defs.sh <cil-out.sil> <cil-host.lisp> <source>...}"
shift 2
status=0

# 1. A column-0 #+ / #- line whose form (on the same line or the next) is a
#    column-0 (DEF... form.
for src in "$@"; do
  awk -v f="$src" '
    pending && /^\(def/ {
      printf "%s:%d: definition behind a host feature conditional (line %d)\n", f, NR, pline
      bad = 1
    }
    { pending = 0 }
    /^#[-+]/ {
      if ($0 ~ /^#[-+](\([^)]*\)|[^ (]+) *\(def/) {
        printf "%s:%d: definition behind a host feature conditional\n", f, NR
        bad = 1
      } else if ($0 ~ /^#[-+](\([^)]*\)|[^ (]+) *$/) {
        pending = 1; pline = NR
      }
    }
    END { exit bad }
  ' "$src" || status=1
done

# 2. Names cil-host.lisp defines must not be defined in the core.
names=$(sed -n -E 's/^\((defun|defvar) ([^ ()]+).*/\1 \2/p' "$HOST")
other=$(sed -n -E 's/^\((def[a-z-]*) .*/\1/p' "$HOST" | grep -v -x -e defun -e defvar)
if [ -n "$other" ]; then
  echo "$HOST: this check only knows DEFUN and DEFVAR; teach it: $(echo $other)" >&2
  status=1
fi
echo "$names" | while read -r kind name; do
  [ -n "$name" ] || continue
  up=$(echo "$name" | tr '[:lower:]' '[:upper:]')
  case $kind in
    defun)
      if grep -q -F -e "(:DEFMETHOD \"$up\" " -e "(:DEFMETHOD-DIRECT \"$up\" " "$SIL"; then
        echo "$SIL: defines $up, which cil-host.lisp defines for the host only" >&2
        exit 1
      fi ;;
    defvar)
      if grep -q -F "(:LOAD-SYM \"$up\") (:CASTCLASS \"Symbol\") (:STLOC #:DEFVAR-SYM_" "$SIL"; then
        echo "$SIL: defines $up, which cil-host.lisp defines for the host only" >&2
        exit 1
      fi ;;
  esac
done || status=1

if [ $status -ne 0 ]; then
  echo "check-host-defs: a host-only definition is in the core. Move it to compiler/cil-host.lisp." >&2
fi
exit $status
