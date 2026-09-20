#!/bin/sh
# What a script run does when an error reaches the debugger, checked from the
# outside: the exit code and whether the forms after the error ran.
#
# This is a process-level property, so it cannot be a regression test inside the
# image -- the whole question is what the process does, not what a form returns.
#
# The case that brought this here: with an ABORT restart in scope the debugger
# used to take it on end-of-input, which unwound into whichever library had
# established that restart (ASDF establishes one per system it loads) and let
# LOAD continue with the next form. The script ran to the end and exited 0 after
# an error nothing had handled. A library-status sweep recorded 83 failed
# QUICKLOADs as successes that way.
#
# Usage: check.sh <repo-root>
set -eu

ROOT="$(cd "${1%/}" && pwd)"
exe="$ROOT/runtime/bin/Debug/net10.0/runtime.exe"
[ -x "$exe" ] || exe="$ROOT/runtime/bin/Debug/net10.0/runtime"
core="$ROOT/compiler/cil-out.sil"

if [ ! -x "$exe" ] || [ ! -f "$core" ]; then
  echo "  SKIP: no built runtime or compiler/cil-out.sil (make build cross-compile)"
  if [ "${DOTCL_CI:-}" = "1" ]; then
    echo "  DOTCL_CI=1: a skipped check counts as a failure here" >&2
    exit 1
  fi
  exit 0
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
fails=0

# Runs one script with stdin closed and checks the exit code and the output.
# want_code: "zero" or "nonzero". want_absent: text that must NOT appear.
run_case() {
  name="$1"; want_code="$2"; want_absent="$3"
  set +e
  out=$("$exe" --asm "$core" "$WORK/$name.lisp" 2>&1 < /dev/null)
  code=$?
  set -e
  case "$want_code" in
    zero)    [ "$code" -eq 0 ] || { echo "  FAIL $name: exit $code, wanted 0"; fails=$((fails + 1)); return; } ;;
    nonzero) [ "$code" -ne 0 ] || { echo "  FAIL $name: exit 0, wanted nonzero"; fails=$((fails + 1)); return; } ;;
  esac
  if [ -n "$want_absent" ] && printf '%s' "$out" | grep -q "$want_absent"; then
    echo "  FAIL $name: output contains '$want_absent'"
    printf '%s\n' "$out" | sed 's/^/    /'
    fails=$((fails + 1))
    return
  fi
  echo "  ok   $name"
}

# An error with an ABORT restart in scope. The restart is the library's, not a
# top level: taking it must not be mistaken for handling the error.
cat > "$WORK/abort-restart.lisp" <<'EOF'
(with-simple-restart (abort "give up on this form")
  (error "boom"))
(format t "~&REACHED~%")
(finish-output)
(dotcl:quit 0)
EOF
run_case abort-restart nonzero REACHED

# The same shape with no restart at all, which already stopped. Here so that a
# future change cannot fix one path by breaking the other.
cat > "$WORK/no-restart.lisp" <<'EOF'
(error "boom")
(format t "~&REACHED~%")
(finish-output)
(dotcl:quit 0)
EOF
run_case no-restart nonzero REACHED

# An error signalled inside a restart that is NOT named ABORT.
cat > "$WORK/other-restart.lisp" <<'EOF'
(restart-case (error "boom")
  (retry () :report "RETRY" nil))
(format t "~&REACHED~%")
(finish-output)
(dotcl:quit 0)
EOF
run_case other-restart nonzero REACHED

# A script that handles its own error still runs to the end and exits 0: the
# check above must not be passing because every script now fails.
cat > "$WORK/handled.lisp" <<'EOF'
(handler-case (error "boom") (error (e) (declare (ignore e)) nil))
(format t "~&REACHED~%")
(finish-output)
(dotcl:quit 0)
EOF
run_case handled zero ""

# The REPL is the other half of the rule and must be untouched: there the
# debugger has somebody to ask, so it still prompts. The `repl` subcommand takes
# the default core rather than --asm, so this case needs that core to exist.
if [ -f "$ROOT/compiler/dotcl.core" ]; then
  set +e
  repl_out=$(printf '(with-simple-restart (abort "x") (error "boom"))\n' | "$exe" repl 2>&1)
  set -e
  if printf '%s' "$repl_out" | grep -q '^0\] '; then
    echo "  ok   repl-still-prompts"
  else
    echo "  FAIL repl-still-prompts: no debugger prompt"
    printf '%s\n' "$repl_out" | sed 's/^/    /'
    fails=$((fails + 1))
  fi
else
  echo "  SKIP repl-still-prompts: compiler/dotcl.core not built"
  if [ "${DOTCL_CI:-}" = "1" ]; then
    echo "  DOTCL_CI=1: a skipped check counts as a failure here" >&2
    fails=$((fails + 1))
  fi
fi

if [ "$fails" -gt 0 ]; then
  echo "cli-exit: $fails check(s) failed" >&2
  exit 1
fi
echo "cli-exit: all checks passed"
