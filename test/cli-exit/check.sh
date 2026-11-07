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

# The cleanup forms of UNWIND-PROTECT run when a script ends: through an error that
# reaches the debugger (exit 1, the form after the error still does not run) and
# through DOTCL:QUIT (its code; with :abort t the process ends at once and they do
# not run).
unwind_case() {
  name="$1"; want_code="$2"; want_cleanup="$3"
  set +e
  out=$("$exe" --asm "$core" "$WORK/$name.lisp" 2>&1 < /dev/null)
  code=$?
  set -e
  if [ "$code" -ne "$want_code" ]; then
    echo "  FAIL $name: exit $code, wanted $want_code"; fails=$((fails + 1)); return
  fi
  has=no; printf '%s' "$out" | grep -q CLEANUP && has=yes
  if [ "$has" != "$want_cleanup" ]; then
    echo "  FAIL $name: cleanup ran: $has, wanted $want_cleanup"
    printf '%s\n' "$out" | sed 's/^/    /'
    fails=$((fails + 1)); return
  fi
  if printf '%s' "$out" | grep -q REACHED; then
    echo "  FAIL $name: the form after the exit ran"; fails=$((fails + 1)); return
  fi
  echo "  ok   $name"
}
cat > "$WORK/unwind-error.lisp" <<'EOF'
(unwind-protect (error "boom")
  (format t "~&CLEANUP~%") (finish-output))
(format t "~&REACHED~%")
EOF
unwind_case unwind-error 1 yes
cat > "$WORK/unwind-quit.lisp" <<'EOF'
(unwind-protect (dotcl:quit 7)
  (format t "~&CLEANUP~%") (finish-output))
(format t "~&REACHED~%")
EOF
unwind_case unwind-quit 7 yes
cat > "$WORK/unwind-quit-abort.lisp" <<'EOF'
(unwind-protect (dotcl:quit 7 :abort t)
  (format t "~&CLEANUP~%") (finish-output))
(format t "~&REACHED~%")
EOF
unwind_case unwind-quit-abort 7 no
# The same through --load and --eval: QUIT unwinds there too.
flag_unwind_case() {
  name="$1"; shift
  set +e
  out=$("$exe" --asm "$core" "$@" 2>&1 < /dev/null)
  code=$?
  set -e
  if [ "$code" -ne 7 ] || ! printf '%s' "$out" | grep -q CLEANUP; then
    echo "  FAIL $name: exit $code, cleanup in output: $(printf '%s' "$out" | grep -c CLEANUP)"; fails=$((fails + 1)); return
  fi
  echo "  ok   $name"
}
flag_unwind_case unwind-quit-load --load "$WORK/unwind-quit.lisp"
flag_unwind_case unwind-quit-eval --eval '(unwind-protect (dotcl:quit 7) (format t "~&CLEANUP~%") (finish-output))'

# An error raised inside a Lisp function is reported at that function's definition
# (the one-line form is unchanged), and DOTCL_BACKTRACE=1 adds the Lisp frames.
cat > "$WORK/where.lisp" <<'EOF'
(defun inner (x) (+ x "s"))
(defun middle (x) (list (inner x)))
(middle 1)
EOF
where_case() {
  name="$1"; want="$2"; unwant="$3"; shift 3
  set +e
  out=$(env "$@" "$exe" --asm "$core" "$WORK/where.lisp" 2>&1 < /dev/null)
  code=$?
  set -e
  if [ "$code" -ne 1 ]; then
    echo "  FAIL $name: exit $code, wanted 1"; fails=$((fails + 1)); return
  fi
  if ! printf '%s\n' "$out" | grep -q "$want"; then
    echo "  FAIL $name: output lacks '$want'"
    printf '%s\n' "$out" | sed 's/^/    /'; fails=$((fails + 1)); return
  fi
  if [ -n "$unwant" ] && printf '%s\n' "$out" | grep -q "$unwant"; then
    echo "  FAIL $name: output has '$unwant'"
    printf '%s\n' "$out" | sed 's/^/    /'; fails=$((fails + 1)); return
  fi
  echo "  ok   $name"
}
where_case error-where 'where.lisp:1: Not a number' '(INNER' DOTCL_BACKTRACE=0
where_case error-backtrace '(INNER 1)' '' DOTCL_BACKTRACE=1

# An error a script signals itself (ERROR) is reported at the file and line too, and a
# program read from standard input is reported at "<stdin>:LINE:".
cat > "$WORK/where.lisp" <<'EOF'
(print 1)
(error "signalled here")
EOF
where_case signalled-error-where 'where.lisp:2: SIMPLE-ERROR: signalled here' '' DOTCL_BACKTRACE=0
stdin_where_case() {
  name="$1"; want="$2"
  set +e
  out=$(env DOTCL_BACKTRACE=0 "$exe" --core "$ROOT/compiler/dotcl.core" - < "$WORK/where.lisp" 2>&1)
  code=$?
  set -e
  if [ "$code" -ne 1 ]; then
    echo "  FAIL $name: exit $code, wanted 1"; fails=$((fails + 1)); return
  fi
  if ! printf '%s\n' "$out" | grep -q "$want"; then
    echo "  FAIL $name: output lacks '$want'"
    printf '%s\n' "$out" | sed 's/^/    /'; fails=$((fails + 1)); return
  fi
  echo "  ok   $name"
}
stdin_where_case stdin-signalled-error-where '<stdin>:2: SIMPLE-ERROR: signalled here'
cat > "$WORK/where.lisp" <<'EOF'
(print 1)
(print (car 1))
EOF
stdin_where_case stdin-runtime-error-where '<stdin>:2: CAR: not a list'
cat > "$WORK/where.lisp" <<'EOF'
(print 1)
(print (list 1 2
EOF
stdin_where_case stdin-reader-error-where '<stdin>:2: Unexpected end of input'

# A reader error in a script is reported at the line the form started on.
cat > "$WORK/where.lisp" <<'EOF'
(print 1)
(print (list 1 2
(print 3)
EOF
where_case reader-error-where 'where.lisp:2: Unexpected end of input' '' DOTCL_BACKTRACE=0
cat > "$WORK/where.lisp" <<'EOF'
(print 1)
(print (no-such-package-xyz:foo))
EOF
where_case reader-error-package 'where.lisp:2: Package' '' DOTCL_BACKTRACE=0

# Resource exhaustion ends in a condition and a clean exit code (1), never a native crash (134):
# a deeply nested structure for the recursive comparisons and the reader, a circular list for LENGTH,
# an integer power too large to build.
exhaust_case() {
  name="$1"; want="$2"; form="$3"
  printf '%s\n' "$form" > "$WORK/where.lisp"
  where_case "$name" "$want" '' DOTCL_BACKTRACE=0
}
exhaust_case exhaust-equal-deep 'Stack overflow in function EQUAL' \
  '(let ((a nil) (b nil)) (dotimes (i 5000000) (setq a (list a) b (list b))) (equal a b))'
exhaust_case exhaust-equalp-deep 'Stack overflow in function EQUALP' \
  '(let ((a nil) (b nil)) (dotimes (i 5000000) (setq a (list a) b (list b))) (equalp a b))'
exhaust_case exhaust-reader-deep 'Stack overflow in function READ' \
  '(read-from-string (concatenate (quote string) (make-string 3000000 :initial-element #\() (make-string 3000000 :initial-element #\))))'
exhaust_case exhaust-length-circular 'circular list' \
  '(let ((x (list 1))) (setf (cdr x) x) (length x))'
exhaust_case exhaust-expt-too-large 'too large to represent' \
  '(expt 2 (expt 2 40))'

# The *debugger-hook* a run starts with must not depend on whether the core was
# given with --asm (the development path every Makefile test uses) or --core
# (the path a shipped dotcl takes). They used to differ: --core installed the
# print-and-exit hook for a script and --asm left it NIL, so a test that passed
# under one could end the process under the other.
cat > "$WORK/hook-state.lisp" <<'EOF'
(format t "~&HOOK=~a~%" (if *debugger-hook* "SET" "NIL"))
(finish-output)
EOF
hook_probe='(progn (format t "~&HOOK=~a~%" (if *debugger-hook* "SET" "NIL")) (finish-output))'
hook_case() {
  name="$1"; want="$2"; shift 2
  for flag in --asm --core; do
    set +e
    hout=$("$exe" "$flag" "$core" "$@" 2>&1 < /dev/null)
    set -e
    got=$(printf '%s\n' "$hout" | sed -n 's/^HOOK=//p' | head -1)
    if [ "$got" != "$want" ]; then
      echo "  FAIL $name ($flag): *debugger-hook* ${got:-(no output)}, wanted $want"
      printf '%s\n' "$hout" | sed 's/^/    /'
      fails=$((fails + 1))
      return
    fi
  done
  echo "  ok   $name"
}
hook_case hook-script NIL "$WORK/hook-state.lisp"
hook_case hook-eval-then-script NIL --eval '(setq cl-user::*x* 1)' "$WORK/hook-state.lisp"
hook_case hook-eval-only NIL --eval "$hook_probe"

# *debugger-hook* is NIL in a script, and the print-and-exit still happens (dotcl's own hook is
# in dotcl:*invoke-debugger-hook*); a hook the script sets itself is run first and may end the run.
cat > "$WORK/user-hook.lisp" <<'EOF'
(setq *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~&USERHOOK ~A~%" (type-of c)) (finish-output) (dotcl:quit 5 :abort t)))
(error "boom")
(format t "~&REACHED~%")
EOF
set +e
uh_out=$("$exe" --asm "$core" "$WORK/user-hook.lisp" 2>&1 < /dev/null)
uh_code=$?
set -e
if [ "$uh_code" -eq 5 ] && printf '%s' "$uh_out" | grep -q USERHOOK && ! printf '%s' "$uh_out" | grep -q REACHED; then
  echo "  ok   user-debugger-hook-runs"
else
  echo "  FAIL user-debugger-hook-runs: exit $uh_code, output: $uh_out"; fails=$((fails + 1))
fi

# A REPL whose input is a pipe has nobody to ask either: the debugger must not
# prompt, the rest of the input must not run, and the exit status must say so.
# (A REPL at a terminal still prompts; that needs a console and is not checked
# here.) The `repl` subcommand takes the default core rather than --asm, so
# this case needs that core to exist.
if [ -f "$ROOT/compiler/dotcl.core" ]; then
  set +e
  repl_out=$(printf '(with-simple-restart (abort "x") (error "boom"))\n(format t "~&REACHED~%%")\n' | "$exe" --no-init repl 2>&1)
  repl_code=$?
  set -e
  if printf '%s' "$repl_out" | grep -q '^0\] '; then
    echo "  FAIL repl-piped-reports: debugger prompted"
    printf '%s\n' "$repl_out" | sed 's/^/    /'
    fails=$((fails + 1))
  elif printf '%s' "$repl_out" | grep -q 'REACHED'; then
    echo "  FAIL repl-piped-reports: the form after the error ran"
    printf '%s\n' "$repl_out" | sed 's/^/    /'
    fails=$((fails + 1))
  elif [ "$repl_code" -eq 0 ]; then
    echo "  FAIL repl-piped-reports: exit 0, wanted nonzero"
    fails=$((fails + 1))
  else
    echo "  ok   repl-piped-reports"
  fi
else
  echo "  SKIP repl-piped-reports: compiler/dotcl.core not built"
  if [ "${DOTCL_CI:-}" = "1" ]; then
    echo "  DOTCL_CI=1: a skipped check counts as a failure here" >&2
    fails=$((fails + 1))
  fi
fi

# TERPRI to T (the terminal stream) leaves the standard output at a line start,
# so a following ~& adds nothing: no blank line may appear between "a" and "b".
cat > "$WORK/terpri-t-fresh-line.lisp" <<'EOF'
(format t "a")
(terpri t)
(format t "~&b~%")
(format t "c")
(terpri *terminal-io*)
(format t "~&d~%")
(finish-output)
EOF
set +e
tf_out=$("$exe" --asm "$core" "$WORK/terpri-t-fresh-line.lisp" 2>&1 < /dev/null)
set -e
if [ "$tf_out" = "a
b
c
d" ]; then
  echo "  ok   terpri-t-fresh-line"
else
  echo "  FAIL terpri-t-fresh-line: unexpected output"
  printf '%s\n' "$tf_out" | sed 's/^/    /'
  fails=$((fails + 1))
fi

# Trying the overloads of a .NET call is not an error: JsonDocument.Parse has five
# overloads of one arity, the string argument converts to only one, and the others
# failing used to open the debugger banner in a REPL (the value was still right).
set +e
ov_out=$(printf '%s\n' '(print (type-of (dotnet:static "System.Text.Json.JsonDocument" "Parse" "{}")))' '(format t "~&REACHED~%")' | "$exe" --asm "$core" repl 2>&1 < /dev/stdin)
set -e
if printf '%s' "$ov_out" | grep -q 'Debugger entered'; then
  echo "  FAIL dotnet-overload-trial-quiet: the debugger banner appeared"
  printf '%s\n' "$ov_out" | sed 's/^/    /'
  fails=$((fails + 1))
elif ! printf '%s' "$ov_out" | grep -q 'REACHED'; then
  echo "  FAIL dotnet-overload-trial-quiet: the form after the call did not run"
  printf '%s\n' "$ov_out" | sed 's/^/    /'
  fails=$((fails + 1))
else
  echo "  ok   dotnet-overload-trial-quiet"
fi

# The console is not interactive once it is redirected (SBCL: isatty): a pipe or a file
# on standard output / input / error answers NIL. (A terminal answering T needs a tty,
# which this harness does not have.)
cat > "$WORK/interactive-redirected.lisp" <<'EOF'
(format t "~&IS=~S ~S ~S ~S~%"
        (interactive-stream-p *standard-output*)
        (interactive-stream-p *standard-input*)
        (interactive-stream-p *error-output*)
        (interactive-stream-p *terminal-io*))
(finish-output)
EOF
set +e
is_out=$("$exe" --asm "$core" "$WORK/interactive-redirected.lisp" 2>&1 < /dev/null)
set -e
if [ "$(printf '%s' "$is_out" | sed -n 's/^IS=//p')" = "NIL NIL NIL NIL" ]; then
  echo "  ok   interactive-redirected"
else
  echo "  FAIL interactive-redirected: $(printf '%s' "$is_out" | sed -n 's/^IS=//p')"
  fails=$((fails + 1))
fi

# Time-bounded checks: a script run must come back (not hang, not run on for ever) in
# the situations below. TMO is a `timeout` command; without one these are skipped.
TMO=$(command -v timeout || command -v gtimeout || true)
timed_case() {
  name="$1"; limit="$2"; want_code="$3"; want_out="$4"; shift 4
  if [ -z "$TMO" ]; then echo "  SKIP $name: no timeout command"; return; fi
  set +e
  tout=$("$@" 2>&1)
  tcode=$?
  set -e
  [ "$tcode" -eq 124 ] && tcode=124
  echo "$name" > /dev/null
  printf '%s' "$tout" > "$WORK/$name.out"
  if [ "$tcode" -eq 124 ]; then
    echo "  FAIL $name: still running after ${limit}s"; fails=$((fails + 1)); return
  fi
  if [ -n "$want_code" ] && [ "$tcode" -ne "$want_code" ]; then
    echo "  FAIL $name: exit $tcode, wanted $want_code"
    printf '%s\n' "$tout" | sed 's/^/    /'; fails=$((fails + 1)); return
  fi
  if [ -n "$want_out" ] && ! printf '%s' "$tout" | grep -q "$want_out"; then
    echo "  FAIL $name: output lacks '$want_out'"
    printf '%s\n' "$tout" | sed 's/^/    /'; fails=$((fails + 1)); return
  fi
  echo "  ok   $name"
}

# Standard input closed (0<&-): a read reports end of file at once. Unix gives file
# descriptor 0 to the first thing the runtime opens, an internal pipe, and a read from
# that waited for ever.
cat > "$WORK/read-line-closed-stdin.lisp" <<'EOF'
(print (read-line))
EOF
if [ -n "$TMO" ]; then
  timed_case closed-stdin 30 1 'end of file' \
    "$TMO" 30 sh -c "exec \"$exe\" --asm \"$core\" \"$WORK/read-line-closed-stdin.lisp\" 0<&-"
  echo '"piped"' | "$exe" --asm "$core" "$WORK/read-line-closed-stdin.lisp" > "$WORK/piped.out" 2>&1 || true
  if grep -q piped "$WORK/piped.out"; then echo "  ok   piped-stdin-still-read"; else
    echo "  FAIL piped-stdin-still-read"; fails=$((fails + 1)); fi
fi

# A reader that went away (| head -1): the script stops at once with status 141 instead of
# writing its 30 million lines into the void. .NET's console stream ignores EPIPE.
cat > "$WORK/pipe-many.lisp" <<'EOF'
(dotimes (i 30000000) (format t "line ~D~%" i))
EOF
if [ -n "$TMO" ]; then
  set +e
  "$TMO" 40 sh -c "( \"$exe\" --asm \"$core\" \"$WORK/pipe-many.lisp\" 2>\"$WORK/pipe-many.err\"; echo \$? > \"$WORK/pipe-many.code\" ) | head -1 > \"$WORK/pipe-many.out\""
  tcode=$?
  set -e
  if [ "$tcode" -eq 124 ]; then
    echo "  FAIL broken-pipe: still running after 40s"; fails=$((fails + 1))
  elif [ "$(cat "$WORK/pipe-many.code" 2>/dev/null)" != "141" ] || [ "$(cat "$WORK/pipe-many.out")" != "line 0" ] \
       || [ -s "$WORK/pipe-many.err" ]; then
    echo "  FAIL broken-pipe: code $(cat "$WORK/pipe-many.code" 2>/dev/null), out '$(cat "$WORK/pipe-many.out")', stderr $(wc -c < "$WORK/pipe-many.err") bytes"
    fails=$((fails + 1))
  else
    echo "  ok   broken-pipe"
  fi
fi

# Standard output closed (1>&-): the failure names the cause, as SBCL does, and the script
# ends with an error status.
cat > "$WORK/closed-stdout.lisp" <<'EOF'
(format t "hello~%")
EOF
set +e
cs_out=$("$exe" --asm "$core" "$WORK/closed-stdout.lisp" 2>&1 1>&-)
cs_code=$?
set -e
if [ "$cs_code" -ne 0 ] && printf '%s' "$cs_out" | grep -q "Bad file descriptor"; then
  echo "  ok   closed-stdout"
else
  echo "  FAIL closed-stdout: exit $cs_code, output: $cs_out"; fails=$((fails + 1))
fi

# Ordinary redirections keep working: a pipe and a file receive the whole output.
cat > "$WORK/count-lines.lisp" <<'EOF'
(dotimes (i 5000) (format t "line ~D~%" i))
EOF
n_pipe=$("$exe" --asm "$core" "$WORK/count-lines.lisp" 2>&1 | wc -l | tr -d ' ')
"$exe" --asm "$core" "$WORK/count-lines.lisp" > "$WORK/count-lines.out" 2>&1
n_file=$(wc -l < "$WORK/count-lines.out" | tr -d ' ')
if [ "$n_pipe" = 5000 ] && [ "$n_file" = 5000 ]; then echo "  ok   redirected-stdout-complete"
else echo "  FAIL redirected-stdout-complete: pipe $n_pipe file $n_file"; fails=$((fails + 1)); fi

# Exit hooks: an error in one hook is reported and the rest run; (dotcl:quit N) inside a hook sets the
# exit status. Both used to hang the exiting process (Environment.Exit again, inside ProcessExit).
cat > "$WORK/exit-hook-error.lisp" <<'EOF'
(push (lambda () (format t "HOOK1~%") (error "hook boom")) dotcl:*exit-hooks*)
(push (lambda () (format t "HOOK0~%")) dotcl:*exit-hooks*)
(format t "BODY~%")
EOF
cat > "$WORK/exit-hook-quit.lisp" <<'EOF'
(push (lambda () (format t "HOOKQ~%") (dotcl:quit 7)) dotcl:*exit-hooks*)
(format t "BODY~%")
EOF
if [ -n "$TMO" ]; then
  timed_case exit-hook-error 30 0 'HOOK1' "$TMO" -k 5 30 "$exe" --asm "$core" "$WORK/exit-hook-error.lisp"
  if grep -q HOOK0 "$WORK/exit-hook-error.out" 2>/dev/null && grep -q "hook boom" "$WORK/exit-hook-error.out"; then
    echo "  ok   exit-hook-error-rest-ran"
  else
    echo "  FAIL exit-hook-error-rest-ran: $(cat "$WORK/exit-hook-error.out" 2>/dev/null)"; fails=$((fails + 1))
  fi
  timed_case exit-hook-quit 30 7 'HOOKQ' "$TMO" -k 5 30 "$exe" --asm "$core" "$WORK/exit-hook-quit.lisp"
fi

# What follows the script's name is the script's: dotcl's own options (--help, --version, --no-init,
# --color=never, --readline) given after the file name reach the script, not dotcl.
cat > "$WORK/script-args.lisp" <<'EOF'
(format t "~&ARGS=~S~%" (dotcl:script-arguments))
EOF
fcore="$ROOT/compiler/dotcl.core"
if [ -f "$fcore" ]; then
  for opt in --help --version --no-init --color=never --readline --no-readline; do
    sa_out=$("$exe" --core "$fcore" "$WORK/script-args.lisp" $opt 2>&1 < /dev/null)
    if [ "$(printf '%s' "$sa_out" | sed -n 's/^ARGS=//p')" = "(\"$opt\")" ]; then
      echo "  ok   script-args$opt"
    else
      echo "  FAIL script-args$opt: $sa_out" | head -3; fails=$((fails + 1))
    fi
  done
  sa_out=$("$exe" --core "$fcore" --no-init "$WORK/script-args.lisp" --version x 2>&1 < /dev/null)
  if [ "$(printf '%s' "$sa_out" | sed -n 's/^ARGS=//p')" = '("--version" "x")' ]; then
    echo "  ok   script-args-own-option-before-script"
  else
    echo "  FAIL script-args-own-option-before-script: $sa_out" | head -3; fails=$((fails + 1))
  fi
else
  echo "  SKIP script-args: no compiler/dotcl.core"
fi

# READ-BYTE on standard input reads the bytes (it answered :EOF), mixed with READ-CHAR at character
# boundaries; at the end of input, :EOF.
cat > "$WORK/stdin-read-byte.lisp" <<'EOF'
(format t "~&BYTES=~S~%" (list (read-byte *standard-input* nil :eof) (read-byte *standard-input* nil :eof)
                               (read-byte *standard-input* nil :eof) (read-byte *standard-input* nil :eof)))
EOF
cat > "$WORK/stdin-mixed.lisp" <<'EOF'
(format t "~&MIX=~S~%" (list (read-char) (read-byte *standard-input* nil :eof) (read-char)))
EOF
rb_out=$(printf 'ab\n' | "$exe" --asm "$core" "$WORK/stdin-read-byte.lisp" 2>&1)
if [ "$(printf '%s' "$rb_out" | sed -n 's/^BYTES=//p')" = "(97 98 10 :EOF)" ]; then echo "  ok   stdin-read-byte"
else echo "  FAIL stdin-read-byte: $rb_out"; fails=$((fails + 1)); fi
rb_out=$(printf 'abc' | "$exe" --asm "$core" "$WORK/stdin-mixed.lisp" 2>&1)
if [ "$(printf '%s' "$rb_out" | sed -n 's/^MIX=//p')" = '(#\a 98 #\c)' ]; then echo "  ok   stdin-read-byte-mixed"
else echo "  FAIL stdin-read-byte-mixed: $rb_out"; fails=$((fails + 1)); fi

# (dotcl:quit "x") is a TYPE-ERROR (exit 1), not a quiet exit 0; (dotcl:quit 3) still exits 3.
echo '(dotcl:quit "x")' > "$WORK/quit-string.lisp"
echo '(dotcl:quit 3)' > "$WORK/quit-three.lisp"
set +e
"$exe" --asm "$core" "$WORK/quit-string.lisp" > "$WORK/quit-string.out" 2>&1 < /dev/null; qs=$?
"$exe" --asm "$core" "$WORK/quit-three.lisp" > /dev/null 2>&1 < /dev/null; q3=$?
set -e
if [ "$qs" -eq 1 ] && [ "$q3" -eq 3 ]; then echo "  ok   quit-exit-codes"
else echo "  FAIL quit-exit-codes: quit \"x\" exit $qs (wanted 1), quit 3 exit $q3 (wanted 3)"; fails=$((fails + 1)); fi

# A bare (exit :code N), as sb-ext:exit is written in a script, exits N; :abort skips cleanup.
echo '(exit :code 7)' > "$WORK/exit-seven.lisp"
echo '(unwind-protect (exit :code 9) (format t "CLEANUP~%"))' > "$WORK/exit-unwind.lisp"
echo '(exit :code 5 :abort t)' > "$WORK/exit-abort.lisp"
echo '(exit)' > "$WORK/exit-bare.lisp"
set +e
"$exe" --asm "$core" "$WORK/exit-seven.lisp" > /dev/null 2>&1 < /dev/null; e7=$?
"$exe" --asm "$core" "$WORK/exit-unwind.lisp" > "$WORK/exit-unwind.out" 2>&1 < /dev/null; e9=$?
"$exe" --asm "$core" "$WORK/exit-abort.lisp" > /dev/null 2>&1 < /dev/null; e5=$?
"$exe" --asm "$core" "$WORK/exit-bare.lisp" > /dev/null 2>&1 < /dev/null; e0=$?
set -e
if [ "$e7" -eq 7 ] && [ "$e9" -eq 9 ] && grep -q CLEANUP "$WORK/exit-unwind.out" && [ "$e5" -eq 5 ] && [ "$e0" -eq 0 ]; then echo "  ok   exit-codes"
else echo "  FAIL exit-codes: seven $e7 (7), unwind $e9 (9), abort $e5 (5), bare $e0 (0)"; fails=$((fails + 1)); fi

# A script run through a symlink has the target as *load-truename* (CLHS: the truename).
echo '(format t "~&TRUENAME=~A~%" *load-truename*)' > "$WORK/lt-real.lisp"
ln -s lt-real.lisp "$WORK/lt-link.lisp"
lt_out=$("$exe" --asm "$core" "$WORK/lt-link.lisp" 2>&1 < /dev/null)
case "$lt_out" in
  *lt-real.lisp*) echo "  ok   load-truename-symlink" ;;
  *) echo "  FAIL load-truename-symlink: $lt_out"; fails=$((fails + 1)) ;;
esac

if [ "$fails" -gt 0 ]; then
  echo "cli-exit: $fails check(s) failed" >&2
  exit 1
fi
echo "cli-exit: all checks passed"
