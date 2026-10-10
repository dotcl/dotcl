#!/bin/sh
# Inside the dotcl-bench-ffi image: QUICKLOAD each system with dotcl and with
# SBCL, one process per system, one at a time, each bounded. A system that
# loads then runs smoke/<system>.lisp, if there is one: a few calls into the C
# library, to see the foreign calls work and not just the library load. A
# system with a test suite then gets one ASDF:TEST-SYSTEM run.
#
# Usage (from the repository root, on the host):
#   docker run --rm -v "$PWD/bench/library-status/ffi:/work" \
#     dotcl-bench-ffi:0.1.32 sh /work/run.sh [system ...]
#
# Writes, under /work/out (out/ next to this script on the host):
#   results.tsv     system, impl, load, load seconds, smoke, test, first error
#   logs/<impl>-<system>[-smoke|-test].txt   the full output of each run
#
#   FFI_LOAD_TIMEOUT   seconds per load (default 1200; sdl2 compiles for minutes)
#   FFI_TEST_TIMEOUT   seconds per smoke or test run (default 600)
#   FFI_IMPLS          which implementations (default "dotcl sbcl")
#   FFI_OUT            where the results go (default /work/out)
set -u

out="${FFI_OUT:-/work/out}"
logs=$out/logs
mkdir -p "$logs"
load_timeout="${FFI_LOAD_TIMEOUT:-1200}"
test_timeout="${FFI_TEST_TIMEOUT:-600}"
impls="${FFI_IMPLS:-dotcl sbcl}"

if [ $# -gt 0 ]; then
  systems="$*"
else
  systems="sdl2 sdl2-image sdl2-ttf cl-yaml cl-devil cl-out123 glfw-blob
           glad-blob bodge-glfw bodge-libc-essentials pzmq common-lisp-jupyter
           glop lispbuilder-sdl"
fi

# Forms that make the test system of SYSTEM loadable, for the systems whose
# release has a test suite reachable through ASDF:TEST-SYSTEM. Quicklisp does
# not index cl-yaml-test, so its dependencies are fetched by name and its .asd
# is loaded from the cl-yaml release.
test_prelude() {
  case "$1" in
    cl-yaml)
      echo '(ql:quickload (list "fiveam" "yason" "generic-comparability" "cl-fad" "trivial-benchmark") :silent t)'
      echo '(asdf:load-asd (merge-pathnames "cl-yaml-test.asd" (asdf:system-source-directory "cl-yaml")))' ;;
    pzmq)
      echo '(ql:quickload "pzmq-test" :silent t)' ;;
  esac
}

# The same driver for both implementations. An unhandled error inside
# QUICKLOAD would enter the debugger; with stdin closed that can return and
# let the later forms run, so the error is caught and reported here instead.
# The report is one line: condition type, then its message.
write_driver() { # file system operation(load|smoke|test)
  cat > "$1" <<EOF
#+dotcl (require "quicklisp")
(format t "~&FFI-DISTS~{ ~a~}~%"
        (mapcar (lambda (d) (format nil "~a=~a" (ql-dist:name d) (ql-dist:version d)))
                (ql-dist:enabled-dists)))
(defun ffi-report (tag e)
  (format t "~&~a ~s: ~a~%" tag (type-of e)
          (substitute #\\Space #\\Newline (princ-to-string e)))
  (finish-output)
  (uiop:quit 1))
(handler-case (ql:quickload "$2" :silent t)
  (error (e) (ffi-report "FFI-LOAD-FAIL" e)))
(format t "~&FFI-LOAD-OK~%")
(finish-output)
EOF
  case "$3" in
    smoke)
      cat >> "$1" <<EOF
(defun smoke (what value)
  (format t "~&FFI-SMOKE ~a: ~s~%" what value)
  (finish-output))
(handler-case (load "/work/smoke/$2.lisp")
  (error (e) (ffi-report "FFI-SMOKE-FAIL" e)))
(format t "~&FFI-SMOKE-DONE~%")
EOF
      ;;
    test)
      { echo '(handler-case (progn'
        test_prelude "$2"
        echo "(asdf:test-system \"$2\"))"
        echo '  (error (e) (ffi-report "FFI-TEST-FAIL" e)))'
        echo '(format t "~&FFI-TEST-DONE~%")'
      } >> "$1" ;;
  esac
  echo '(finish-output) (uiop:quit 0)' >> "$1"
}

run_impl() { # impl driver log timeout
  case "$1" in
    dotcl) timeout "$4" dotcl "$2" > "$3" 2>&1 < /dev/null ;;
    sbcl)  timeout "$4" ros run -- --non-interactive --load "$2" > "$3" 2>&1 < /dev/null ;;
  esac
}

first_error() { # log
  e=$(grep -a -m1 -E '^FFI-(LOAD|SMOKE|TEST)-FAIL ' "$1" | sed 's/^FFI-[A-Z]*-FAIL //')
  [ -n "$e" ] || e=$(grep -a -m1 -E 'Unhandled|debugger invoked|Error|error' "$1")
  [ -n "$e" ] || e=$(grep -a -v '^[[:space:]]*$' "$1" | tail -1)
  printf '%s' "$e" | tr '\t\r' '  ' | cut -c1-300
}

# The smoke results on one line, "what: value; ...", or the error.
smoke_summary() { # log
  if grep -q '^FFI-SMOKE-DONE' "$1"; then
    sed -n 's/^FFI-SMOKE //p' "$1" | tr '\t\n' ' ;' | cut -c1-200
  else
    printf 'error: %s' "$(first_error "$1")"
  fi
}

# One line of test summary, for the framework these suites use (fiveam).
test_summary() { # log
  grep -a -E 'Did [0-9]+ checks|Pass: [0-9]+|Fail: [0-9]+' "$1" | tr -s ' \n' ' ' | cut -c1-200
}

[ -f "$out/results.tsv" ] \
  || printf 'system\timpl\tload\tseconds\tsmoke\ttest\terror\n' > "$out/results.tsv"

work=$(mktemp -d)
for sys in $systems; do
  for impl in $impls; do
    log=$logs/$impl-$sys.txt
    write_driver "$work/load.lisp" "$sys" load
    t0=$(date +%s)
    run_impl "$impl" "$work/load.lisp" "$log" "$load_timeout"
    code=$?
    t1=$(date +%s)
    err=""
    smk=""
    tst=""
    if [ $code -eq 0 ] && grep -q '^FFI-LOAD-OK' "$log"; then
      load=ok
      if [ -f "/work/smoke/$sys.lisp" ]; then
        slog=$logs/$impl-$sys-smoke.txt
        write_driver "$work/smoke.lisp" "$sys" smoke
        run_impl "$impl" "$work/smoke.lisp" "$slog" "$test_timeout"
        if [ $? -eq 124 ]; then smk=timeout; else smk=$(smoke_summary "$slog"); fi
      fi
      if [ -n "$(test_prelude "$sys")" ]; then
        tlog=$logs/$impl-$sys-test.txt
        write_driver "$work/test.lisp" "$sys" test
        run_impl "$impl" "$work/test.lisp" "$tlog" "$test_timeout"
        tcode=$?
        if [ $tcode -eq 124 ]; then tst="timeout"
        elif grep -q '^FFI-TEST-DONE' "$tlog"; then tst="ran: $(test_summary "$tlog")"
        else tst="error: $(first_error "$tlog")"
        fi
      fi
    elif [ $code -eq 124 ]; then
      load=timeout
    else
      load=fail
      err=$(first_error "$log")
    fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$sys" "$impl" "$load" "$((t1 - t0))" \
      "$smk" "$tst" "$err" | tee -a "$out/results.tsv"
  done
done
rm -rf "$work"

# The container runs as root; hand the results to whoever owns the mount.
chown -R "$(stat -c %u:%g /work)" "$out"
