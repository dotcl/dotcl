#!/bin/sh
# Stage 2b of the library-status pipeline: run the test suite of each system
# that loads, and write tests.json.
#
# There is no one command that says "this library's tests passed" across
# libraries. ASDF's TEST-SYSTEM runs whatever the system's TEST-OP does, and
# what that is -- which framework, whether a failure signals, whether anything
# runs at all -- is up to each library. So this stage does two separate things:
#
#   1. Run (asdf:test-system SYS) in a fresh dotcl, after quickloading SYS and
#      the systems its TEST-OP depends on (Quicklisp only fetches on QUICKLOAD).
#   2. Judge the output with one small recogniser per test framework, from the
#      summary line that framework prints. The judge runs here, on the log,
#      outside the process being judged, so a run that dies half way is still
#      judged from what it printed.
#
# The verdicts, and what each one claims:
#
#   pass        a recognised framework summary was printed, it counted at least
#               one passing check and no failing one, and the run finished.
#   fail        a recognised summary counted at least one failure.
#   error       TEST-SYSTEM signalled, the debugger was entered, or the process
#               exited abnormally, and no recognised summary says otherwise.
#   no-result   the run finished but printed nothing a recogniser knows. This is
#               "nothing was looked at", not "nothing went wrong": a library
#               whose TEST-OP is empty, or whose framework has no recogniser
#               yet, lands here, and it is never counted as a pass.
#   timeout     the bound was hit.
#   load-fail   the system or one of its test systems did not load.
#
# Only `pass` makes a row `ok` in the published table (render.lisp).
#
# The same rules as run-quickload.sh: one system per process, in sequence, each
# bounded by `timeout`, and only the process this script started is ever
# stopped. tests.json is rewritten after every system; LIBRARY_STATUS_RESUME=1
# continues an interrupted run.
#
# Usage:  run-tests.sh [repo-root]
#   LIMIT=N                        stop after N systems
#   LIBRARY_STATUS_RESUME=1        skip systems already in the output
#   LIBRARY_STATUS_TEST_TIMEOUT    seconds per system (default 900)
#   LIBRARY_STATUS_TEST_TARGETS    a file of system names to test; by default
#                                  every row of results.json that loads
#                                  (load-only or patched), in targets.txt order
#   LIBRARY_STATUS_JSON            results.json to take the default list from
#   LIBRARY_STATUS_TESTS_JSON      output tests.json
#   LIBRARY_STATUS_LOGDIR          per-system output, kept for diagnosis
set -eu

root="${1:-.}"
here="$root/bench/library-status"
results="${LIBRARY_STATUS_JSON:-$here/results.json}"
targets="$here/targets.txt"
out="${LIBRARY_STATUS_TESTS_JSON:-$here/tests.json}"
logdir="${LIBRARY_STATUS_LOGDIR:-$root/out/library-status-tests}"
per="${LIBRARY_STATUS_TEST_TIMEOUT:-900}"
limit="${LIMIT:-0}"
resume="${LIBRARY_STATUS_RESUME:-0}"

exe="$root/runtime/bin/Debug/net10.0/runtime.exe"
[ -x "$exe" ] || exe="$root/runtime/bin/Debug/net10.0/runtime"
[ -x "$exe" ] || { echo "run-tests: no built runtime; run make build first" >&2; exit 1; }
core="$root/compiler/cil-out.sil"
# GNU timeout bounds each run. macOS has none by default; Homebrew coreutils
# installs it as gtimeout. Without either, every run would fail with 127 and
# be recorded as a library failure, so stop here instead.
if command -v timeout >/dev/null 2>&1; then timeout=timeout
elif command -v gtimeout >/dev/null 2>&1; then timeout=gtimeout
else echo "run-tests: needs GNU timeout (coreutils; gtimeout on macOS)" >&2; exit 1
fi
[ -f "$core" ] || { echo "run-tests: no $core; run make cross-compile first" >&2; exit 1; }

checked=$(date +%Y-%m-%d)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$logdir"

jstr() {
  printf '%s' "$1" \
    | tr -d '"\\' | tr '\n\r\t' '   ' \
    | sed 's|^[. ]*||; s|  *| |g; s| *$||' \
    | scrub_paths \
    | cut -c1-160
}

# Notes are published: replace absolute paths with <path>. Same rule as
# run-quickload.sh, which explains it.
scrub_paths() {
  sed -E 's@(^|[^A-Za-z0-9])(#P)?[A-Za-z]:[/A-Za-z][^ )"'"'"']*@\1<path>@g; s@(^|[^A-Za-z0-9])/(home|Users|tmp|mnt)/[^ )"'"'"']*@\1<path>@g'
}

# The work list. Default: the rows that load, in the published order.
if [ -n "${LIBRARY_STATUS_TEST_TARGETS:-}" ]; then
  grep -v '^#' "$LIBRARY_STATUS_TEST_TARGETS" | grep -v '^[[:space:]]*$' > "$work/list"
else
  # POSIX ERE: alternation in a BRE (\|) is a GNU extension that BSD sed
  # (macOS) treats as a literal and so matches nothing. [{] because a bare {
  # after an atom starts an interval in an ERE.
  sed -En 's/^  [{]"system": "([^"]*)".*"status": "(load-only|patched)".*/\1/p' "$results" \
    | sort -u > "$work/loads"
  grep -v '^#' "$targets" | grep -v '^[[:space:]]*$' | grep -Fxf "$work/loads" > "$work/list" || true
fi

: > "$work/entries"
if [ "$resume" = "1" ] && [ -f "$out" ]; then
  grep '^  {"system"' "$out" | sed 's/,*$//' > "$work/entries" || true
  sed -n 's/^  {"system": "\([^"]*\)".*/\1/p' "$work/entries" | sort -u > "$work/done"
else
  : > "$work/done"
fi

write_json() {
  {
    echo '['
    sed '$!s/$/,/' "$work/entries"
    echo ']'
  } > "$out"
}

# judge LOG -> prints "VERDICT FRAMEWORK PASSED FAILED"
#
# Only the part of the log after LIBTEST-BEGIN is read: a summary line printed
# while loading is not a test result. ANSI colour sequences are removed first
# (rove and prove colour their summary when they think they may). awk runs in
# the C locale so the non-ASCII markers Try prints compare as bytes, the same
# way whichever awk and locale the host has. The escape character comes from
# printf: \x1b in a sed script is a GNU extension.
esc=$(printf '\033')
judge() {
  sed -n '/^LIBTEST-BEGIN/,$p' "$1" | sed "s/${esc}\[[0-9;]*m//g" | LC_ALL=C awk '
    # fiveam: "Did N checks." then "Pass: P (..%)" and "Fail: F (..%)".
    # Nested suites print one block each, indented; they are summed.
    /^[[:space:]]*Did [0-9]+ checks?\./ { fw["fiveam"]=1; next }
    /^[[:space:]]*Pass: [0-9]+ \(/ { if (fw["fiveam"]) { sub(/^[[:space:]]*Pass: /, ""); pass += $1 } next }
    /^[[:space:]]*Fail: [0-9]+ \(/ { if (fw["fiveam"]) { sub(/^[[:space:]]*Fail: /, ""); fail += $1 } next }
    # rt: "Doing N pending tests of M tests total." then either
    # "No tests failed." or "K out of M total tests failed: ...".
    /^Doing [0-9]+ pending tests? of [0-9]+ tests total\./ { rt_total = $2; next }
    /^No tests failed\./ { fw["rt"]=1; pass += rt_total; rt_total = 0; next }
    / out of [0-9]+ total tests failed/ {
      fw["rt"]=1; fail += $1; pass += $4 - $1; rt_total = 0; next }
    # rove and prove: "N tests completed" or "K of M tests failed".
    /[0-9]+ tests? completed/ {
      fw["rove/prove"]=1
      for (i = 1; i <= NF; i++) if ($(i+1) ~ /^tests?$/ && $(i+2) == "completed") pass += $i
      next }
    /[0-9]+ of [0-9]+ tests? failed/ {
      fw["rove/prove"]=1
      for (i = 1; i <= NF; i++) if ($(i+1) == "of" && $(i+3) ~ /^tests?$/) { fail += $i; pass += $(i+2) - $i }
      next }
    # parachute: ";; Summary:" then "Passed: N" / "Failed: N" (plain report),
    # or "Passed: N (..%)" (largescale report).
    /^;; Summary:/ { fw["parachute"]=1; in_para = 1; next }
    in_para && /^Passed: +[0-9]+/ { gsub(/[^0-9 ]/, "", $2); pass += $2; next }
    in_para && /^Failed: +[0-9]+/ { gsub(/[^0-9 ]/, "", $2); fail += $2; in_para = 0; next }
    # hu.dwim.stefil / stefil: "#<test-run: N tests, A assertions, F failures in ...".
    # A failure there is a failed assertion or an unexpected error.
    # A test op that calls the suite without printing its result leaves only
    # progress characters, so the driver prints the last result stefil kept
    # after LIBTEST-STEFIL-RESULT. That line is used only when the test output
    # had no summary of its own (otherwise it would count the last run twice).
    /^LIBTEST-STEFIL-RESULT #<test-run: [0-9]+ tests?, [0-9]+ assertions?, [0-9]+ failures?/ {
      s = $0; sub(/.*#<test-run: /, "", s); split(s, w, /[ ,]+/)
      last_pass = w[3] - w[5]; last_fail = w[5]; last_seen = 1; next }
    /#<test-run: [0-9]+ tests?, [0-9]+ assertions?, [0-9]+ failures?/ {
      fw["stefil"]=1
      s = $0; sub(/.*#<test-run: /, "", s); split(s, w, /[ ,]+/)
      pass += w[3] - w[5]; fail += w[5]; next }
    # fiasco: one "NAME.....[ OK ]" (or "[FAIL]", "[SKIP]") line per test.
    # RUN-TESTS prints "Test run had N failures:" only when something failed;
    # a run with no failures prints no summary at all (the "no failures" line
    # comes from DESCRIBE-FAILED-TESTS, which RUN-TESTS then does not call).
    # So the per-test lines are the result, and a summary, when present, gives
    # the failure count. Lines not followed by a summary are counted at END.
    /\[ OK \][[:space:]]*$/ { fiasco_ok++; next }
    /\[FAIL\][[:space:]]*$/ { fiasco_fail++; next }
    /^Test run had no failures\./ { fw["fiasco"]=1; pass += fiasco_ok; fiasco_ok = 0; fiasco_fail = 0; next }
    /^Test run had [0-9]+ failures?:/ {
      fw["fiasco"]=1; pass += fiasco_ok; fail += $4; fiasco_ok = 0; fiasco_fail = 0; next }
    # clunit and clunit2: "Tested N assertions." then one line each for the
    # non-zero counts: "Passed: P/N ...", "Failed: F/N ...", "Errors: E/N ...".
    # An error there is an assertion that signalled, so it counts as a failure.
    /^[[:space:]]*Tested [0-9]+ assertions?\./ { fw["clunit"]=1; in_clunit = 1; next }
    in_clunit && /^[[:space:]]*Passed: [0-9]+\/[0-9]+/ { split($2, w, "/"); pass += w[1]; next }
    in_clunit && /^[[:space:]]*(Failed|Errors): [0-9]+\/[0-9]+/ { split($2, w, "/"); fail += w[1]; next }
    in_clunit && !/^[[:space:]]*$/ { in_clunit = 0 }
    # Try: a trial prints as "#<TRY:TRIAL (NAME) OUTCOME 1.234s COUNTS>", where
    # COUNTS is one marker+number per outcome category that occurred, in the
    # fancy markers or the ASCII ones:
    #   expected success "." or U+22C5, expected failure "f" or U+00D7  -> pass
    #   unexpected failure "F" or U+22A0, unexpected success ":" or U+22A1,
    #   abort "!" or U+229F                                             -> fail
    #   skip "-"                                                        -> neither
    # Only a line that starts with the trial is read (what the test op prints
    # when it is done); a trial mentioned inside a failure report is not.
    # The name can contain spaces, so the line is read from its end.
    /^#<(TRY:)?TRIAL / {
      s = $0; sub(/>[[:space:]]*$/, "", s); nw = split(s, w, /[[:space:]]+/)
      tp = 0; tf = 0; seen = 0
      for (i = nw; i > 0; i--) {
        if (w[i] ~ /^[0-9.]+s$/) { seen = 1; break }
        m = w[i]; sub(/[0-9]+$/, "", m); c = substr(w[i], length(m) + 1)
        if (m == "" || c == "") break
        if (m == "." || m == "f" || m == "\342\213\205" || m == "\303\227") tp += c
        else if (m == "F" || m == ":" || m == "!" || m == "\342\212\240" || m == "\342\212\241" || m == "\342\212\237") tf += c
      }
      if (seen) {
        fw["try"]=1
        # A trial whose own outcome is unexpected or aborted has failed even
        # when nothing under it is counted (an error before the first check).
        if (tf == 0 && i > 1 && w[i-1] ~ /^(UNEXPECTED-|ABORT)/) tf = 1
        pass += tp; fail += tf
      }
      next }
    /^LIBTEST-ERROR/ { err = 1 }
    /^; Debugger entered on/ { err = 1 }
    /^LIBTEST-END/ { ended = 1 }
    END {
      if (fiasco_ok > 0 || fiasco_fail > 0) {
        fw["fiasco"]=1; pass += fiasco_ok; fail += fiasco_fail }
      if (last_seen && !("stefil" in fw)) {
        fw["stefil"]=1; pass += last_pass; fail += last_fail }
      names = ""
      for (k in fw) names = names (names == "" ? "" : "+") k
      if (names == "") names = "-"
      if (fail > 0) v = "fail"
      else if (names != "-" && pass > 0 && !err && ended) v = "pass"
      else if (err || !ended) v = "error"
      else v = "no-result"
      printf "%s %s %d %d\n", v, names, pass, fail
    }'
}

n=0
ran=0
skipped=0
started=$(date +%s)
write_json

for sys in $(cat "$work/list"); do
  if grep -qx "$sys" "$work/done" 2>/dev/null; then
    skipped=$((skipped + 1))
    continue
  fi
  n=$((n + 1))
  if [ "$limit" -gt 0 ] && [ "$n" -gt "$limit" ]; then n=$((n - 1)); break; fi

  # Every error the driver can see is turned into a marker line and an exit,
  # because with stdin closed the debugger returns rather than stopping and the
  # forms after it would still run.
  cat > "$work/drv.lisp" <<DRVEOF
(require "quicklisp")
(format t "~&LIBTEST-DISTS ~{~a~^ ~}~%"
        (mapcar (lambda (d) (format nil "~a=~a" (ql-dist:name d) (ql-dist:version d)))
                (ql-dist:enabled-dists)))
(defun libtest-die (tag e code)
  (format t "~%~a ~a~%" tag e)
  (finish-output)
  (dotcl:quit code))
(handler-case (ql:quickload "$sys")
  (error (e) (libtest-die "LIBTEST-LOAD-FAIL" e 3)))
;; The systems TEST-OP on SYS depends on, usually one "SYS/test". Quicklisp
;; fetches a missing dependency only through QUICKLOAD, so load them that way
;; before ASDF gets to them.
(defun libtest-test-systems (name)
  (let ((system (asdf:find-system name)) (names '()))
    (dolist (dep (asdf:component-depends-on (asdf:make-operation 'asdf:test-op) system))
      (dolist (c (cdr dep))
        (let ((s (ignore-errors (asdf:find-system c nil))))
          (when (and s (not (eq s system)))
            (pushnew (asdf:component-name s) names :test #'string=)))))
    (nreverse names)))
(let ((names (handler-case (libtest-test-systems "$sys")
               (error (e) (libtest-die "LIBTEST-LOAD-FAIL" e 3)))))
  (format t "~&LIBTEST-TEST-SYSTEMS ~{~a~^ ~}~%" names)
  (dolist (name names)
    (handler-case (ql:quickload name)
      (error (e) (libtest-die "LIBTEST-LOAD-FAIL" e 3)))))
;; Some TEST-OPs load their test system by calling OPERATE from inside the
;; PERFORM method instead of naming it as a dependency (babel does), which
;; Quicklisp never sees. Fetch the conventionally named test systems the dists
;; know about, so their dependencies are on disk when that happens. These are
;; guesses, so a failure here is only noted: if the TEST-OP really needs the
;; system, the run fails on its own.
(dolist (name '("$sys/test" "$sys/tests" "$sys-test" "$sys-tests"))
  (when (ql-dist:find-system name)
    (handler-case (progn (ql:quickload name)
                         (format t "~&LIBTEST-EXTRA-SYSTEM ~a~%" name))
      (error (e) (format t "~&LIBTEST-NOTE ~a did not load: ~a~%" name e)))))
;; The markers start with ~% rather than ~&: the judge only reads a marker at the
;; start of a line, and a FRESH-LINE that misjudges the column (after PRINT, on
;; some streams) would glue it to the test output and lose the result.
(format t "~%LIBTEST-BEGIN~%")
(finish-output)
(handler-case (asdf:test-system "$sys")
  (error (e) (libtest-die "LIBTEST-ERROR" e 2)))
;; stefil keeps the result of the last suite run in *LAST-TEST-RESULT*. A test
;; op that does not print it leaves only progress characters, so print it here.
(dolist (p '("HU.DWIM.STEFIL" "STEFIL"))
  (let ((s (and (find-package p) (find-symbol "*LAST-TEST-RESULT*" p))))
    (when (and s (boundp s) (symbol-value s))
      (format t "~%LIBTEST-STEFIL-RESULT ~a~%" (symbol-value s)))))
(format t "~%LIBTEST-END~%")
(finish-output)
(dotcl:quit 0)
DRVEOF

  log="$logdir/$sys.txt"
  t0=$(date +%s)
  set +e
  "$timeout" "$per" "$exe" --asm "$core" "$work/drv.lisp" > "$log" 2>&1 < /dev/null
  code=$?
  set -e
  t1=$(date +%s)
  elapsed=$((t1 - t0))

  if [ "$code" -eq 124 ]; then
    set -- timeout - 0 0
    note="timeout after ${per}s"
  elif grep -q '^LIBTEST-LOAD-FAIL' "$log"; then
    set -- load-fail - 0 0
    note=$(sed -n 's/^LIBTEST-LOAD-FAIL //p' "$log" | head -1)
  else
    # shellcheck disable=SC2046
    set -- $(judge "$log")
    # An abnormal exit with no marker is an error however the output reads.
    if [ "$code" -ne 0 ] && [ "$1" = "pass" ]; then set -- error "$2" "$3" "$4"; fi
    note=$(sed -n 's/^LIBTEST-ERROR //p' "$log" | head -1)
    [ -n "$note" ] || note=$(sed -n 's/^; Debugger entered on //p' "$log" | head -1)
  fi
  verdict=$1; framework=$2; passed=$3; failed=$4
  tsys=$(sed -n 's/^LIBTEST-TEST-SYSTEMS *//p; s/^LIBTEST-EXTRA-SYSTEM //p' "$log" | tr '\n' ' ')

  printf '  {"system": "%s", "checked": "%s", "verdict": "%s", "framework": "%s", "passed": %d, "failed": %d, "test-systems": "%s", "note": "%s"}\n' \
    "$sys" "$checked" "$verdict" "$framework" "$passed" "$failed" "$(jstr "$tsys")" "$(jstr "${note:-}")" >> "$work/entries"
  write_json
  ran=$((ran + 1))

  printf '%3d %-28s %-9s %-10s %5d pass %4d fail %4ds %s\n' \
    "$ran" "$sys" "$verdict" "$framework" "$passed" "$failed" "$elapsed" "$(jstr "${note:-}")"
done

finished=$(date +%s)
echo ""
echo "run-tests: $ran systems in $((finished - started))s ($skipped already recorded) -> $out"
for s in pass fail error no-result timeout load-fail; do
  printf '  %-10s %s\n' "$s" "$(grep -c "\"verdict\": \"$s\"" "$work/entries" || true)"
done
echo "  logs       $logdir"
