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
#   no-tests    the run finished and there was nothing to run: TEST-OP has no
#               test system among its dependencies and no PERFORM method of
#               its own (only ASDF's empty default), or hu.dwim.asdf said "No
#               tests were run". Like no-result, never counted as a pass.
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
#   LIBRARY_STATUS_ALLOW_STALE_DIST=1  measure even if the dotcl dist is older
#                                  than the published one (see dists.sh)
#   LIBRARY_STATUS_CONFIG          Release (default) or Debug: which build of
#                                  the runtime measures (see runtime.sh)
#
#        run-tests.sh --judge LOG...
#                                  judge logs already written (by this script,
#                                  or by the same driver on another Lisp) and
#                                  print "LOG VERDICT FRAMEWORK PASSED FAILED"
#                                  per log; nothing is run
set -eu

mode=run
if [ "${1:-}" = "--judge" ]; then mode=judge; shift; fi
root="${1:-.}"
# Note filters (scrub_note), shared with the other stage.
. "$(dirname "$0")/scrub.sh"
. "$(dirname "$0")/dists.sh"
here="$root/bench/library-status"
results="${LIBRARY_STATUS_JSON:-$here/results.json}"
targets="$here/targets.txt"
out="${LIBRARY_STATUS_TESTS_JSON:-$here/tests.json}"
logdir="${LIBRARY_STATUS_LOGDIR:-$root/out/library-status-tests}"
per="${LIBRARY_STATUS_TEST_TIMEOUT:-900}"
limit="${LIMIT:-0}"
resume="${LIBRARY_STATUS_RESUME:-0}"

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
    # Fold what is counted only at the end of a region (per-test lines,
    # last-result lines) into the totals, and clear it.
    function settle(  k) {
      if (fiasco_ok > 0 || fiasco_fail > 0) {
        fw["fiasco"]=1; pass += fiasco_ok; fail += fiasco_fail }
      if (lift_seen) { fw["lift"]=1; pass += lift_pass; fail += lift_fail }
      if (fp_seen && !("fiveam" in fw)) { fw["fiveam"]=1; pass += fp_pass; fail += fp_fail }
      for (k in lr_seen) if (!(k in fw)) { fw[k]=1; pass += lr_pass[k]; fail += lr_fail[k] }
      if (last_seen && !("stefil" in fw)) {
        fw["stefil"]=1; pass += last_pass; fail += last_fail }
      fiasco_ok = fiasco_fail = lift_seen = fp_seen = fp_pass = fp_fail = last_seen = 0
      for (k in lr_seen) delete lr_seen[k]
      for (k in lr_pass) delete lr_pass[k]
      for (k in lr_fail) delete lr_fail[k]
    }
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
    # 1am (lparallel bundles a copy): "Success: N tests, M checks." after
    # the run. A failing check signals an error instead of printing a
    # summary, which the driver reports as LIBTEST-ERROR.
    /^Success: [0-9]+ tests?, [0-9]+ checks?\./ { fw["1am"]=1; pass += $4; next }
    # The cl-ppcre harness (its own DO-TESTS): one "Test: NAME" line per suite, then a
    # "  NNN:" line per failing test in it, and at the end "All tests
    # passed." or "Some tests failed.". It prints no counts (the dots are
    # "one per ten tests"), so the unit here is the suite: a suite with a
    # failing test is a failure. Read only when that closing line is seen.
    # The WITH-TEST-SUITE harness of flexi-streams (and other libraries by
    # the same author) has the same shape with "Test suite: NAME" lines, and
    # reports a failing check as "Test FORM failed." (maybe with "signalling
    # error ..."), or "Expected condition TYPE not signalled.".
    /^Test: / { ppcre_suites++; ppcre_cur = 0; next }
    ppcre_suites && /^ *[0-9]+:$/ { if (!ppcre_cur) { ppcre_bad++; ppcre_cur = 1 } next }
    /^Test suite: / { wts_suites++; wts_cur = 0; next }
    wts_suites && (/^Test .* failed/ || /^Expected condition .* not signalled\.$/) {
      if (!wts_cur) { wts_bad++; wts_cur = 1 } next }
    /^(All tests passed|Some tests failed)\.$/ {
      if (ppcre_suites) {
        fw["cl-ppcre"]=1; pass += ppcre_suites - ppcre_bad; fail += ppcre_bad
        if ($1 == "Some" && ppcre_bad == 0) fail += 1
        ppcre_suites = 0; ppcre_bad = 0 }
      if (wts_suites) {
        fw["with-test-suite"]=1; pass += wts_suites - wts_bad; fail += wts_bad
        if ($1 == "Some" && wts_bad == 0) fail += 1
        wts_suites = 0; wts_bad = 0 }
      next }
    # lisp-unit2: "Test Summary for :NAME (N tests 0.12 sec)" (or without the
    # "for :NAME" part), then one "  | N WHAT" line per count. The counts read
    # are "passed", "failed" and "execution errors" (a test that signalled,
    # counted as a failure). "warnings", "empty" and "missing tests" are
    # neither. The block ends at the first line that is not a "|" line.
    /^[[:space:]]*Test Summary (for .* )?\([0-9]+ tests? [0-9.]+ sec\)/ { fw["lisp-unit2"]=1; in_lu2 = 1; next }
    in_lu2 && /^[[:space:]]*\| [0-9]+ passed[[:space:]]*$/ { pass += $2; next }
    in_lu2 && /^[[:space:]]*\| [0-9]+ failed[[:space:]]*$/ { fail += $2; next }
    in_lu2 && /^[[:space:]]*\| [0-9]+ execution errors?[[:space:]]*$/ { fail += $2; next }
    in_lu2 && /^[[:space:]]*\|/ { next }
    in_lu2 { in_lu2 = 0 }
    # lisp-unit (the first one): "Unit Test Summary", then " | N assertions
    # total", " | P passed", " | F failed", " | E execution errors" and
    # " | M missing tests". The unit is the assertion; an execution error is
    # a test that signalled, counted as a failure. A missing test is a name
    # asked for that has no test, which is neither.
    /^[[:space:]]*Unit Test Summary[[:space:]]*$/ { fw["lisp-unit"]=1; in_lu1 = 1; next }
    in_lu1 && /^[[:space:]]*\| [0-9]+ passed[[:space:]]*$/ { pass += $2; next }
    in_lu1 && /^[[:space:]]*\| [0-9]+ failed[[:space:]]*$/ { fail += $2; next }
    in_lu1 && /^[[:space:]]*\| [0-9]+ execution errors?[[:space:]]*$/ { fail += $2; next }
    in_lu1 && /^[[:space:]]*\|/ { next }
    in_lu1 { in_lu1 = 0 }
    # lift: "Test Report for SUITE: N tests run, all passed!" or
    # "...: N tests run, F Failures." (", E Errors." may follow). The unit is
    # the test; a failure or an error is a failing test. Lift can print the
    # same report more than once (the run, then DESCRIBE of its result), so
    # the last report is the one counted, at END.
    # The report can follow the "Start: SUITE" progress line without a newline.
    /Test Report for .*: [0-9]+ tests? run, / {
      s = $0; sub(/.*Test Report for /, "", s); sub(/.*: /, "", s); n = s; sub(/ .*/, "", n); f = 0
      rest = s
      while (match(rest, /[0-9]+ (Failures?|Errors?)/)) {
        c = substr(rest, RSTART, RLENGTH); sub(/ .*/, "", c); f += c
        rest = substr(rest, RSTART + RLENGTH) }
      lift_seen = 1; lift_pass = n - f; lift_fail = f; next }
    # ptester (cl-base64, puri): "Errors detected in this test: E" and
    # "Successes this test:S" after each "Begin NAME test".
    /^Errors detected in this test: *[0-9]+/ { fw["ptester"]=1; s = $0; sub(/.*: */, "", s); fail += s + 0; next }
    /^Successes this test: *[0-9]+/ { fw["ptester"]=1; s = $0; sub(/.*: */, "", s); pass += s + 0; next }
    # hunchentoot: its confidence script (WITH-SCRIPT-CONTEXT) prints one line when
    # it is done, ";; all tests PASSED" or ";; N assertion(s) FAILED". It counts
    # no passing checks, so a pass is counted as 1: the script, not a check.
    /^;; all tests PASSED$/ { fw["hunchentoot"]=1; pass += 1; next }
    /^;; [0-9]+ assertions? FAILED$/ { fw["hunchentoot"]=1; fail += $2; next }
    # unit-test (cl-fad, yason): "Running "NAME" ..." and "=> P passed." per
    # test, then a line of "=" and "N tests total." with one indented line
    # for each non-zero count: "P passed.", "F failed.", "C crashed.". The
    # unit is the test; a crash is a test that signalled, counted as a
    # failure.
    /^[0-9]+ tests? total\.$/ { fw["unit-test"]=1; in_ut = 1; next }
    in_ut && /^[[:space:]]+[0-9]+ passed\.$/ { pass += $1; next }
    in_ut && /^[[:space:]]+[0-9]+ (failed|crashed)\.$/ { fail += $1; next }
    in_ut { in_ut = 0 }
    # should-test (rutils): "Test NAME: " when a test starts, then "  OK" or
    # "  FAILED" at the end of a line when it is done (output of the test,
    # such as a failure report, can come in between). No summary is printed.
    /^Test [^ :]+: / { st_open = 1 }
    st_open && /  OK$/ { fw["should-test"]=1; pass++; st_open = 0; next }
    st_open && /  FAILED$/ { fw["should-test"]=1; fail++; st_open = 0; next }
    # fiveam without a summary: RUN (rather than RUN!) prints only progress,
    # "Running test NAME " followed by one character per check: "." passed,
    # "f" failed, "X" signalled, "s" skipped. Counted at END, and only when no
    # "Did N checks." summary was printed. A line where the test printed
    # something of its own after the name is not read.
    /^[[:space:]]*Running test [^ ]+ [.fXs]*[[:space:]]*$/ {
      s = $NF; if (s !~ /^[.fXs]+$/) next
      fp_seen = 1; fp_pass += gsub(/\./, "", s); fp_fail += gsub(/[fX]/, "", s); next }
    # What the driver printed from the result a framework keeps (LIFT,
    # clunit): "LIBTEST-LAST-RESULT FRAMEWORK PASSED FAILED". Used at END
    # only when the output had no summary of that framework.
    /^LIBTEST-LAST-RESULT [a-z0-9]+ [0-9]+ [0-9]+$/ {
      lr_seen[$2] = 1; lr_pass[$2] += $3; lr_fail[$2] += $4; next }
    # Nothing to run (see the driver), or hu.dwim.asdf naming a test system
    # that does not exist.
    /^LIBTEST-NO-TEST-OP/ { notests = 1; next }
    /^WARNING: No tests were run; system: / { notests = 1; next }
    /^LIBTEST-ERROR/ { err = 1 }
    /^; Debugger entered on/ { err = 1 }
    /^LIBTEST-END/ { ended = 1 }
    # The driver loads the test systems after LIBTEST-BEGIN and prints
    # LIBTEST-TEST-OP before it runs the test op. A suite that runs while its
    # system loads and again from the test op would otherwise be counted
    # twice, so what was recognised while loading is set aside here and used
    # only when the test op itself printed nothing a recogniser knows.
    /^LIBTEST-TEST-OP$/ {
      settle()
      for (k in fw) { lfw[k] = 1; delete fw[k] }
      lpass = pass; lfail = fail; pass = 0; fail = 0
      in_para = in_clunit = in_lu2 = in_lu1 = in_ut = st_open = 0
      ppcre_suites = ppcre_bad = wts_suites = wts_bad = 0
      next }
    END {
      settle()
      n = 0; for (k in fw) n++
      if (n == 0) { for (k in lfw) fw[k] = 1; pass = lpass; fail = lfail }
      names = ""
      for (k in fw) names = names (names == "" ? "" : "+") k
      if (names == "") names = "-"
      if (fail > 0) v = "fail"
      else if (names != "-" && pass > 0 && !err && ended) v = "pass"
      else if (err || !ended) v = "error"
      else if (notests) v = "no-tests"
      else v = "no-result"
      printf "%s %s %d %d\n", v, names, pass, fail
    }'
}

if [ "$mode" = judge ]; then
  for log in "$@"; do printf '%s %s\n' "$log" "$(judge "$log")"; done
  exit 0
fi

# The Release build by default, and refused when older than the sources: see
# runtime.sh.
. "$(dirname "$0")/runtime.sh"
select_runtime run-tests
# GNU timeout bounds each run. macOS has none by default; Homebrew coreutils
# installs it as gtimeout. Without either, every run would fail with 127 and
# be recorded as a library failure, so stop here instead.
if command -v timeout >/dev/null 2>&1; then timeout=timeout
elif command -v gtimeout >/dev/null 2>&1; then timeout=gtimeout
else echo "run-tests: needs GNU timeout (coreutils; gtimeout on macOS)" >&2; exit 1
fi

check_dists run-tests
checked=$(date +%Y-%m-%d)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$logdir"

jstr() {
  printf '%s' "$1" \
    | tr -d '"\\' | tr '\n\r\t' '   ' \
    | sed 's|^[. ]*||; s|  *| |g; s| *$||' \
    | scrub_note \
    | cut -c1-160
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
;; A test system's own files are loaded after LIBTEST-BEGIN, not here: many
;; test systems run their suite while loading (a RUN! at top level), and the
;; judge reads nothing printed before the marker. Here only what the test
;; system depends on is fetched and loaded, so that what follows the marker is
;; the test system and its tests. A test system whose definition cannot be
;; read before its dependencies are on disk is loaded whole here, as before.
(defun libtest-load-dependencies (name)
  "Quickload what system NAME depends on. True if that worked."
  (ignore-errors
   (let ((ql (ql-dist:find-system name)))
     (when ql (ql-dist:ensure-installed ql)))
   (let ((system (asdf:find-system name nil)))
     (when system
       (dolist (dep (asdf:system-depends-on system) t)
         (let ((d (ignore-errors (asdf/find-component:resolve-dependency-spec system dep))))
           (when d (ql:quickload (asdf:component-name d)))))))))
;; Each entry is (NAME . REQUIRED): a test system TEST-OP depends on is
;; required, a guessed one (below) is not.
(defvar *libtest-deferred* '())
(defun libtest-prepare (name required)
  "Load NAME now if its dependencies cannot be loaded apart from it, else
remember it to load after LIBTEST-BEGIN."
  (cond ((assoc name *libtest-deferred* :test #'string=))
        ((libtest-load-dependencies name)
         (setf *libtest-deferred*
               (append *libtest-deferred* (list (cons name required)))))
        (t (ql:quickload name))))
(let ((names (handler-case (libtest-test-systems "$sys")
               (error (e) (libtest-die "LIBTEST-LOAD-FAIL" e 3)))))
  (format t "~&LIBTEST-TEST-SYSTEMS ~{~a~^ ~}~%" names)
  (dolist (name names)
    (handler-case (libtest-prepare name t)
      (error (e) (libtest-die "LIBTEST-LOAD-FAIL" e 3)))))
;; Some TEST-OPs load their test system by calling OPERATE from inside the
;; PERFORM method instead of naming it as a dependency (babel does), which
;; Quicklisp never sees. Fetch the conventionally named test systems the dists
;; know about, so their dependencies are on disk when that happens. These are
;; guesses, so a failure here is only noted: if the TEST-OP really needs the
;; system, the run fails on its own.
(dolist (name '("$sys/test" "$sys/tests" "$sys-test" "$sys-tests"))
  (when (ql-dist:find-system name)
    (handler-case (progn (libtest-prepare name nil)
                         (format t "~&LIBTEST-EXTRA-SYSTEM ~a~%" name))
      (error (e) (format t "~&LIBTEST-NOTE ~a did not load: ~a~%" name e)))))
;; The markers start with ~% rather than ~&: the judge only reads a marker at the
;; start of a line, and a FRESH-LINE that misjudges the column (after PRINT, on
;; some streams) would glue it to the test output and lose the result.
(format t "~%LIBTEST-BEGIN~%")
(loop for (name . required) in *libtest-deferred*
      do (handler-case (ql:quickload name)
           (error (e)
             (if required
                 (libtest-die "LIBTEST-LOAD-FAIL" e 3)
                 (format t "~&LIBTEST-NOTE ~a did not load: ~a~%" name e)))))
;; A TEST-OP with nothing to do: no test system among its dependencies and
;; no PERFORM method of the library's own, only ASDF's empty default one.
;; Marked so the judge can tell "defines no tests" from "ran, but printed
;; nothing a recogniser knows". Printed after LIBTEST-BEGIN, since the judge
;; reads nothing before it.
(let* ((system (asdf:find-system "$sys"))
       (specs (find-symbol "METHOD-SPECIALIZERS" "DOTCL-MOP"))
       (op-class (find-class 'asdf:test-op))
       (own (remove-if-not
             (lambda (m)
               (destructuring-bind (o c &rest r) (funcall specs m)
                 (declare (ignore r))
                 (and (or (not (typep o 'class)) (subtypep o op-class))
                      (not (and (eq o op-class)
                                (eq c (find-class 'asdf:component)))))))
             (compute-applicable-methods
              #'asdf:perform (list (asdf:make-operation 'asdf:test-op) system)))))
  (when (and specs (null own) (null (libtest-test-systems "$sys")))
    (format t "~%LIBTEST-NO-TEST-OP~%")))
(finish-output)
;; An error nothing handles ends the run through the debugger hook rather
;; than a HANDLER-CASE around the call: a handler here would take every error
;; before the debugger is reached, and a library that tests its own debugger
;; handling (trivial-custom-debugger signals an error inside WITH-DEBUGGER and
;; expects its hook to see it) could never pass.
(format t "~%LIBTEST-TEST-OP~%")
(finish-output)
(let ((*debugger-hook* (lambda (c hook)
                         (declare (ignore hook))
                         (libtest-die "LIBTEST-ERROR" c 2))))
  (asdf:test-system "$sys"))
;; stefil keeps the result of the last suite run in *LAST-TEST-RESULT*. A test
;; op that does not print it leaves only progress characters, so print it here.
(dolist (p '("HU.DWIM.STEFIL" "STEFIL"))
  (let ((s (and (find-package p) (find-symbol "*LAST-TEST-RESULT*" p))))
    (when (and s (boundp s) (symbol-value s))
      (format t "~%LIBTEST-STEFIL-RESULT ~a~%" (symbol-value s)))))
;; LIFT, clunit and clunit2 also keep the last result, and a suite run while
;; loading or from a test op that drops the result prints only progress. Print
;; the counts it holds as "LIBTEST-LAST-RESULT FRAMEWORK PASSED FAILED"; the
;; judge uses them only when the output had no summary of that framework. A
;; LIFT error, and a clunit error (an assertion that signalled), is a failure.
(flet ((sym (name package)
         (let ((p (find-package package)))
           (and p (find-symbol name p))))
       (len (x) (if (listp x) (length x) 0)))
  (let* ((s (sym "*TEST-RESULT*" "LIFT"))
         (r (and s (boundp s) (symbol-value s))))
    (when r
      (ignore-errors
       (let ((run (len (funcall (sym "TESTS-RUN" "LIFT") r)))
             (bad (+ (len (funcall (sym "FAILURES" "LIFT") r))
                     (len (funcall (sym "ERRORS" "LIFT") r)))))
         (format t "~%LIBTEST-LAST-RESULT lift ~d ~d~%" (max 0 (- run bad)) bad)))))
  (dolist (p '("CLUNIT" "CLUNIT2"))
    (let* ((s (sym "*LAST-CLUNIT-REPORT*" p))
           (r (and s (boundp s) (symbol-value s))))
      (when r
        (ignore-errors
         (format t "~%LIBTEST-LAST-RESULT clunit ~d ~d~%"
                 (slot-value r (sym "PASSED" p))
                 (+ (slot-value r (sym "FAILED" p))
                    (slot-value r (sym "ERRORS" p)))))))))
(format t "~%LIBTEST-END~%")
(finish-output)
(dotcl:quit 0)
DRVEOF

  # A slash in a system name ("foo/test") is not a directory here.
  log="$logdir/$(printf '%s' "$sys" | tr / _).txt"
  t0=$(date +%s)
  set +e
  "$timeout" "$per" "$exe" --core "$core" "$work/drv.lisp" > "$log" 2>&1 < /dev/null
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

  printf '  {"system": "%s", "checked": "%s", "verdict": "%s", "framework": "%s", "passed": %d, "failed": %d, "test-systems": "%s", "note": "%s", "dists": "%s", "runtime": "%s", "seconds": %d}\n' \
    "$sys" "$checked" "$verdict" "$framework" "$passed" "$failed" "$(jstr "$tsys")" "$(jstr "${note:-}")" "$dists_desc" "$runtime_desc" "$elapsed" >> "$work/entries"
  write_json
  ran=$((ran + 1))

  printf '%3d %-28s %-9s %-10s %5d pass %4d fail %4ds %s\n' \
    "$ran" "$sys" "$verdict" "$framework" "$passed" "$failed" "$elapsed" "$(jstr "${note:-}")"
done

finished=$(date +%s)
echo ""
echo "run-tests: $ran systems in $((finished - started))s ($skipped already recorded) -> $out"
for s in pass fail error no-result no-tests timeout load-fail; do
  printf '  %-10s %s\n' "$s" "$(grep -c "\"verdict\": \"$s\"" "$work/entries" || true)"
done
echo "  logs       $logdir"
