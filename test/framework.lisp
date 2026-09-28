;;; framework.lisp: Lightweight deftest framework
;;;
;;; Loaded by the dotcl-specific test harnesses before their test bodies:
;;;   test/regression/run.lisp   (make test-regression)
;;;   test/mop-protocol.lisp     (make test-mop)

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)
(defvar *fail-names* nil)

;;; Bound to NIL around a test whose subject IS the unhandled path -- one that
;;; needs *DEBUGGER-HOOK* to run, which cannot happen while an outer handler is
;;; in scope. Such a test keeps the old hazard (an unexpected error in it takes
;;; the run down), and that is inherent: it is asking what happens when nothing
;;; handles the condition.
(defvar *deftest-contain-errors* t)

;;; A test records its outcome; it never takes the run down with it.
;;;
;;; A value mismatch was always recorded and the run continued. An ERROR was
;;; not: it propagated out of the form, out of the LOAD, and out of the whole
;;; run, so every test after it silently did not run and no FAIL line named the
;;; one that did it -- the output was a debugger banner and nothing else. That
;;; asymmetry cost this project time twice in one night, once in each suite.
;;;
;;; The handler covers the FORM only, not the comparison or the printing, so a
;;; bug in the harness still surfaces as a harness bug rather than as a test
;;; failure. ERROR and not CONDITION: a WARNING must stay a warning, and a
;;; non-error condition signalled with SIGNAL does not unwind anything anyway.
;;;
;;; Tests that expect an error are unaffected -- every one of them wraps the
;;; error INSIDE the form (HANDLER-CASE, HANDLER-BIND, SIGNALS-ERROR and the
;;; file-local helpers), so the handler here never sees it.
;;;
;;; HANDLER-CASE unwinds, so an UNWIND-PROTECT inside the form still runs, and
;;; the paired top-level SETFs that several files bracket themselves with are
;;; now more likely to be restored, not less: the old behaviour could leave
;;; global state set and then skip the very tests that would have noticed.
(defmacro deftest (name form &rest expected)
  (let ((result-var (gensym "R"))
        (expected-var (gensym "E"))
        (cond-var (gensym "C"))
        (caught-var (gensym "CAUGHT"))
        (run-var (gensym "RUN")))
    `(let ((,expected-var '(,@expected))
           (,result-var nil)
           (,cond-var nil))
       (incf *test-count*)
       (flet ((,run-var () (multiple-value-list ,form)))
         (if *deftest-contain-errors*
             (handler-case (setq ,result-var (,run-var))
               (error (,caught-var) (setq ,cond-var ,caught-var)))
             (setq ,result-var (,run-var))))
       (cond (,cond-var
              (incf *fail-count*)
              (push ',name *fail-names*)
              (print (list 'FAIL ',name))
              (print (list 'EXPECTED ,expected-var))
              (print (list 'ERROR (type-of ,cond-var)
                           (princ-to-string ,cond-var))))
             ((equal ,result-var ,expected-var)
              (incf *pass-count*))
             (t
              (incf *fail-count*)
              (push ',name *fail-names*)
              (print (list 'FAIL ',name))
              (print (list 'EXPECTED ,expected-var))
              (print (list 'GOT ,result-var)))))))

;;; Some tests assert a COMPILER behaviour rather than a language behaviour: a
;;; compile-time warning, a compile-time type error, the IL size limit, or the
;;; refusal to generate code. The tree-walk interpreter does none of those
;;; things, it has no compile phase to diagnose from, so on those forms its
;;; answer is legitimately different, not wrong. Mark such tests with this so
;;; the suite can also be run as a gate on the emit-free evaluator:
;;;
;;;   dotnet run ... -- --asm compiler/cil-out.sil \
;;;     --eval '(setq dotcl:*evaluator-mode* :interpret)' test/regression/run.lisp
;;;
;;; Skipping is the point: a compile-time-diagnostic test left in place under
;;; :interpret either fails, or, worse, for the ones asserting that a real type
;;; declaration stays QUIET, passes vacuously, because nothing was analysed.
;;; The predicate matches Runtime.UseInterpreter (by symbol name, so 'INTERPRET
;;; and :INTERPRET both count).
;;; Two ways to end up without a compiler, and both must skip:
;;;   * *EVALUATOR-MODE* is :INTERPRET on an ordinary build
;;;   * the build has no Reflection.Emit at all (netstandard2.0 / wasm /
;;;     -p:DotclNoEmit=true). There *EVALUATOR-MODE* still reads :COMPILE,
;;;     nothing rebinds it, so testing it alone let every compile-time test run
;;;     on the one build that can never satisfy them. :DOTCL-EMIT is the feature
;;;     that answers the question directly.
;;; True when the image compiles what it runs. The consing assertions and the
;;; measurements that feed them are statements about emitted code, so both are
;;; skipped in the tree-walk and emit-free builds -- a measurement loop left
;;; running there is thousands of interpreted iterations for a number nobody
;;; looks at.
;;; True when *EVALUATOR-MODE* is :INTERPRET on a build that has an emitter:
;;; the tree-walk mode of `make test-regression-interp'. It is false on the
;;; emit-free build, where *EVALUATOR-MODE* still reads :COMPILE.
(defun interpret-mode-p ()
  (and (symbolp dotcl:*evaluator-mode*)
       (string= (symbol-name dotcl:*evaluator-mode*) "INTERPRET")
       t))

(defun compiled-mode-p ()
  (not (or (interpret-mode-p)
           (not (find :dotcl-emit *features*)))))

(defmacro deftest-compiled-only (name form &rest expected)
  `(when (compiled-mode-p)
     (deftest ,name ,form ,@expected)))

;;; True when the image has an emitter at all, whatever *EVALUATOR-MODE* says.
;;; The distinction from COMPILED-MODE-P matters for the tests that read back
;;; emitted code with DOTCL:FUNCTION-SIL: DEFUN goes through the compiler even
;;; under :INTERPRET (only the surrounding EVAL is a tree walk), so the SIL is
;;; there and those assertions still mean something. On an emit-free build
;;; nothing is emitted, FUNCTION-SIL answers NIL, and every count taken from it
;;; is 0 -- which makes "this instruction is gone" pass for the wrong reason and
;;; "this slot is native" fail for the wrong reason.
(defun emitting-mode-p ()
  (and (find :dotcl-emit *features*) t))

(defmacro deftest-emitting-only (name form &rest expected)
  `(when (emitting-mode-p)
     (deftest ,name ,form ,@expected)))


;;; A test whose subject is a runtime (C#) function and whose cost is large.
;;;
;;; Running such a test in every evaluator mode buys nothing: the function, its
;;; frame size and the 256 MB stack it runs on are the same in all of them, so
;;; the size that makes the test meaningful is the same too. Saying it three
;;; times costs minutes and adds no coverage.
;;;
;;; Distinct from DEFTEST-COMPILED-ONLY, which skips the other modes because the
;;; assertion is about emitted code and cannot hold there. Here it would hold --
;;; it just takes minutes to hold again.
;;;
;;; Only for the expensive assertion. The cheap ones in the same file stay in
;;; every mode, where they cost nothing and catch mode-specific surprises.
(defmacro deftest-runtime-once (name form &rest expected)
  `(when (compiled-mode-p)
     (deftest ,name ,form ,@expected)))

;;; --- Smaller loop counts on the emit-free build ---
;;;
;;; On an emit-free build even LOAD goes through the tree-walk interpreter, so a
;;; loop written in a test file runs interpreted, tens of thousands of times
;;; slower than in the other two runs. A count chosen for what it shows in
;;; compiled code (a million tail calls that must not grow the stack there) can
;;; therefore cost a minute on that build while showing nothing more than a far
;;; smaller count would.
;;;
;;; (EMIT-FREE-SCALE FULL REDUCED) answers FULL when the image has an emitter and
;;; REDUCED on an emit-free build. Use it only where the large count means
;;; something in compiled code and the reduced one still reaches everything the
;;; test is about when interpreted (say, values past the Fixnum cache). A count
;;; that means nothing in any mode is simply made smaller everywhere instead,
;;; and a count that provokes a race or proves constant stack is left alone.
;;;
;;; Like the file skips above, this is an economy, so it is counted:
;;; EMIT-FREE-SCALE-SUMMARY prints how many times the reduced count was chosen.
;;; Outside the emit-free build that number must read 0.

(defvar *emit-free-scale-reduced* 0)

(defun emit-free-scale (full reduced)
  (if (emitting-mode-p)
      full
      (progn (incf *emit-free-scale-reduced*) reduced)))

(defun emit-free-scale-summary ()
  (print (list 'EMIT-FREE-SCALE
               (if (emitting-mode-p) 'EMITTING 'EMIT-FREE)
               'REDUCED *emit-free-scale-reduced*))
  t)
;;; --- Files skipped in the interpret mode ---
;;;
;;; The regression suite runs three times: compiled, with *EVALUATOR-MODE*
;;; :INTERPRET, and on an emit-free build. The interpret mode starts from the
;;; same --asm image as the compiled run and LOAD still compiles each top-level
;;; form there; only an explicit EVAL reaches the tree-walk interpreter. A file
;;; whose subject does not depend on the evaluator (a runtime (C#) function, a
;;; child process, a compiler optimisation, or a file that binds
;;; *EVALUATOR-MODE* and runs both evaluators itself) therefore repeats the
;;; compiled run there and adds no coverage.
;;;
;;; LOAD-SKIP-UNDER-INTERP loads such a file in every mode except the interpret
;;; mode (INTERPRET-MODE-P). The emit-free build still loads it: that run is
;;; also the only one that starts the suite from --core dotcl.core on a
;;; DotclNoEmit build, and a file that does not care about the evaluator can
;;; still fail there for that reason.
;;;
;;; Three different reasons to skip, kept apart on purpose:
;;;   * DEFTEST-COMPILED-ONLY / DEFTEST-EMITTING-ONLY: the assertion does not
;;;     hold in the other modes (it is about emitted code).
;;;   * (WHEN (FIND :DOTCL-EMIT *FEATURES*) (LOAD ...)) in run.lisp: the file
;;;     cannot run at all without an emitter.
;;;   * LOAD-SKIP-UNDER-INTERP (this) and DEFTEST-RUNTIME-ONCE: the assertions
;;;     would hold, running them again only costs time.
;;; Only the last kind is an economy, and it stays countable so that it can be
;;; undone file by file.
;;;
;;; A skip must not be silent. SKIP-UNDER-INTERP-SUMMARY prints, at the end of
;;; every run, how many marked files were loaded and how many were skipped;
;;; outside the interpret mode the skipped count is printed too, and must read
;;; 0. For a skipped file the number of tests is estimated from its source: the
;;; DEFTEST and DEFTEST-EMITTING-ONLY forms in it. The estimate misses tests
;;; that a file makes with its own macro or a loop, and counts a DEFTEST
;;; written inside a string, so the other runs print the exact number of tests
;;; the marked files ran next to the same estimate, and the gap can be seen.

(defvar *skip-under-interp-loaded-files* 0)
(defvar *skip-under-interp-loaded-tests* 0)
(defvar *skip-under-interp-loaded-source-tests* 0)
(defvar *skip-under-interp-skipped-files* 0)
(defvar *skip-under-interp-skipped-source-tests* 0)

;;; Count the DEFTEST forms in the text of PATH that would run in the current
;;; mode. A scan of the text, line by line, not a READ: reading would need the
;;; file's own packages and reader macros, which exist only once it has been
;;; loaded. It also has to stay cheap on the emit-free build, where this
;;; function itself is interpreted, so the per-character work is left to
;;; SEARCH. A ";" ends the line for this purpose, even inside a string, which
;;; can only make the estimate smaller.
(defun %deftest-delimiter-p (text index)
  (or (>= index (length text))
      (member (char text index) '(#\Space #\Tab #\Return #\())))

(defun %count-deftests-in-line (line emitting)
  (let* ((semi (position #\; line))
         (code (if semi (subseq line 0 semi) line))
         (count 0)
         (start 0))
    (loop
      (let ((hit (search "(deftest" code :start2 start :test #'char-equal)))
        (when (null hit) (return count))
        (let ((after (+ hit 8)))
          (cond ((%deftest-delimiter-p code after)
                 (incf count))
                ((and emitting
                      (let ((tail "-emitting-only"))
                        (and (<= (+ after (length tail)) (length code))
                             (string-equal tail code
                                           :start2 after
                                           :end2 (+ after (length tail)))
                             (%deftest-delimiter-p
                              code (+ after (length tail))))))
                 (incf count)))
          (setq start after))))))

(defun %count-source-deftests (path)
  (let ((emitting (emitting-mode-p))
        (count 0))
    (with-open-file (in path :direction :input)
      (loop
        (let ((line (read-line in nil nil)))
          (when (null line) (return count))
          (incf count (%count-deftests-in-line line emitting)))))))

(defun load-skip-under-interp (path)
  (if (interpret-mode-p)
      (progn
        (incf *skip-under-interp-skipped-files*)
        (incf *skip-under-interp-skipped-source-tests*
              (%count-source-deftests path)))
      (let ((before *test-count*))
        (load path)
        (incf *skip-under-interp-loaded-files*)
        (incf *skip-under-interp-loaded-tests* (- *test-count* before))
        (incf *skip-under-interp-loaded-source-tests*
              (%count-source-deftests path))))
  t)

(defun skip-under-interp-summary ()
  (if (interpret-mode-p)
      (print (list 'SKIP-UNDER-INTERP 'INTERPRET-MODE
                   'SKIPPED-FILES *skip-under-interp-skipped-files*
                   'DEFTESTS-IN-SOURCE *skip-under-interp-skipped-source-tests*
                   'LOADED-FILES *skip-under-interp-loaded-files*))
      (print (list 'SKIP-UNDER-INTERP 'NOT-INTERPRET-MODE
                   'LOADED-FILES *skip-under-interp-loaded-files*
                   'TESTS-RUN *skip-under-interp-loaded-tests*
                   'DEFTESTS-IN-SOURCE *skip-under-interp-loaded-source-tests*
                   'SKIPPED-FILES *skip-under-interp-skipped-files*)))
  t)
(defmacro do-tests-summary ()
  '(progn
     (print (list *pass-count* 'PASSED *fail-count* 'FAILED
                  'OF *test-count* 'TOTAL))
     (if (= *fail-count* 0)
         (print 'ALL-TESTS-PASSED)
         (progn (print (list 'FAILED-TESTS *fail-names*))))))

;;; --- Scratch files: one temp directory per test process ---
;;;
;;; The system temp directory is shared by every worktree on the machine, so a
;;; fixed name under it (say dotcl-packed-r2r/) is shared too: two worktrees
;;; running the suite at once overwrite each other's fixtures, or fail to copy
;;; onto a DLL the other run's child still has open. That failure names a test,
;;; a file and a line, and reads exactly like a real regression.
;;;
;;; Tests put scratch files under REGRESSION-TEMP-DIR instead of the system
;;; temp directory. It is created once per process, named after the process ID
;;; plus a random part, so no two runs share it whatever the tests call their
;;; files inside it. CLEANUP-REGRESSION-TEMP-DIR deletes it at the end of the
;;; run. A run that dies first, or a file still locked at that point, leaves it
;;; behind; the next run removes any such directory whose process is gone.
;;;
;;; The value has the shape the old call sites used for the temp directory:
;;; forward slashes and no trailing slash, so it drops in where
;;; (or (dotcl:getenv "TMPDIR") (dotcl:getenv "TEMP") "/tmp") used to be.

(defvar *regression-temp-dir* nil)

(defun %regression-temp-base ()
  (string-right-trim "/" (substitute #\/ (code-char 92)
                                     (dotnet:static "System.IO.Path" "GetTempPath"))))

(defun %regression-temp-owner-alive-p (name)
  "NAME is dotcl-regress-<pid>-<random>. True unless that process is known gone.
A name that does not parse is treated as alive, so it is never deleted."
  (let* ((start (length "dotcl-regress-"))
         (end (position #\- name :start start))
         (pid (and end (ignore-errors (parse-integer name :start start :end end)))))
    (or (null pid)
        (and (ignore-errors
              (dotnet:static "System.Diagnostics.Process" "GetProcessById" pid))
             t))))

(defun %reap-regression-temp-dirs (base)
  "Delete the per-process directories of runs that are no longer alive."
  (let ((dirs (ignore-errors
               (dotnet:static "System.IO.Directory" "GetDirectories" base
                              "dotcl-regress-*"))))
    (when dirs
      (dotimes (i (dotnet:invoke dirs "Length"))
        (let* ((dir (dotnet:invoke dirs "GetValue" i))
               (name (dotnet:static "System.IO.Path" "GetFileName" dir)))
          (unless (%regression-temp-owner-alive-p name)
            (ignore-errors
             (dotnet:static "System.IO.Directory" "Delete" dir t))))))))

(defun regression-temp-dir ()
  "This process's own scratch directory: forward slashes, no trailing slash."
  (or *regression-temp-dir*
      (let* ((base (%regression-temp-base))
             (dir (format nil "~a/dotcl-regress-~d-~a" base
                          (dotnet:static "System.Environment" "ProcessId")
                          (subseq (dotnet:invoke (dotnet:static "System.Guid" "NewGuid")
                                                 "ToString" "N")
                                  0 12))))
        (%reap-regression-temp-dirs base)
        (ensure-directories-exist (concatenate 'string dir "/"))
        (setf *regression-temp-dir* dir))))

(defun regression-temp-file (name)
  "NAME inside this process's scratch directory, as a string."
  (concatenate 'string (regression-temp-dir) "/" name))

(defun cleanup-regression-temp-dir ()
  "Delete this process's scratch directory, if one was made. Best effort."
  (when *regression-temp-dir*
    (ignore-errors
     (dotnet:static "System.IO.Directory" "Delete" *regression-temp-dir* t))
    (setf *regression-temp-dir* nil)))

;;; ansi-test compatibility helpers
(defun notnot (x) (not (not x)))
(defun eqt (x y) (notnot (eq x y)))
(defun eqlt (x y) (notnot (eql x y)))
(defun equalt (x y) (notnot (equal x y)))

;;; signals-error: returns T if form signals a condition of the given type
(defmacro signals-error (form condition-type)
  (let ((c (gensym "C")))
    `(handler-case (progn ,form nil)
       (,condition-type (,c) t))))

;;; --- Allocation measurement, for the consing assertions ---
;;;
;;; Element 4 of DOTCL:GC-STATS is a process-wide monotonic count of bytes
;;; allocated, so anything else alive in the image only ever adds to a sample.
;;; MIN of a few runs keeps the least-polluted one, and taking the DIFFERENCE
;;; between two loop lengths cancels whatever the harness itself costs.
;;;
;;; The counts are deliberately small. These assertions compare per-operation
;;; bounds of tens to hundreds of bytes, which the counter resolves at 10^4
;;; iterations as well as at 10^5 (per-op measured stable to 1% across
;;; 400000/100000, 40000/10000 and 20000/5000). Every permanent test costs CI
;;; time in three modes, so the larger counts bought nothing.
;;;
;;; FN takes an iteration count and runs its body that many times. Callers write
;;; the bound in bytes per operation, which is the number the design records
;;; quote, rather than as a product with the loop length.
(defun bytes-per-op (fn &optional (big 40000) (small 10000))
  (flet ((sample (n)
           (let ((best nil))
             (dotimes (r 3 best)
               (let ((before (nth 4 (dotcl:gc-stats))))
                 (funcall fn n)
                 (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
                   (when (or (null best) (< used best)) (setq best used))))))))
    (funcall fn 1000)                   ; warm: first-call JIT is not the subject
    (/ (- (sample big) (sample small)) (float (- big small)))))
