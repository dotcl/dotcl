;;; framework.lisp — Lightweight deftest framework
;;;
;;; Loaded by the dotcl-specific test harnesses before their test bodies:
;;;   test/regression/run.lisp   (make test-regression)
;;;   test/mop-protocol.lisp     (make test-mop)

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)
(defvar *fail-names* nil)

(defmacro deftest (name form &rest expected)
  (let ((result-var (gensym "R"))
        (expected-var (gensym "E")))
    `(let ((,result-var (multiple-value-list ,form))
           (,expected-var '(,@expected)))
       (incf *test-count*)
       (if (equal ,result-var ,expected-var)
           (incf *pass-count*)
           (progn
             (incf *fail-count*)
             (push ',name *fail-names*)
             (print (list 'FAIL ',name))
             (print (list 'EXPECTED ,expected-var))
             (print (list 'GOT ,result-var)))))))

;;; Some tests assert a COMPILER behaviour rather than a language behaviour: a
;;; compile-time warning, a compile-time type error, the IL size limit, or the
;;; refusal to generate code. The tree-walk interpreter does none of those
;;; things — it has no compile phase to diagnose from — so on those forms its
;;; answer is legitimately different, not wrong. Mark such tests with this so
;;; the suite can also be run as a gate on the emit-free evaluator:
;;;
;;;   dotnet run ... -- --asm compiler/cil-out.sil \
;;;     --eval '(setq dotcl:*evaluator-mode* :interpret)' test/regression/run.lisp
;;;
;;; Skipping is the point: a compile-time-diagnostic test left in place under
;;; :interpret either fails, or — worse, for the ones asserting that a real type
;;; declaration stays QUIET — passes vacuously, because nothing was analysed.
;;; The predicate matches Runtime.UseInterpreter (by symbol name, so 'INTERPRET
;;; and :INTERPRET both count).
;;; Two ways to end up without a compiler, and both must skip:
;;;   * *EVALUATOR-MODE* is :INTERPRET on an ordinary build
;;;   * the build has no Reflection.Emit at all (netstandard2.0 / wasm /
;;;     -p:DotclNoEmit=true). There *EVALUATOR-MODE* still reads :COMPILE —
;;;     nothing rebinds it — so testing it alone let every compile-time test run
;;;     on the one build that can never satisfy them. :DOTCL-EMIT is the feature
;;;     that answers the question directly.
(defmacro deftest-compiled-only (name form &rest expected)
  `(unless (or (and (symbolp dotcl:*evaluator-mode*)
                    (string= (symbol-name dotcl:*evaluator-mode*) "INTERPRET"))
               (not (find :dotcl-emit *features*)))
     (deftest ,name ,form ,@expected)))

(defmacro do-tests-summary ()
  '(progn
     (print (list *pass-count* 'PASSED *fail-count* 'FAILED
                  'OF *test-count* 'TOTAL))
     (if (= *fail-count* 0)
         (print 'ALL-TESTS-PASSED)
         (progn (print (list 'FAILED-TESTS *fail-names*))))))

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
