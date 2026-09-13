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
;;; True when the image compiles what it runs. The consing assertions and the
;;; measurements that feed them are statements about emitted code, so both are
;;; skipped in the tree-walk and emit-free builds -- a measurement loop left
;;; running there is thousands of interpreted iterations for a number nobody
;;; looks at.
(defun compiled-mode-p ()
  (not (or (and (symbolp dotcl:*evaluator-mode*)
                (string= (symbol-name dotcl:*evaluator-mode*) "INTERPRET"))
           (not (find :dotcl-emit *features*)))))

(defmacro deftest-compiled-only (name form &rest expected)
  `(when (compiled-mode-p)
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
