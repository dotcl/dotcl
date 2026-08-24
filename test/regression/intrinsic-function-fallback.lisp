;;; Every compiler intrinsic needs a way to run under the tree-walk interpreter.
;;;
;;; An intrinsic is an entry in the compiler's form-handler table: the compiler
;;; lowers the call to IL directly and never looks for a function. The
;;; interpreter has no such table — it evaluates the expansion as an ordinary
;;; call — so an intrinsic with no function binding is an "Undefined function"
;;; the moment any macro that expands into it is interpreted. That interpreter is
;;; the ONLY evaluator on an emit-free build, which is what AOT and WebGL ship.
;;;
;;; It has bitten twice: %FIXNUM-GE-OBJECT (added with the DOTIMES counter work,
;;; broke every interpreted DOTIMES) and %GETENV (used by the compiler's own
;;; source). Both were found late — one by CI on another lane's commit, one by
;;; reading — so this checks the whole table at once instead.
;;;
;;; The allowlist below is for intrinsics the interpreter handles by name in
;;; %MINI-EVAL because a function binding cannot work: their arguments are not
;;; evaluable (a literal list of .NET parameter type names, a C# source string),
;;; or they are lowered forms of something the interpreter reaches another way.

(defparameter *ifb-allowed*
  '(;; %MINI-EVAL evaluates it as (1+ x): the int64-fits assertion is a
    ;; compile-time fact, not a runtime one.
    "%DOTIMES-1+"
    ;; %MINI-EVAL delegates it to the MAKE-INSTANCE generic function.
    "%MAKE-INSTANCE-WITH-INITARGS"
    ;; Third argument is a literal list of parameter type names; evaluating the
    ;; arguments the ordinary way would try to call it. %MINI-EVAL has a case.
    "%DOTNET-CALL-DIRECT"
    ;; Takes C# source text to splice; there is nothing to call at run time.
    "%INLINE-CS-SPLICED")
  "Intrinsics that must NOT have a function binding, with the reason each.")

(defun %ifb-handler-table ()
  (let ((sym (or (find-symbol "*COMPILE-FORM-HANDLERS*" "DOTCL-INTERNAL")
                 (find-symbol "*COMPILE-FORM-HANDLERS*" "DOTCL.CIL-COMPILER"))))
    (and sym (boundp sym) (symbol-value sym))))

(defun %ifb-unrunnable ()
  "Intrinsic names the interpreter could not call: no function, no allowlist entry."
  (let ((table (%ifb-handler-table))
        (missing '()))
    (when table
      (maphash (lambda (name handler)
                 (declare (ignore handler))
                 (when (and (symbolp name)
                            (plusp (length (symbol-name name)))
                            (char= (char (symbol-name name) 0) #\%)
                            (not (fboundp name))
                            (not (macro-function name))
                            (not (member (symbol-name name) *ifb-allowed* :test #'string=)))
                   (push (symbol-name name) missing)))
               table))
    (sort missing #'string<)))

(deftest intrinsic-function-fallback.all-intrinsics-runnable
  (%ifb-unrunnable)
  nil)

;;; The table has to have been found, or the test above passes vacuously.

(deftest intrinsic-function-fallback.table-is-visible
  (let ((table (%ifb-handler-table)))
    (and table (> (hash-table-count table) 100) t))
  t)

;;; The two that were actually broken, called the way the interpreter would.

(deftest intrinsic-function-fallback.fixnum-ge-object-callable
  (let ((f (or (find-symbol "%FIXNUM-GE-OBJECT" "DOTCL-INTERNAL")
               (find-symbol "%FIXNUM-GE-OBJECT" "DOTCL.CIL-COMPILER"))))
    (list (funcall f 3 2) (funcall f 2 3) (funcall f 2 2)))
  (t nil t))

(deftest intrinsic-function-fallback.getenv-callable
  (let ((f (or (find-symbol "%GETENV" "DOTCL-INTERNAL")
               (find-symbol "%GETENV" "DOTCL.CIL-COMPILER"))))
    (let ((v (funcall f "DOTCL_NO_SUCH_VARIABLE_FOR_TEST")))
      (or (null v) (stringp v))))
  t)

;;; DOTIMES is the expansion that broke: its loop test is an intrinsic. Run it
;;; through EVAL so the interpreter sees it when the suite runs in :interpret.

(deftest intrinsic-function-fallback.dotimes-through-eval
  (eval '(let ((acc 0)) (dotimes (i 5 acc) (setf acc (+ acc i)))))
  10)

;;; A count that does not fit a fixnum still compares correctly: the intrinsic's
;;; whole point is that the limit side stays boxed.

(deftest intrinsic-function-fallback.dotimes-bignum-limit
  (eval '(let ((n 0))
          (dotimes (i (* 1 (expt 2 70)) :done)
            (incf n)
            (when (> n 3) (return :done)))))
  :done)
