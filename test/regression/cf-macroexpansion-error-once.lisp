;;; COMPILE-FILE runs the expander of a macro form once, also when the expander
;;; signals an ERROR. A failing expansion was not remembered: the top level
;;; processing tried it, then the compiler expanded it again, and each analysis
;;; walk over a function body expanded it once more. A macro that does real work
;;; while expanding (a type checker, a diagnostics hook) did it several times.
;;; The counts below are SBCL 2.6.8's on the same files.

(defvar *cfmo-count* 0)
(defmacro cfmo-bad () (incf *cfmo-count*) (error "cfmo boom"))
(defmacro cfmo-good () (incf *cfmo-count*) 1)

(defun cfmo-compile (name text)
  "Compile TEXT as file NAME. Returns the number of expansions and the
   WARNINGS-P / FAILURE-P values, or :ESCAPED when an error escaped."
  (let ((path (concatenate 'string (namestring (regression-temp-dir)) "/" name))
        (*error-output* (make-broadcast-stream)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    (setf *cfmo-count* 0)
    (let ((r (handler-case
                 (multiple-value-bind (fasl warnings-p failure-p) (compile-file path)
                   (list (and fasl t) (and warnings-p t) (and failure-p t)))
               (error () :escaped))))
      (list *cfmo-count* r))))

(deftest-emitting-only cf-macroexpansion-error-once.top-level
  (cfmo-compile "cfmo1.lisp" "(cfmo-bad)")
  (1 (t t t)))

(deftest-emitting-only cf-macroexpansion-error-once.progn
  (cfmo-compile "cfmo2.lisp" "(progn (cfmo-bad))")
  (1 (t t t)))

(deftest-emitting-only cf-macroexpansion-error-once.let
  (cfmo-compile "cfmo3.lisp" "(let () (cfmo-bad))")
  (1 (t t t)))

(deftest-emitting-only cf-macroexpansion-error-once.in-a-function
  (cfmo-compile "cfmo4.lisp" "(defun cfmo-f () (list (cfmo-bad) (cfmo-bad)))")
  (2 (t t t)))

;;; An fbound name: the DEFUN is also evaluated at compile time, right away.
(defun cfmo-g () nil)
(deftest-emitting-only cf-macroexpansion-error-once.redefined-function
  (cfmo-compile "cfmo5.lisp" "(defun cfmo-g () (cfmo-bad))")
  (1 (t t t)))

(deftest-emitting-only cf-macroexpansion-error-once.compile-time-too
  (cfmo-compile "cfmo6.lisp"
                "(eval-when (:compile-toplevel :load-toplevel :execute) (cfmo-bad))")
  (1 (t t t)))

(deftest-emitting-only cf-macroexpansion-error-once.compile-time-only
  (cfmo-compile "cfmo7.lisp" "(eval-when (:compile-toplevel) (cfmo-bad))")
  (1 :escaped))

(deftest-emitting-only cf-macroexpansion-error-once.no-error
  (cfmo-compile "cfmo8.lisp" "(cfmo-good) (defun cfmo-h () (cfmo-good))")
  (2 (t nil nil)))
