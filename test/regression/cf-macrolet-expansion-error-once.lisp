;;; COMPILE-FILE runs the expander of a MACROLET macro once, also when it
;;; signals an ERROR. The analysis walks and the compiler each made their own
;;; expander for one binding, so a failure the expansion cache kept for one was
;;; not used for the next. A top level MACROLET's trial expansion also let the
;;; error out of COMPILE-FILE when no handler was around it. The counts are
;;; SBCL 2.6.8's on the same files.

(defvar *cfml-count* 0)

(defun cfml-compile (name text)
  "Compile TEXT as file NAME, with no handler around COMPILE-FILE. Returns the
   number of expansions and the three values of COMPILE-FILE."
  (let ((path (concatenate 'string (namestring (regression-temp-dir)) "/" name))
        (*error-output* (make-broadcast-stream)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    (setf *cfml-count* 0)
    (multiple-value-bind (fasl warnings-p failure-p) (compile-file path)
      (list *cfml-count* (list (and fasl t) (and warnings-p t) (and failure-p t))))))

(deftest-emitting-only cf-macrolet-expansion-error-once.in-a-function
  (cfml-compile "cfml1.lisp"
                "(defun cfml-f () (macrolet ((m () (incf *cfml-count*) (error \"cfml\"))) (m)))")
  (1 (t t t)))

(deftest-emitting-only cf-macrolet-expansion-error-once.two-calls
  (cfml-compile "cfml2.lisp"
                "(defun cfml-f () (macrolet ((m () (incf *cfml-count*) (error \"cfml\"))) (list (m) (m))))")
  (2 (t t t)))

(deftest-emitting-only cf-macrolet-expansion-error-once.under-symbol-macrolet
  (cfml-compile "cfml3.lisp"
                "(defun cfml-f () (macrolet ((m () (incf *cfml-count*) (error \"cfml\"))) (let ((x 1)) (symbol-macrolet ((y x)) (m)))))")
  (1 (t t t)))

(deftest-emitting-only cf-macrolet-expansion-error-once.from-a-macrolet-expansion
  (cfml-compile "cfml4.lisp"
                "(defun cfml-f () (macrolet ((o () `(macrolet ((m () (incf *cfml-count*) (error \"cfml\"))) (m)))) (o)))")
  (1 (t t t)))

(deftest-emitting-only cf-macrolet-expansion-error-once.top-level
  (cfml-compile "cfml5.lisp"
                "(macrolet ((m () (incf *cfml-count*) (error \"cfml\"))) (m))")
  (1 (t t t)))

(deftest-emitting-only cf-macrolet-expansion-error-once.top-level-nested
  (cfml-compile "cfml6.lisp"
                "(symbol-macrolet ((y 1)) (macrolet ((a () 1)) (macrolet ((m () (incf *cfml-count*) (error \"cfml\"))) (m))))")
  (1 (t t t)))

(deftest-emitting-only cf-macrolet-expansion-error-once.top-level-two-forms
  (cfml-compile "cfml7.lisp"
                "(macrolet ((m (x) (incf *cfml-count*) (if (eql x 1) (error \"cfml\") x))) (m 1) (m 2))")
  (2 (t t t)))

(deftest-emitting-only cf-macrolet-expansion-error-once.no-error
  (cfml-compile "cfml8.lisp"
                "(macrolet ((m () (incf *cfml-count*) 1)) (m)) (defun cfml-f () (macrolet ((m () (incf *cfml-count*) 1)) (m)))")
  (2 (t nil nil)))

;;; The expander calls a function of the FLET around the MACROLET. The walks
;;; cannot run it and the compiler can, so the expansion still works.
(deftest-emitting-only cf-macrolet-expansion-error-once.enclosing-flet
  (cadr (cfml-compile "cfml9.lisp"
                      "(defun cfml-f () (flet ((d (x) (* x 2))) (macrolet ((m (x) (d x))) (m 5))))"))
  (t nil nil))
