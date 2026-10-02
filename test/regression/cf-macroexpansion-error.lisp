;;; A macro whose expander signals an ERROR does not stop COMPILE-FILE.
;;;
;;; The error escaped COMPILE-FILE: no fasl, no return values, and the rest of the
;;; file was never compiled. SBCL reports it ("caught ERROR: (during macroexpansion
;;; of ...)"), compiles the form into one that signals when it is evaluated, goes on
;;; with the file, and returns T for both WARNINGS-P and FAILURE-P. Code that looks
;;; at those values -- a library's own tests of its compile-time errors, build
;;; tools -- saw dotcl abort where SBCL answered. The expected values below are
;;; SBCL 2.6.8's on the same files.

(defmacro cfme-boom () (error "macro boom"))

(defun cfme-file (name text)
  (let ((path (concatenate 'string (namestring (regression-temp-dir)) "/" name)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))

(defun cfme-compile (name text)
  "Compile TEXT as file NAME. Returns (:fasl warnings-p failure-p) and the fasl,
or (:escaped message) when an error escaped COMPILE-FILE."
  (let ((path (cfme-file name text))
        (*error-output* (make-broadcast-stream)))
    (handler-case
        (multiple-value-bind (fasl warnings-p failure-p) (compile-file path)
          (values (list (and fasl :fasl) (and warnings-p t) (and failure-p t)) fasl))
      (error (e) (list :escaped (princ-to-string e))))))

(defun cfme-load (fasl)
  (handler-case (progn (load fasl) :loaded)
    (error (e) (if (search "compiled with errors" (princ-to-string e)) :load-error e))))

;;; At top level: reported, the rest of the file compiled, the form signals when
;;; the fasl reaches it.
(deftest-emitting-only compile-file-macroexpansion-error.top-level
  (multiple-value-bind (r fasl)
      (cfme-compile "cfme1.lisp" "(defun cfme-ok1 () 1) (cfme-boom) (defun cfme-ok2 () 2)")
    (list r (and fasl (cfme-load fasl)) (fboundp 'cfme-ok1)))
  ((:fasl t t) :load-error t))

;;; Inside a function: the file loads, the function signals when called.
(deftest-emitting-only compile-file-macroexpansion-error.in-a-function
  (multiple-value-bind (r fasl)
      (cfme-compile "cfme2.lisp"
                    "(defun cfme-bad () (+ 1 (cfme-boom))) (defun cfme-ok3 () 3)")
    (list r (and fasl (cfme-load fasl)) (funcall 'cfme-ok3)
          (handler-case (progn (funcall 'cfme-bad) :no-error)
            (error (e) (and (search "compiled with errors" (princ-to-string e)) :call-error)))))
  ((:fasl t t) :loaded 3 :call-error))

;;; Evaluated only at compile time: nothing is compiled for later, so the
;;; expander's own error comes out of COMPILE-FILE, as from EVAL (and in SBCL).
(deftest-emitting-only compile-file-macroexpansion-error.compile-time-only
  (cfme-compile "cfme3.lisp" "(eval-when (:compile-toplevel) (cfme-boom))")
  (:escaped "macro boom"))

;;; Compile time too: reported once, not run at compile time as well.
(deftest-emitting-only compile-file-macroexpansion-error.compile-time-too
  (values (cfme-compile "cfme4.lisp"
                        "(eval-when (:compile-toplevel :load-toplevel :execute) (cfme-boom))"))
  (:fasl t t))
