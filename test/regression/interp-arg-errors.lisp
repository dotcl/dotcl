;;; An interpreted call that is given the wrong arguments says which function and
;;; what was wrong, the way a compiled one does (Runtime.CheckArityExact:
;;; "FOO: wrong number of arguments: 3 (expected 1)"). The interpreter used to
;;; signal a bare PROGRAM-ERROR, which prints as #<PROGRAM-ERROR> -- same fault,
;;; no way to tell which call raised it. On an emit-free build the interpreter is
;;; the only evaluator, so that was every argument mistake in the program.
;;;
;;; The callee's name comes from the innermost backtrace frame, which its caller
;;; pushed before binding parameters. These run through EVAL so the closures are
;;; interpreted in either build.

(defun %iae-message (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (program-error (e) (princ-to-string e))))

(defun %iae-eval (form) (eval form))

(deftest interp-arg-errors.too-few
  (progn (%iae-eval '(defun iae-1 (x) x))
         (%iae-message (lambda () (funcall (symbol-function 'iae-1)))))
  "IAE-1: wrong number of arguments: 0 (expected 1)")

(deftest interp-arg-errors.too-many
  (progn (%iae-eval '(defun iae-2 (x) x))
         (%iae-message (lambda () (funcall (symbol-function 'iae-2) 1 2))))
  "IAE-2: wrong number of arguments: 2 (expected 1)")

(deftest interp-arg-errors.optional-bounds
  (progn (%iae-eval '(defun iae-3 (x &optional y) (list x y)))
         (list (%iae-message (lambda () (funcall (symbol-function 'iae-3))))
               (%iae-message (lambda () (funcall (symbol-function 'iae-3) 1 2 3)))))
  ("IAE-3: too few arguments: 0 (expected at least 1)"
   "IAE-3: too many arguments: 3 (expected at most 2)"))

(deftest interp-arg-errors.unknown-keyword
  (progn (%iae-eval '(defun iae-4 (x &key a) (list x a)))
         (%iae-message (lambda () (funcall (symbol-function 'iae-4) 1 :zz 2))))
  "IAE-4: unrecognized keyword argument :ZZ")

(deftest interp-arg-errors.odd-keyword-list
  (progn (%iae-eval '(defun iae-5 (x &key a) (list x a)))
         (%iae-message (lambda () (funcall (symbol-function 'iae-5) 1 :a))))
  "IAE-5: odd number of keyword arguments")

;;; A correct call is unaffected: the name lookup only runs once a check has
;;; already failed.
(deftest interp-arg-errors.correct-calls-still-work
  (progn (%iae-eval '(defun iae-6 (x &optional (y 2) &key (z 3)) (list x y z)))
         (list (funcall (symbol-function 'iae-6) 1)
               (funcall (symbol-function 'iae-6) 1 9)
               (funcall (symbol-function 'iae-6) 1 9 :z 8)))
  ((1 2 3) (1 9 3) (1 9 8)))
