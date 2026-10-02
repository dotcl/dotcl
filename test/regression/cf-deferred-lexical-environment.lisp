;;; COMPILE-FILE defers two compile-time evaluations until they are needed: a
;;; DEFMACRO's expander (made at its first expansion) and a top level DEFUN
;;; (made at its first call, for a macro later in the file). A deferred
;;; evaluation runs outside the forms around it, so a DEFMACRO or DEFUN inside
;;; a top level MACROLET or SYMBOL-MACROLET lost the local macros its body
;;; uses ("Undefined function: DBL"). Such forms are evaluated at once, as
;;; before deferral. A deferred DEFUN whose body uses a macro that is defined
;;; again later in the file is evaluated before the redefinition, so it sees
;;; the macro as it was at the DEFUN.
;;;
;;; The first two also run under :INTERPRET, where the compile-time evaluation
;;; of the DEFMACRO / DEFUN is interpreted: the interpreter expands a function
;;; body when it runs, so it is given the local macros as its environment. The
;;; third stays compiled-only: an interpreted early definition expands CFDL-K
;;; when it is first called, after the redefinition.

(defun %cfdl-compile-load (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (load (compile-file src :output-file fasl)))
    t))

(deftest-emitting-only cf-deferred-lexical-environment.defmacro-in-macrolet
  (progn
    (%cfdl-compile-load
     "cfdl-a"
     '("(macrolet ((cfdl-dbl (x) `(* 2 ,x)))"
       "  (defmacro cfdl-m (y) (cfdl-dbl y)))"
       "(defun cfdl-f () (cfdl-m 21))"
       "(symbol-macrolet ((cfdl-k 21)) (defmacro cfdl-m2 () cfdl-k))"
       "(defun cfdl-f2 () (cfdl-m2))"))
    (list (funcall 'cfdl-f) (funcall 'cfdl-f2)))
  (42 21))

(deftest-emitting-only cf-deferred-lexical-environment.defun-in-macrolet
  (progn
    (%cfdl-compile-load
     "cfdl-b"
     '("(macrolet ((cfdl-twice (x) `(* 2 ,x)))"
       "  (defun cfdl-helper (n) (cfdl-twice n)))"
       "(defmacro cfdl-use () (cfdl-helper 21))"
       "(defun cfdl-g () (cfdl-use))"))
    (funcall 'cfdl-g))
  42)

(deftest-compiled-only cf-deferred-lexical-environment.macro-redefined-later
  (progn
    (%cfdl-compile-load
     "cfdl-c"
     '("(defmacro cfdl-k () 1)"
       "(defun cfdl-h () (cfdl-k))"
       "(defmacro cfdl-k () 2)"
       "(defmacro cfdl-u () (cfdl-h))"
       "(defun cfdl-v () (cfdl-u))"))
    (list (funcall 'cfdl-v) (funcall 'cfdl-h)))
  (1 1))
