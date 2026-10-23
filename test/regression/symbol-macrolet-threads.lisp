;;; The compiler's analysis walks entered a SYMBOL-MACROLET (and left a LET that
;;; shadows a symbol macro) by assigning the symbol macro list. On a thread with
;;; no binding of its own, as under COMPILE-FILE, that assigned the global
;;; value, so another thread compiling at the same time saw the binding: with
;;; ironclad compiled on 4 threads, sha3.lisp expanded its X with the
;;; WITH-SLOTS binding of X from another file and recursed until the stack ran
;;; out. Here the free-variable walk runs on a thread with no bindings, and a
;;; macro inside the SYMBOL-MACROLET looks at the global value from another
;;; thread.

(require "dotcl-thread")

(defvar *smt-seen* :unset)

(defun %smt-global (sym)
  (let ((v nil))
    (dotcl-thread:thread-join
     (dotcl-thread:make-thread (lambda () (setf v (symbol-value sym)))))
    v))

(defmacro %smt-look ()
  (setf *smt-seen* (%smt-global 'dotcl-internal::*symbol-macros*))
  nil)

(deftest-compiled-only symbol-macrolet-threads.walk-binds-symbol-macros
  (progn
    (setf *smt-seen* :unset)
    (dotcl-thread:thread-join
     (dotcl-thread:make-thread
      (lambda ()
        (dotcl.cil-compiler::find-free-vars-expr
         '(symbol-macrolet ((%smt-x (car o))) (%smt-look) %smt-x)
         nil (list nil)))))
    *smt-seen*)
  nil)
