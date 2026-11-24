;;; A global macro inside a top level SYMBOL-MACROLET is expanded with the symbol macros in
;;; its environment: COMPILE-FILE must not expand it ahead of the scope (that expansion has
;;; no environment). The macro below reads a symbol macro through &ENVIRONMENT, as
;;; serapeum's DEFCONSTRUCTOR does for the union it belongs to.

(defmacro %smg-read (var marker &environment env)
  `(defparameter ,var ',(macroexpand marker env)))

(defun %smg-compile-and-load (forms)
  (let* ((src (regression-temp-file (format nil "smg-~D.lisp" (random 1000000))))
         (out (regression-temp-file (format nil "smg-~D.fasl" (random 1000000)))))
    (with-open-file (o src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f o) (terpri o))))
    (unwind-protect (progn (compile-file src :output-file out) (load out))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file out)))))

(deftest-emitting-only toplevel-symbol-macrolet-global-macro-env.sees-symbol-macro
  (progn
    (%smg-compile-and-load '((symbol-macrolet ((%smg-marker the-union))
                               (declaim (optimize (safety 1)))
                               (%smg-read *smg-value* %smg-marker))))
    (symbol-value '*smg-value*))
  the-union)
