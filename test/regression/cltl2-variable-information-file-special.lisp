;;; DOTCL-CLTL2:VARIABLE-INFORMATION answers :SPECIAL for a variable a DEFVAR
;;; or a SPECIAL proclamation earlier in the same file made special, while
;;; COMPILE-FILE is still compiling that file (the runtime marks the symbol
;;; only when the fasl is loaded, but the compiler already binds it
;;; dynamically). A code walker such as cl-environments asks at macroexpansion
;;; time and must get the compiler's answer.

(defun %cvifs-compile-and-load ()
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "cvifs.lisp" dir))
         (fasl (merge-pathnames "cvifs.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f '((defvar *cvifs-var*)
                     (declaim (special cvifs-proclaimed))
                     (defmacro cvifs-kind (s)
                       `',(dotcl-cltl2:variable-information s))
                     (defun cvifs-kinds ()
                       (list (cvifs-kind *cvifs-var*)
                             (cvifs-kind cvifs-proclaimed)
                             (cvifs-kind cvifs-lexical)
                             (cvifs-kind *print-base*)))))
          (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (load fasl)
    (funcall (intern "CVIFS-KINDS"))))

(deftest-compiled-only cltl2-variable-information.file-special
  (%cvifs-compile-and-load)
  (:special :special nil :special))
