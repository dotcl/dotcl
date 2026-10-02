;;; At its end COMPILE-FILE removes the early definitions its compilation made
;;; (a plain DEFUN has no compile-time effect). It took every function cell
;;; assigned while it ran as its own, also the ones another thread assigned
;;; meanwhile, so a function defined at a REPL while a SLIME or SLY worker
;;; compiled a file was gone when the compilation ended. Here a macro in the
;;; file holds the compilation until the main thread has defined two
;;; functions, then lets it finish.

(require "dotcl-thread")

(defvar *cfot-expanding* nil)
(defvar *cfot-go* nil)

(defun %cfot-wait (var)
  (loop repeat 2000 until (symbol-value var) do (sleep 0.005))
  (symbol-value var))

(defmacro %cfot-hold ()
  (setf *cfot-expanding* t)
  (%cfot-wait '*cfot-go*)
  1)

(deftest-compiled-only cf-other-thread-definitions.kept
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "cfot.lisp" dir))
         (fasl (merge-pathnames "cfot.fasl" dir)))
    (setf *cfot-expanding* nil *cfot-go* nil)
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(defun cfot-file-fn () (%cfot-hold))" s))
    (let ((th (dotcl-thread:make-thread
               (lambda ()
                 (let ((*error-output* (make-broadcast-stream)))
                   (compile-file src :output-file fasl))))))
      (%cfot-wait '*cfot-expanding*)
      (eval '(defun cfot-other-thread-fn () :alive))
      (setf (symbol-function (intern "CFOT-OTHER-THREAD-FN2")) #'car)
      (setf *cfot-go* t)
      (dotcl-thread:thread-join th))
    (list (fboundp (intern "CFOT-OTHER-THREAD-FN"))
          (fboundp (intern "CFOT-OTHER-THREAD-FN2"))
          (fboundp (intern "CFOT-FILE-FN"))))
  (t t nil))
