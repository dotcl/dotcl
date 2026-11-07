;;; A DEFUN under a conditional that does not run (the fallback idiom
;;; (unless (fboundp 'f) (defun f ...))) is not the definition that later
;;; calls in the same COMPILE-FILE'd file may bind to.

(deftest-compiled-only same-file.conditional-defun
  (let ((src (regression-temp-file "sfcd.lisp"))
        (fasl (regression-temp-file "sfcd.fasl")))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(defun cl-user::sfcd-f (x) (list :real x))" s)
             (write-line "(defun cl-user::sfcd-before (y) (cl-user::sfcd-f y))" s)
             (write-line "(unless (fboundp 'cl-user::sfcd-f) (defun cl-user::sfcd-f (x) (list :fallback x)))" s)
             (write-line "(defun cl-user::sfcd-after (y) (cl-user::sfcd-f y))" s)
             (write-line "(defvar cl-user::*sfcd-r* (list (cl-user::sfcd-before 1) (cl-user::sfcd-after 2) (cl-user::sfcd-f 3) (funcall 'cl-user::sfcd-f 4)))" s))
           (compile-file src :output-file fasl)
           (load fasl)
           (symbol-value 'cl-user::*sfcd-r*))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  ((:real 1) (:real 2) (:real 3) (:real 4)))
