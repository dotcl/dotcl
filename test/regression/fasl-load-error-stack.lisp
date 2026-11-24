;;; An error raised while the top-level code of a fasl runs reaches the caller as the same
;;; condition (type and message), and the code before it has run.

(defun flse-fasl (name forms)
  (let* ((dir (regression-temp-dir/))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(deftest-emitting-only fasl-load-error-stack.condition-unchanged
  (let ((fasl (flse-fasl "flse-1" '((defun flse-f () 1) (error "boom from a fasl") (defun flse-g () 2)))))
    (list (handler-case (progn (load fasl) :no-error)
            (simple-error (e) (list :simple-error (princ-to-string e))))
          (fboundp 'flse-f)
          (fboundp 'flse-g)))
  ((:simple-error "boom from a fasl") t nil))

(deftest-emitting-only fasl-load-error-stack.type-error
  (let ((fasl (flse-fasl "flse-2" '((car 5)))))
    (handler-case (progn (load fasl) :no-error)
      (type-error (e) (list :type-error (princ-to-string (type-error-datum e))))))
  (:type-error "5"))
