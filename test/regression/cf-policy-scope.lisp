;;; A DECLAIM of OPTIMIZE in a file given to COMPILE-FILE is in force to the end
;;; of that file only, as in SBCL. It used to stay in force afterwards, so after
;;; compiling a file that declaims (safety 0), everything compiled later in the
;;; image lost its loop interrupt polls.

(setf dotcl:*save-sil* t)

(defun %cfps-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))))

(deftest-compiled-only compile-file-policy-scope.variables-restored
  (let ((dotcl-internal::*optimize-safety* 1)
        (dotcl-internal::*optimize-debug* 1)
        (dotcl-internal::*proclaimed-policy* '()))
    (%cfps-compile "cfps1" '("(declaim (optimize (safety 0) (debug 0) (speed 3)))"
                             "(defun cfps1-f (n) (let ((s 0)) (dotimes (i n s) (incf s i))))"))
    (list dotcl-internal::*optimize-safety*
          dotcl-internal::*optimize-debug*
          dotcl-internal::*proclaimed-policy*))
  (1 1 nil))

(deftest-compiled-only compile-file-policy-scope.later-code-keeps-polls
  (let ((dotcl-internal::*optimize-safety* 1))
    (%cfps-compile "cfps2" '("(declaim (optimize (safety 0)))"))
    (eval '(defun %cfps-after (n) (let ((s 0)) (dotimes (i n s) (setq s (+ s i))))))
    (and (search "PollInterrupt" (%flat-text (dotcl:function-sil #'%cfps-after))) t))
  t)
