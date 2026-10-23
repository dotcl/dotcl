;;; A DECLAIM of OPTIMIZE in a file given to LOAD, source or fasl, is in force
;;; to the end of that file only, as in SBCL. After loading a source file that
;;; declaims (safety 0), everything compiled later in the image used to lose its
;;; loop interrupt polls.

(setf dotcl:*save-sil* t)

(defun %lps-write (name lines)
  (let ((src (merge-pathnames (format nil "~a.lisp" name) (regression-temp-dir))))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    src))

(defun %lps-policy ()
  (list dotcl-internal::*optimize-safety*
        dotcl-internal::*optimize-debug*
        dotcl-internal::*proclaimed-policy*))

(deftest load-policy-scope.source
  (let ((dotcl-internal::*optimize-safety* 1)
        (dotcl-internal::*optimize-debug* 1)
        (dotcl-internal::*proclaimed-policy* '()))
    (load (%lps-write "lps1" '("(declaim (optimize (safety 0) (debug 0) (speed 3)))"
                               "(defun lps1-f (n) (let ((s 0)) (dotimes (i n s) (incf s i))))")))
    (%lps-policy))
  (1 1 nil))

(deftest-compiled-only load-policy-scope.later-code-keeps-polls
  (let ((dotcl-internal::*optimize-safety* 1))
    (load (%lps-write "lps3" '("(declaim (optimize (safety 0)))")))
    (eval '(defun %lps-after (n) (let ((s 0)) (dotimes (i n s) (setq s (+ s i))))))
    (and (search "PollInterrupt" (%flat-text (dotcl:function-sil #'%lps-after))) t))
  t)

(deftest-compiled-only load-policy-scope.fasl
  (let ((dotcl-internal::*optimize-safety* 1)
        (dotcl-internal::*optimize-debug* 1)
        (dotcl-internal::*proclaimed-policy* '())
        (src (%lps-write "lps2" '("(declaim (optimize (safety 0) (debug 0)))"))))
    (load (let ((*error-output* (make-broadcast-stream)))
            (compile-file src :output-file (merge-pathnames "lps2.fasl" (regression-temp-dir)))))
    (%lps-policy))
  (1 1 nil))
