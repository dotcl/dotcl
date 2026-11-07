;;; An error while COMPILE-FILE reads a form (a reader error, or one in a #.
;;; evaluation) ends the compilation as in SBCL: the error is reported, no output is
;;; kept, and COMPILE-FILE returns NIL, T, T instead of signalling it to the caller.

(defun %cfre-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (when (probe-file fasl) (delete-file fasl))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (multiple-value-bind (out warnings-p failure-p)
          (handler-case (compile-file src :output-file fasl)
            (error (c) (values :signalled (type-of c) nil)))
        (list (and out (not (eq out :signalled)) t) warnings-p failure-p
              (and (probe-file fasl) t))))))

(deftest-compiled-only compile-file-read-error.unknown-dispatch
  (%cfre-compile "cfre1" '("(defun cfre-a () 1)" "#!/x" "(defun cfre-b () 2)"))
  (nil t t nil))

(deftest-compiled-only compile-file-read-error.hash-dot-unbound
  (%cfre-compile "cfre2" '("(defvar *cfre-r* 0)" "#.(incf *cfre-r* 1)" "(defun cfre-c () 1)"))
  (nil t t nil))

(deftest-compiled-only compile-file-read-error.valid-file-unchanged
  (%cfre-compile "cfre3" '("(defvar *cfre-v* 0)" "(defun cfre-d () #.(+ 1 2))" "#+(not :cfre-no-such-feature) (defun cfre-e () 3)"))
  (t nil nil t))
