;;; A global FTYPE proclamation says the function exists, so a call to it that is not
;;; defined yet is not reported as an undefined function. A name with no declaration
;;; still is.

(defun %fdu-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir))
         (undefined '()))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (handler-bind ((style-warning
                     (lambda (c)
                       (let ((text (princ-to-string c)))
                         (when (search "undefined function" text)
                           (push text undefined))
                         (muffle-warning c)))))
      (let ((*error-output* (make-broadcast-stream)))
        (compile-file src :output-file fasl)))
    (nreverse undefined)))

(deftest-compiled-only ftype-declaim-undefined.function-ftype
  (%fdu-compile "fdu1" '("(declaim (ftype function fdu-known-1))"
                         "(defun fdu-caller-1 () (fdu-known-1 1))"))
  nil)

(deftest-compiled-only ftype-declaim-undefined.full-ftype
  (%fdu-compile "fdu2" '("(declaim (ftype (function (fixnum) fixnum) fdu-known-2))"
                         "(defun fdu-caller-2 () (fdu-known-2 1))"))
  nil)

(deftest-compiled-only ftype-declaim-undefined.undeclared-still-warns
  (length (%fdu-compile "fdu3" '("(defun fdu-caller-3 () (fdu-unknown-3 1))")))
  1)
