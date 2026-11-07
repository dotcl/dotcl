;;; A function that DEFCLASS (:accessor, :reader, :writer), DEFGENERIC or DEFMETHOD
;;; defines is not an undefined function for a call later in the same file, as in
;;; SBCL; WARNINGS-P stays NIL. A name nothing defines still gets the warning.

(defun %ufc-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir))
         (undefined '()) (wp nil))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (handler-bind ((warning
                       (lambda (c)
                         (let ((text (princ-to-string c)))
                           (when (search "undefined function" text)
                             (push text undefined))))))
        (setf wp (nth-value 1 (compile-file src :output-file fasl)))))
    (list (nreverse undefined) wp)))

(deftest-compiled-only undefined-function-clos-names.accessors
  (%ufc-compile "ufc1" '("(defclass ufc-c () ((x :accessor ufc-x) (y :reader ufc-y) (z :writer ufc-set-z)))"
                         "(defun ufc-use (o) (list (ufc-x o) (ufc-y o) (ufc-set-z 1 o)))"))
  (nil nil))

(deftest-compiled-only undefined-function-clos-names.defgeneric-and-defmethod
  (%ufc-compile "ufc2" '("(defgeneric ufc-g (x))"
                         "(defmethod ufc-g ((x integer)) (ufc-g (list x)))"
                         "(defun ufc-use-g () (ufc-g 1))"
                         "(defmethod ufc-h ((x integer)) x)"
                         "(defun ufc-use-h () (ufc-h 1))"))
  (nil nil))

(deftest-compiled-only undefined-function-clos-names.typo-still-warns
  (let ((r (%ufc-compile "ufc3" '("(defclass ufc-d () ((x :accessor ufc-dx)))"
                                  "(defun ufc-bad (o) (list (ufc-dx o) (ufc-no-such-function o)))"))))
    (list (length (first r)) (and (search "UFC-NO-SUCH-FUNCTION" (first (first r))) t) (second r)))
  (1 t t))
