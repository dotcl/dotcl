;;; A warning that a handler outside COMPILE-FILE muffles is not counted in
;;; warnings-p / failure-p, and one nobody muffles is counted, reported once and
;;; seen once by the outer handlers. The expected values are SBCL 2.6.8's.

(define-condition cfmw-style (style-warning) ())
(define-condition cfmw-full (warning) ())

(defun cfmw-path (name text)
  (let ((path (concatenate 'string (namestring (regression-temp-dir)) "/" name)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))

(defun cfmw-values (path)
  (multiple-value-bind (fasl warnings-p failure-p) (compile-file path)
    (list (and fasl :fasl) (and warnings-p t) (and failure-p t))))

(defun cfmw-style-file ()
  (cfmw-path "cfmw-style.lisp"
             "(defmacro cfmw-m1 () (warn 'cfmw-style) nil) (defun cfmw-f1 () (cfmw-m1))"))

(defun cfmw-full-file ()
  (cfmw-path "cfmw-full.lisp"
             "(defmacro cfmw-m2 () (warn 'cfmw-full) nil) (defun cfmw-f2 () (cfmw-m2))"))

(deftest-emitting-only compile-file-muffled.style-warning
  (let ((*error-output* (make-broadcast-stream)))
    (handler-bind ((style-warning #'muffle-warning))
      (cfmw-values (cfmw-style-file))))
  (:fasl nil nil))

(deftest-emitting-only compile-file-muffled.full-warning
  (let ((*error-output* (make-broadcast-stream)))
    (handler-bind ((warning #'muffle-warning))
      (cfmw-values (cfmw-full-file))))
  (:fasl nil nil))

(deftest-emitting-only compile-file-muffled.not-muffled
  (let ((seen 0)
        (out (make-string-output-stream)))
    (let ((r (let ((*error-output* out))
               (handler-bind ((cfmw-full (lambda (c) (declare (ignore c)) (incf seen))))
                 (cfmw-values (cfmw-full-file))))))
      (list r seen
            (let ((text (get-output-stream-string out)))
              (loop for i = (search "CFMW-FULL" text) then (search "CFMW-FULL" text :start2 (1+ i))
                    while i count t)))))
  ((:fasl t t) 1 1))

(deftest-emitting-only compile-file-muffled.not-muffled-style
  (let ((*error-output* (make-broadcast-stream)))
    (handler-bind ((warning (lambda (c) (declare (ignore c)) nil)))
      (cfmw-values (cfmw-style-file))))
  (:fasl t nil))

(deftest-emitting-only compile-file-muffled.uiop-uninteresting-conditions
  (progn
    (require "asdf")
    (let ((*error-output* (make-broadcast-stream))
          (uninteresting (find-symbol "*UNINTERESTING-CONDITIONS*" "UIOP")))
      (progv (list uninteresting) (list (cons 'cfmw-full (symbol-value uninteresting)))
        (multiple-value-bind (fasl warnings-p failure-p)
            (funcall (find-symbol "COMPILE-FILE*" "UIOP") (cfmw-full-file))
          (list (and fasl :fasl) (and warnings-p t) (and failure-p t))))))
  (:fasl nil nil))

;;; A COMPILE-FILE at compile time of another: the inner file's warning is the
;;; inner one's, not the outer's.
(deftest-emitting-only compile-file-muffled.nested
  (let* ((*error-output* (make-broadcast-stream))
         (inner (cfmw-full-file))
         (outer (cfmw-path "cfmw-outer.lisp"
                           (format nil "(eval-when (:compile-toplevel)
                                          (setf (symbol-value '~s)
                                                (~s ~s)))
                                        (defun cfmw-f3 () 1)"
                                   '*cfmw-inner* 'cfmw-values inner))))
    (defparameter *cfmw-inner* nil)
    (list (cfmw-values outer) (symbol-value '*cfmw-inner*)))
  ((:fasl nil nil) (:fasl t t)))
