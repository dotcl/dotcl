;;; COMPILE-FILE and COMPILE answer FAILURE-P T for a macro whose expander
;;; signals an ERROR, also when a handler outside muffles every WARNING: the
;;; error was detected by the compiler (CLHS COMPILE-FILE). A WARNING the file
;;; signals itself and that a handler outside muffles does not count, as in
;;; SBCL 2.6.8. A STYLE-WARNING gives WARNINGS-P T and FAILURE-P NIL (CLHS).

(defmacro cfmw-bad () (error "cfmw boom"))
(defmacro cfmw-warn () (warn "cfmw warning") nil)
(defmacro cfmw-style () (warn 'style-warning :format-control "cfmw style") nil)

(defun cfmw-compile (name text how)
  (let ((path (concatenate 'string (namestring (regression-temp-dir)) "/" name))
        (*error-output* (make-broadcast-stream)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    (flet ((cf ()
             (multiple-value-bind (fasl warnings-p failure-p) (compile-file path)
               (declare (ignore fasl))
               (list (and warnings-p t) (and failure-p t)))))
      (ecase how
        (:plain (cf))
        (:muffle (handler-bind ((warning #'muffle-warning)) (cf)))
        (:muffle-style (handler-bind ((style-warning #'muffle-warning)) (cf)))
        (:decline (handler-bind ((warning (lambda (c) c nil))) (cf)))))))

(deftest-emitting-only cf-muffled-warnings-p.expansion-error
  (mapcar (lambda (how) (cfmw-compile "cfmw1.lisp" "(cfmw-bad)" how))
          '(:plain :muffle :muffle-style :decline))
  ((t t) (t t) (t t) (t t)))

(deftest-emitting-only cf-muffled-warnings-p.expansion-error-in-defun
  (cfmw-compile "cfmw2.lisp" "(defun cfmw-f () (cfmw-bad))" :muffle)
  (t t))

(deftest-emitting-only cf-muffled-warnings-p.warning
  (mapcar (lambda (how) (cfmw-compile "cfmw3.lisp" "(cfmw-warn)" how))
          '(:plain :muffle :muffle-style :decline))
  ((t t) (nil nil) (t t) (t t)))

(deftest-emitting-only cf-muffled-warnings-p.style-warning
  (mapcar (lambda (how) (cfmw-compile "cfmw4.lisp" "(cfmw-style)" how))
          '(:plain :muffle :muffle-style :decline))
  ((t nil) (nil nil) (nil nil) (t nil)))

(deftest-emitting-only cf-muffled-warnings-p.compile
  (let ((*error-output* (make-broadcast-stream)))
    (list (cdr (multiple-value-list (compile nil '(lambda () (cfmw-bad)))))
          (cdr (multiple-value-list
                (handler-bind ((warning #'muffle-warning))
                  (compile nil '(lambda () (cfmw-bad))))))))
  ((t t) (t t)))
