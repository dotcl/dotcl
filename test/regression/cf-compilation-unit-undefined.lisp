;;; Inside WITH-COMPILATION-UNIT, the undefined-function warnings of the files compiled
;;; are signalled when the unit ends (CLHS 3.2.5), once per name, and only for names that are
;;; still undefined by then; outside a unit each file reports its own at its end.

(defun %cfcu-source (name text)
  (let ((path (merge-pathnames name (regression-temp-dir))))
    (with-open-file (s path :direction :output :if-exists :supersede) (write-string text s))
    path))

(defmacro %cfcu-collect (&body body)
  `(let ((log '()))
     (handler-bind ((warning (lambda (c)
                               (when (search "undefined function" (princ-to-string c))
                                 (push (princ-to-string c) log))
                               (muffle-warning c))))
       (let ((*error-output* (make-broadcast-stream)))
         ,@body))
     (nreverse log)))

(deftest-compiled-only cf-compilation-unit-undefined.deferred-to-the-end
  (let ((marks '())
        (a (%cfcu-source "cfcu-a.lisp" "(defun cfcu-a () (cfcu-undefined-a))"))
        (b (%cfcu-source "cfcu-b.lisp" "(defun cfcu-b () (cfcu-undefined-b))")))
    (let ((log (handler-bind ((warning (lambda (c)
                                         (when (search "undefined function" (princ-to-string c))
                                           (push (list :warn (princ-to-string c)) marks))
                                         (muffle-warning c))))
                 (let ((*error-output* (make-broadcast-stream)))
                   (with-compilation-unit ()
                     (compile-file a :output-file (merge-pathnames "cfcu-a.fasl" (regression-temp-dir)))
                     (push :between marks)
                     (compile-file b :output-file (merge-pathnames "cfcu-b.fasl" (regression-temp-dir)))
                     (push :body-end marks))
                   (push :after marks)))))
      (declare (ignore log))
      (mapcar (lambda (m) (if (consp m) (car m) m)) (reverse marks))))
  (:between :body-end :warn :warn :after))

(deftest-compiled-only cf-compilation-unit-undefined.defined-later-is-not-reported
  (let ((a (%cfcu-source "cfcu-c.lisp" "(defun cfcu-c () (cfcu-later))")))
    (length (%cfcu-collect
              (with-compilation-unit ()
                (compile-file a :output-file (merge-pathnames "cfcu-c.fasl" (regression-temp-dir)))
                (eval '(defun cfcu-later () 1))))))
  0)

(deftest-compiled-only cf-compilation-unit-undefined.same-name-once
  (let ((a (%cfcu-source "cfcu-d.lisp" "(defun cfcu-d () (cfcu-same-undefined))"))
        (b (%cfcu-source "cfcu-e.lisp" "(defun cfcu-e () (cfcu-same-undefined))")))
    (length (%cfcu-collect
              (with-compilation-unit ()
                (compile-file a :output-file (merge-pathnames "cfcu-d.fasl" (regression-temp-dir)))
                (compile-file b :output-file (merge-pathnames "cfcu-e.fasl" (regression-temp-dir)))))))
  1)

(deftest-compiled-only cf-compilation-unit-undefined.outside-a-unit-per-file
  (let ((a (%cfcu-source "cfcu-f.lisp" "(defun cfcu-f () (cfcu-undefined-f))")))
    (length (%cfcu-collect
              (compile-file a :output-file (merge-pathnames "cfcu-f.fasl" (regression-temp-dir))))))
  1)
