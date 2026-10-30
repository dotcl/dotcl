;;; Two package units each define PUNAME::MCX, PUNAME::TF and PUNAME::PBOX.
;;; Tables keyed by a symbol's name (method combinations, TRACE, the pprint
;;; dispatch table) must keep the two units' same-named symbols apart.

(defvar *punt-a* (dotcl::%make-package-unit "PUNT-A" "PUNAME"))
(defvar *punt-b* (dotcl::%make-package-unit "PUNT-B" "PUNAME"))

(defun punt-in (unit thunk) (dotcl::%call-in-package-unit unit thunk))
(defun punt-ev (string) (eval (read-from-string string)))

(defun punt-define (operator tag)
  (punt-ev "(defpackage \"PUNAME\" (:use \"COMMON-LISP\"))")
  (let ((*package* (find-package "PUNAME")))
    (punt-ev (format nil "(progn
      (define-method-combination mcx :operator ~A)
      (defgeneric gx (x) (:method-combination mcx))
      (defmethod gx mcx ((x integer)) 5)
      (defun tf (x) x)
      (defstruct pbox v)
      (set-pprint-dispatch 'pbox (lambda (s o) (declare (ignore o)) (write-string ~S s))))"
                     operator tag))))

(punt-in *punt-a* (lambda () (punt-define "+" "AAA")))
(punt-in *punt-b* (lambda () (punt-define "list" "BBB")))

(deftest package-units-method-combination-own-unit
  (list (punt-in *punt-a* (lambda () (punt-ev "(puname::gx 1)")))
        (punt-in *punt-b* (lambda () (punt-ev "(puname::gx 1)"))))
  (5 (5)))

(deftest package-units-trace-own-unit
  (flet ((traced-output (unit)
           (punt-in unit
                    (lambda ()
                      (punt-ev "(trace puname::tf)")
                      (unwind-protect
                           (with-output-to-string (*trace-output*)
                             (punt-ev "(puname::tf 1)"))
                        (punt-ev "(untrace)"))))))
    (let ((a (traced-output *punt-a*))
          (b (traced-output *punt-b*)))
      (list (plusp (length a)) (plusp (length b)))))
  (t t))

(deftest package-units-pprint-dispatch-own-unit
  (flet ((shown (unit)
           (punt-in unit (lambda ()
                           (let ((*print-pretty* t))
                             (princ-to-string (punt-ev "(puname::make-pbox)")))))))
    (list (shown *punt-a*) (shown *punt-b*)))
  ("AAA" "BBB"))

;;; DEFPACKAGE source locations, compound pprint dispatch specifiers and the
;;; unknown-type warning memo are keyed by the package, not its short name.

(punt-in *punt-a* (lambda ()
                    (punt-ev "(set-pprint-dispatch '(or puname::pbox (eql :punt-zzz))
                                (lambda (s o) (declare (ignore o)) (write-string \"A-OR\" s)) 10)")))
(punt-in *punt-b* (lambda ()
                    (punt-ev "(set-pprint-dispatch '(or puname::pbox (eql :punt-zzz))
                                (lambda (s o) (declare (ignore o)) (write-string \"B-OR\" s)) 10)")))

(deftest package-units-pprint-dispatch-compound-own-unit
  (flet ((shown (unit)
           (punt-in unit (lambda ()
                           (let ((*print-pretty* t))
                             (princ-to-string (punt-ev "(puname::make-pbox)")))))))
    (list (shown *punt-a*) (shown *punt-b*)))
  ("A-OR" "B-OR"))

(deftest-emitting-only package-units-unknown-type-warning-own-unit
  (flet ((warnings (unit)
           (punt-in unit
                    (lambda ()
                      (let ((n 0))
                        (handler-bind ((warning (lambda (c) (incf n) (muffle-warning c))))
                          (punt-ev "(compile nil '(lambda (x) (declare (type puname::punt-nosuch x)) x))")
                          (punt-ev "(compile nil '(lambda (x) (declare (type puname::punt-nosuch x)) x))"))
                        n)))))
    (list (warnings *punt-a*) (warnings *punt-b*)))
  (1 1))

(defun punt-source (dir name line-count)
  (let ((path (merge-pathnames name dir)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (dotimes (i line-count) (terpri s))
      (write-line "(defpackage \"PUNAME\" (:use \"COMMON-LISP\"))" s))
    (namestring path)))

(defun punt-package-line (unit)
  (punt-in unit (lambda ()
                  (let ((plist (first (dotcl:definition-sources "PUNAME"))))
                    (and plist (getf plist :line))))))

(deftest package-units-defpackage-source-own-unit
  (let ((dir (regression-temp-dir)))
    (punt-in *punt-a* (lambda () (load (punt-source dir "punt-a-pkg.lisp" 0))))
    (punt-in *punt-b* (lambda () (load (punt-source dir "punt-b-pkg.lisp" 4))))
    (let ((before (list (punt-package-line *punt-a*) (punt-package-line *punt-b*))))
      (punt-in *punt-a* (lambda () (delete-package "PUNAME")))
      (list before (punt-package-line *punt-a*) (punt-package-line *punt-b*))))
  ((1 5) nil 5))
