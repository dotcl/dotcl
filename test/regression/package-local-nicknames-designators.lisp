;;; Package-local nicknames, as trivial-package-local-nicknames' own suite
;;; checks them on every implementation. A local nickname of the current
;;; package has to work wherever a package is named, not only in the reader and
;;; FIND-PACKAGE; adding one that clashes has to signal PACKAGE-ERROR; and a
;;; deleted package has to disappear from every nickname table.

(defun %pln-reset ()
  (dolist (n '("PLN-TEST-1" "PLN-TEST-2"))
    (when (find-package n) (delete-package n)))
  (make-package "PLN-TEST-2" :use nil)
  (export (intern "CONS" "PLN-TEST-2") "PLN-TEST-2")
  (let ((p (make-package "PLN-TEST-1" :use nil)))
    (dotcl:add-package-local-nickname :l :cl p)
    p))

(deftest pln-find-symbol-with-local-nickname
  (let ((*package* (%pln-reset)))
    (multiple-value-list (find-symbol "CONS" :l)))
  (cons :external))

(deftest pln-find-symbol-with-character-designator
  (let ((*package* (%pln-reset)))
    (multiple-value-list (find-symbol "CONS" #\L)))
  (cons :external))

(deftest pln-find-package-with-character-designator
  (let ((*package* (%pln-reset)))
    (eq (find-package #\L) (find-package :cl)))
  t)

(deftest pln-intern-with-local-nickname
  (let ((*package* (%pln-reset)))
    (dotcl:add-package-local-nickname :two "PLN-TEST-2")
    (eq (intern "FOO" :two) (find-symbol "FOO" "PLN-TEST-2")))
  t)

(deftest pln-collision-signals-package-error
  (let ((p (%pln-reset)))
    (handler-case (progn (dotcl:add-package-local-nickname :l "PLN-TEST-2" p) :no-error)
      (package-error () :package-error)))
  :package-error)

(deftest pln-collision-continue-keeps-old
  (let ((p (%pln-reset)))
    (handler-bind ((package-error #'continue))
      (dotcl:add-package-local-nickname :l "PLN-TEST-2" p))
    (eq (cdr (assoc "L" (dotcl:package-local-nicknames p) :test #'string=))
        (find-package :cl)))
  t)

(deftest pln-same-nickname-twice-is-fine
  (let ((p (%pln-reset)))
    (dotcl:add-package-local-nickname :l :cl p)
    (dotcl:add-package-local-nickname #\L :cl p)
    (length (dotcl:package-local-nicknames p)))
  1)

(deftest pln-remove-with-character
  (let ((p (%pln-reset)))
    (values (not (null (dotcl:remove-package-local-nickname #\L p)))
            (dotcl:package-local-nicknames p)))
  t nil)

(deftest pln-own-name-signals-then-continue-wins
  (let ((p (%pln-reset)))
    (values
     (handler-case (progn (dotcl:add-package-local-nickname "PLN-TEST-1" "PLN-TEST-2" p) :no-error)
       (package-error () :package-error))
     (progn
       (handler-bind ((package-error #'continue))
         (dotcl:add-package-local-nickname "PLN-TEST-1" "PLN-TEST-2" p))
       (let ((*package* p))
         (eq (intern "FOO" "PLN-TEST-1") (find-symbol "FOO" "PLN-TEST-2"))))))
  :package-error t)

(deftest pln-delete-nicknamed-package-drops-nickname
  (let ((p (%pln-reset)))
    (dotcl:add-package-local-nickname :two "PLN-TEST-2" p)
    (delete-package "PLN-TEST-2")
    (mapcar #'car (dotcl:package-local-nicknames p)))
  ("L"))

(deftest pln-delete-nicknaming-package-leaves-no-trace
  (let* ((p (%pln-reset))
         (two (find-package "PLN-TEST-2")))
    (dotcl:add-package-local-nickname :two two p)
    (delete-package p)
    (dotcl:package-locally-nicknamed-by-list two))
  nil)

(deftest pln-cleanup
  (progn (dolist (n '("PLN-TEST-1" "PLN-TEST-2"))
           (when (find-package n) (delete-package n)))
         t)
  t)
