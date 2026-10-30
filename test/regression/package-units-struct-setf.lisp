;;; Two package units each define a structure PUSTRUCT::BOX. The SETF of its
;;; accessor compiled in one unit, after the other unit defined its BOX, must
;;; store into this unit's BOX: the SETF expander is keyed by the package's own
;;; name, not by the name the unit shows (PUSTRUCT in both).

(defun pustruct-make-unit (name)
  (dotcl::%make-package-unit name "PUSTRUCT"))

(defvar *pustruct-a* (pustruct-make-unit "PUSTRUCT-A"))
(defvar *pustruct-b* (pustruct-make-unit "PUSTRUCT-B"))

(defun pustruct-in (unit thunk) (dotcl::%call-in-package-unit unit thunk))

(defun pustruct-define ()
  (eval '(defpackage "PUSTRUCT" (:use "COMMON-LISP")))
  (let ((*package* (find-package "PUSTRUCT")))
    (eval (read-from-string "(defstruct box (v nil))"))))

(pustruct-in *pustruct-a* #'pustruct-define)
(pustruct-in *pustruct-b* #'pustruct-define)

(defun pustruct-set ()
  (let ((b (funcall (read-from-string "pustruct::make-box"))))
    (funcall (compile nil (read-from-string "(lambda (b) (setf (pustruct::box-v b) :set) b)")) b)
    (funcall (read-from-string "pustruct::box-v") b)))

(deftest-emitting-only package-units-struct-setf-own-unit
  (list (pustruct-in *pustruct-a* #'pustruct-set)
        (pustruct-in *pustruct-b* #'pustruct-set))
  (:set :set))
