;;; PROGV on a constant variable is an error, not a silent no-op that leaves the
;;; constant and its old value in place. The check happens before anything is
;;; bound, so a bad symbol leaves the earlier ones unbound too.

(defvar *pc-dyn* 1)

(defun pc-progv (syms vals) (progv syms vals (list *pc-dyn*)))

(defmacro pc-kind (form)
  `(handler-case (progn ,form :no-error)
     (program-error () :program-error)
     (type-error () :type-error)))

(deftest progv-constant.defconstant-and-cl-constants
  (list (pc-kind (pc-progv '(pi) '(3)))
        (pc-kind (pc-progv '(t) '(3)))
        (pc-kind (pc-progv '(nil) '(3)))
        (pc-kind (pc-progv '(:keyword) '(3)))
        (pc-kind (pc-progv '(most-positive-fixnum) '(3))))
  (:program-error :program-error :program-error :program-error :program-error))

(deftest progv-constant.nothing-is-bound-when-one-symbol-is-bad
  (list (pc-kind (pc-progv '(*pc-dyn* pi) '(2 3)))
        *pc-dyn*)
  (:program-error 1))

(deftest progv-constant.ordinary-progv-still-works
  (list (pc-progv '(*pc-dyn*) '(5)) (pc-progv '(*pc-dyn* #:other) '(6)) *pc-dyn*)
  ((5) (6) 1))

(deftest progv-constant.not-a-symbol-is-a-type-error
  (pc-kind (pc-progv '(5) '(1)))
  :type-error)
