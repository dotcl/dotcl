;;; A DEFSTRUCT named by a COMMON-LISP symbol (integer, list, string) signalled an
;;; error only after it had changed the type tables: afterwards (typep 5 'integer)
;;; was false and every later DEFSTRUCT failed. The name is refused up front now.

(deftest defstruct-cl-symbol-name.refused-without-damage
  (flet ((try (form) (handler-case (progn (eval form) :defined) (error () :error))))
    (list (try '(defstruct integer x))
          (try '(defstruct (string) x))
          (try '(defstruct list x))
          (typep 5 'integer)
          (typep "a" 'string)
          (typep nil 'list)))
  (:error :error :error t t t))

(deftest defstruct-cl-symbol-name.later-defstruct-works
  (progn
    (ignore-errors (eval '(defstruct list x)))
    (eval '(defstruct dcsn-ok a))
    (funcall (intern "DCSN-OK-A") (funcall (intern "MAKE-DCSN-OK") :a 5)))
  5)
