;;; DESTRUCTURING-BIND (and a macro lambda list) finds a &KEY key only in key
;;; positions of the keyword argument list.
;;;
;;; Each key used to be looked up with MEMBER over the whole list, so a keyword
;;; that is the VALUE of another key was taken for that key: with (:export
;;; :accessor), ACCESSOR was bound to the element after :ACCESSOR (NIL) instead
;;; of its default. hu.dwim.defclass-star's (slot 42 :export :accessor) lost the
;;; slot's accessor that way.

(deftest destructuring-key-position.value-is-a-key
  (destructuring-bind (&key (accessor 'missing) (export 'missing)) '(:export :accessor)
    (list accessor export))
  (missing :accessor))

(deftest destructuring-key-position.supplied-p
  (destructuring-bind (&key (a 'm a-p) (b 'm b-p)) '(:b :a)
    (list a a-p b b-p))
  (m nil :a t))

(deftest destructuring-key-position.first-occurrence-wins
  (destructuring-bind (&key a) '(:a 1 :a 2) a)
  1)

(defmacro dkp-macro (&key (a 'm) (b 'm)) `'(,a ,b))

(deftest destructuring-key-position.macro-lambda-list
  (dkp-macro :b :a)
  (m :a))
