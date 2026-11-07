;;; A (:type list) / (:type vector) structure checks a slot's :type, in the constructors
;;; (keyword and BOA) and in (setf accessor), as a standard structure does (and as SBCL
;;; does for these). They used to store anything. Values of the declared type, and
;;; slots with no :type, are unchanged.

(defstruct (dtsc-l (:type list)) (x 1 :type fixnum) (y nil))
(defstruct (dtsc-v (:type vector)) (x 1 :type fixnum) (name "n" :type string))
(defstruct (dtsc-nv (:type vector) :named) (x 1 :type fixnum))
(defstruct (dtsc-b (:type list) (:constructor dtsc-b-make (x &optional y))) (x 0 :type fixnum) (y 1 :type fixnum))

(deftest defstruct-typed-slot-check.keyword-constructor
  (list (handler-case (dtsc-l-x (make-dtsc-l :x "no")) (type-error () :type-error))
        (handler-case (dtsc-v-x (make-dtsc-v :x "no")) (type-error () :type-error))
        (handler-case (dtsc-nv-x (make-dtsc-nv :x 'a)) (type-error () :type-error)))
  (:type-error :type-error :type-error))

(deftest defstruct-typed-slot-check.setf-accessor
  (let ((l (make-dtsc-l)) (v (make-dtsc-v)))
    (list (handler-case (setf (dtsc-l-x l) "no") (type-error () :type-error))
          (handler-case (setf (dtsc-v-name v) 5) (type-error () :type-error))
          (dtsc-l-x l) (dtsc-v-name v)))
  (:type-error :type-error 1 "n"))

(deftest defstruct-typed-slot-check.boa-constructor
  (list (handler-case (dtsc-b-make "no") (type-error () :type-error))
        (handler-case (dtsc-b-make 1 "no") (type-error () :type-error))
        (dtsc-b-make 3))
  (:type-error :type-error (3 1)))

(deftest defstruct-typed-slot-check.valid-values-unchanged
  (let ((l (make-dtsc-l :x 5 :y "anything")) (v (make-dtsc-v :x 2 :name "abc")))
    (setf (dtsc-l-x l) 7 (dtsc-l-y l) 'free)
    (setf (dtsc-v-x v) 9)
    (list (dtsc-l-x l) (dtsc-l-y l) (dtsc-v-x v) (dtsc-v-name v) l (dtsc-nv-x (make-dtsc-nv))))
  (7 free 9 "abc" (7 free) 1))
