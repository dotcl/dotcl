;;; MAP with one list, vector or string and a function object, and SUBSTITUTE of a character in a
;;; string, are done without the generic intermediate steps. Results, result types, the order the
;;; function is called in, multiple values, and the errors are as before; the expected values are
;;; those of the build before the change.

(deftest map-substitute-fast.1
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION 1+) (QUOTE (1 2 3))))) (error () (list :error)))
  ((2 3 4)))

(deftest map-substitute-fast.2
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION 1+) #(1 2 3)))) (error () (list :error)))
  ((2 3 4)))

(deftest map-substitute-fast.3
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION CHAR-UPCASE) "abc"))) (error () (list :error)))
  ((#\A #\B #\C)))

(deftest map-substitute-fast.4
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION 1+) NIL))) (error () (list :error)))
  (NIL))

(deftest map-substitute-fast.5
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION 1+) #()))) (error () (list :error)))
  (NIL))

(deftest map-substitute-fast.6
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION 1+) ""))) (error () (list :error)))
  (NIL))

(deftest map-substitute-fast.7
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE VECTOR) (FUNCTION 1+) (QUOTE (1 2 3))))) (error () (list :error)))
  ((:VEC (SIMPLE-VECTOR 3) (2 3 4))))

(deftest map-substitute-fast.8
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE VECTOR) (FUNCTION 1+) #(1 2 3)))) (error () (list :error)))
  ((:VEC (SIMPLE-VECTOR 3) (2 3 4))))

(deftest map-substitute-fast.9
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE VECTOR) (FUNCTION CHAR-CODE) "abc"))) (error () (list :error)))
  ((:VEC (SIMPLE-VECTOR 3) (97 98 99))))

(deftest map-substitute-fast.10
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE VECTOR) (FUNCTION 1+) NIL))) (error () (list :error)))
  ((:VEC (SIMPLE-VECTOR 0) NIL)))

(deftest map-substitute-fast.11
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE SIMPLE-VECTOR) (FUNCTION 1+) (QUOTE (1 2))))) (error () (list :error)))
  ((:VEC (SIMPLE-VECTOR 2) (2 3))))

(deftest map-substitute-fast.12
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE SIMPLE-VECTOR) (FUNCTION 1+) #(1 2)))) (error () (list :error)))
  ((:VEC (SIMPLE-VECTOR 2) (2 3))))

(deftest map-substitute-fast.13
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (TYPE-OF (MAP (QUOTE VECTOR) (FUNCTION 1+) (QUOTE (1 2)))))) (error () (list :error)))
  ((SIMPLE-VECTOR 2)))

(deftest map-substitute-fast.14
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (ARRAY-ELEMENT-TYPE (MAP (QUOTE VECTOR) (FUNCTION 1+) #(1 2))))) (error () (list :error)))
  (T))

(deftest map-substitute-fast.15
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP NIL (FUNCTION 1+) (QUOTE (1 2))))) (error () (list :error)))
  (NIL))

(deftest map-substitute-fast.16
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (LET ((R NIL)) (MAP NIL (LAMBDA (X) (PUSH X R)) (QUOTE (1 2 3))) R))) (error () (list :error)))
  ((3 2 1)))

(deftest map-substitute-fast.17
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (LET ((R NIL)) (MAP NIL (LAMBDA (X) (PUSH X R)) #(1 2 3)) R))) (error () (list :error)))
  ((3 2 1)))

(deftest map-substitute-fast.18
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (LET ((R NIL)) (MAP NIL (LAMBDA (X) (PUSH X R)) "abc") R))) (error () (list :error)))
  ((#\c #\b #\a)))

(deftest map-substitute-fast.19
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (LET ((R NIL)) (MAP (QUOTE LIST) (LAMBDA (X) (PUSH X R) X) (QUOTE (1 2 3))) (REVERSE R)))) (error () (list :error)))
  ((1 2 3)))

(deftest map-substitute-fast.20
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE STRING) (FUNCTION CHAR-UPCASE) "abc"))) (error () (list :error)))
  ("ABC"))

(deftest map-substitute-fast.21
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE STRING) (FUNCTION IDENTITY) (QUOTE (#\a #\b))))) (error () (list :error)))
  ("ab"))

(deftest map-substitute-fast.22
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE (VECTOR FIXNUM)) (FUNCTION 1+) (QUOTE (1 2))))) (error () (list :error)))
  ((:VEC (SIMPLE-ARRAY FIXNUM (2)) (2 3))))

(deftest map-substitute-fast.23
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE (SIMPLE-ARRAY FIXNUM (*))) (FUNCTION 1+) #(1 2)))) (error () (list :error)))
  ((:VEC (SIMPLE-ARRAY FIXNUM (2)) (2 3))))

(deftest map-substitute-fast.24
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION 1+) (QUOTE (1 2)) (QUOTE (10 20))))) (error () (list :error)))
  (:ERROR))

(deftest map-substitute-fast.25
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION +) (QUOTE (1 2 3)) #(10 20)))) (error () (list :error)))
  ((11 22)))

(deftest map-substitute-fast.26
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (QUOTE CAR) (QUOTE ((1) (2)))))) (error () (list :error)))
  ((1 2)))

(deftest map-substitute-fast.27
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (QUOTE SFN-MAP-UNDEFINED-XYZ) (QUOTE (1))))) (error () (list :error)))
  (:ERROR))

(deftest map-substitute-fast.28
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (QUOTE SFN-MAP-UNDEFINED-XYZ) NIL))) (error () (list :error)))
  (NIL))

(deftest map-substitute-fast.29
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION IDENTITY) 5))) (error () (list :error)))
  (:ERROR))

(deftest map-substitute-fast.30
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION IDENTITY) (QUOTE (1 2 . 3))))) (error () (list :error)))
  (:ERROR))

(deftest map-substitute-fast.31
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP NIL (FUNCTION IDENTITY) (QUOTE (1 2 . 3))))) (error () (list :error)))
  (:ERROR))

(deftest map-substitute-fast.32
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE VECTOR) (FUNCTION IDENTITY) (QUOTE (1 2 . 3))))) (error () (list :error)))
  (:ERROR))

(deftest map-substitute-fast.33
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (HANDLER-CASE (MAP (QUOTE LIST) (FUNCTION CAR) (QUOTE (1 2))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest map-substitute-fast.34
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (HANDLER-CASE (MAP (QUOTE LIST) (FUNCTION (LAMBDA (A B) A)) (QUOTE (1 2))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest map-substitute-fast.35
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (HANDLER-CASE (MAP (QUOTE LIST) (FUNCTION 1+) #(1 A)) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest map-substitute-fast.36
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (HANDLER-CASE (MAP (QUOTE FOO) (FUNCTION 1+) (QUOTE (1))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest map-substitute-fast.37
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (HANDLER-CASE (MAP) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest map-substitute-fast.38
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (MAP (QUOTE LIST)))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest map-substitute-fast.39
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION FLOOR) (QUOTE (5 7))))) (error () (list :error)))
  ((5 7)))

(deftest map-substitute-fast.40
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (LAMBDA (X) (VALUES X 1)) (QUOTE (1 2))))) (error () (list :error)))
  ((1 2)))

(deftest map-substitute-fast.41
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION IDENTITY) (MAKE-ARRAY 3 :FILL-POINTER 2 :INITIAL-CONTENTS (QUOTE (1 2 3)))))) (error () (list :error)))
  ((1 2)))

(deftest map-substitute-fast.42
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION IDENTITY) (MAKE-ARRAY 3 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)) :INITIAL-CONTENTS (QUOTE (1 2 3)))))) (error () (list :error)))
  ((1 2 3)))

(deftest map-substitute-fast.43
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE VECTOR) (FUNCTION IDENTITY) "ab"))) (error () (list :error)))
  ((:VEC (SIMPLE-VECTOR 2) (#\a #\b))))

(deftest map-substitute-fast.44
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION IDENTITY) (MAKE-ARRAY (QUOTE (2 2)) :INITIAL-CONTENTS (QUOTE ((1 2) (3 4))))))) (error () (list :error)))
  ((1 2 3 4)))

(deftest map-substitute-fast.45
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (LET ((L (LIST 1 2 3))) (MAP (QUOTE LIST) (LAMBDA (X) (WHEN (= X 1) (SETF (CDR (CDR L)) NIL)) X) L)))) (error () (list :error)))
  ((1 2)))

(deftest map-substitute-fast.46
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (LET ((V (VECTOR 1 2 3))) (MAP (QUOTE LIST) (LAMBDA (X) (WHEN (= X 1) (SETF (AREF V 2) 99)) X) V)))) (error () (list :error)))
  ((1 2 99)))

(deftest map-substitute-fast.47
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (LET ((N 0)) (MAP NIL (LAMBDA (X) (INCF N X)) (MAKE-LIST 5 :INITIAL-ELEMENT 2)) N))) (error () (list :error)))
  (10))

(deftest map-substitute-fast.48
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MAP (QUOTE LIST) (FUNCTION IDENTITY) (LIST 1 2 3)))) (error () (list :error)))
  ((1 2 3)))

(deftest map-substitute-fast.49
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (EQ NIL (MAP (QUOTE LIST) (FUNCTION IDENTITY) NIL)))) (error () (list :error)))
  (T))

(deftest map-substitute-fast.50
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MULTIPLE-VALUE-LIST (MAP (QUOTE LIST) (FUNCTION IDENTITY) (QUOTE (1 2)))))) (error () (list :error)))
  (((1 2))))

(deftest map-substitute-fast.51
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (MULTIPLE-VALUE-LIST (MAP NIL (FUNCTION IDENTITY) (QUOTE (1 2)))))) (error () (list :error)))
  ((NIL)))

(deftest map-substitute-fast.52
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (LET ((S (MAKE-STRING 3 :INITIAL-ELEMENT #\a))) (SUBSTITUTE #\b #\a S)))) (error () (list :error)))
  ("bbb"))

(deftest map-substitute-fast.53
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a "banana"))) (error () (list :error)))
  ("bbnbnb"))

(deftest map-substitute-fast.54
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a "banana" :COUNT 2))) (error () (list :error)))
  ("bbnbna"))

(deftest map-substitute-fast.55
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a "banana" :START 2))) (error () (list :error)))
  ("banbnb"))

(deftest map-substitute-fast.56
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a "banana" :END 3))) (error () (list :error)))
  ("bbnana"))

(deftest map-substitute-fast.57
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a "banana" :START 1 :END 4 :COUNT 1))) (error () (list :error)))
  ("bbnana"))

(deftest map-substitute-fast.58
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a "banana" :FROM-END T :COUNT 1))) (error () (list :error)))
  ("bananb"))

(deftest map-substitute-fast.59
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\A "banana"))) (error () (list :error)))
  ("banana"))

(deftest map-substitute-fast.60
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\A "banana" :TEST (FUNCTION CHAR-EQUAL)))) (error () (list :error)))
  ("bbnbnb"))

(deftest map-substitute-fast.61
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a "banana" :TEST (FUNCTION CHAR=)))) (error () (list :error)))
  ("bbnbnb"))

(deftest map-substitute-fast.62
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a "banana" :TEST-NOT (FUNCTION EQL)))) (error () (list :error)))
  ("bababa"))

(deftest map-substitute-fast.63
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a "banana" :KEY (FUNCTION CHAR-UPCASE)))) (error () (list :error)))
  ("banana"))

(deftest map-substitute-fast.64
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\A "banana" :KEY (FUNCTION CHAR-UPCASE)))) (error () (list :error)))
  ("bbnbnb"))

(deftest map-substitute-fast.65
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE 5 #\a "banana"))) (error () (list :error)))
  (:ERROR))

(deftest map-substitute-fast.66
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (HANDLER-CASE (SUBSTITUTE 5 #\a "xyz") (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  ("xyz"))

(deftest map-substitute-fast.67
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (HANDLER-CASE (SUBSTITUTE 5 #\a "banana" :COUNT 0) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  ("banana"))

(deftest map-substitute-fast.68
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b 97 "banana"))) (error () (list :error)))
  ("banana"))

(deftest map-substitute-fast.69
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a ""))) (error () (list :error)))
  (""))

(deftest map-substitute-fast.70
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a (COERCE "banana" (QUOTE LIST))))) (error () (list :error)))
  ((#\b #\b #\n #\b #\n #\b)))

(deftest map-substitute-fast.71
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (SUBSTITUTE #\b #\a (MAKE-ARRAY 6 :ELEMENT-TYPE (QUOTE CHARACTER) :INITIAL-CONTENTS "banana")))) (error () (list :error)))
  ("bbnbnb"))

(deftest map-substitute-fast.72
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (list :vec (type-of x) (coerce x (quote list))) x)) (multiple-value-list (HANDLER-CASE (SUBSTITUTE #\b #\a "banana" :START 9) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

