;;; FIND / POSITION / COUNT / REMOVE / DELETE / SUBSTITUTE with the default EQL test, and
;;; MEMBER / ASSOC / ADJOIN / the set functions with :TEST #'EQUAL, take a direct compare for
;;; the common items. Results, errors and the order the test and key are called in are the
;;; same as before; the expected values are those of the build before the fast paths.

(deftest seq-eql-fast.1
  (handler-case (multiple-value-list (FIND 3 (QUOTE (1 2 3 4)))) (error () (list :error)))
  (3))

(deftest seq-eql-fast.2
  (handler-case (multiple-value-list (FIND 3 #(1 2 3 4))) (error () (list :error)))
  (3))

(deftest seq-eql-fast.3
  (handler-case (multiple-value-list (FIND 3.0 (QUOTE (1 2 3.0 4)))) (error () (list :error)))
  (3.0))

(deftest seq-eql-fast.4
  (handler-case (multiple-value-list (FIND 3.0 (QUOTE (1 2 3 4)))) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.5
  (handler-case (multiple-value-list (FIND 3 (QUOTE (1 2 3.0 4)))) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.6
  (handler-case (multiple-value-list (FIND 1000000000000000000000 (LIST 1 1000000000000000000000 2))) (error () (list :error)))
  (1000000000000000000000))

(deftest seq-eql-fast.7
  (handler-case (multiple-value-list (FIND #\a "bab")) (error () (list :error)))
  (#\a))

(deftest seq-eql-fast.8
  (handler-case (multiple-value-list (FIND #\a (QUOTE (#\b #\a)))) (error () (list :error)))
  (#\a))

(deftest seq-eql-fast.9
  (handler-case (multiple-value-list (FIND (QUOTE B) (QUOTE (A B C)))) (error () (list :error)))
  (B))

(deftest seq-eql-fast.10
  (handler-case (multiple-value-list (FIND NIL (QUOTE (A NIL C)))) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.11
  (handler-case (multiple-value-list (FIND T (QUOTE (A T C)))) (error () (list :error)))
  (T))

(deftest seq-eql-fast.12
  (handler-case (multiple-value-list (FIND "ab" (LIST "ab" "cd"))) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.13
  (handler-case (multiple-value-list (LET ((S "ab")) (FIND S (LIST "ab" S)))) (error () (list :error)))
  ("ab"))

(deftest seq-eql-fast.14
  (handler-case (multiple-value-list (LET ((C (LIST 1))) (FIND C (LIST (LIST 1) C)))) (error () (list :error)))
  ((1)))

(deftest seq-eql-fast.15
  (handler-case (multiple-value-list (FIND 2 (QUOTE (1 2 3)) :KEY (FUNCTION 1+))) (error () (list :error)))
  (1))

(deftest seq-eql-fast.16
  (handler-case (multiple-value-list (FIND 2 (QUOTE (1 2 3)) :TEST (FUNCTION <))) (error () (list :error)))
  (3))

(deftest seq-eql-fast.17
  (handler-case (multiple-value-list (FIND 2 (QUOTE (1 2 3)) :TEST-NOT (FUNCTION EQL))) (error () (list :error)))
  (1))

(deftest seq-eql-fast.18
  (handler-case (multiple-value-list (FIND 2 (QUOTE (2 2 2)) :FROM-END T)) (error () (list :error)))
  (2))

(deftest seq-eql-fast.19
  (handler-case (multiple-value-list (FIND 5 (QUOTE (1 2 3 4 5)) :START 1 :END 4)) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.20
  (handler-case (multiple-value-list (FIND 5 (QUOTE (1 2 3 4 5)) :START 1 :END 5)) (error () (list :error)))
  (5))

(deftest seq-eql-fast.21
  (handler-case (multiple-value-list (FIND 3 (QUOTE (1 2 3)) :TEST (FUNCTION EQL))) (error () (list :error)))
  (3))

(deftest seq-eql-fast.22
  (handler-case (multiple-value-list (FIND 3 (QUOTE (1 2 3)) :TEST (QUOTE EQL))) (error () (list :error)))
  (3))

(deftest seq-eql-fast.23
  (handler-case (multiple-value-list (FIND 3 (QUOTE (1 2 3)) :TEST (FUNCTION =))) (error () (list :error)))
  (3))

(deftest seq-eql-fast.24
  (handler-case (multiple-value-list (FIND NIL (QUOTE NIL))) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.25
  (handler-case (multiple-value-list (FIND 3 NIL)) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.26
  (handler-case (multiple-value-list (HANDLER-CASE (FIND 3 (QUOTE (1 2 . 3))) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest seq-eql-fast.27
  (handler-case (multiple-value-list (HANDLER-CASE (FIND 3 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest seq-eql-fast.28
  (handler-case (multiple-value-list (HANDLER-CASE (FIND 3 (QUOTE (1 2 3)) :START 5) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest seq-eql-fast.29
  (handler-case (multiple-value-list (MULTIPLE-VALUE-LIST (FIND 3 (QUOTE (1 2 3))))) (error () (list :error)))
  ((3)))

(deftest seq-eql-fast.30
  (handler-case (multiple-value-list (POSITION 3 (QUOTE (1 2 3 4)))) (error () (list :error)))
  (2))

(deftest seq-eql-fast.31
  (handler-case (multiple-value-list (POSITION 3 #(1 2 3 4))) (error () (list :error)))
  (2))

(deftest seq-eql-fast.32
  (handler-case (multiple-value-list (POSITION 3 (QUOTE (1 2 3 3)) :FROM-END T)) (error () (list :error)))
  (3))

(deftest seq-eql-fast.33
  (handler-case (multiple-value-list (POSITION (QUOTE C) (QUOTE (A B C)))) (error () (list :error)))
  (2))

(deftest seq-eql-fast.34
  (handler-case (multiple-value-list (POSITION 4 (QUOTE (1 2 3)))) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.35
  (handler-case (multiple-value-list (POSITION 3 (QUOTE (1 3 3)) :START 2)) (error () (list :error)))
  (2))

(deftest seq-eql-fast.36
  (handler-case (multiple-value-list (POSITION 3 (QUOTE (1 3 3)) :END 1)) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.37
  (handler-case (multiple-value-list (POSITION 3.5d0 (QUOTE (1 3.5d0 3)))) (error () (list :error)))
  (1))

(deftest seq-eql-fast.38
  (handler-case (multiple-value-list (POSITION 2 (QUOTE ((1) (2) (3))) :KEY (FUNCTION CAR))) (error () (list :error)))
  (1))

(deftest seq-eql-fast.39
  (handler-case (multiple-value-list (COUNT 3 (QUOTE (1 3 3 4)))) (error () (list :error)))
  (2))

(deftest seq-eql-fast.40
  (handler-case (multiple-value-list (COUNT 3 #(1 3 3 4))) (error () (list :error)))
  (2))

(deftest seq-eql-fast.41
  (handler-case (multiple-value-list (COUNT (QUOTE A) (QUOTE (A B A)))) (error () (list :error)))
  (2))

(deftest seq-eql-fast.42
  (handler-case (multiple-value-list (COUNT "a" (LIST "a" "a"))) (error () (list :error)))
  (0))

(deftest seq-eql-fast.43
  (handler-case (multiple-value-list (COUNT 3 (QUOTE (1 3 3 4)) :START 2)) (error () (list :error)))
  (1))

(deftest seq-eql-fast.44
  (handler-case (multiple-value-list (COUNT 3 (QUOTE (1 3 3 4)) :FROM-END T)) (error () (list :error)))
  (2))

(deftest seq-eql-fast.45
  (handler-case (multiple-value-list (COUNT 3 (QUOTE (1 3 3 4)) :KEY (FUNCTION 1+))) (error () (list :error)))
  (0))

(deftest seq-eql-fast.46
  (handler-case (multiple-value-list (COUNT 3.0 (QUOTE (3 3.0 3.0)))) (error () (list :error)))
  (2))

(deftest seq-eql-fast.47
  (handler-case (multiple-value-list (REMOVE 3 (QUOTE (1 3 2 3)))) (error () (list :error)))
  ((1 2)))

(deftest seq-eql-fast.48
  (handler-case (mapcar (lambda (x) (if (vectorp x) (coerce x 'list) x)) (multiple-value-list (REMOVE 3 #(1 3 2 3)))) (error () (list :error)))
  ((1 2)))

(deftest seq-eql-fast.49
  (handler-case (multiple-value-list (REMOVE 3 (QUOTE (1 3 2 3)) :COUNT 1)) (error () (list :error)))
  ((1 2 3)))

(deftest seq-eql-fast.50
  (handler-case (multiple-value-list (REMOVE 3 (QUOTE (1 3 2 3)) :COUNT 1 :FROM-END T)) (error () (list :error)))
  ((1 3 2)))

(deftest seq-eql-fast.51
  (handler-case (multiple-value-list (REMOVE (QUOTE A) (QUOTE (A B A C)))) (error () (list :error)))
  ((B C)))

(deftest seq-eql-fast.52
  (handler-case (multiple-value-list (REMOVE 3 (QUOTE (1 2)))) (error () (list :error)))
  ((1 2)))

(deftest seq-eql-fast.53
  (handler-case (multiple-value-list (LET ((L (LIST 1 2))) (EQ L (REMOVE 3 L)))) (error () (list :error)))
  (T))

(deftest seq-eql-fast.54
  (handler-case (multiple-value-list (REMOVE 3 (QUOTE (1 3 2 3)) :START 2)) (error () (list :error)))
  ((1 3 2)))

(deftest seq-eql-fast.55
  (handler-case (multiple-value-list (REMOVE 3 (QUOTE (1 3 2 3)) :KEY (FUNCTION 1+))) (error () (list :error)))
  ((1 3 3)))

(deftest seq-eql-fast.56
  (handler-case (multiple-value-list (REMOVE #\a "banana")) (error () (list :error)))
  ("bnn"))

(deftest seq-eql-fast.57
  (handler-case (multiple-value-list (REMOVE 3 (QUOTE (3 3 3)))) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.58
  (handler-case (multiple-value-list (DELETE 3 (LIST 1 3 2 3))) (error () (list :error)))
  ((1 2)))

(deftest seq-eql-fast.59
  (handler-case (mapcar (lambda (x) (if (vectorp x) (coerce x 'list) x)) (multiple-value-list (DELETE 3 (VECTOR 1 3 2 3)))) (error () (list :error)))
  ((1 2)))

(deftest seq-eql-fast.60
  (handler-case (multiple-value-list (DELETE (QUOTE A) (LIST (QUOTE A) (QUOTE B) (QUOTE A)))) (error () (list :error)))
  ((B)))

(deftest seq-eql-fast.61
  (handler-case (multiple-value-list (DELETE 3 (LIST 3 3 3))) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.62
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 4 :ADJUSTABLE T :FILL-POINTER 4 :INITIAL-CONTENTS (QUOTE (1 3 2 3))))) (DELETE 3 V) (LIST (FILL-POINTER V) (COERCE V (QUOTE LIST))))) (error () (list :error)))
  ((2 (1 2))))

(deftest seq-eql-fast.63
  (handler-case (multiple-value-list (SUBSTITUTE 9 3 (QUOTE (1 3 2 3)))) (error () (list :error)))
  ((1 9 2 9)))

(deftest seq-eql-fast.64
  (handler-case (mapcar (lambda (x) (if (vectorp x) (coerce x 'list) x)) (multiple-value-list (SUBSTITUTE 9 3 #(1 3 2 3)))) (error () (list :error)))
  ((1 9 2 9)))

(deftest seq-eql-fast.65
  (handler-case (multiple-value-list (SUBSTITUTE 9 (QUOTE A) (QUOTE (A B A)))) (error () (list :error)))
  ((9 B 9)))

(deftest seq-eql-fast.66
  (handler-case (multiple-value-list (SUBSTITUTE 9 3 (QUOTE (1 3 2 3)) :COUNT 1)) (error () (list :error)))
  ((1 9 2 3)))

(deftest seq-eql-fast.67
  (handler-case (multiple-value-list (SUBSTITUTE 9 3.0 (QUOTE (3 3.0)))) (error () (list :error)))
  ((3 9)))

(deftest seq-eql-fast.68
  (handler-case (multiple-value-list (REMOVE-DUPLICATES (QUOTE (1 2 1 3 2)))) (error () (list :error)))
  ((1 3 2)))

(deftest seq-eql-fast.69
  (handler-case (multiple-value-list (REMOVE-DUPLICATES (QUOTE ("a" "b" "a")) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (("b" "a")))

(deftest seq-eql-fast.70
  (handler-case (multiple-value-list (MEMBER 3 (QUOTE (1 2 3 4)) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  ((3 4)))

(deftest seq-eql-fast.71
  (handler-case (multiple-value-list (MEMBER "b" (QUOTE ("a" "b" "c")) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (("b" "c")))

(deftest seq-eql-fast.72
  (handler-case (multiple-value-list (MEMBER (QUOTE (1)) (QUOTE ((0) (1) (2))) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (((1) (2))))

(deftest seq-eql-fast.73
  (handler-case (multiple-value-list (MEMBER 3 (QUOTE (1 2 3)) :TEST (FUNCTION EQUAL) :KEY (FUNCTION IDENTITY))) (error () (list :error)))
  ((3)))

(deftest seq-eql-fast.74
  (handler-case (multiple-value-list (MEMBER 3 (QUOTE (1 2 3)) :TEST-NOT (FUNCTION EQUAL))) (error () (list :error)))
  ((1 2 3)))

(deftest seq-eql-fast.75
  (handler-case (multiple-value-list (MEMBER 2.0 (QUOTE (1 2 3)) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (NIL))

(deftest seq-eql-fast.76
  (handler-case (multiple-value-list (MEMBER 2.0 (QUOTE (1 2.0 3)) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  ((2.0 3)))

(deftest seq-eql-fast.77
  (handler-case (multiple-value-list (MULTIPLE-VALUE-LIST (MEMBER 2 (QUOTE (1 2 3)) :TEST (FUNCTION EQUAL)))) (error () (list :error)))
  (((2 3))))

(deftest seq-eql-fast.78
  (handler-case (multiple-value-list (ASSOC "b" (QUOTE (("a" . 1) ("b" . 2))) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (("b" . 2)))

(deftest seq-eql-fast.79
  (handler-case (multiple-value-list (ASSOC (QUOTE (1)) (QUOTE (((0) . A) ((1) . B))) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (((1) . B)))

(deftest seq-eql-fast.80
  (handler-case (multiple-value-list (ASSOC 2 (QUOTE ((1 . A) (2 . B))) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  ((2 . B)))

(deftest seq-eql-fast.81
  (handler-case (multiple-value-list (ASSOC 3 (QUOTE ((1 . A) NIL (3 . C))) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  ((3 . C)))

(deftest seq-eql-fast.82
  (handler-case (multiple-value-list (HANDLER-CASE (ASSOC 3 (QUOTE ((1 . A) 5)) :TEST (FUNCTION EQUAL)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest seq-eql-fast.83
  (handler-case (multiple-value-list (RASSOC "b" (QUOTE ((1 . "a") (2 . "b"))) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  ((2 . "b")))

(deftest seq-eql-fast.84
  (handler-case (multiple-value-list (ADJOIN "a" (QUOTE ("a" "b")) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (("a" "b")))

(deftest seq-eql-fast.85
  (handler-case (multiple-value-list (ADJOIN "c" (QUOTE ("a" "b")) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (("c" "a" "b")))

(deftest seq-eql-fast.86
  (handler-case (multiple-value-list (ADJOIN (QUOTE (1)) (QUOTE ((1) (2))) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (((1) (2))))

(deftest seq-eql-fast.87
  (handler-case (multiple-value-list (UNION (QUOTE ("a" "b")) (QUOTE ("b" "c")) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (("a" "b" "c")))

(deftest seq-eql-fast.88
  (handler-case (multiple-value-list (INTERSECTION (QUOTE ("a" "b")) (QUOTE ("b" "c")) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (("b")))

(deftest seq-eql-fast.89
  (handler-case (multiple-value-list (SET-DIFFERENCE (QUOTE ("a" "b")) (QUOTE ("b" "c")) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (("a")))

(deftest seq-eql-fast.90
  (handler-case (multiple-value-list (SUBSETP (QUOTE ("a")) (QUOTE ("a" "b")) :TEST (FUNCTION EQUAL))) (error () (list :error)))
  (T))

(deftest seq-eql-fast.91
  (handler-case (multiple-value-list (LET ((N 0)) (MEMBER 3 (QUOTE (1 2 3)) :TEST (LAMBDA (A B) (INCF N) (= A B))) N)) (error () (list :error)))
  (3))

(deftest seq-eql-fast.92
  (handler-case (multiple-value-list (LET ((CALLS NIL)) (FIND 3 (QUOTE (1 2 3 4)) :TEST (LAMBDA (A B) (PUSH (LIST A B) CALLS) (EQL A B))) (REVERSE CALLS))) (error () (list :error)))
  (((3 1) (3 2) (3 3))))

(deftest seq-eql-fast.93
  (handler-case (multiple-value-list (LET ((CALLS NIL)) (POSITION 3 (QUOTE (1 2 3 4)) :KEY (LAMBDA (X) (PUSH X CALLS) X)) (REVERSE CALLS))) (error () (list :error)))
  ((1 2 3)))

(deftest seq-eql-fast.94
  (handler-case (multiple-value-list (LET ((CALLS NIL)) (COUNT 3 (QUOTE (1 3)) :TEST (LAMBDA (A B) (PUSH (LIST A B) CALLS) (EQL A B))) (REVERSE CALLS))) (error () (list :error)))
  (((3 1) (3 3))))

(deftest seq-eql-fast.95
  (handler-case (multiple-value-list (LET ((CALLS NIL)) (REMOVE 3 (QUOTE (1 3)) :TEST (LAMBDA (A B) (PUSH (LIST A B) CALLS) (EQL A B))) (REVERSE CALLS))) (error () (list :error)))
  (((3 1) (3 3))))

