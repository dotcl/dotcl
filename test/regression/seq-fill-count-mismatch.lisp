;;; FILL on a vector that keeps its elements as objects, COUNT of a character in a string and
;;; MISMATCH of two strings or two vectors with the plain EQL test work in place. Results,
;;; the ranges, :test / :key / :from-end, other element types and the errors are as before; the
;;; expected values are those of the build before the fast paths.

(deftest seq-fill-count-mismatch.1
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (FILL (MAKE-ARRAY 5 :INITIAL-ELEMENT 0) 7))) (error () (list :error)))
  ((7 7 7 7 7)))

(deftest seq-fill-count-mismatch.2
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (FILL (MAKE-ARRAY 5 :INITIAL-ELEMENT 0) 7 :START 1 :END 3))) (error () (list :error)))
  ((0 7 7 0 0)))

(deftest seq-fill-count-mismatch.3
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (FILL (MAKE-ARRAY 5 :INITIAL-ELEMENT 0) 7 :START 3))) (error () (list :error)))
  ((0 0 0 7 7)))

(deftest seq-fill-count-mismatch.4
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (FILL (MAKE-ARRAY 5 :INITIAL-ELEMENT 0) 7 :END 2))) (error () (list :error)))
  ((7 7 0 0 0)))

(deftest seq-fill-count-mismatch.5
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (FILL (MAKE-ARRAY 5 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)) :INITIAL-ELEMENT 0) 200))) (error () (list :error)))
  ((200 200 200 200 200)))

(deftest seq-fill-count-mismatch.6
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (FILL (MAKE-ARRAY 5 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)) :INITIAL-ELEMENT 0) 300) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.7
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (FILL (MAKE-ARRAY 6 :ELEMENT-TYPE (QUOTE BIT) :INITIAL-ELEMENT 0) 1 :START 2 :END 5))) (error () (list :error)))
  ((0 0 1 1 1 0)))

(deftest seq-fill-count-mismatch.8
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (FILL (MAKE-ARRAY 4 :ELEMENT-TYPE (QUOTE BIT)) 2) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.9
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (FILL (MAKE-ARRAY 4 :ELEMENT-TYPE (QUOTE CHARACTER) :INITIAL-ELEMENT #\a) #\z :START 1))) (error () (list :error)))
  ("azzz"))

(deftest seq-fill-count-mismatch.10
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (FILL (MAKE-ARRAY 4 :ELEMENT-TYPE (QUOTE CHARACTER) :INITIAL-ELEMENT #\a) 5) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.11
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (LET ((V (MAKE-ARRAY 8 :FILL-POINTER 5 :INITIAL-ELEMENT 0))) (FILL V 9) (LIST (FILL-POINTER V) (COERCE V (QUOTE LIST)) (AREF V 6))))) (error () (list :error)))
  ((5 (9 9 9 9 9) 0)))

(deftest seq-fill-count-mismatch.12
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (LET* ((B (MAKE-ARRAY 6 :INITIAL-ELEMENT 0)) (D (MAKE-ARRAY 3 :DISPLACED-TO B :DISPLACED-INDEX-OFFSET 2))) (FILL D 5) (COERCE B (QUOTE LIST))))) (error () (list :error)))
  ((0 0 5 5 5 0)))

(deftest seq-fill-count-mismatch.13
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (FILL (MAKE-ARRAY 3 :INITIAL-ELEMENT 0) 1 :END 9) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.14
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (FILL (MAKE-ARRAY 3 :INITIAL-ELEMENT 0) 1 :START 2 :END 1) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.15
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (LET ((S (MAKE-STRING 4 :INITIAL-ELEMENT #\a))) (FILL S #\b :START 1 :END 3) S))) (error () (list :error)))
  ("abba"))

(deftest seq-fill-count-mismatch.16
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (FILL (LIST 1 2 3 4) 0 :START 1 :END 3))) (error () (list :error)))
  ((1 0 0 4)))

(deftest seq-fill-count-mismatch.17
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (FILL (MAKE-ARRAY 3 :ELEMENT-TYPE (QUOTE DOUBLE-FLOAT) :INITIAL-ELEMENT 0.0d0) 1.5d0))) (error () (list :error)))
  ((1.5d0 1.5d0 1.5d0)))

(deftest seq-fill-count-mismatch.18
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (LET ((V (VECTOR 1 2 3))) (EQ V (FILL V 0))))) (error () (list :error)))
  (T))

(deftest seq-fill-count-mismatch.19
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a "banana"))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.20
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\A "banana"))) (error () (list :error)))
  (0))

(deftest seq-fill-count-mismatch.21
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\A "banana" :TEST (FUNCTION CHAR-EQUAL)))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.22
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a "banana" :START 2))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.23
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a "banana" :END 4))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.24
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a "banana" :START 1 :END 3))) (error () (list :error)))
  (1))

(deftest seq-fill-count-mismatch.25
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a "banana" :FROM-END T))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.26
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a "banana" :KEY (FUNCTION CHAR-UPCASE)))) (error () (list :error)))
  (0))

(deftest seq-fill-count-mismatch.27
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\A "banana" :KEY (FUNCTION CHAR-UPCASE)))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.28
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a "banana" :TEST-NOT (FUNCTION EQL)))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.29
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a "banana" :TEST (FUNCTION CHAR=)))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.30
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a "banana" :TEST (LAMBDA (A B) (CHAR< A B))))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.31
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT 97 "banana"))) (error () (list :error)))
  (0))

(deftest seq-fill-count-mismatch.32
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT (QUOTE A) "banana"))) (error () (list :error)))
  (0))

(deftest seq-fill-count-mismatch.33
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a (COERCE "banana" (QUOTE LIST))))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.34
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a (COERCE "banana" (QUOTE VECTOR))))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.35
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a ""))) (error () (list :error)))
  (0))

(deftest seq-fill-count-mismatch.36
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (COUNT #\a "banana" :START 9) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.37
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (COUNT #\a "banana" :START 4 :END 2) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.38
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a (MAKE-ARRAY 6 :ELEMENT-TYPE (QUOTE CHARACTER) :INITIAL-CONTENTS "banana")))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.39
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT #\a (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE CHARACTER) :FILL-POINTER 6 :INITIAL-CONTENTS "bananaxx")))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.40
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (COUNT (CODE-CHAR 228) (COERCE (LIST (CODE-CHAR 228) (CODE-CHAR 228) #\b) (QUOTE STRING))))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.41
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "hello world" "hello there"))) (error () (list :error)))
  (6))

(deftest seq-fill-count-mismatch.42
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abc" "abc"))) (error () (list :error)))
  (NIL))

(deftest seq-fill-count-mismatch.43
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abc" "abcd"))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.44
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abcd" "abc"))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.45
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "" ""))) (error () (list :error)))
  (NIL))

(deftest seq-fill-count-mismatch.46
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abc" "xyz"))) (error () (list :error)))
  (0))

(deftest seq-fill-count-mismatch.47
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abcdef" "xxcdef" :START1 2 :START2 2))) (error () (list :error)))
  (NIL))

(deftest seq-fill-count-mismatch.48
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abcdef" "abcxyz" :END1 3 :END2 3))) (error () (list :error)))
  (NIL))

(deftest seq-fill-count-mismatch.49
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abcdef" "xbcdef" :START1 1 :END1 4 :START2 1 :END2 5))) (error () (list :error)))
  (4))

(deftest seq-fill-count-mismatch.50
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "ABC" "abd" :TEST (FUNCTION CHAR-EQUAL)))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.51
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abc" "abd" :KEY (FUNCTION CHAR-UPCASE)))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.52
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abc" "abd" :TEST-NOT (FUNCTION CHAR=)))) (error () (list :error)))
  (0))

(deftest seq-fill-count-mismatch.53
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abcdef" "xyzdef" :FROM-END T))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.54
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(1 2 3) #(1 2 4)))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.55
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(1 2 3) #(1 2 3)))) (error () (list :error)))
  (NIL))

(deftest seq-fill-count-mismatch.56
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(1 2 3) #(1 2 3 4)))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.57
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(1 2.0 3) #(1 2 3)))) (error () (list :error)))
  (1))

(deftest seq-fill-count-mismatch.58
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(1 2.0 3) #(1 2.0 4)))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.59
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(1000000000000000000000 2) #(1000000000000000000000 3)))) (error () (list :error)))
  (1))

(deftest seq-fill-count-mismatch.60
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(A B C) #(A B D)))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.61
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(A "x" C) #(A "x" C)))) (error () (list :error)))
  (1))

(deftest seq-fill-count-mismatch.62
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH (QUOTE (1 2 3)) (QUOTE (1 2 4))))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.63
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH (QUOTE (1 2 3)) #(1 2 4)))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.64
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(1 2 3) (QUOTE (1 2 4))))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.65
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH "abc" #(#\a #\b #\x)))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.66
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH #(#\a #\b) "ab"))) (error () (list :error)))
  (NIL))

(deftest seq-fill-count-mismatch.67
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH (MAKE-ARRAY 3 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)) :INITIAL-CONTENTS (QUOTE (1 2 3))) #(1 2 4)))) (error () (list :error)))
  (2))

(deftest seq-fill-count-mismatch.68
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MISMATCH (MAKE-ARRAY 5 :FILL-POINTER 3 :INITIAL-CONTENTS (QUOTE (1 2 3 4 5))) #(1 2 3)))) (error () (list :error)))
  (NIL))

(deftest seq-fill-count-mismatch.69
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (LET ((N 0)) (MISMATCH #(1 2 3) #(1 2 4) :TEST (LAMBDA (A B) (INCF N) (EQL A B))) N))) (error () (list :error)))
  (3))

(deftest seq-fill-count-mismatch.70
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (MISMATCH "abc" "abd" :START1 9) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.71
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (MISMATCH 5 "abd") (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.72
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (HANDLER-CASE (MISMATCH "abc" "abd" :START2 3 :END2 1) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

(deftest seq-fill-count-mismatch.73
  (handler-case (mapcar (lambda (x) (if (and (vectorp x) (not (stringp x))) (coerce x (quote list)) x)) (multiple-value-list (MULTIPLE-VALUE-LIST (MISMATCH "abc" "abd")))) (error () (list :error)))
  ((2)))

