;;; FIND / POSITION / COUNT of a fixnum (EQL or =) and FIND-IF with MINUSP / ZEROP / PLUSP on a
;;; vector of integers kept in a typed array are answered from the array itself. The results for every
;;; integer element type, values outside the element range, floats and bignums as the item,
;;; :key / :test / :test-not, :from-end, :start / :end, displaced and fill-pointer arrays, and the
;;; errors are as before; the expected values are those of the build before the change.

(deftest seq-numeric-array-scan.1
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.2
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.3
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 16)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.4
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 16)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.5
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 32)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.6
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 32)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.7
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 64)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.8
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 64)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.9
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 32)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.10
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 32)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.11
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 8)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.12
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 8)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.13
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 16)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.14
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 16)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.15
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 64)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.16
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 64)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.17
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 15)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.18
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 15)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.19
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (INTEGER -5 300)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.20
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (INTEGER -5 300)) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.21
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE FIXNUM) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND 3 V) (POSITION 3 V) (COUNT 3 V) (FIND 7 V :FROM-END T) (POSITION 3 V :FROM-END T) (POSITION 3 V :START 1) (POSITION 3 V :END 2) (COUNT 3 V :START 1 :END 5) (FIND 99 V) (POSITION 0 V :TEST (FUNCTION =)) (COUNT 0 V :TEST (FUNCTION EQL)) (POSITION 5 V :TEST (FUNCTION EQ))))) (error () (list :error)))
  ((3 0 3 7 6 2 0 1 NIL 1 2 5)))

(deftest seq-numeric-array-scan.22
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE FIXNUM) :INITIAL-CONTENTS (LIST 3 0 3 7 0 5 3 1)))) (LIST (FIND-IF (FUNCTION ZEROP) V) (POSITION-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T) (FIND-IF (FUNCTION MINUSP) V) (POSITION-IF (FUNCTION MINUSP) V) (COUNT-IF (FUNCTION ZEROP) V) (FIND-IF (FUNCTION PLUSP) V :START 4) (FIND-IF (FUNCTION ZEROP) V :END 1)))) (error () (list :error)))
  ((0 1 1 NIL NIL 2 5 NIL)))

(deftest seq-numeric-array-scan.23
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 6 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 32)) :INITIAL-CONTENTS (QUOTE (-3 4 -3 0 9 -1))))) (LIST (FIND -3 V) (POSITION -3 V :FROM-END T) (COUNT -3 V) (FIND-IF (FUNCTION MINUSP) V) (FIND-IF (FUNCTION MINUSP) V :FROM-END T) (POSITION-IF (FUNCTION MINUSP) V :START 1) (FIND-IF (FUNCTION PLUSP) V) (FIND-IF (FUNCTION ZEROP) V)))) (error () (list :error)))
  ((-3 2 2 -3 -1 2 4 0)))

(deftest seq-numeric-array-scan.24
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 6 :ELEMENT-TYPE (QUOTE (SIGNED-BYTE 64)) :INITIAL-CONTENTS (LIST -1 (- (EXPT 2 62)) 4611686018427387904 5 0 9223372036854775807)))) (LIST (FIND 4611686018427387904 V) (POSITION 9223372036854775807 V) (FIND (- (EXPT 2 62)) V) (COUNT -1 V) (FIND 9223372036854775808 V) (FIND-IF (FUNCTION PLUSP) V :FROM-END T)))) (error () (list :error)))
  ((4611686018427387904 5 -4611686018427387904 1 NIL 9223372036854775807)))

(deftest seq-numeric-array-scan.25
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 4 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)) :INITIAL-CONTENTS (QUOTE (1 2 3 4))))) (LIST (FIND 2.0 V) (FIND 2.0 V :TEST (FUNCTION =)) (POSITION 2.0 V :TEST (FUNCTION =)) (FIND 300 V) (FIND -1 V) (COUNT 2 V :KEY (FUNCTION 1+)) (FIND 3 V :KEY (FUNCTION 1+)) (POSITION 4 V :TEST (FUNCTION <)) (FIND 2 V :TEST-NOT (FUNCTION =))))) (error () (list :error)))
  ((NIL 2 1 NIL NIL 1 2 NIL 1)))

(deftest seq-numeric-array-scan.26
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 4 :ELEMENT-TYPE (QUOTE DOUBLE-FLOAT) :INITIAL-CONTENTS (QUOTE (1.0d0 2.0d0 3.0d0 2.0d0))))) (LIST (FIND 2 V :TEST (FUNCTION =)) (POSITION 2.0d0 V) (COUNT 2 V :TEST (FUNCTION =)) (FIND-IF (FUNCTION PLUSP) V) (FIND 2 V)))) (error () (list :error)))
  ((2.0d0 1 2 1.0d0 NIL)))

(deftest seq-numeric-array-scan.27
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 4 :ELEMENT-TYPE (QUOTE BIT) :INITIAL-CONTENTS (QUOTE (1 0 1 1))))) (LIST (FIND 0 V) (POSITION 1 V) (COUNT 1 V) (POSITION 1 V :FROM-END T) (FIND-IF (FUNCTION ZEROP) V)))) (error () (list :error)))
  ((0 0 3 3 0)))

(deftest seq-numeric-array-scan.28
  (handler-case (multiple-value-list (LET* ((B (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 16)) :INITIAL-CONTENTS (QUOTE (0 1 2 3 4 5 6 7)))) (D (MAKE-ARRAY 4 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 16)) :DISPLACED-TO B :DISPLACED-INDEX-OFFSET 3))) (LIST (FIND 5 D) (POSITION 6 D) (COUNT 3 D) (FIND-IF (FUNCTION ZEROP) D)))) (error () (list :error)))
  ((5 3 1 NIL)))

(deftest seq-numeric-array-scan.29
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 8 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 16)) :FILL-POINTER 4 :INITIAL-CONTENTS (QUOTE (0 1 2 3 4 5 6 7))))) (LIST (FIND 5 V) (POSITION 3 V) (COUNT 2 V) (FIND 3 V :FROM-END T) (FIND-IF (FUNCTION PLUSP) V :FROM-END T)))) (error () (list :error)))
  ((NIL 3 1 3 3)))

(deftest seq-numeric-array-scan.30
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 3 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)) :INITIAL-CONTENTS (QUOTE (1 2 3))))) (LIST (HANDLER-CASE (FIND 2 V :START 5) (ERROR NIL :ERROR)) (HANDLER-CASE (POSITION 2 V :START 2 :END 1) (ERROR NIL :ERROR)) (HANDLER-CASE (COUNT 2 V :END 9) (ERROR NIL :ERROR))))) (error () (list :error)))
  ((:ERROR :ERROR :ERROR)))

(deftest seq-numeric-array-scan.31
  (handler-case (multiple-value-list (LET ((V (MAKE-ARRAY 5 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)) :INITIAL-CONTENTS (QUOTE (1 2 3 4 5)))) (N 0)) (LIST (FIND 3 V :TEST (LAMBDA (A B) (INCF N) (= A B))) N))) (error () (list :error)))
  ((3 3)))

(deftest seq-numeric-array-scan.32
  (handler-case (multiple-value-list (LET ((V (VECTOR 1 2.0 3 2 5))) (LIST (FIND 2 V :TEST (FUNCTION =)) (POSITION 2 V :TEST (FUNCTION =)) (COUNT 2 V :TEST (FUNCTION =)) (FIND 2 V) (POSITION 2.0 V :TEST (FUNCTION =)) (FIND 5 V :TEST (FUNCTION =)) (COUNT 2 V :TEST (FUNCTION =) :FROM-END T)))) (error () (list :error)))
  ((2.0 1 2 2 1 5 2)))

(deftest seq-numeric-array-scan.33
  (handler-case (multiple-value-list (LET ((V (VECTOR 1 (EXPT 10 30) 3))) (LIST (FIND 3 V :TEST (FUNCTION =)) (POSITION (EXPT 10 30) V :TEST (FUNCTION =)) (COUNT 1 V :TEST (FUNCTION =))))) (error () (list :error)))
  ((3 1 1)))

(deftest seq-numeric-array-scan.34
  (handler-case (multiple-value-list (LET ((L (LIST 1 2.0 3 2))) (LIST (FIND 2 L :TEST (FUNCTION =)) (POSITION 2 L :TEST (FUNCTION =)) (COUNT 2 L :TEST (FUNCTION =))))) (error () (list :error)))
  ((2.0 1 2)))

(deftest seq-numeric-array-scan.35
  (handler-case (multiple-value-list (LET ((V (VECTOR 1 "a" 3))) (HANDLER-CASE (FIND 3 V :TEST (FUNCTION =)) (ERROR NIL :ERROR)))) (error () (list :error)))
  (:ERROR))

