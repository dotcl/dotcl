;;; WITH-HASH-TABLE-ITERATOR and LOOP's hash-key / hash-value paths walk a copy of the
;;; table's entries held in one vector (key, value, key, value, ...). Order, the entries seen
;;; while the body changes the table, the end of the iteration and the errors are as before;
;;; the expected values are those of the build before the change.

(deftest hash-iter-direct.1
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 5) (SETF (GETHASH I H) (* I I))) (LOOP FOR K BEING THE HASH-KEYS OF H COLLECT K))) (error () (list :error)))
  ((0 1 2 3 4)))

(deftest hash-iter-direct.2
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 5) (SETF (GETHASH I H) (* I I))) (LOOP FOR V BEING THE HASH-VALUES OF H COLLECT V))) (error () (list :error)))
  ((0 1 4 9 16)))

(deftest hash-iter-direct.3
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 5) (SETF (GETHASH I H) (* I I))) (LOOP FOR K BEING THE HASH-KEYS OF H USING (HASH-VALUE V) COLLECT (LIST K V)))) (error () (list :error)))
  (((0 0) (1 1) (2 4) (3 9) (4 16))))

(deftest hash-iter-direct.4
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 5) (SETF (GETHASH I H) (* I I))) (LOOP FOR V BEING THE HASH-VALUES OF H USING (HASH-KEY K) COLLECT (LIST K V)))) (error () (list :error)))
  (((0 0) (1 1) (2 4) (3 9) (4 16))))

(deftest hash-iter-direct.5
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE :TEST (QUOTE EQUAL)))) (SETF (GETHASH "a" H) 1 (GETHASH "b" H) 2 (GETHASH (QUOTE (1)) H) 3) (LOOP FOR (K . V) BEING THE HASH-KEYS OF H USING (HASH-VALUE V2) COLLECT (LIST K V V2)))) (error () (list :error)))
  (:ERROR))

(deftest hash-iter-direct.6
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (LOOP FOR K BEING THE HASH-KEYS OF H COLLECT K))) (error () (list :error)))
  (NIL))

(deftest hash-iter-direct.7
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (SETF (GETHASH 1 H) 1) (REMHASH 1 H) (SETF (GETHASH 2 H) 2) (LOOP FOR K BEING THE HASH-KEYS OF H COLLECT K))) (error () (list :error)))
  ((2)))

(deftest hash-iter-direct.8
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 5) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS OF H DO (REMHASH K H)) (HASH-TABLE-COUNT H))) (error () (list :error)))
  (0))

(deftest hash-iter-direct.9
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS OF H DO (SETF (GETHASH (+ K 10) H) 0)) (HASH-TABLE-COUNT H))) (error () (list :error)))
  (6))

(deftest hash-iter-direct.10
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 5) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS OF H WHEN (EVENP K) RETURN K))) (error () (list :error)))
  (0))

(deftest hash-iter-direct.11
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 5) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS OF H SUM K))) (error () (list :error)))
  (10))

(deftest hash-iter-direct.12
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 5) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS IN H COUNT (ODDP K)))) (error () (list :error)))
  (2))

(deftest hash-iter-direct.13
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (LOOP FOR K BEING EACH HASH-KEY OF H FOR J FROM 0 COLLECT (LIST K J)))) (error () (list :error)))
  (((0 0) (1 1) (2 2))))

(deftest hash-iter-direct.14
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS OF H FINALLY (RETURN (LIST K))))) (error () (list :error)))
  ((2)))

(deftest hash-iter-direct.15
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS OF H REPEAT 2 COLLECT K))) (error () (list :error)))
  ((0 1)))

(deftest hash-iter-direct.16
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (LOOP FOR A BEING THE HASH-KEYS OF H APPEND (LOOP FOR B BEING THE HASH-VALUES OF H COLLECT (LIST A B))))) (error () (list :error)))
  (((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))

(deftest hash-iter-direct.17
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 4) (SETF (GETHASH I H) I)) (LET (R) (WITH-HASH-TABLE-ITERATOR (NX H) (LOOP (MULTIPLE-VALUE-BIND (M K V) (NX) (UNLESS M (RETURN)) (PUSH (LIST K V) R)))) (REVERSE R)))) (error () (list :error)))
  (((0 0) (1 1) (2 2) (3 3))))

(deftest hash-iter-direct.18
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (WITH-HASH-TABLE-ITERATOR (NX H) (LIST (MULTIPLE-VALUE-LIST (NX)) (MULTIPLE-VALUE-LIST (NX)) (MULTIPLE-VALUE-LIST (NX)) (MULTIPLE-VALUE-LIST (NX)) (MULTIPLE-VALUE-LIST (NX)))))) (error () (list :error)))
  (((T 0 0) (T 1 1) (T 2 2) (NIL) (NIL))))

(deftest hash-iter-direct.19
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (WITH-HASH-TABLE-ITERATOR (NX H) (MULTIPLE-VALUE-LIST (NX))))) (error () (list :error)))
  ((NIL)))

(deftest hash-iter-direct.20
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 4) (SETF (GETHASH I H) I)) (LET (R) (WITH-HASH-TABLE-ITERATOR (NX H) (LOOP (MULTIPLE-VALUE-BIND (M K V) (NX) (UNLESS M (RETURN)) (REMHASH K H) (PUSH K R)))) (LIST (REVERSE R) (HASH-TABLE-COUNT H))))) (error () (list :error)))
  (((0 1 2 3) 0)))

(deftest hash-iter-direct.21
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 4) (SETF (GETHASH I H) I)) (LET (R) (WITH-HASH-TABLE-ITERATOR (A H) (WITH-HASH-TABLE-ITERATOR (B H) (PUSH (LIST (MULTIPLE-VALUE-LIST (A)) (MULTIPLE-VALUE-LIST (B)) (MULTIPLE-VALUE-LIST (A))) R))) R))) (error () (list :error)))
  ((((T 0 0) (T 0 0) (T 1 1)))))

(deftest hash-iter-direct.22
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE :WEAKNESS :KEY))) (SETF (GETHASH 1 H) 2) (LOOP FOR K BEING THE HASH-KEYS OF H COLLECT K))) (error () (list :error)))
  ((1)))

(deftest hash-iter-direct.23
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE :SYNCHRONIZED T))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS OF H COLLECT K))) (error () (list :error)))
  ((0 1 2)))

(deftest hash-iter-direct.24
  (handler-case (multiple-value-list (HANDLER-CASE (LOOP FOR K BEING THE HASH-KEYS OF 5 COLLECT K) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest hash-iter-direct.25
  (handler-case (multiple-value-list (HANDLER-CASE (WITH-HASH-TABLE-ITERATOR (NX 5) (NX)) (ERROR NIL :ERROR))) (error () (list :error)))
  (:ERROR))

(deftest hash-iter-direct.26
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (SETF (GETHASH (QUOTE A) H) 1 (GETHASH (QUOTE B) H) 2) (LOOP FOR K BEING THE HASH-KEYS OF H USING (HASH-VALUE V) MAXIMIZE V))) (error () (list :error)))
  (2))

(deftest hash-iter-direct.27
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (LET ((K 99)) (LOOP FOR K BEING THE HASH-KEYS OF H COLLECT K) K))) (error () (list :error)))
  (99))

(deftest hash-iter-direct.28
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (LOOP FOR K OF-TYPE FIXNUM BEING THE HASH-KEYS OF H COLLECT K))) (error () (list :error)))
  ((0 1 2)))

(deftest hash-iter-direct.29
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 6) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS OF H WHILE (< K 3) COLLECT K))) (error () (list :error)))
  ((0 1 2)))

(deftest hash-iter-direct.30
  (handler-case (multiple-value-list (LET ((H (MAKE-HASH-TABLE))) (DOTIMES (I 3) (SETF (GETHASH I H) I)) (LOOP FOR K BEING THE HASH-KEYS OF H AND V = 0 COLLECT (LIST K V)))) (error () (list :error)))
  (((0 0) (1 0) (2 0))))

