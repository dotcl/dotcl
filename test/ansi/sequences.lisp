;;; sequences.lisp — ANSI tests for sequence operations (TDD)

;;; --- MAKE-SEQUENCE ---

(deftest make-sequence.list.1
  (let ((x (make-sequence 'list 4)))
    (and (eql (length x) 4) (listp x)))
  t)

(deftest make-sequence.list.initial-element
  (make-sequence 'list 4 :initial-element 'a)
  (a a a a))

(deftest make-sequence.string.1
  (make-sequence 'string 5 :initial-element #\a)
  "aaaaa")

(deftest make-sequence.vector.1
  (let ((v (make-sequence 'vector 3 :initial-element 7)))
    (and (vectorp v) (= (length v) 3) (eql (elt v 0) 7) (eql (elt v 1) 7)))
  t)

(deftest make-sequence.null.1
  (make-sequence 'null 0)
  nil)

;;; --- MAP-INTO ---

(deftest map-into.list.1
  (let ((result (list 0 0 0)))
    (map-into result #'+ '(1 2 3) '(10 20 30))
    result)
  (11 22 33))

(deftest map-into.vector.1
  (let ((v (vector 0 0 0)))
    (map-into v #'1+ '(1 2 3))
    (elt v 0))
  2)

(deftest map-into.nil-result
  (map-into nil #'+ '(1 2 3))
  nil)

(deftest map-into.shorter-input
  (let ((result (list 0 0 0 0 0)))
    (map-into result #'identity '(1 2 3))
    result)
  (1 2 3 0 0))

;;; --- MERGE ---

(deftest merge.list.1
  (merge 'list '(1 3 7) '(2 4 5) #'<)
  (1 2 3 4 5 7))

(deftest merge.list.empty1
  (merge 'list nil '(2 4 5) #'<)
  (2 4 5))

(deftest merge.list.both-empty
  (merge 'list nil nil #'<)
  nil)

(deftest merge.vector.1
  (let ((v (merge 'vector '(1 3 7) '(2 4 5) #'<)))
    (and (vectorp v) (eql (elt v 0) 1) (eql (elt v 1) 2)))
  t)

(deftest merge.string.1
  (merge 'string (list #\1 #\3) (list #\2 #\4) #'char<)
  "1234")

;;; --- MISMATCH ---

(deftest mismatch.equal
  (mismatch '(a b c) '(a b c))
  nil)

(deftest mismatch.differ.1
  (mismatch '(a b c) '(a b d))
  2)

(deftest mismatch.seq1-shorter
  (mismatch '() '(a b c))
  0)

(deftest mismatch.seq2-shorter
  (mismatch '(a b c) '())
  0)

(deftest mismatch.string.1
  (mismatch "" "111")
  0)

(deftest mismatch.string.equal
  (mismatch "abc" "abc")
  nil)

(deftest mismatch.string.differ
  (mismatch "abc" "abd")
  2)

(deftest mismatch.start1
  (mismatch '(a b c) '(b c) :start1 1)
  nil)

(deftest mismatch.from-end
  (mismatch '(a b c d) '(a b c e))
  3)

;;; --- FILL ---

(deftest fill.list.1
  (let ((x (list 'a 'b 'c)))
    (values (eq x (fill x 'z)) x))
  t (z z z))

(deftest fill.string.1
  (let ((s (copy-seq "abcde")))
    (fill s #\z)
    s)
  "zzzzz")

(deftest fill.string.start-end
  (let ((s (copy-seq "abcde")))
    (fill s #\z :start 1 :end 3)
    s)
  "azzde")

(deftest fill.vector.1
  (let ((v (vector 1 2 3)))
    (fill v 0)
    (and (= (elt v 0) 0) (= (elt v 1) 0) (= (elt v 2) 0)))
  t)

;;; --- NSUBSTITUTE ---

(deftest nsubstitute.1
  (nsubstitute 'b 'a '(a b a c))
  (b b b c))

(deftest nsubstitute.count
  (nsubstitute 'b 'a '(a b a c) :count 1)
  (b b a c))

(deftest nsubstitute-if.1
  (nsubstitute-if 'x #'evenp '(1 2 3 4))
  (1 x 3 x))

(deftest nsubstitute-if-not.1
  (nsubstitute-if-not 'x #'evenp '(1 2 3 4))
  (x 2 x 4))

;;; --- SUBSTITUTE with COUNT/FROM-END/START/END ---

(deftest substitute.count.1
  (substitute 'b 'a '(a b a c) :count 1)
  (b b a c))

(deftest substitute.count.nil
  (substitute 'b 'a '(a b a c) :count nil)
  (b b b c))

(deftest substitute.from-end.count
  (substitute 'b 'a '(a b a c) :count 1 :from-end t)
  (a b b c))

(deftest substitute.start.1
  (substitute 'b 'a '(a b a c) :start 2)
  (a b b c))

;;; --- SUBSTITUTE-IF-NOT ---

(deftest substitute-if-not.1
  (substitute-if-not 'b #'null '(nil a nil b))
  (nil b nil b))

;;; --- POSITION-IF-NOT ---

(deftest position-if-not.1
  (position-if-not #'evenp '(2 4 1 6))
  2)

(deftest position-if-not.not-found
  (position-if-not #'evenp '(2 4 6))
  nil)

(deftest position-if-not.start
  (position-if-not #'evenp '(2 4 1 6 3) :start 3)
  4)

;;; --- FIND with START/END/FROM-END ---

(deftest find.start.1
  (find 'c '(a b c d e c a) :start 3)
  c)

(deftest find.start.not-found
  (find 'c '(a b c d e c a) :start 6)
  nil)

(deftest find.end.1
  (find 'c '(a b c d e c a) :end 2)
  nil)

(deftest find.from-end.1
  (find 'c '(a b c d e c a) :from-end t)
  c)

(deftest find-if.start.1
  (find-if #'evenp '(1 3 2 4 5) :start 3)
  4)

(deftest find-if-not.start.1
  (find-if-not #'evenp '(2 4 1 6) :start 2)
  1)

(do-tests-summary)
