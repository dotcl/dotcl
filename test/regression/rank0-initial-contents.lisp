;;; :INITIAL-CONTENTS for a zero-dimensional array.
;;;
;;; A rank-0 array holds exactly one element, and :initial-contents IS that
;;; element -- not a sequence containing it. Treating it as a sequence stored
;;; the first element instead, so #0A(1 2) came out as #0A1. Eclector's
;;; SHARPSIGN-A builds its arrays this way, which is where it showed.

(deftest r0ic-list-is-the-element
  (aref (make-array nil :initial-contents '(1 2)))
  (1 2))

(deftest r0ic-nested-list-keeps-every-level
  (aref (make-array nil :initial-contents '(((1)))))
  (((1))))

(deftest r0ic-atom-still-works
  (aref (make-array nil :initial-contents 5))
  5)

(deftest r0ic-empty-list-is-the-element
  (aref (make-array nil :initial-contents '()))
  nil)

(deftest r0ic-vector-is-the-element-too
  (let ((v (vector 1 2)))
    (eq (aref (make-array nil :initial-contents v)) v))
  t)

;;; '() and NIL name the same dimensions.
(deftest r0ic-dimensions-written-as-empty-list
  (aref (make-array '() :initial-contents '(1 2)))
  (1 2))

;;; Rank 1 and up are unchanged: there the contents is a sequence.
(deftest r0ic-rank1-still-spreads
  (let ((a (make-array 2 :initial-contents '(1 2))))
    (list (aref a 0) (aref a 1)))
  (1 2))

(deftest r0ic-rank2-still-nests
  (let ((a (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
    (list (aref a 0 0) (aref a 1 1)))
  (1 4))

;;; The reader agreed all along; this pins the two together.
(deftest r0ic-reader-and-make-array-agree
  (equalp (read-from-string "#0A(1 2)")
          (make-array nil :initial-contents '(1 2)))
  t)

(deftest r0ic-prints-back
  (prin1-to-string (make-array nil :initial-contents '(1 2)))
  "#0A(1 2)")
