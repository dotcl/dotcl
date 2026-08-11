;;; A hash table must find a key its own test function considers the same.
;;; EQL and EQUAL compare numbers by value, so every number has to hash by
;;; value; bignums, ratios and complexes used to fall through to an identity
;;; hash, and the table's private EQL knew only fixnums/characters/floats.
;;; SBCL's inline-constant table is an EQUAL table keyed by lists holding raw
;;; float constants — every miss appended a duplicate constant to the code
;;; object it was compiling.

(defun ht-roundtrip (test k1 k2)
  "Store under K1, look up under the separately-built K2."
  (let ((h (make-hash-table :test test)))
    (setf (gethash k1 h) :found)
    ;; GETHASH returns two values; the tests compare only the value found.
    (values (gethash k2 h))))

(deftest hash-eql-bignum
  (ht-roundtrip #'eql (expt 2 100) (expt 2 100)) :found)
(deftest hash-eql-ratio
  (ht-roundtrip #'eql (/ 1 3) (/ 1 3)) :found)
(deftest hash-eql-complex
  (ht-roundtrip #'eql (complex 1 2) (complex 1 2)) :found)
(deftest hash-eql-double
  (ht-roundtrip #'eql (/ 3.0d0 2.0d0) (/ 3.0d0 2.0d0)) :found)
(deftest hash-eql-single
  (ht-roundtrip #'eql (/ 3.0 2.0) (/ 3.0 2.0)) :found)

(deftest hash-equal-double
  (ht-roundtrip #'equal (/ 3.0d0 2.0d0) (/ 3.0d0 2.0d0)) :found)
(deftest hash-equal-list-with-double
  (ht-roundtrip #'equal (list :df (/ 3.0d0 2.0d0)) (list :df (/ 3.0d0 2.0d0))) :found)
(deftest hash-equal-list-with-bignum
  (ht-roundtrip #'equal (list :b (expt 2 100)) (list :b (expt 2 100))) :found)
(deftest hash-equal-list-with-ratio
  (ht-roundtrip #'equal (list :r (/ 1 3)) (list :r (/ 1 3))) :found)
(deftest hash-equal-nested-double
  (ht-roundtrip #'equal (list (list (/ 3.0d0 2.0d0))) (list (list (/ 3.0d0 2.0d0)))) :found)

;; Keys the test function distinguishes must stay distinct.
(deftest hash-eql-zero-signs
  (ht-roundtrip #'eql 0.0d0 -0.0d0) nil)
(deftest hash-eql-float-vs-integer
  (ht-roundtrip #'eql 1 1.0d0) nil)
(deftest hash-eql-single-vs-double
  (ht-roundtrip #'eql 1.5 1.5d0) nil)

;; And repeated stores under equal keys must not grow the table.
(deftest hash-equal-no-duplicate-entries
  (let ((h (make-hash-table :test #'equal)))
    (dotimes (i 10) (setf (gethash (list :df 0.0d0) h) i))
    (hash-table-count h))
  1)
