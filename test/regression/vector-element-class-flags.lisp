;;; STRINGP, SIMPLE-VECTOR-P, BIT-VECTOR-P and VECTORP answer by the element type each vector was made with,
;;; for every way of making one (MAKE-ARRAY, COPY-SEQ, SUBSEQ, VECTOR, COERCE, displaced and adjustable).

(defun %vec-kinds (x)
  (list (and (stringp x) t) (and (simple-vector-p x) t) (and (bit-vector-p x) t) (and (vectorp x) t)))

(deftest vector-element-class-flags.kinds
  (list (%vec-kinds (vector 1 2))
        (%vec-kinds (make-array 2))
        (%vec-kinds (make-array 2 :element-type 'character :initial-element #\a))
        (%vec-kinds (make-array 2 :element-type 'bit))
        (%vec-kinds (make-array 2 :element-type '(unsigned-byte 8)))
        (%vec-kinds (make-array 2 :element-type 'base-char :initial-element #\a))
        (%vec-kinds (make-array 2 :adjustable t))
        (%vec-kinds (make-array 2 :fill-pointer 1))
        (%vec-kinds (copy-seq (vector 1 2)))
        (%vec-kinds (copy-seq (make-array 2 :element-type 'bit)))
        (%vec-kinds (subseq (make-array 3 :element-type 'bit) 1))
        (%vec-kinds (subseq (vector 1 2 3) 1))
        (%vec-kinds (coerce '(1 2) 'simple-vector))
        (%vec-kinds (make-array 2 :displaced-to (vector 1 2 3)))
        (%vec-kinds (adjust-array (make-array 2 :adjustable t) 4))
        (%vec-kinds (make-array 2 :element-type nil))
        (%vec-kinds "abc"))
  ((nil t nil t) (nil t nil t) (t nil nil t) (nil nil t t) (nil nil nil t) (t nil nil t)
   (nil nil nil t) (nil nil nil t) (nil t nil t) (nil nil t t) (nil nil t t) (nil t nil t) (nil t nil t)
   (nil nil nil t) (nil nil nil t) (t nil nil t) (t nil nil t)))
