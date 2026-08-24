;;; The bit-array operations, over every word-boundary case.
;;;
;;; They run on the packed representation a word at a time, and now through a
;;; vector view of those words. That splits each call into a vector part and a
;;; scalar tail, so the sizes that matter are the ones around a word and around
;;; a vector group -- and the result may legally be one of the inputs, which the
;;; lane-wise form has to leave correct.
;;;
;;; Every expected value here was read off SBCL.

(defun %bvw-make (n seed)
  (let ((v (make-array n :element-type 'bit)))
    (dotimes (i n v) (setf (aref v i) (mod (+ (* i 7) seed (floor i 3)) 2)))))

(defun %bvw-string (v) (map 'string (lambda (b) (if (= b 1) #\1 #\0)) v))

;;; All ten operations on one awkward size, spelled out.
(deftest bit-vector-word-ops.all-operations
  (let ((a #*11010010) (b #*10110101))
    (mapcar (lambda (f) (%bvw-string (funcall f a b)))
            (list #'bit-and #'bit-ior #'bit-xor #'bit-eqv #'bit-nand #'bit-nor
                  #'bit-andc1 #'bit-andc2 #'bit-orc1 #'bit-orc2)))
  ("10010000" "11110111" "01100111" "10011000" "01101111" "00001000"
   "00100101" "01000010" "10111101" "11011010"))

;;; A size that is not a whole number of words, and one that is.
(deftest bit-vector-word-ops.word-boundaries
  (mapcar (lambda (n)
            (let ((a (%bvw-make n 0)) (b (%bvw-make n 1)))
              (list (count 1 (bit-xor a b)) (count 1 (bit-nand a b))
                    (count 1 (bit-and a b)) (count 1 (bit-not a)))))
          '(0 1 63 64 65 127 128 129 255 256 257))
  ((0 0 0 0) (1 1 0 1) (63 63 0 42) (64 64 0 43) (65 65 0 43)
   (127 127 0 85) (128 128 0 85) (129 129 0 86) (255 255 0 170)
   (256 256 0 171) (257 257 0 171)))

;;; The bits past the end of the last word are not part of the vector: an
;;; operation that sets them (NAND fills the whole word) must not make two equal
;;; results compare unequal, or change what COUNT sees.
(deftest bit-vector-word-ops.padding-is-not-part-of-the-value
  (let* ((a (%bvw-make 65 0)) (b (%bvw-make 65 1)))
    (list (equal (bit-nand a b) (bit-nand a b))
          (equalp (bit-nand a b) (bit-nand a b))
          (length (bit-nand a b))
          (count 1 (bit-nand a b))))
  (t t 65 65))

;;; The result may be an input (the T form writes back into the first argument),
;;; or a vector supplied by the caller.
(deftest bit-vector-word-ops.in-place-and-supplied-destination
  (let* ((a (%bvw-make 129 0)) (b (%bvw-make 129 1))
         (into (%bvw-make 129 9))
         (target (%bvw-make 129 0))
         (r1 (bit-xor target b t))
         (r2 (bit-and a b into)))
    (list (eq r1 target) (%bvw-string r1) (eq r2 into)
          (count 1 r2) (%bvw-string (bit-and a b))))
  (t #.(make-string 129 :initial-element #\1) t 0
     #.(make-string 129 :initial-element #\0)))
