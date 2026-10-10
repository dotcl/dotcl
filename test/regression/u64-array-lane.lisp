;;; An array declared (simple-array (unsigned-byte 64) (*)) is read and written
;;; as its elements' bits: (AREF A I) is an operand of the 64-bit lane that is
;;; already exact, and (SETF (AREF A I) X) stores bits without making the
;;; value. These are the answers the generic path gives, for words on both
;;; sides of 2^63, for arrays the declaration does not describe, and for
;;; values the array cannot hold; and the shape of Argon2's G allocates
;;; nothing.

(declaim (inline %u64a-add %u64a-mul %u64a-ror))
(defun %u64a-add (a b) (declare (type (unsigned-byte 64) a b)) (ldb (byte 64 0) (+ a b)))
(defun %u64a-mul (a b) (declare (type (unsigned-byte 64) a b)) (ldb (byte 64 0) (* a b)))
(defun %u64a-ror (a s)
  (declare (type (unsigned-byte 64) a) (type (integer 0 64) s))
  (logior (ldb (byte 64 0) (ash a (- 64 s))) (ash a (- s))))

;; Argon2's G on four words of the block, written on the array elements as
;; ironclad writes it (SETF of AREF places).
(defmacro %u64a-g (a b c d)
  `(setf ,a (%u64a-add ,a (%u64a-add ,b (%u64a-mul 2 (%u64a-mul (logand ,a #xffffffff)
                                                                 (logand ,b #xffffffff)))))
         ,d (%u64a-ror (logxor ,d ,a) 32)
         ,c (%u64a-add ,c (%u64a-add ,d (%u64a-mul 2 (%u64a-mul (logand ,c #xffffffff)
                                                                 (logand ,d #xffffffff)))))
         ,b (%u64a-ror (logxor ,b ,c) 24)
         ,a (%u64a-add ,a (%u64a-add ,b (%u64a-mul 2 (%u64a-mul (logand ,a #xffffffff)
                                                                 (logand ,b #xffffffff)))))
         ,d (%u64a-ror (logxor ,d ,a) 16)
         ,c (%u64a-add ,c (%u64a-add ,d (%u64a-mul 2 (%u64a-mul (logand ,c #xffffffff)
                                                                 (logand ,d #xffffffff)))))
         ,b (%u64a-ror (logxor ,b ,c) 63)))

(defun %u64a-rounds (w)
  (declare (type (simple-array (unsigned-byte 64) (*)) w))
  (loop for i from 0 below 16 by 4
        do (%u64a-g (aref w i) (aref w (+ i 1)) (aref w (+ i 2)) (aref w (+ i 3))))
  (dotimes (i 16)
    (setf (aref w i) (logxor (aref w i) (aref w (- 15 i)))))
  w)

;; The same kernel with no declaration: every step on the generic path.
(defun %u64a-rounds-generic (w)
  (flet ((add (a b) (mod (+ a b) (expt 2 64)))
         (mul (a b) (mod (* a b) (expt 2 64)))
         (ror (a s) (mod (logior (ash a (- 64 s)) (ash a (- s))) (expt 2 64))))
    (macrolet ((g (a b c d)
                 `(setf ,a (add ,a (add ,b (mul 2 (mul (logand ,a #xffffffff)
                                                       (logand ,b #xffffffff)))))
                        ,d (ror (logxor ,d ,a) 32)
                        ,c (add ,c (add ,d (mul 2 (mul (logand ,c #xffffffff)
                                                       (logand ,d #xffffffff)))))
                        ,b (ror (logxor ,b ,c) 24)
                        ,a (add ,a (add ,b (mul 2 (mul (logand ,a #xffffffff)
                                                       (logand ,b #xffffffff)))))
                        ,d (ror (logxor ,d ,a) 16)
                        ,c (add ,c (add ,d (mul 2 (mul (logand ,c #xffffffff)
                                                       (logand ,d #xffffffff)))))
                        ,b (ror (logxor ,b ,c) 63))))
      (loop for i from 0 below 16 by 4
            do (g (aref w i) (aref w (+ i 1)) (aref w (+ i 2)) (aref w (+ i 3))))
      (dotimes (i 16)
        (setf (aref w i) (logxor (aref w i) (aref w (- 15 i)))))
      w)))

(defun %u64a-words (seed)
  (let ((rs (%u64a-lcg seed)))
    (loop repeat 16 collect (funcall rs))))

;; A small deterministic generator (no RANDOM-STATE, so the expected checksum
;; below is the same in every implementation): a 64-bit LCG.
(defun %u64a-lcg (seed)
  (let ((x seed))
    (lambda ()
      (setf x (mod (+ (* x 6364136223846793005) 1442695040888963407) (expt 2 64))))))

(defun %u64a-make (words)
  (make-array (length words) :element-type '(unsigned-byte 64) :initial-contents words))

(deftest u64-array-lane.matches-generic
  (loop for seed from 1 to 200
        for words = (%u64a-words seed)
        unless (equal (coerce (%u64a-rounds (%u64a-make words)) 'list)
                      (coerce (%u64a-rounds-generic (coerce words 'vector)) 'list))
          collect seed)
  nil)

;; The kernel iterated 1000 times from one seed, folded to one integer. The
;; expected value is what SBCL gives for the same source.
(deftest u64-array-lane.checksum
  (let ((w (%u64a-make (%u64a-words 42))))
    (dotimes (i 1000) (%u64a-rounds w))
    (reduce #'logxor w))
  3494963111721914087)

;; An array the declaration does not describe goes through the per-element
;; entries: a non-simple (unsigned-byte 64) vector is a TYPE-ERROR above
;; (safety 0) (the declaration says SIMPLE-ARRAY), and so is a general vector.
;; A check of compiled code: the emit-free build's interpreter does not act on
;; declarations.
(deftest-emitting-only u64-array-lane.declaration-violations
  (list (handler-case (progn (%u64a-rounds (make-array 16 :element-type '(unsigned-byte 64)
                                                         :adjustable t))
                             :ran)
          (type-error () :type-error))
        (handler-case (progn (%u64a-rounds (make-array 16 :initial-element 1)) :ran)
          (type-error () :type-error)))
  (:type-error :type-error))

;; Without a SIMPLE-ARRAY declaration nothing is hoisted, and the per-element
;; entries read and write a non-simple vector too.
(defun %u64a-xor-into (dst src)
  (declare (type (vector (unsigned-byte 64)) dst src))
  (dotimes (i (length dst) dst)
    (setf (aref dst i) (logxor (aref dst i) (aref src i)))))

(deftest u64-array-lane.vector-declaration
  (let ((big (expt 2 63)))
    (list (coerce (%u64a-xor-into (%u64a-make (list 1 big (1- (expt 2 64))))
                                  (make-array 3 :element-type '(unsigned-byte 64)
                                                :adjustable t :fill-pointer 3
                                                :initial-contents (list 3 big 1)))
                  'list)
          (coerce (%u64a-xor-into (make-array 3 :element-type '(unsigned-byte 64)
                                                :adjustable t
                                                :initial-contents (list 1 big 5))
                                  (%u64a-make (list 3 1 big)))
                  'list)))
  ((2 0 18446744073709551614)
   (2 9223372036854775809 9223372036854775813)))

;; Stores of values the element type cannot hold signal, whatever path
;; computed them.
(defun %u64a-store (a i x)
  (declare (type (simple-array (unsigned-byte 64) (*)) a) (type fixnum i))
  (setf (aref a i) x))
(defun %u64a-store-fixnum (a i x)
  (declare (type (simple-array (unsigned-byte 64) (*)) a) (type fixnum i x))
  (setf (aref a i) x))
(defun %u64a-store-sum (a i x)
  (declare (type (simple-array (unsigned-byte 64) (*)) a) (type fixnum i))
  (setf (aref a i) (logxor (aref a i) x)))

(deftest u64-array-lane.store-checks
  (let ((a (%u64a-make (list 0 0 0))))
    (flet ((try (f &rest args)
             (handler-case (progn (apply f a args) (aref a (first args)))
               (type-error () :type-error))))
      (list (try #'%u64a-store 0 (1- (expt 2 64)))
            (try #'%u64a-store 0 -1)
            (try #'%u64a-store 0 (expt 2 64))
            (try #'%u64a-store 0 1.0d0)
            (try #'%u64a-store-fixnum 1 -7)
            (try #'%u64a-store-fixnum 1 most-positive-fixnum)
            (try #'%u64a-store-sum 2 (expt 2 63))
            (try #'%u64a-store-sum 2 -1)
            (try #'%u64a-store-sum 2 (expt 2 64))
            (%u64a-store a 0 (expt 2 63)))))
  (18446744073709551615 :type-error :type-error :type-error :type-error
   #.most-positive-fixnum 9223372036854775808 :type-error :type-error
   9223372036854775808))

;; The value of the SETF form, read in value position, and the element read
;; where its type is not known: integers, bignums at or above 2^63.
(defun %u64a-swap (a)
  (declare (type (simple-array (unsigned-byte 64) (*)) a))
  (list (setf (aref a 0) (aref a 1))
        (aref a 0)
        (incf (aref a 2))
        (decf (aref a 2) 2)))

(deftest u64-array-lane.values
  (%u64a-swap (%u64a-make (list 1 (1- (expt 2 64)) (expt 2 63))))
  (18446744073709551615 18446744073709551615 9223372036854775809 9223372036854775807))

;; A local made by MAKE-ARRAY with the element type is known the same way.
(defun %u64a-local-sum (n)
  (let ((a (make-array n :element-type '(unsigned-byte 64))))
    (dotimes (i n)
      (setf (aref a i) (%u64a-mul (+ i 1) #x9E3779B97F4A7C15)))
    (let ((acc 0))
      (dotimes (i n acc)
        (setf acc (%u64a-add acc (aref a i)))))))

(deftest u64-array-lane.make-array-local
  (%u64a-local-sum 10)
  #.(mod (* 55 #x9E3779B97F4A7C15) (expt 2 64)))

;; Argon2's G on the array allocates nothing: the words at or above 2^63 were
;; a bignum per store, about 11 KB per block of 128 words.
(defun %u64a-bytes () (nth 4 (dotcl:gc-stats)))
(deftest-compiled-only u64-array-lane.no-allocation
  (let ((w (%u64a-make (%u64a-words 7))))
    (%u64a-rounds w)
    (let ((b0 (%u64a-bytes)))
      (dotimes (i 10000) (%u64a-rounds w))
      (< (/ (- (%u64a-bytes) b0) 10000) 8)))
  t)
