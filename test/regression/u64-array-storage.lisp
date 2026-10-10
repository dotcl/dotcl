;;; An (unsigned-byte 64) array keeps its elements as their bits (a ulong[]),
;;; not as boxed integers: a value at or above 2^63 is a bignum only when it
;;; is read where its type is not known. These are the generic paths: every
;;; sequence and array function, printing, EQUALP, streams, fasl literals and
;;; pinned vectors, with values on both sides of 2^63.

(defparameter *u64s-big* (expt 2 63))
(defparameter *u64s-max* (1- (expt 2 64)))

(defun %u64s-make (&rest xs)
  (make-array (length xs) :element-type '(unsigned-byte 64) :initial-contents xs))

(deftest u64-array-storage.upgraded-type
  (list (array-element-type (make-array 1 :element-type '(unsigned-byte 64)))
        (upgraded-array-element-type '(unsigned-byte 64))
        (upgraded-array-element-type `(integer 0 ,(1- (expt 2 64))))
        (upgraded-array-element-type `(integer ,(expt 2 63) ,(1- (expt 2 64))))
        (upgraded-array-element-type `(mod ,(expt 2 64)))
        (upgraded-array-element-type '(unsigned-byte 63))
        (upgraded-array-element-type '(unsigned-byte 65))
        (upgraded-array-element-type `(integer -1 ,(expt 2 63)))
        (type-of (make-array 4 :element-type '(unsigned-byte 64)))
        (typep (make-array 1 :element-type '(unsigned-byte 64)) 'simple-vector))
  ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64)
   (unsigned-byte 64) (unsigned-byte 63) t t
   (simple-array (unsigned-byte 64) (4)) nil))

(deftest u64-array-storage.round-trip
  (let ((a (make-array 4 :element-type '(unsigned-byte 64) :initial-element *u64s-max*)))
    (setf (aref a 1) 5
          (aref a 2) *u64s-big*
          (aref a 3) (1- *u64s-big*))
    (list (coerce a 'list)
          (coerce (make-array 2 :element-type '(unsigned-byte 64)) 'list)
          (coerce (make-array 2 :element-type '(unsigned-byte 64)
                                :initial-element *u64s-big*)
                  'list)))
  ((18446744073709551615 5 9223372036854775808 9223372036854775807)
   (0 0) (9223372036854775808 9223372036854775808)))

(deftest u64-array-storage.store-out-of-range-signals
  (let ((a (%u64s-make 7)))
    (list (handler-case (progn (setf (aref a 0) -1) :stored) (type-error () :type-error))
          (handler-case (progn (setf (aref a 0) (expt 2 64)) :stored) (type-error () :type-error))
          (handler-case (progn (setf (aref a 0) 1.0) :stored) (type-error () :type-error))
          (handler-case (progn (%u64s-make 1 (expt 2 64)) :made) (type-error () :type-error))
          (handler-case (progn (make-array 1 :element-type '(unsigned-byte 64) :initial-element -5)
                               :made)
            (type-error () :type-error))
          (aref a 0)))
  (:type-error :type-error :type-error :type-error :type-error 7))

(deftest u64-array-storage.sequence-functions
  (let ((a (%u64s-make 1 *u64s-big* *u64s-max*)))
    (list (equalp a (vector 1 *u64s-big* *u64s-max*))
          (equalp a (%u64s-make 1 *u64s-big* (1- *u64s-max*)))
          (coerce (reverse a) 'list)
          (coerce (sort (copy-seq a) #'>) 'list)
          (coerce (subseq a 1) 'list)
          (coerce (replace (make-array 3 :element-type '(unsigned-byte 64)) a :start1 1) 'list)
          (coerce (fill (copy-seq a) *u64s-big* :start 2) 'list)
          (coerce (map '(vector (unsigned-byte 64)) #'1+ (vector 1 *u64s-big*)) 'list)
          (array-element-type (map '(vector (unsigned-byte 64)) #'1+ (vector 1)))
          (length (concatenate '(vector (unsigned-byte 64)) a a))
          (coerce (remove *u64s-big* a) 'list)
          (position *u64s-max* a) (find *u64s-big* a) (count 1 a)
          (search (vector *u64s-big*) a)
          (row-major-aref a 2)
          (coerce (coerce '(1 2 3) '(vector (unsigned-byte 64))) 'list)))
  (t nil
   (18446744073709551615 9223372036854775808 1)
   (18446744073709551615 9223372036854775808 1)
   (9223372036854775808 18446744073709551615)
   (0 1 9223372036854775808)
   (1 9223372036854775808 9223372036854775808)
   (2 9223372036854775809)
   (unsigned-byte 64)
   6
   (1 18446744073709551615)
   2 9223372036854775808 1 1 18446744073709551615 (1 2 3)))

(deftest u64-array-storage.equalp-hash
  (let ((h (make-hash-table :test 'equalp)))
    (setf (gethash (%u64s-make 1 *u64s-big*) h) :hit)
    (gethash (vector 1 *u64s-big*) h))
  :hit t)

(deftest u64-array-storage.displaced-adjustable-multidim
  (let* ((a (%u64s-make 1 *u64s-big* *u64s-max*))
         (d (make-array 2 :element-type '(unsigned-byte 64)
                          :displaced-to a :displaced-index-offset 1))
         (f (make-array 0 :element-type '(unsigned-byte 64) :adjustable t :fill-pointer 0))
         (m (make-array '(2 2) :element-type '(unsigned-byte 64)
                               :initial-contents (list (list 1 *u64s-big*) (list *u64s-max* 0)))))
    (setf (aref d 1) 42)
    (dotimes (i 3) (vector-push-extend (+ *u64s-big* i) f))
    (setf f (adjust-array f (array-dimension f 0) :fill-pointer 4))
    (setf (aref f 3) *u64s-max*)
    (setf (aref m 1 1) *u64s-big*)
    (list (coerce a 'list) (coerce d 'list) (coerce f 'list)
          (aref m 1 0) (aref m 1 1) (aref m 0 1)))
  ((1 9223372036854775808 42) (9223372036854775808 42)
   (9223372036854775808 9223372036854775809 9223372036854775810
    18446744073709551615)
   18446744073709551615 9223372036854775808 9223372036854775808))

(deftest u64-array-storage.print
  (list (prin1-to-string (%u64s-make 0 *u64s-big* *u64s-max*))
        (handler-case (let ((*print-readably* t)) (prin1-to-string (%u64s-make 1)))
          (print-not-readable () :not-readable)))
  ("#(0 9223372036854775808 18446744073709551615)" :not-readable))

(deftest u64-array-storage.binary-stream
  (let ((path (regression-temp-file "u64-array-storage.bin"))
        (v (make-array 3 :element-type '(unsigned-byte 64))))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 64)
                            :if-exists :supersede)
      (write-sequence (%u64s-make 1 *u64s-big* *u64s-max*) s))
    (with-open-file (s path :element-type '(unsigned-byte 64))
      (read-sequence v s))
    (delete-file path)
    (coerce v 'list))
  (1 9223372036854775808 18446744073709551615))

(deftest-emitting-only u64-array-storage.literal-in-compiled-code
  (let ((f (compile nil `(lambda ()
                           ,(%u64s-make 3 *u64s-big* *u64s-max*)))))
    (let ((v (funcall f)))
      (list (array-element-type v) (coerce v 'list))))
  ((unsigned-byte 64) (3 9223372036854775808 18446744073709551615)))

(deftest u64-array-storage.pinned-vector
  ;; The storage is the values' bits at 8 bytes each, which is what foreign
  ;; code reading a uint64_t array expects.
  (let* ((p (dotcl:make-pinned-vector 3 '(unsigned-byte 64)))
         (addr (progn (setf (aref p 0) *u64s-max* (aref p 1) *u64s-big*)
                      (dotcl:pinned-vector-address p))))
    (dotnet:mem-write (1- *u64s-big*) :uint64 addr 16)
    (prog1 (list (dotnet:mem-read :uint64 addr 0)
                 (dotnet:mem-read :uint64 addr 8)
                 (aref p 2)
                 (array-element-type p))
      (dotcl:unpin-vector p)))
  (18446744073709551615 9223372036854775808 9223372036854775807 (unsigned-byte 64)))
