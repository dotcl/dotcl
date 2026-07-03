;;; Regression tests for unboxed numeric array storage: bounded integer
;;; element types map to byte[]/ushort[]/int[]/long[] backings (with
;;; (integer LO HI) upgraded to the canonical fixed-width type), mirroring
;;; the bit-vector _bitData pattern. Contract: identical observable behavior
;;; to general storage — aref/setf, adjust-array, fill-pointer growth,
;;; displacement, sequence functions — plus loud errors on element-type
;;; violations (never a silent wrap).

;; (integer LO HI) upgrades to the canonical fixed-width element type.
(deftest nas-upgrade-u16
  (array-element-type (make-array 3 :element-type '(integer 0 2000)))
  (unsigned-byte 16))

(deftest nas-upgrade-u8
  (array-element-type (make-array 3 :element-type '(integer 0 255)))
  (unsigned-byte 8))

(deftest nas-upgrade-bit
  (array-element-type (make-array 3 :element-type '(integer 0 1)))
  bit)

(deftest nas-upgrade-signed
  (array-element-type (make-array 3 :element-type '(integer -10 10)))
  (signed-byte 32))

(deftest nas-upgrade-unbounded-stays-integer
  (array-element-type (make-array 3 :element-type '(integer 0)))
  integer)

;; Basic read/write across the storage kinds.
(defun %nas-rw (etype vals)
  (let ((v (make-array (length vals) :element-type etype)))
    (dotimes (i (length vals))
      (setf (aref v i) (nth i vals)))
    (coerce v 'list)))

(deftest nas-rw-u8
  (%nas-rw '(unsigned-byte 8) '(0 1 127 255))
  (0 1 127 255))

(deftest nas-rw-u16
  (%nas-rw '(unsigned-byte 16) '(0 70 65535))
  (0 70 65535))

(deftest nas-rw-i32
  (%nas-rw '(signed-byte 32) '(-2147483648 -1 0 2147483647))
  (-2147483648 -1 0 2147483647))

(deftest nas-rw-fixnum
  (%nas-rw 'fixnum (list most-negative-fixnum -1 0 most-positive-fixnum))
  #.(list most-negative-fixnum -1 0 most-positive-fixnum))

;; :initial-element fills; default init is 0.
(deftest nas-initial-element
  (coerce (make-array 4 :element-type '(unsigned-byte 16) :initial-element 1234) 'list)
  (1234 1234 1234 1234))

(deftest nas-default-zero
  (coerce (make-array 3 :element-type '(unsigned-byte 8)) 'list)
  (0 0 0))

;; :initial-contents.
(deftest nas-initial-contents
  (coerce (make-array 3 :element-type '(unsigned-byte 16) :initial-contents '(5 6 7)) 'list)
  (5 6 7))

;; Storing a value outside the storage range signals (no silent wrap).
(deftest nas-store-out-of-range
  (let ((v (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
    (handler-case (progn (setf (aref v 0) 256) :no-error)
      (error () :err)))
  :err)

;; Storing a non-integer signals.
(deftest nas-store-non-integer
  (let ((v (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
    (handler-case (progn (setf (aref v 0) 'x) :no-error)
      (error () :err)))
  :err)

;; typep against the declared (pre-upgrade) and upgraded element types.
(deftest nas-typep
  (let ((v (make-array 3 :element-type '(integer 0 2000))))
    (list (notnot (typep v '(array (integer 0 2000))))
          (notnot (typep v '(array (unsigned-byte 16))))
          (notnot (typep v '(vector (unsigned-byte 16) 3)))))
  (t t t))

;; 2D / 3D typed arrays.
(defun %nas-2d (n)
  (declare (fixnum n))
  (let ((a (make-array (list n n) :element-type '(integer 0 2000) :initial-element 1)))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref a i j) (+ (aref a i j) (* i n) j))))
    (list (aref a 0 0) (aref a 2 3) (aref a (1- n) (1- n)))))

(deftest nas-2d
  (%nas-2d 5)
  (1 14 25))

(deftest nas-3d
  (let ((a (make-array '(2 3 4) :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref a 1 2 3) 42)
    (list (aref a 1 2 3) (aref a 0 0 0)))
  (42 0))

;; adjust-array: grow and shrink keep contents; new cells get initial-element.
(deftest nas-adjust
  (let* ((v (make-array 3 :element-type '(unsigned-byte 16) :initial-contents '(1 2 3)
                          :adjustable t))
         (v2 (adjust-array v 5 :initial-element 9)))
    (coerce v2 'list))
  (1 2 3 9 9))

;; fill-pointer + vector-push-extend on a numeric-backed vector.
(deftest nas-vector-push-extend
  (let ((v (make-array 2 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (dotimes (i 5) (vector-push-extend (* i 10) v))
    (coerce v 'list))
  (0 10 20 30 40))

;; Displaced array over a numeric-backed target: reads and writes go through.
(deftest nas-displaced
  (let* ((base (make-array 10 :element-type '(unsigned-byte 16) :initial-element 0))
         (d (make-array 4 :element-type '(unsigned-byte 16)
                          :displaced-to base :displaced-index-offset 3)))
    (setf (aref d 0) 111)
    (setf (aref base 4) 222)
    (list (aref base 3) (aref d 1) (coerce d 'list)))
  (111 222 (111 222 0 0)))

;; Sequence functions over numeric-backed vectors.
(deftest nas-sequence-ops
  (let ((v (make-array 6 :element-type '(unsigned-byte 8) :initial-contents '(3 1 2 5 4 6))))
    (list (find 5 v)
          (position 4 v)
          (reduce #'+ v)
          (coerce (sort (copy-seq v) #'<) 'list)
          (coerce (reverse v) 'list)))
  (5 4 21 (1 2 3 4 5 6) (6 4 5 2 1 3)))

(deftest nas-fill-replace
  (let ((v (make-array 5 :element-type '(unsigned-byte 16) :initial-element 1)))
    (fill v 7 :start 1 :end 3)
    (replace v #(100 200) :start1 3)
    (coerce v 'list))
  (1 7 7 100 200))

;; search over a typed vector (the cl-bench 1d-arrays shape).
(deftest nas-search
  (let ((v (make-array 8 :element-type '(integer 0 2000)
                         :initial-contents '(1 2 3 4 5 6 7 8))))
    (list (search '(4 5 6) v) (search '(9 9) v)))
  (3 nil))

;; print/read round-trip of a typed vector's contents.
(deftest nas-print
  (let ((v (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
    (format nil "~a" v))
  "#(1 2 3)")
