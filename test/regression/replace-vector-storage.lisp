;;; REPLACE between two vectors of the same element type copies the storage
;;; directly. It used to read each element boxed and store it unboxed again,
;;; which for an (UNSIGNED-BYTE 32) or (UNSIGNED-BYTE 8) vector is an
;;; allocation per element past the small integer cache. ironclad's scrypt
;;; moves its 64-byte blocks around with REPLACE. The results must be what
;;; the element-by-element copy gave, also for overlapping regions of one
;;; vector, fill pointers, and vectors of different element types.

(defun %rvs-iota (n type &optional (k 1))
  (let ((v (make-array n :element-type type)))
    (dotimes (i n v) (setf (aref v i) (* k (1+ i))))))

(deftest replace-vector-storage.same-type
  (list (coerce (replace (%rvs-iota 6 '(unsigned-byte 8))
                         (%rvs-iota 4 '(unsigned-byte 8) 10) :start1 1 :start2 1)
                'list)
        (coerce (replace (%rvs-iota 5 '(unsigned-byte 32))
                         (%rvs-iota 5 '(unsigned-byte 32) #x10000000) :end1 3)
                'list)
        (coerce (replace (%rvs-iota 4 'fixnum) (%rvs-iota 2 'fixnum -1) :start1 2) 'list)
        (coerce (replace (%rvs-iota 4 'double-float 1d0) (%rvs-iota 2 'double-float 0.5d0))
                'list)
        (replace (copy-seq "abcdef") "XYZ" :start1 3 :end2 2)
        (coerce (replace (vector 1 'a "b" nil) (vector :x :y) :start1 1) 'list))
  ((1 20 30 40 5 6)
   (#x10000000 #x20000000 #x30000000 4 5)
   (1 2 -1 -2)
   (0.5d0 1.0d0 3.0d0 4.0d0)
   "abcXYf"
   (1 :x :y nil)))

;; One vector as both target and source: the result is as if the source
;; region were copied out first, in both directions.
(deftest replace-vector-storage.overlap
  (let ((a (%rvs-iota 8 '(unsigned-byte 32)))
        (b (%rvs-iota 8 '(unsigned-byte 32)))
        (c (%rvs-iota 8 t)))
    (replace a a :start1 2 :end1 7)
    (replace b b :start2 2)
    (replace c c :start1 1)
    (list (coerce a 'list) (coerce b 'list) (coerce c 'list)))
  ((1 2 1 2 3 4 5 8)
   (3 4 5 6 7 8 7 8)
   (1 1 2 3 4 5 6 7)))

;; A fill pointer bounds the copy; elements past it are not touched.
(deftest replace-vector-storage.fill-pointer
  (let ((a (make-array 6 :element-type '(unsigned-byte 8) :fill-pointer 2
                         :initial-element 0))
        (src (%rvs-iota 6 '(unsigned-byte 8))))
    (replace a src)
    (setf (fill-pointer a) 6)
    (coerce a 'list))
  (1 2 0 0 0 0))

;; Different element types still go element by element, with the target's
;; type check.
(deftest replace-vector-storage.mixed-types
  (list (coerce (replace (%rvs-iota 3 '(unsigned-byte 32)) (%rvs-iota 2 '(unsigned-byte 8) 7))
                'list)
        (coerce (replace (%rvs-iota 3 t) (%rvs-iota 2 '(unsigned-byte 32) 9)) 'list)
        (handler-case (replace (%rvs-iota 2 '(unsigned-byte 8)) (vector 300))
          (type-error () :type-error))
        (handler-case (replace (%rvs-iota 2 '(unsigned-byte 8)) (%rvs-iota 2 'fixnum -1))
          (type-error () :type-error)))
  ((7 14 3) (9 18 3) :type-error :type-error))

;; A displaced vector shares its target's storage.
(deftest replace-vector-storage.displaced
  (let* ((base (%rvs-iota 6 '(unsigned-byte 32)))
         (d (make-array 3 :element-type '(unsigned-byte 32) :displaced-to base
                          :displaced-index-offset 2)))
    (replace d (%rvs-iota 3 '(unsigned-byte 32) 100))
    (coerce base 'list))
  (1 2 100 200 300 6))

(defun %rvs-bytes () (nth 4 (dotcl:gc-stats)))

;; One copy of 100,000 (UNSIGNED-BYTE 32) elements above the small integer
;; cache: the element-by-element copy allocated a box per element (2.4 MB). A
;; single call, so that what an evaluator allocates around it does not count.
;; The elements are #x100000 + i: above the cache and inside the element type.
(deftest replace-vector-storage.no-boxing
  (let ((a (make-array 100000 :element-type '(unsigned-byte 32)))
        (b (make-array 100000 :element-type '(unsigned-byte 32))))
    (dotimes (i 100000) (setf (aref a i) (+ #x100000 i)))
    (replace b a :end1 10)
    (let ((b0 (%rvs-bytes)))
      (replace b a)
      (list (< (- (%rvs-bytes) b0) 100000) (aref b 99999))))
  (t #.(+ #x100000 99999)))
