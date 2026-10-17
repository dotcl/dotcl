;;; UPGRADED-ARRAY-ELEMENT-TYPE answers one of the element types arrays are
;;; specialized on, or T. (integer -10 0) used to upgrade to (SIGNED-BYTE 5),
;;; which no array type list (serapeum's WITH-INT-VECTOR, say) names.

(defparameter *uaet-specialized*
  '(bit (unsigned-byte 2) (unsigned-byte 4) (unsigned-byte 7) (unsigned-byte 8)
    (unsigned-byte 15) (unsigned-byte 16) (unsigned-byte 31) (unsigned-byte 32)
    (unsigned-byte 62) (unsigned-byte 63) (unsigned-byte 64)
    (signed-byte 8) (signed-byte 16) (signed-byte 32) fixnum (signed-byte 64)
    character base-char single-float double-float
    (complex single-float) (complex double-float) nil t))

(defparameter *uaet-types*
  '(bit fixnum (signed-byte 1) (signed-byte 2) (signed-byte 5) (signed-byte 9)
    (signed-byte 17) (signed-byte 33) (signed-byte 63) (signed-byte 64) (signed-byte 65)
    (unsigned-byte 1) (unsigned-byte 3) (unsigned-byte 5) (unsigned-byte 9)
    (unsigned-byte 17) (unsigned-byte 33) (unsigned-byte 62) (unsigned-byte 65)
    (integer -10 0) (integer (-11) 0) (integer -1 0) (integer -129 0) (integer 0 256)
    (integer -100000 100000) (integer 0 65536) (mod 3) (eql 5) (eql -1)
    (integer 0 4294967295) (integer -4611686018427387904 4611686018427387903)
    integer standard-char base-char character short-float long-float float))

(deftest uaet-specialized-set.member
  (remove-if (lambda (ty)
               (member (upgraded-array-element-type ty) *uaet-specialized* :test #'equal))
             *uaet-types*)
  nil)

(deftest uaet-specialized-set.values
  (mapcar #'upgraded-array-element-type
          '((integer -10 0) (signed-byte 5) (integer -1 0) (unsigned-byte 1)
            (unsigned-byte 3) (unsigned-byte 9) (unsigned-byte 17) (unsigned-byte 33)
            (signed-byte 17) (signed-byte 33) fixnum standard-char))
  ((signed-byte 8) (signed-byte 8) (signed-byte 8) bit
   (unsigned-byte 4) (unsigned-byte 15) (unsigned-byte 31) (unsigned-byte 62)
   (signed-byte 32) fixnum fixnum base-char))

(deftest uaet-specialized-set.array-element-type
  (loop for ty in *uaet-types*
        for a = (make-array 2 :element-type ty)
        unless (equal (array-element-type a) (upgraded-array-element-type ty))
          collect ty)
  nil)

;; FIXNUM is the same type as (SIGNED-BYTE 64) here (not in SBCL, where it is
;; (SIGNED-BYTE 63)), and (UNSIGNED-BYTE 1) is BIT.
(deftest uaet-specialized-set.same-type-same-array
  (list (typep (make-array 1 :element-type 'fixnum) '(simple-array (signed-byte 64) (*)))
        (typep (make-array 1 :element-type '(signed-byte 64)) '(simple-array fixnum (*)))
        (typep (make-array 1 :element-type 'bit) '(simple-array (unsigned-byte 1) (*)))
        (typep (make-array 1 :element-type '(unsigned-byte 1)) 'simple-bit-vector))
  (t t t t))

(deftest uaet-specialized-set.subtype-preserved
  (let ((ups (mapcar #'upgraded-array-element-type *uaet-types*)))
    (loop for t1 in *uaet-types* for u1 in ups
          append (loop for t2 in *uaet-types* for u2 in ups
                       when (and (subtypep t1 t2) (not (subtypep u1 u2)))
                         collect (list t1 t2))))
  nil)

;; serapeum's range vectors: a negative range is one of these array types.
(deftest uaet-specialized-set.int-vector-dispatch
  (loop for ty in '((integer (-10) 0) (integer -1000 (0)) (integer 0 (10)) (integer -100000 0))
        unless (typep (make-array 3 :element-type ty)
                      '(or (simple-array bit (*)) (simple-array (unsigned-byte 2) (*))
                        (simple-array (unsigned-byte 4) (*)) (simple-array (unsigned-byte 7) (*))
                        (simple-array (signed-byte 8) (*)) (simple-array (unsigned-byte 8) (*))
                        (simple-array (signed-byte 16) (*)) (simple-array (unsigned-byte 16) (*))
                        (simple-array (signed-byte 32) (*)) (simple-array (unsigned-byte 32) (*))
                        (simple-array fixnum (*))))
          collect ty)
  nil)

(defun %uaet-s5-store (v)
  (declare (type (simple-array (signed-byte 5) (*)) v) (optimize (safety 1)))
  (setf (aref v 0) -100)
  (aref v 0))

;; (signed-byte 5) is stored as (signed-byte 8): the store checks that width.
(deftest uaet-specialized-set.declared-narrow
  (list (%uaet-s5-store (make-array 2 :element-type '(signed-byte 5)))
        (handler-case (progn (setf (aref (make-array 2 :element-type '(integer -10 0)) 0) 200) :stored)
          (type-error () :type-error)))
  (-100 :type-error))

;; FIXNUM arrays report FIXNUM, (signed-byte 33..63) upgrades to FIXNUM and
;; (unsigned-byte 33..62) to (unsigned-byte 62), as in SBCL 2.6.8.
(defun %uaet-make (et) (make-array 2 :element-type et))

(deftest uaet-specialized-set.fixnum-name
  (let ((a (make-array 3 :element-type 'fixnum :initial-element 1)))
    (list (array-element-type a)
          (array-element-type (%uaet-make 'fixnum))
          (array-element-type (make-array '(2 2) :element-type 'fixnum))
          (array-element-type (make-array 2 :element-type '(signed-byte 64)))
          (array-element-type (make-array 2 :element-type '(signed-byte 40)))
          (array-element-type (make-array 2 :element-type '(unsigned-byte 40)))
          (array-element-type (copy-seq a))
          (array-element-type (subseq a 1))
          (array-element-type (coerce '(1 2) '(vector fixnum)))
          (array-element-type (make-array 2 :element-type (array-element-type a)))
          (upgraded-array-element-type 'fixnum)
          (upgraded-array-element-type '(signed-byte 64))
          (type-of a)))
  (fixnum fixnum fixnum (signed-byte 64) fixnum (unsigned-byte 62) fixnum fixnum
   fixnum fixnum fixnum (signed-byte 64) (simple-array fixnum (3))))

;; Here they are one type, so the arrays are too (SBCL answers NIL: its FIXNUM
;; is narrower).
(deftest uaet-specialized-set.fixnum-same-as-signed-byte-64
  (let ((a (make-array 2 :element-type 'fixnum :initial-element 0))
        (b (make-array 2 :element-type '(signed-byte 64) :initial-element 0)))
    (list (subtypep '(array fixnum) '(array (signed-byte 64)))
          (subtypep '(array (signed-byte 64)) '(array fixnum))
          (equalp a b)
          (progn (replace a b) (setf (aref a 0) most-positive-fixnum) (aref a 0))))
  (t t t #.most-positive-fixnum))
