;;; UPGRADED-ARRAY-ELEMENT-TYPE has to agree with the arrays MAKE-ARRAY builds.
;;;
;;; CLHS defines ARRAY-ELEMENT-TYPE as the upgraded element type, so the two
;;; cannot disagree. dotcl specialized (unsigned-byte 8), double-float and the
;;; rest -- it type-checks stores and coerces on the way in -- while
;;; UPGRADED-ARRAY-ELEMENT-TYPE answered T for all of them, so portable code
;;; that probes for a specialization took the fallback path against arrays that
;;; were not general at all.

(defun uaet-agrees (spec)
  (equal (upgraded-array-element-type spec)
         (array-element-type (make-array 1 :element-type spec))))

(deftest uaet-agrees-with-array-element-type
  (remove-if #'uaet-agrees
             '((unsigned-byte 8) (unsigned-byte 16) (signed-byte 32)
               double-float single-float fixnum bit character base-char t))
  nil)

(deftest uaet-byte-type
  (upgraded-array-element-type '(unsigned-byte 8))
  (unsigned-byte 8))

(deftest uaet-double-float
  (upgraded-array-element-type 'double-float)
  double-float)

;;; An integer range lands on the width that holds it, which is the type the
;;; unboxed backing is chosen by.
(deftest uaet-integer-range
  (upgraded-array-element-type '(integer 0 100))
  (unsigned-byte 8))

;;; Exclusive bounds name the same range: (integer 0 (256)) is 0..255.
(deftest uaet-exclusive-bound
  (upgraded-array-element-type '(integer 0 (256)))
  (unsigned-byte 8))

(deftest uaet-exclusive-bound-bit
  (upgraded-array-element-type '(integer 0 (2)))
  bit)

;;; Narrow types have to land in the same lattice, or upgrading would stop
;;; preserving subtype relations: (eql 1) is a subtype of BIT.
(deftest uaet-eql-integer
  (list (upgraded-array-element-type '(eql 1))
        (upgraded-array-element-type '(eql 8)))
  (bit (unsigned-byte 8)))

(deftest uaet-mod
  (upgraded-array-element-type '(mod 5))
  (unsigned-byte 8))

;;; Types dotcl does not specialize upgrade to T, and that answer must be a
;;; supertype of the argument.
(deftest uaet-unspecialized-is-t
  (list (upgraded-array-element-type 'symbol)
        (upgraded-array-element-type 'cons)
        (upgraded-array-element-type 'float))
  (t t t))

;;; The upgraded type is a supertype of what was asked for (CLHS 15.1.2.1).
(deftest uaet-result-is-a-supertype
  (remove-if (lambda (spec)
               (multiple-value-bind (sub sure)
                   (subtypep spec (upgraded-array-element-type spec))
                 (and sure sub)))
             '((unsigned-byte 8) (eql 1) (eql 8) (mod 5) (integer 0 100)
               bit character fixnum double-float symbol cons))
  nil)

;;; And upgrading preserves subtype relations.
(deftest uaet-preserves-subtype-relations
  (remove-if (lambda (pair)
               (destructuring-bind (narrow wide) pair
                 (multiple-value-bind (sub sure)
                     (subtypep (upgraded-array-element-type narrow)
                               (upgraded-array-element-type wide))
                   (and sure sub))))
             '(((eql 1) bit)
               ((eql 8) (unsigned-byte 8))
               ((eql 8) fixnum)
               (bit (unsigned-byte 8))
               ((unsigned-byte 8) fixnum)
               (base-char character)))
  nil)

;;; The specialization is real: stores are checked, and an integer stored into a
;;; double-float array is coerced. This is why claiming T was wrong.
(deftest uaet-specialization-is-real
  (list (handler-case (let ((a (make-array 1 :element-type '(unsigned-byte 8))))
                        (setf (aref a 0) 300)
                        :no-error)
          (type-error () :type-error))
        (let ((a (make-array 1 :element-type 'double-float)))
          (setf (aref a 0) 1)
          (aref a 0)))
  (:type-error 1.0d0))
