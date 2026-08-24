;;; A complex of two doubles keeps them unboxed.
;;;
;;; The parts used to be two separate number objects, so every complex operation
;;; allocated three: the complex and a box for each part. The double/double case
;;; now stores the two doubles in the complex itself, which is one object. Which
;;; representation a value gets is decided at construction, so the two never
;;; describe the same value and nothing has to compare across them.
;;;
;;; What must not change is anything observable: the type, the parts and their
;;; types, identity under EQL/EQUAL/EQUALP, and hash table lookup.

(defun %cdr-make (a b) (complex a b))

(deftest complex-double-representation.parts-and-type
  (let ((z (%cdr-make 1.0d0 2.0d0)))
    (list (realpart z) (imagpart z)
          (type-of (realpart z)) (type-of (imagpart z))
          (complexp z) (numberp z) (typep z 'complex) (typep z 'number)))
  (1.0d0 2.0d0 double-float double-float t t t t))

;;; Two separately built complexes with the same parts are EQL (CLHS: EQL on
;;; numbers of the same type compares values), and a literal is the same again.
(deftest complex-double-representation.identity
  (let ((z1 (%cdr-make 1.0d0 2.0d0)) (z2 (%cdr-make 1.0d0 2.0d0)))
    (list (eql z1 z2) (equal z1 z2) (equalp z1 z2) (= z1 z2)
          (eql z1 #c(1.0d0 2.0d0)) (eql z1 (%cdr-make 1.0d0 2.5d0))))
  (t t t t t nil))

;;; A key found again is the property the hash of a complex has to have.
(deftest complex-double-representation.hash-table-keys
  (let ((z (%cdr-make 1.0d0 2.0d0))
        (h (make-hash-table :test #'eql))
        (he (make-hash-table :test #'equal)))
    (setf (gethash z h) :a (gethash z he) :b)
    (list (gethash (%cdr-make 1.0d0 2.0d0) h)
          (gethash (%cdr-make 1.0d0 2.0d0) he)
          (gethash #c(1.0d0 2.0d0) h)))
  (:a :b :a))

;;; The operations answer what they answered before, to the last digit.
(deftest complex-double-representation.operations
  (let ((z (%cdr-make 1.0d0 2.0d0)))
    (list (- z) (conjugate z) (abs z) (* z z) (/ z z) (+ z 1)
          (coerce z '(complex single-float))))
  (#c(-1.0d0 -2.0d0) #c(1.0d0 -2.0d0) 2.23606797749979d0
   #c(-3.0d0 4.0d0) #c(1.0d0 0.0d0) #c(2.0d0 2.0d0) #c(1.0 2.0)))

;;; Rational and single-float complexes are untouched by all this.
(deftest complex-double-representation.other-representations
  (list (%cdr-make 1 2) (%cdr-make 1/2 3/4) (%cdr-make 1.5 2.5)
        (type-of (realpart (%cdr-make 1.5 2.5)))
        (eql (%cdr-make 1 2) (%cdr-make 1 2))
        (eql (%cdr-make 1.5 2.5) (%cdr-make 1.5 2.5)))
  (#c(1 2) #c(1/2 3/4) #c(1.5 2.5) single-float t t))

;;; One object per operation, not three. Difference of two loop lengths, smallest
;;; of several runs.
(defun %cdr-mul (n z)
  (declare (fixnum n))
  (let ((acc z))
    (dotimes (i n acc) (setq acc (* acc #c(1.0000001d0 0.0000001d0))))))

(defun %cdr-bytes-for (n)
  (let ((best nil) (z #c(1.0d0 1.0d0)))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (%cdr-mul n z)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: this is a statement about the objects the operation builds,
;; measured in a compiled loop. An emit-free build interprets the loop and its
;; own bookkeeping dominates the measurement.
(deftest-compiled-only complex-double-representation.multiply-allocates-one-object
  (progn (%cdr-bytes-for 1000)              ; warm
         (let ((small (%cdr-bytes-for 10000))
               (large (%cdr-bytes-for 110000)))
           ;; One 32-byte object per multiply is 3.2 MB for the 100k extra ones;
           ;; the three-object form was 80 bytes each, so 8 MB.
           (< (- large small) 5000000)))
  t)

;;; A complex works as an EQUALP hash table key.
;;;
;;; EQUALP compares numbers with =, so #C(1 2) and #C(1.0d0 2.0d0) are EQUALP and
;;; have to land in the same bucket, and a complex whose imaginary part is zero
;;; has to agree with the real it equals. The hash asked ToDouble for the value of
;;; the complex itself, which has none, so any complex key threw. Expected values
;;; from SBCL.
(deftest complex-double-representation.equalp-hash-keys
  (flet ((probe (key lookup)
           (let ((h (make-hash-table :test #'equalp)))
             (setf (gethash key h) :found)
             (gethash lookup h))))
    (list (probe (complex 1.0d0 2.0d0) (complex 1.0d0 2.0d0))
          (probe (complex 1 2) (complex 1 2))
          (probe (complex 1 2) (complex 1.0d0 2.0d0))
          (probe (complex 1.0d0 0.0d0) 1.0d0)
          (probe (complex 1.5 2.5) (complex 1.5 2.5))
          (probe (complex 1.0d0 2.0d0) (complex 1.0d0 3.0d0))))
  (:found :found :found :found :found nil))

;;; The comparison these hashes have to agree with.
(deftest complex-double-representation.equalp-comparison
  (list (equalp #c(1.0d0 2.0d0) #c(1 2))
        (equalp (complex 1.0d0 0.0d0) 1.0d0)
        (= (complex 1.0d0 0.0d0) 1.0d0)
        (equalp (complex 1.0d0 2.0d0) (complex 1.0d0 2.0d0)))
  (t t t t))
