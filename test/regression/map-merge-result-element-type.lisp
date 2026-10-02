;;; MAP and MERGE with a specialized vector result type.
;;;
;;; The result type's element type was dropped: (map '(simple-array fixnum (*))
;;; ...) returned a vector of element type T, which then failed a
;;; (simple-array fixnum (*)) declaration. COERCE, CONCATENATE and
;;; MAKE-SEQUENCE already honoured it. SBCL's perfect-hash generator maps into
;;; such a type, and building SBCL with dotcl as the host stopped there.

(defun mmr-fixnum-length (a)
  (declare (type (simple-array fixnum (*)) a))
  (length a))

(deftest map-result-simple-array-fixnum
  (let ((v (map '(simple-array fixnum (*)) #'1+ '(1 2))))
    (list (coerce v 'list)
          (equal (array-element-type v) (upgraded-array-element-type 'fixnum))
          (mmr-fixnum-length v)))
  ((2 3) t 2))

(deftest map-result-vector-element-types
  (list (equal (array-element-type (map '(vector fixnum) #'1+ '(1 2)))
               (upgraded-array-element-type 'fixnum))
        (equal (array-element-type (map '(simple-array (unsigned-byte 8) (*)) #'1+ #(1 2)))
               (upgraded-array-element-type '(unsigned-byte 8)))
        (equal (array-element-type (map '(vector double-float) (lambda (x) (float x 1d0)) '(1 2)))
               (upgraded-array-element-type 'double-float))
        (array-element-type (map 'vector #'1+ '(1 2)))
        (array-element-type (map 'simple-vector #'1+ '(1 2)))
        (array-element-type (map '(vector *) #'1+ '(1 2))))
  (t t t t t t))

(deftest merge-result-vector-fixnum
  (let ((v (merge '(simple-array fixnum (*)) (list 1 3) (list 2) #'<)))
    (list (coerce v 'list) (equal (array-element-type v) (upgraded-array-element-type 'fixnum))
          (mmr-fixnum-length v)))
  ((1 2 3) t 3))
