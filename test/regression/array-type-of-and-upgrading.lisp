;;; TYPE-OF of a vector carries its length, and an array made with an element
;;; type that has no specialized storage IS a general array.
;;;
;;; (type-of (vector 1 2 3)) was SIMPLE-VECTOR where SBCL answers
;;; (SIMPLE-VECTOR 3), and (make-array 4 :element-type 'integer) kept INTEGER as
;;; its element type -- TYPE-OF said (SIMPLE-ARRAY INTEGER (4)), ARRAY-ELEMENT-TYPE
;;; said INTEGER and SIMPLE-VECTOR-P said NIL -- while
;;; (upgraded-array-element-type 'integer) answered T. CLHS 15.1.2.1: the array's
;;; element type is the upgraded one, so the two cannot disagree. array-operations
;;; compares TYPE-OF against '(SIMPLE-VECTOR 4). Expected values are SBCL 2.6.8's
;;; (the fill of a general array is unspecified and left out).

(deftest array-type-of.vector-lengths
  (list (type-of (vector 1 2 3))
        (type-of (make-array 3 :element-type 'bit))
        (type-of (make-array 3 :adjustable t))
        (type-of (make-array 3 :fill-pointer 1)))
  ((simple-vector 3) (simple-bit-vector 3) (vector t 3) (vector t 3)))

(deftest array-type-of.unspecialized-element-types
  (mapcar (lambda (et)
            (let ((a (make-array 4 :element-type et)))
              (list (type-of a) (array-element-type a) (simple-vector-p a))))
          '(integer float number (unsigned-byte 128) (integer 0 *)))
  (((simple-vector 4) t t) ((simple-vector 4) t t) ((simple-vector 4) t t)
   ((simple-vector 4) t t) ((simple-vector 4) t t)))

;; The numeric element types still start out as 0, as they did before.
(deftest array-type-of.integer-array-fill
  (coerce (make-array 3 :element-type 'integer) 'list)
  (0 0 0))

(deftest array-type-of.specialized-element-types
  (mapcar (lambda (et)
            (let ((a (make-array 2 :element-type et)))
              (list (type-of a) (simple-vector-p a))))
          '(fixnum single-float (short-float 0.0 1.0) (unsigned-byte 8)))
  (((simple-array fixnum (2)) nil) ((simple-array single-float (2)) nil)
   ((simple-array single-float (2)) nil) ((simple-array (unsigned-byte 8) (2)) nil)))

(deftest array-type-of.upgraded-array-element-type
  (mapcar #'upgraded-array-element-type
          '(integer float number rational real (unsigned-byte 128) (integer 0 *)
            fixnum signed-byte unsigned-byte (short-float 0.0 1.0)))
  (t t t t t t t fixnum t t single-float))

;; (array X) names the arrays whose element type is X upgraded.
(deftest array-type-of.typep-upgraded
  (list (typep (make-array 3 :element-type 'integer) '(array integer))
        (typep (vector 1) '(array integer))
        (typep (vector 1) '(simple-array float (*)))
        (typep (make-array 2 :element-type 'single-float) '(array float))
        (typep (make-array 2 :element-type '(unsigned-byte 8)) '(array integer))
        (typep (make-array 2 :element-type '(unsigned-byte 8)) '(array (integer 0 255)))
        (typep (make-array 2 :element-type 'fixnum) '(array integer))
        (typep (vector 1) '(array fixnum)))
  (t t t nil nil t nil nil))

;; (simple-vector SIZE) takes the size as its only argument, and the size of a
;; (vector t SIZE) is the dimension, not the fill pointer, so every vector is of
;; the type TYPE-OF gives.
(deftest array-type-of.vector-size-types
  (list (typep (vector 1 2) '(simple-vector 3))
        (typep (vector 1 2 3) '(simple-vector 3))
        (typep (make-array 4 :fill-pointer 2) '(vector t 4))
        (let ((v (make-array 4 :fill-pointer 2))) (typep v (type-of v))))
  (nil t t t))
