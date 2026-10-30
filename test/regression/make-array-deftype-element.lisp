;;; (MAKE-ARRAY N :ELEMENT-TYPE 'NAME) with NAME a DEFTYPE known when the call is
;;; compiled uses the expansion: a direct constructor when the expansion has
;;; storage of its own. The element type is the one the generic call gives.

(setf dotcl:*save-sil* t)

(deftype %madt-lane () '(unsigned-byte 64))
(deftype %madt-lane2 () '%madt-lane)
(deftype %madt-small () '(integer -5 5))
(deftype %madt-sym () 'symbol)

(defun %madt-lane-arr () (make-array 25 :element-type '%madt-lane :initial-element 0))
(defun %madt-lane2-arr () (make-array 3 :element-type '%madt-lane2))
(defun %madt-small-arr () (make-array 2 :element-type '%madt-small :initial-element -5))
(defun %madt-sym-arr () (make-array 2 :element-type '%madt-sym :initial-element 'a))
(defun %madt-dims (d) (make-array d :element-type '%madt-lane :initial-element 7))
(defun %madt-generic (n type) (make-array n :element-type type))

(deftest make-array-deftype-element.values
  (list (array-element-type (%madt-lane-arr)) (length (%madt-lane-arr))
        (array-element-type (%madt-lane2-arr))
        (array-element-type (%madt-small-arr)) (aref (%madt-small-arr) 1)
        (array-element-type (%madt-sym-arr)) (aref (%madt-sym-arr) 0)
        (let ((a (%madt-dims '(2 2)))) (list (array-element-type a) (aref a 1 1)))
        (equal (array-element-type (%madt-small-arr))
               (array-element-type (%madt-generic 1 '%madt-small))))
  ((unsigned-byte 64) 25 (unsigned-byte 64) (signed-byte 8) -5 t a ((unsigned-byte 64) 7) t))

(deftest-compiled-only make-array-deftype-element.direct
  (list (and (search "MakeArrayOfTypeInit" (%flat-text (dotcl:function-sil #'%madt-lane-arr))) t)
        (and (search "MakeArrayOfType" (%flat-text (dotcl:function-sil #'%madt-lane2-arr))) t))
  (t t))
