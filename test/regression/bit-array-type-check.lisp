;;; Only 0 and 1 are of type BIT, so a bit array has to refuse anything else.
;;; Every store folded "non-zero" to 1 and everything else to 0 and said nothing:
;;; (setf (aref bv 0) 7) left #*100, and :initial-element 'x quietly produced
;;; #*000. SBCL signals a TYPE-ERROR in all four shapes below.

(deftest bit-array-type-check.initial-element
  (list (handler-case (progn (make-array 3 :element-type 'bit :initial-element 5) :no-error)
          (type-error () :type-error) (error () :other))
        (handler-case (progn (make-array 3 :element-type 'bit :initial-element 'x) :no-error)
          (type-error () :type-error) (error () :other))
        (handler-case (progn (make-array 3 :element-type 'bit :initial-element -1) :no-error)
          (type-error () :type-error) (error () :other)))
  (:type-error :type-error :type-error))

(deftest bit-array-type-check.initial-contents
  (handler-case (progn (make-array 3 :element-type 'bit :initial-contents '(0 5 1)) :no-error)
    (type-error () :type-error) (error () :other))
  :type-error)

(deftest bit-array-type-check.store
  (let ((b (make-array 3 :element-type 'bit)))
    (list (handler-case (progn (setf (aref b 0) 7) :no-error)
            (type-error () :type-error) (error () :other))
          (handler-case (progn (setf (aref b 0) nil) :no-error)
            (type-error () :type-error) (error () :other))
          ;; and the array is untouched by the refused stores
          (coerce b 'list)))
  (:type-error :type-error (0 0 0)))

;;; The values that ARE bits still work, through every path.
(deftest bit-array-type-check.valid-values
  (let ((b (make-array 4 :element-type 'bit :initial-element 1))
        (c (make-array 3 :element-type 'bit :initial-contents '(1 0 1)))
        (d (make-array 3 :element-type 'bit)))
    (setf (aref d 1) 1)
    (setf (aref d 1) 0)
    (setf (aref d 2) 1)
    (list (coerce b 'list) (coerce c 'list) (coerce d 'list)
          (bit-and c c) (bit-xor c c)))
  ((1 1 1 1) (1 0 1) (0 0 1) #*101 #*000))

;;; A bit vector with no :initial-element is all zeros, not an error.
(deftest bit-array-type-check.default-is-zero
  (coerce (make-array 3 :element-type 'bit) 'list)
  (0 0 0))
