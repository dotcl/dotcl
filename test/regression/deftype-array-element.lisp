;;; An array declared with an element type named by a DEFTYPE --
;;; (simple-array octet (*)), (simple-array lane (25)) -- is the array its
;;; expansion names, and reads and writes through it behave the same, including
;;; a declaration the argument does not satisfy.

(deftype %dae-octet () '(unsigned-byte 8))
(deftype %dae-lane () '(unsigned-byte 64))
(deftype %dae-state () '(simple-array %dae-lane (5)))
(deftype %dae-ub (n) `(unsigned-byte ,n))
(deftype %dae-real () 'double-float)

(defun %dae-sum (v)
  (declare (type (simple-array %dae-octet (*)) v))
  (let ((s 0))
    (dotimes (i (length v) s) (incf s (aref v i)))))

(defun %dae-xor (st)
  (declare (type %dae-state st))
  (logxor (aref st 0) (aref st 4)))

(defun %dae-set (st i x)
  (declare (type %dae-state st))
  (setf (aref st i) x))

(defun %dae-param (v i)
  (declare (type (simple-array (%dae-ub 16) (*)) v))
  (aref v i))

(defun %dae-float (v)
  (declare (type (simple-array %dae-real (*)) v))
  (+ (aref v 0) (aref v 1)))

(deftest deftype-array-element.values
  (let ((o (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 250)))
        (st (make-array 5 :element-type '(unsigned-byte 64)
                          :initial-contents '(#xFFFFFFFFFFFFFFFF 0 0 0 #x8000000000000001)))
        (w (make-array 2 :element-type '(unsigned-byte 16) :initial-contents '(7 65535)))
        (d (make-array 2 :element-type 'double-float :initial-contents '(1.5d0 2.25d0))))
    (%dae-set st 1 #x8000000000000000)
    (list (%dae-sum o) (%dae-xor st) (aref st 1) (%dae-param w 1) (%dae-float d)))
  (256 #x7FFFFFFFFFFFFFFE #x8000000000000000 65535 3.75d0))

(deftest deftype-array-element.wrong-array
  (list (handler-case (progn (%dae-sum (make-array 2 :initial-element 1)) :no-error)
          (type-error () :type-error))
        (handler-case (progn (%dae-xor (make-array 5 :element-type '(unsigned-byte 8))) :no-error)
          (type-error () :type-error))
        (handler-case (progn (%dae-set (make-array 5 :element-type '(unsigned-byte 64)) 0 -1) :no-error)
          (error () :error)))
  (:type-error :type-error :error))
