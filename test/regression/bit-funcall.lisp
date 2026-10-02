;;; BIT and SBIT called through their function objects (FUNCALL, APPLY, or a
;;; call the compiler does not open-code) with one subscript take a direct
;;; two-argument entry. These pin that it answers what AREF does, and that
;;; everything else (a displaced or multidimensional bit array, a bad
;;; subscript) still reaches the general path.

(deftest bit-funcall.packed
  (let ((v (make-array 130 :element-type 'bit)))
    (setf (sbit v 0) 1 (sbit v 64) 1 (sbit v 129) 1)
    (list (funcall #'sbit v 0) (funcall #'sbit v 1) (funcall #'sbit v 64)
          (funcall #'bit v 129) (apply #'sbit v '(128))))
  (1 0 1 1 0))

(deftest bit-funcall.general-shapes
  (let* ((base (make-array 8 :element-type 'bit :initial-contents '(0 1 0 1 1 0 0 1)))
         (d (make-array 4 :element-type 'bit :displaced-to base :displaced-index-offset 3))
         (m (make-array '(2 2) :element-type 'bit :initial-contents '((1 0) (0 1)))))
    (list (funcall #'sbit d 0) (funcall #'bit d 1)
          (funcall #'bit m 1 1) (funcall #'bit m 0 1)))
  (1 1 1 0))

(deftest bit-funcall.errors
  (let ((v (make-array 4 :element-type 'bit)))
    (list (handler-case (funcall #'sbit v 4) (error () :error))
          (handler-case (funcall #'sbit v -1) (error () :error))
          (handler-case (funcall #'sbit v 'x) (error () :error))))
  (:error :error :error))
