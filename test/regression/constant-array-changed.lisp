;;; A DEFCONSTANT array read in a loop is fetched once, with no per-read
;;; fallback. If the constant's value is later replaced by an array whose
;;; storage the fetch declines (a pinned foreign-width vector), the fetch
;;; answers null and the code signals a TYPE-ERROR naming the constant
;;; (Runtime.ConstantArrayChanged) instead of reading through a null buffer.

(setf dotcl:*save-sil* t)

(defconstant +cac-fix+
  (make-array 4 :element-type '(signed-byte 64) :initial-contents '(1 2 3 4)))
(defconstant +cac-s32+
  (make-array 4 :element-type '(signed-byte 32) :initial-contents '(5 6 7 8)))

(defun %cac-fix (n)
  (declare (fixnum n))
  (let ((s 0)) (dotimes (i n) (incf s (aref +cac-fix+ (logand i 3)))) s))
(defun %cac-s32 (n)
  (declare (fixnum n))
  (let ((s 0)) (dotimes (i n) (incf s (aref +cac-s32+ (logand i 3)))) s))

(defun %cac-te (thunk)
  (handler-case (funcall thunk)
    (type-error (e) (list :type-error (type-error-datum e)))
    (error () :error)))

(deftest-emitting-only constant-array-changed.shape
  (flet ((n (fn) (plusp (search "Runtime.ConstantArrayChanged"
                                (%flat-text (dotcl:function-sil fn))))))
    (list (n #'%cac-fix) (n #'%cac-s32)))
  (t t))

(deftest-emitting-only constant-array-changed.unchanged
  (list (%cac-fix 8) (%cac-s32 6))
  (20 37))

(deftest-emitting-only constant-array-changed.signals-when-replaced
  (progn
    (setf (symbol-value '+cac-fix+) (dotcl:make-pinned-vector 4 '(unsigned-byte 32)))
    (setf (symbol-value '+cac-s32+) (dotcl:make-pinned-vector 4 '(signed-byte 8)))
    (list (car (%cac-te (lambda () (%cac-fix 8))))
          (car (%cac-te (lambda () (%cac-s32 8))))
          (let ((c (handler-case (%cac-fix 2) (type-error (e) e))))
            (and (typep c 'type-error) (eq (type-error-datum c) '+cac-fix+)))))
  (:type-error :type-error t))
