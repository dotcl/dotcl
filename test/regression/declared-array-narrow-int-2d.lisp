;;; AREF on parameters declared (simple-array (signed-byte 32) (* *)) and
;;; (simple-array (unsigned-byte 16) (* *)) takes the rank-2 backing path with
;;; the int32 / uint16 helpers. No other test reached either helper.

(setf dotcl:*save-sil* t)

(defun %dni-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %dni-sum-s32 (arr)
  (declare (type (simple-array (signed-byte 32) (* *)) arr)
           (optimize (speed 3) (safety 1) (debug 0)))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i (array-dimension arr 0) s)
      (dotimes (j (array-dimension arr 1))
        (incf s (aref arr i j))))))

(defun %dni-sum-u16 (arr)
  (declare (type (simple-array (unsigned-byte 16) (* *)) arr)
           (optimize (speed 3) (safety 1) (debug 0)))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i (array-dimension arr 0) s)
      (dotimes (j (array-dimension arr 1))
        (incf s (aref arr i j))))))

(defparameter *dni-s32*
  (make-array '(2 3) :element-type '(signed-byte 32)
                     :initial-contents '((1 -2 3) (-2147483648 5 2147483647))))

(defparameter *dni-u16*
  (make-array '(2 3) :element-type '(unsigned-byte 16)
                     :initial-contents '((1 2 3) (65535 5 0))))

(deftest declared-array-narrow-int-2d.values
  (list (%dni-sum-s32 *dni-s32*) (%dni-sum-u16 *dni-u16*))
  (6 65546))

(deftest-emitting-only declared-array-narrow-int-2d.uses-the-narrow-backing
  (list (%dni-count "Runtime.BackingNDI32" (%flat-text (dotcl:function-sil #'%dni-sum-s32)))
        (%dni-count "Runtime.BackingNDU16" (%flat-text (dotcl:function-sil #'%dni-sum-u16))))
  (1 1))
