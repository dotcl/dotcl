;;; (SETF (AREF A I...) V) on an array with an integer element type, used as a value (an operand of
;;; LOGXOR, +, a LET init), is the stored integer and takes the typed path: its range is the
;;; element's, so the operation around it is not the generic one. The store is still range
;;; checked, and an array of another kind is left to the generic path.

(setf dotcl:*save-sil* t)

(defun sav-chain (regs blk n)
  (declare (type (simple-array (unsigned-byte 32) (64)) regs)
           (type (simple-array (unsigned-byte 32) (16)) blk) (type fixnum n))
  (dotimes (j n)
    (dotimes (i 16)
      (setf (aref regs (+ i 16)) (logxor (aref blk i) (setf (aref regs (+ i 32)) (aref regs i)))))))
(defun sav-chain-split (regs blk n)
  (declare (type (simple-array (unsigned-byte 32) (64)) regs)
           (type (simple-array (unsigned-byte 32) (16)) blk) (type fixnum n))
  (dotimes (j n)
    (dotimes (i 16)
      (setf (aref regs (+ i 32)) (aref regs i))
      (setf (aref regs (+ i 16)) (logxor (aref blk i) (aref regs (+ i 32)))))))
(defun sav-sum (a b)
  (declare (type (simple-array fixnum (8)) a b))
  (let ((s 0)) (declare (type fixnum s))
    (dotimes (i 8 s) (incf s (+ 1 (setf (aref a i) (* (aref b i) 3)))))))
(defun sav-let (a i v)
  (declare (type (simple-array (unsigned-byte 8) (*)) a) (type fixnum i v))
  (let ((x (setf (aref a i) v))) (+ x x)))
(defun sav-2d (a i j v)
  (declare (type (simple-array (signed-byte 16) (4 4)) a) (type fixnum i j v))
  (logxor 255 (setf (aref a i j) v)))
(defun sav-untyped (a i v) (+ 1 (setf (aref a i) v)))
(defun sav-double (a i)
  (declare (type (simple-array double-float (4)) a) (type fixnum i))
  (setf (aref a i) 2.5d0))

(setf dotcl:*save-sil* nil)

(defun %sav-array (type n &optional (fill 0)) (make-array n :element-type type :initial-element fill))

(deftest setf-aref-as-value.chain-agrees-with-the-split-store
  (let ((bad nil))
    (dolist (seed '(0 1 7 #xFFFFFFFF #x80000000 #x12345678))
      (let ((r1 (%sav-array '(unsigned-byte 32) 64 seed)) (r2 (%sav-array '(unsigned-byte 32) 64 seed))
            (b (%sav-array '(unsigned-byte 32) 16 (logxor seed #xA5A5A5A5))))
        (dotimes (i 64) (setf (aref r1 i) (logand #xFFFFFFFF (* (1+ i) 2654435761))
                              (aref r2 i) (aref r1 i)))
        (sav-chain r1 b 3) (sav-chain-split r2 b 3)
        (unless (equalp r1 r2) (push seed bad))))
    bad)
  nil)

(deftest setf-aref-as-value.the-value-is-the-stored-integer
  (let ((a (%sav-array 'fixnum 8)) (b (make-array 8 :element-type 'fixnum :initial-contents '(1 2 3 4 5 6 7 8))))
    (list (sav-sum a b) (coerce a 'list)))
  (116 (3 6 9 12 15 18 21 24)))

(deftest setf-aref-as-value.let-init-and-two-dimensions
  (let ((u8 (%sav-array '(unsigned-byte 8) 4)) (s16 (make-array '(4 4) :element-type '(signed-byte 16) :initial-element 0)))
    (list (sav-let u8 2 100) (aref u8 2) (sav-2d s16 1 3 -5) (aref s16 1 3) (sav-2d s16 0 0 32767)))
  (200 100 #.(logxor 255 -5) -5 #.(logxor 255 32767)))

(deftest setf-aref-as-value.the-store-is-still-range-checked
  (list (handler-case (sav-let (%sav-array '(unsigned-byte 8) 4) 0 256) (error () :error))
        (handler-case (sav-let (%sav-array '(unsigned-byte 8) 4) 0 -1) (error () :error))
        (handler-case (sav-let (%sav-array '(unsigned-byte 8) 4) 4 1) (error () :error))
        (handler-case (sav-2d (make-array '(4 4) :element-type '(signed-byte 16)) 0 0 32768) (error () :error)))
  (:error :error :error :error))

(deftest setf-aref-as-value.other-arrays-are-left-alone
  (list (sav-untyped (vector 0 0 0) 1 41) (sav-untyped (make-array 3 :initial-element nil) 0 5)
        (sav-double (make-array 4 :element-type 'double-float) 1)
        (handler-case (sav-untyped (list 1 2) 0 1) (error () :error)))
  (42 6 2.5d0 :error))

(deftest-emitting-only setf-aref-as-value.chain-has-no-generic-xor
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "Runtime.Logxor2" #'sav-chain) (has "Runtime.ArefSetL" #'sav-chain)))
  (nil nil))
