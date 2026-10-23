;;; A constant array read through its hoisted element buffer has no per-read
;;; fallback: the buffer is checked to be there once, where it is fetched. A
;;; subscript outside the array is still an error.

(setf dotcl:*save-sil* t)

(declaim (type (simple-array (unsigned-byte 32) (256)) +canf-tab+))
(defconstant +canf-tab+
  (if (boundp '+canf-tab+)
      (symbol-value '+canf-tab+)
      (let ((a (make-array 256 :element-type '(unsigned-byte 32))))
        (dotimes (i 256 a) (setf (aref a i) (logand #xFFFFFFFF (* (1+ i) 2654435761)))))))

;; The AES round shape: four bytes of a word, each into the table.
(defun %canf-mix (x)
  (declare (type (unsigned-byte 32) x) (optimize speed))
  (logxor (aref +canf-tab+ (ldb (byte 8 24) x))
          (aref +canf-tab+ (ldb (byte 8 16) x))
          (aref +canf-tab+ (ldb (byte 8 8) x))
          (aref +canf-tab+ (logand x 255))))

(defun %canf-sum (i)
  (declare (fixnum i))
  (let ((s 0))
    (dotimes (k 4 s) (incf s (aref +canf-tab+ (+ i k))))))

(deftest constant-array-no-fallback.values
  (list (multiple-value-list (%canf-mix #x12345678)) (%canf-mix 0)
        (%canf-sum 0) (%canf-sum 252)
        (handler-case (%canf-sum 253) (error () :error))
        (handler-case (%canf-sum -1) (error () :error)))
  ((1937827336) 0 9364488426 9271110106 :error :error))

(deftest-compiled-only constant-array-no-fallback.shape
  (let ((a (%flat-text (dotcl:function-sil #'%canf-mix)))
        (b (%flat-text (dotcl:function-sil #'%canf-sum))))
    (list (and (search "Runtime.ArefNumL" a) t) (and (search "(LDELEM-I8)" a) t)
          (and (search "Runtime.ArefNumL" b) t) (and (search "(LDELEM-I8)" b) t)))
  (nil t nil t))
