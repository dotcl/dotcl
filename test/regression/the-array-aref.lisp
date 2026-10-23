;;; (AREF (THE <numeric array type> FORM) I...): the element is read on the
;;; unboxed path, as for a declared local, instead of being boxed and unboxed.
;;; ironclad's Streebog reads its round constants as
;;; (aref (the (simple-array (unsigned-byte 64) (8)) (aref +streebog-c+ i)) k).

(setf dotcl:*save-sil* t)

(defconstant +tar-c+
  (vector (make-array 4 :element-type '(unsigned-byte 64)
                        :initial-contents '(1 #x8000000000000000 #xFFFFFFFFFFFFFFFF 7))
          (make-array 4 :element-type '(unsigned-byte 64)
                        :initial-contents '(#x0123456789ABCDEF 2 3 #xFEDCBA9876543210))))
(defparameter *tar-u32*
  (make-array '(2 2) :element-type '(unsigned-byte 32)
                     :initial-contents '((#xFFFFFFFF 1) (2 #x80000000))))

(defun %tar-round (x i)
  (declare (type (simple-array (unsigned-byte 64) (4)) x)
           (optimize (speed 3) (safety 0)))
  (let ((r0 (logxor (aref x 0) (aref (the (simple-array (unsigned-byte 64) (4)) (aref +tar-c+ i)) 0)))
        (r1 (logxor (aref x 1) (aref (the (simple-array (unsigned-byte 64) (4)) (aref +tar-c+ i)) 1))))
    (declare (type (unsigned-byte 64) r0 r1))
    (setf (aref x 2) (logxor r0 (ash r1 -1)))
    (coerce x 'list)))

(defun %tar-u32 (i j)
  (+ (aref (the (simple-array (unsigned-byte 32) (2 2)) *tar-u32*) i j) 1))

(defun %tar-generic (x)
  (aref (the (simple-array (unsigned-byte 64) (*)) x) 1))

(deftest the-array-aref.values
  (list (%tar-round (make-array 4 :element-type '(unsigned-byte 64) :initial-contents '(3 4 5 6)) 0)
        (%tar-round (make-array 4 :element-type '(unsigned-byte 64)
                                  :initial-contents '(#xFFFFFFFFFFFFFFFF 4 5 6))
                    1)
        (%tar-u32 0 0) (%tar-u32 1 1)
        (%tar-generic (aref +tar-c+ 0)))
  ((3 4 4611686018427387904 6) (18446744073709551615 4 18364758544493064723 6)
   4294967296 2147483649 9223372036854775808))

(deftest-emitting-only the-array-aref.unboxed
  (let ((d (%flat-text (dotcl:function-sil #'%tar-round))))
    (list (search "Runtime.ArefL" d) (search "Runtime.BoxU64" d)))
  (nil nil))
