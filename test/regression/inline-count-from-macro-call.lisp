;;; An INLINE function whose shift-count parameter receives a macro call that
;;; expands to a constant (the shape of ironclad's ROL64 called with
;;; GET-KECCAK-ROTATE-OFFSET) shifts by a constant: the word stays in the lane,
;;; no generic Runtime.Ash / Logand2 / Logxor2 on the compiled path.

(setf dotcl:*save-sil* t)

(declaim (inline icm-rol64))
(defun icm-rol64 (a s)
  (declare (type (unsigned-byte 64) a) (type (integer 0 64) s))
  (logior (ldb (byte 64 0) (ash a s)) (ash a (- s 64))))

(defmacro icm-offset (x y)
  (+ (* 3 x) y))

(defun icm-use (x)
  (declare (type (unsigned-byte 64) x) (optimize (speed 3) (safety 0)))
  (logand #xFFFFFF
          (logxor x (icm-rol64 x (icm-offset 3 4)) (icm-rol64 x (icm-offset 0 1)))))

(defun icm-plain (x)
  (flet ((rol (a s) (logior (ldb (byte 64 0) (ash a s)) (ash a (- s 64)))))
    (logand #xFFFFFF (logxor x (rol x 13) (rol x 1)))))

(deftest-emitting-only inline-count-from-macro-call.no-variable-shift
  (let ((sil (dotcl:function-sil #'icm-use)))
    (count-if (lambda (i) (and (consp i) (stringp (cadr i))
                               (or (search "ShlU64" (cadr i)) (search "ShrU64" (cadr i)))))
              sil))
  0)

(deftest inline-count-from-macro-call.value
  (loop for x in '(0 1 255 #x123456789abcdef #xFFFFFFFFFFFFFFFF)
        always (= (icm-use x) (icm-plain x)))
  t)
