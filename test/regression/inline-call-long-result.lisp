;;; A call to an INLINE function whose FTYPE promises an integer that fits int64
;;; is compiled with its last form left as a raw int64 where the caller wants
;;; one, instead of boxing the result of the expansion's BLOCK only to unbox it
;;; again. (mod32+ a b) and (rol32 x 7) in ironclad's Salsa20 core allocated a
;;; Fixnum each for that.

(setf dotcl:*save-sil* t)

(defun %icl-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %icl-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(declaim (inline %icl-add32)
         (ftype (function ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32))
                %icl-add32))
(defun %icl-add32 (a b)
  (declare (type (unsigned-byte 32) a b))
  (logand (+ a b) #xffffffff))

(declaim (inline %icl-rol32)
         (ftype (function ((unsigned-byte 32) (integer 0 32)) (unsigned-byte 32))
                %icl-rol32))
(defun %icl-rol32 (a s)
  (declare (type (unsigned-byte 32) a) (type (integer 0 32) s))
  (logior (logand (ash a s) #xffffffff) (ash a (- s 32))))

;; A body that returns from its own block: it cannot be compiled with its last
;; form as the value, and still has to give the right answer.
(declaim (inline %icl-clamp)
         (ftype (function (fixnum) (unsigned-byte 8)) %icl-clamp))
(defun %icl-clamp (x)
  (declare (fixnum x))
  (when (< x 0) (return-from %icl-clamp 0))
  (min x 255))

(defun %icl-step (v)
  (declare (type (simple-array (unsigned-byte 32) (*)) v))
  (setf (aref v 0)
        (logxor (aref v 0) (%icl-rol32 (%icl-add32 (aref v 1) (aref v 2)) 7)))
  (aref v 0))

(defun %icl-clamped (v x)
  (declare (type (simple-array (unsigned-byte 32) (*)) v) (fixnum x))
  (aref v (%icl-clamp x)))

(defun %icl-vec (&rest xs)
  (make-array (length xs) :element-type '(unsigned-byte 32) :initial-contents xs))

(defparameter *icl-big* (let ((v (make-array 256 :element-type '(unsigned-byte 32))))
                          (dotimes (i 256 v) (setf (aref v i) (* i 2)))))

(deftest inline-call-long-result.values
  (list (%icl-step (%icl-vec 0 1 2))
        (%icl-step (%icl-vec #xffffffff #xffffffff 1))
        (%icl-clamped *icl-big* -5)
        (%icl-clamped *icl-big* 9)
        (%icl-clamped *icl-big* 1000))
  (#x180 #xffffffff 0 18 510))

;; The two inline results are not kept in a BLOCK's boxed result slot (BRES):
;; their last forms are computed as int64s.
(deftest-emitting-only inline-call-long-result.no-box
  (let ((d (%icl-sil #'%icl-step)))
    (list (%icl-count "BRES" d)
          (%icl-count "Runtime.UnwrapMv" d)
          (%icl-count "Runtime.Logxor" d)))
  (0 0 0))
