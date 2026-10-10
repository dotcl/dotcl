;;; A function proclaimed INLINE whose result form is provably an integer in a
;;; range -- ironclad's UB16REF/LE, a DPB of two octets, and UB32REF/BE, a DPB
;;; of two of those -- is known to return that range where a call of it is
;;; substituted, as if it had an FTYPE. Before, a word read with such a reader
;;; was boxed at the end of the substituted body, the DPB around it went
;;; through the generic LOGAND / ASH / LOGIOR, and a LET variable declared
;;; (UNSIGNED-BYTE 32) bound from it kept a boxed slot.

(setf dotcl:*save-sil* t)

(defun %irr-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %irr-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(declaim (inline %irr-u16 %irr-u32))
(defun %irr-u16 (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v)
           (type (integer 0 1000000) i))
  (dpb (aref v i) (byte 8 8) (aref v (1+ i))))
(defun %irr-u32 (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v)
           (type (integer 0 1000000) i))
  (dpb (%irr-u16 v i) (byte 16 16) (%irr-u16 v (+ i 2))))

(defun %irr-mix (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  (let ((a (%irr-u32 v 0)) (b (%irr-u32 v 4)))
    (declare (type (unsigned-byte 32) a b))
    (dotimes (k 3)
      (setf a (logxor a b)
            b (logand (+ b a) #xffffffff)))
    (list a b)))

(defun %irr-product (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  (* (%irr-u16 v 0) (%irr-u16 v 2)))

(defparameter *irr-v*
  (make-array 8 :element-type '(unsigned-byte 8)
                :initial-contents '(#xde #xad #xbe #xef #x01 #x23 #x45 #x67)))

(deftest inline-return-range.values
  (list (%irr-u16 *irr-v* 0) (%irr-u32 *irr-v* 0) (%irr-u32 *irr-v* 4)
        (%irr-mix *irr-v*) (%irr-product *irr-v*))
  (#xdead #xdeadbeef #x01234567
   (#x20d24731 #x40c14387)
   #.(* #xdead #xbeef)))

;; The words are not boxed: no generic bit operation, and the two LET variables
;; are boxed only to build the returned list (once in each copy of the body).
(deftest-emitting-only inline-return-range.raw
  (let ((s (%irr-sil #'%irr-mix)))
    (list (%irr-count "Runtime.Logior" s)
          (%irr-count "Runtime.Logand" s)
          (%irr-count "Runtime.Ash" s)
          (<= (%irr-count "Fixnum.Make" s) 4)))
  (0 0 0 t))

;; A call that stays a call is not trusted: NOTINLINE here, and the value is
;; still right.
(defun %irr-notinline (v)
  (declare (notinline %irr-u32))
  (let ((a (%irr-u32 v 0)))
    (declare (type (unsigned-byte 32) a))
    (logxor a 1)))

(deftest inline-return-range.notinline
  (%irr-notinline *irr-v*)
  #xdeadbeee)

;; An inline function that returns from its BLOCK early has no single result
;; form, so nothing is recorded; the answer is the same.
(declaim (inline %irr-early))
(defun %irr-early (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type (integer 0 100) i))
  (when (zerop (aref v i)) (return-from %irr-early :zero))
  (dpb (aref v i) (byte 8 8) 0))

(deftest inline-return-range.early-return
  (let ((z (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(0 3))))
    (list (%irr-early z 0) (%irr-early z 1)))
  (:zero #x300))
