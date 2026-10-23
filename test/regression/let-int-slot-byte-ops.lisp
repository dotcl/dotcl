;;; An undeclared LET variable whose init has a known integer range gets a raw
;;; Int64 slot when the body reads it as an integer operand. LDB, DPB,
;;; MASK-FIELD, DEPOSIT-FIELD and the two-argument LOGANDC1 .. LOGEQV count as
;;; such reads: with a literal byte specifier they are compiled as masks and
;;; shifts of the raw value. A variable read only through them used to stay
;;; boxed, a Fixnum made per binding (ironclad's DES key schedule).

(setf dotcl:*save-sil* t)

(defun %lsb-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %lsb-mask (v i)
  (declare (type (simple-array (unsigned-byte 32) (*)) v) (fixnum i))
  (let ((d (aref v i)))
    (logior (ash (mask-field (byte 6 18) d) 6)
            (ash (mask-field (byte 6 6) d) -6))))

(defun %lsb-ldb (v i)
  (declare (type (simple-array (unsigned-byte 32) (*)) v) (fixnum i))
  (let ((d (aref v i)))
    (list (ldb (byte 8 24) d) (dpb 5 (byte 4 4) d)
          (deposit-field 0 (byte 8 8) d))))

(defun %lsb-complement (v i)
  (declare (type (simple-array (unsigned-byte 32) (*)) v) (fixnum i))
  (let ((a (aref v i)) (b (aref v (1+ i))))
    (list (logandc1 a b) (logandc2 a b) (logorc1 a b) (logorc2 a b)
          (lognand a b) (lognor a b) (logeqv a b))))

(defparameter *lsb-v*
  (make-array 4 :element-type '(unsigned-byte 32)
                :initial-contents '(#xFFFFFFFF #x00FC0FC0 #x80000001 0)))

(deftest let-int-slot-byte-ops.values
  (list (%lsb-mask *lsb-v* 0) (%lsb-mask *lsb-v* 1) (%lsb-mask *lsb-v* 3)
        (%lsb-ldb *lsb-v* 0) (%lsb-ldb *lsb-v* 2)
        (equal (%lsb-complement *lsb-v* 0)
               (let ((a #xFFFFFFFF) (b #x00FC0FC0))
                 (list (funcall #'logandc1 a b) (funcall #'logandc2 a b)
                       (funcall #'logorc1 a b) (funcall #'logorc2 a b)
                       (funcall #'lognand a b) (funcall #'lognor a b)
                       (funcall #'logeqv a b))))
        (equal (%lsb-complement *lsb-v* 2)
               (let ((a #x80000001) (b 0))
                 (list (funcall #'logandc1 a b) (funcall #'logandc2 a b)
                       (funcall #'logorc1 a b) (funcall #'logorc2 a b)
                       (funcall #'lognand a b) (funcall #'lognor a b)
                       (funcall #'logeqv a b)))))
  (#x3F00003F #x3F00003F 0
   (#xFF #xFFFFFF5F #xFFFF00FF) (#x80 #x80000051 #x80000001)
   t t))

;; The variable is a raw slot.
(defun %lsb-local-type (fn prefix)
  (dolist (i (dotcl:function-sil fn))
    (when (and (consp i) (eq (car i) :declare-local)
               (let ((n (symbol-name (second i))))
                 (and (> (length n) (length prefix))
                      (string= prefix n :end2 (length prefix)))))
      (return (third i)))))

(deftest-emitting-only let-int-slot-byte-ops.raw-slot
  (list (%lsb-local-type #'%lsb-mask "D_")
        (%lsb-local-type #'%lsb-ldb "D_")
        (%lsb-local-type #'%lsb-complement "A_"))
  ("Int64" "Int64" "Int64"))
