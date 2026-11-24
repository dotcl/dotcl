;;; Words held in locals across a multi-pass hash round: LET variables read from
;;; (unsigned-byte 64) structure slots and updated by modular arithmetic stay
;;; raw words when the bytes of a word are taken by an INLINE function whose
;;; parameter is declared UNSIGNED-BYTE (as Tiger's tables are indexed), instead
;;; of boxing the word at every read.

(setf dotcl:*save-sil* t)

(declaim (inline %iwl-byte0 %iwl-byte1 %iwl-byte2))
(declaim (ftype (function (unsigned-byte) (unsigned-byte 8)) %iwl-byte0 %iwl-byte1 %iwl-byte2))
(defun %iwl-byte0 (ub) (declare (type unsigned-byte ub)) (ldb (byte 8 0) ub))
(defun %iwl-byte1 (ub) (declare (type unsigned-byte ub)) (ldb (byte 8 8) ub))
(defun %iwl-byte2 (ub) (declare (type unsigned-byte ub)) (ldb (byte 8 16) ub))

(defstruct (%iwl-regs (:constructor %iwl-make-regs ()))
  (a 1 :type (unsigned-byte 64))
  (b 2 :type (unsigned-byte 64))
  (c 3 :type (unsigned-byte 64)))

(defvar *%iwl-table*
  (let ((v (make-array 256 :element-type '(unsigned-byte 64))))
    (dotimes (i 256 v) (setf (aref v i) (ldb (byte 64 0) (* (1+ i) 6364136223846793005))))))
(declaim (type (simple-array (unsigned-byte 64) (256)) *%iwl-table*))

(defun %iwl-passes (regs)
  (declare (type %iwl-regs regs) (optimize (speed 3) (safety 0)))
  (let ((a (%iwl-regs-a regs)) (b (%iwl-regs-b regs)) (c (%iwl-regs-c regs))
        (tbl *%iwl-table*))
    (declare (type (simple-array (unsigned-byte 64) (256)) tbl))
    (macrolet ((rnd (a b c)
                 `(setf ,c (logxor ,c ,a)
                        ,a (ldb (byte 64 0) (- ,a (logxor (aref tbl (%iwl-byte0 ,c))
                                                          (aref tbl (%iwl-byte1 ,c)))))
                        ,b (ldb (byte 64 0) (+ ,b (aref tbl (%iwl-byte2 ,c)))))))
      (rnd a b c) (rnd b c a) (rnd c a b)
      (rnd a b c) (rnd b c a) (rnd c a b))
    (setf (%iwl-regs-a regs) a (%iwl-regs-b regs) b (%iwl-regs-c regs) c)
    regs))

(defun %iwl-reference (a b c)
  (flet ((byte-of (x k) (ldb (byte 8 (* 8 k)) x))
         (m (x) (ldb (byte 64 0) x)))
    (macrolet ((rnd (a b c)
                 `(setf ,c (logxor ,c ,a)
                        ,a (m (- ,a (logxor (aref *%iwl-table* (byte-of ,c 0))
                                            (aref *%iwl-table* (byte-of ,c 1)))))
                        ,b (m (+ ,b (aref *%iwl-table* (byte-of ,c 2)))))))
      (rnd a b c) (rnd b c a) (rnd c a b)
      (rnd a b c) (rnd b c a) (rnd c a b))
    (list a b c)))

(deftest inline-word-lane.same-result
  (let ((regs (%iwl-make-regs)))
    (setf (%iwl-regs-a regs) #x0123456789abcdef
          (%iwl-regs-b regs) #xfedcba9876543210
          (%iwl-regs-c regs) #xf096a5b4c3b2e187)
    (let ((expected (%iwl-reference #x0123456789abcdef #xfedcba9876543210 #xf096a5b4c3b2e187)))
      (%iwl-passes regs)
      (equal expected (list (%iwl-regs-a regs) (%iwl-regs-b regs) (%iwl-regs-c regs)))))
  t)

(defun %iwl-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(deftest-compiled-only inline-word-lane.no-boxing
  (%iwl-count "BoxU64" (%flat-text (dotcl:function-sil #'%iwl-passes)))
  0)

(deftest inline-word-lane.diagnostic-kept
  ;; The parameter's declared type still applies to a call that is not substituted.
  (handler-case (funcall '%iwl-byte0 'not-an-integer)
    (error () :error))
  :error)
