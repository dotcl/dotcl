;;; Registers of an unrolled hash round: LET variables assigned (MOD32+-style)
;;; from local functions declared INLINE (ironclad's SHA-256 shape).

(setf dotcl:*save-sil* t)

(declaim (inline %lir-m32+ %lir-r32))
(defun %lir-m32+ (a b)
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (+ a b)))
(defun %lir-r32 (a s)
  (declare (type (unsigned-byte 32) a) (type (integer 0 32) s))
  (logior (ldb (byte 32 0) (ash a s)) (ash a (- s 32))))

(defun %lir-rounds (v)
  (declare (type (simple-array (unsigned-byte 32) (8)) v))
  (let ((%lir-a (aref v 0)) (%lir-b (aref v 1)) (%lir-c (aref v 2)))
    (flet ((ch (%lir-x y z) (logxor z (logand %lir-x (logxor y z))))
           (s0 (x) (logxor (%lir-r32 x 30) (%lir-r32 x 19) (%lir-r32 x 10))))
      (declare (inline ch s0))
      (macrolet ((rnd (a b c)
                   `(setf ,a (%lir-m32+ (ch ,a ,b ,c) ,b)
                          ,b (%lir-m32+ (s0 ,a) ,c))))
        (rnd %lir-a %lir-b %lir-c)
        (rnd %lir-c %lir-a %lir-b)
        (rnd %lir-b %lir-c %lir-a)
        (rnd %lir-a %lir-b %lir-c)
        (rnd %lir-c %lir-a %lir-b)
        (rnd %lir-b %lir-c %lir-a))
      (setf (aref v 0) %lir-a (aref v 1) %lir-b (aref v 2) %lir-c)
      v)))

(defun %lir-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (if q (progn (incf k) (setq p (1+ q))) (return k))))))

(defun %lir-local-types (fn prefix)
  "The types of the IL locals of FN whose names start with PREFIX."
  (let ((types '()))
    (dolist (i (dotcl:function-sil fn) (remove-duplicates types :test #'equal))
      (when (and (consp i) (eq (car i) :declare-local)
                 (symbolp (cadr i))
                 (eql 0 (search prefix (symbol-name (cadr i)))))
        (push (caddr i) types)))))

;;; Values from SBCL 2.6.8.
(deftest local-inline-int-registers.value
  (coerce (%lir-rounds (make-array 8 :element-type '(unsigned-byte 32) :initial-element 7))
          'list)
  (3101316452 3970654764 1084187590 7 7 7 7 7))

(deftest-compiled-only local-inline-int-registers.raw-registers
  (%lir-local-types #'%lir-rounds "%LIR-A")
  ("Int64"))

(deftest-compiled-only local-inline-int-registers.raw-parameters
  (%lir-local-types #'%lir-rounds "%LIR-X")
  ("Int64"))

(deftest-compiled-only local-inline-int-registers.no-boxes
  (list (%lir-sil-count #'%lir-rounds "Fixnum.Make")
        (%lir-sil-count #'%lir-rounds "LispFunction.Invoke"))
  (0 0))

(setf dotcl:*save-sil* nil)
