;;; (setf (ldb (byte SIZE POS) place) v) with literal SIZE and POS keeps the byte
;;; specifier in the expansion, so the DPB is plain integer arithmetic (no
;;; boxing, no consing of the byte specifier). Results match the general path.

(defun %slb-u64 (a v)
  (declare (type (simple-array (unsigned-byte 64) (2)) a) (type (unsigned-byte 32) v))
  (setf (ldb (byte 32 0) (aref a 1)) v)
  (aref a 1))

(deftest setf-ldb-literal.u64-element
  (let ((a (make-array 2 :element-type '(unsigned-byte 64)
                         :initial-contents '(0 #xFFFFFFFFFFFFFFFF))))
    (list (%slb-u64 a #x12345678)
          (%slb-u64 a 0)))
  (#xFFFFFFFF12345678 #xFFFFFFFF00000000))

(deftest setf-ldb-literal.boxed-sil
  (let ((sil (%flat-text (dotcl:function-sil #'%slb-u64))))
    (list (search "Runtime.Dpb" sil) (search "MakeCons" sil)))
  (nil nil))

(deftest setf-ldb-literal.variable-negative
  (let ((x -1)) (setf (ldb (byte 8 4) x) #x5a) x)
  -2641)

(deftest setf-ldb-literal.variable-bignum
  (let ((x (ash 1 100))) (setf (ldb (byte 8 4) x) 255) x)
  1267650600228229401496703209456)

(deftest setf-ldb-literal.value-and-order
  (let ((log '()) (x 0))
    (list (setf (ldb (byte 4 4) x) (progn (push :v log) #x1ff))
          x (reverse log)))
  (511 240 (:v)))

(deftest setf-ldb-literal.aref-place-evaluated-once
  (let ((a (make-array 3 :initial-element 0)) (n 0))
    (setf (ldb (byte 8 8) (aref a (incf n))) #xab)
    (list n (coerce a 'list)))
  (1 (0 43776 0)))

(deftest setf-ldb-literal.incf-field
  (let ((x #xf0)) (incf (ldb (byte 4 4) x) 1) x)
  #x00)

(deftest setf-ldb-literal.nonliteral-spec-still-works
  (let ((x 0) (bs (byte 4 8))) (setf (ldb bs x) 15) x)
  3840)

(deftest setf-ldb-literal.past-64-bits-generic
  (let ((x 0)) (setf (ldb (byte 8 60) x) 255) x)
  293994983674745978880)
