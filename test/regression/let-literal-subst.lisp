;;; A LET variable bound to an integer literal and only read through plain
;;; arithmetic / comparison calls is replaced by the literal, and the comparisons
;;; and MIN / MAX of literals are folded. This is the shape a generated
;;; (shift K X) takes: the IF is decided and the ASH has a literal count, so
;;; the surrounding unsigned 32-bit lane stays raw.

(setf dotcl:*save-sil* t)

(defun %lls-xorshift (old)
  (declare (type (unsigned-byte 32) old))
  (let ((a (logand #xFFFFFFFF
                   (logxor old (let ((k 13)) (if (< k 0) (ash old (max k -32)) (ash old (min k 32))))))))
    (declare (type (unsigned-byte 32) a))
    (let ((b (logxor a (let ((k -17)) (if (< k 0) (ash a (max k -32)) (ash a (min k 32)))))))
      (declare (type (unsigned-byte 32) b))
      (let ((c (logand #xFFFFFFFF
                       (logxor b (let ((k 5)) (if (< k 0) (ash b (max k -32)) (ash b (min k 32))))))))
        (declare (type (unsigned-byte 32) c))
        c))))

(defun %lls-xorshift-ref (old)
  (let* ((a (logand #xFFFFFFFF (logxor old (ash old 13))))
         (b (logxor a (ash a -17)))
         (c (logand #xFFFFFFFF (logxor b (ash b 5)))))
    c))

(defun %lls-shadow (x) (let ((k 1)) (+ x (let ((k 2)) k) k)))
(defun %lls-setq (x) (let ((k 3)) (setq k (+ k x)) k))
(defun %lls-closure (x) (let ((k 4)) (funcall (lambda () (+ k x)))))
(defun %lls-two (x) (let ((a 2) (b x)) (+ a b (* a a))))
(defun %lls-ignorable (x) (let ((k 7) (j x)) (declare (ignorable k)) (min j 5)))
(defun %lls-quote (x) (let ((k 9)) (list 'k k x)))
(defun %lls-fold (x) (let ((k 3)) (if (> k 2) (+ x (* k k)) (- x 1))))
(defun %lls-false (x) (let ((k 3)) (if (> k 5) x nil)))
(defvar *%lls-special* 0)
(defun %lls-special (x) (let ((*%lls-special* 11)) (+ x *%lls-special*)))

(deftest let-literal-subst.xorshift-matches-reference
  (let ((ok t))
    (dolist (v '(1 12345 4294967295 2463534242 305419896 0))
      (unless (= (%lls-xorshift v) (%lls-xorshift-ref v)) (setf ok nil)))
    ok)
  t)

(deftest let-literal-subst.semantics
  (list (%lls-shadow 10) (%lls-setq 4) (%lls-closure 1) (%lls-two 5) (%lls-ignorable 8)
        (%lls-quote 2) (%lls-fold 1) (%lls-false 1) (%lls-special 1))
  (13 7 5 11 5 (k 9 2) 10 nil 12))

(defun %lls-sil (fn) (%flat-text (dotcl:function-sil fn)))
(deftest-emitting-only let-literal-subst.lane-stays-raw
  (list (and (search "Logxor2" (%lls-sil #'%lls-xorshift)) t)
        (and (search "Logand2" (%lls-sil #'%lls-xorshift)) t)
        (and (search "(XOR)" (%lls-sil #'%lls-xorshift)) t))
  (nil nil t))
(setf dotcl:*save-sil* nil)
