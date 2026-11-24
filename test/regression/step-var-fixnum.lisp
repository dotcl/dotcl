;;; A LET variable that only steps (assigned (+ V k) / (- V k) / INCF / DECF with a constant k within
;;; +-2^20, bound to an init within +-2^40) is an int64 slot, declared FIXNUM: its steps are checked for
;;; overflow. The values are those of the boxed variable; a variable outside those bounds is left as it
;;; was, a bignum when it passes int64. The only difference is a step that leaves int64 from inside
;;; the bounds, which takes more than 2^42 steps: it signals a TYPE-ERROR instead of becoming a bignum.

(setf dotcl:*save-sil* t)

(defun svf-down (p lim k)
  (declare (type (integer -1099511627776 1099511627776) p lim))
  (let ((pos p) (n 0))
    (loop :while (> pos lim) :do (decf pos 2) (incf n))
    (list pos n k)))

(defun svf-fd (q p pos-lim buf)
  (declare (type (signed-byte 32) q p pos-lim) (type (simple-base-string 24) buf))
  (let ((q0 q) (pos p))
    (declare (type (signed-byte 32) q0))
    (loop :while (> pos pos-lim)
          :do (setf (char buf (- pos 1)) (code-char (+ 48 (mod q0 10))))
              (setf q0 (floor q0 10))
              (decf pos 2))
    nil))

;; The same loop with a step that is not a constant: not a step variable, boxed as before.
(defun svf-fd-ref (q p pos-lim buf step)
  (declare (type (signed-byte 32) q p pos-lim) (type (simple-base-string 24) buf))
  (let ((q0 q) (pos p))
    (declare (type (signed-byte 32) q0))
    (loop :while (> pos pos-lim)
          :do (setf (char buf (- pos 1)) (code-char (+ 48 (mod q0 10))))
              (setf q0 (floor q0 10))
              (setq pos (- pos step)))
    nil))

(defun svf-wide (a k)
  (declare (type integer a))
  (let ((v a)) (dotimes (i k) (incf v 3)) v))

(defun svf-big-k (p n)
  (declare (type (integer -1099511627776 1099511627776) p) (fixnum n))
  (let ((v p)) (dotimes (i n) (incf v 1048576)) v))

(defun svf-too-big-k (p n)
  (declare (type (integer -1099511627776 1099511627776) p) (fixnum n))
  (let ((v p)) (dotimes (i n) (incf v 1048577)) v))

(setf dotcl:*save-sil* nil)

(deftest svf.values-equal-the-boxed-version
  (let ((b1 (make-string 24 :element-type 'base-char :initial-element #\-))
        (b2 (make-string 24 :element-type 'base-char :initial-element #\-)))
    (loop for (q p lim) in '((123456789 21 3) (0 21 3) (-5 20 3) (2147483647 23 0) (-2147483648 23 0)
                             (7 3 3) (7 4 3) (7 5 3) (7 2 3) (1 0 0) (1 1 1))
          always (progn (fill b1 #\-) (fill b2 #\-)
                        (and (eql (svf-fd q p lim b1) (svf-fd-ref q p lim b2 2))
                             (string= b1 b2)))))
  t)

(deftest svf.guard-equality-and-negative-ranges
  (list (svf-down 10 10 0) (svf-down 11 10 0) (svf-down 12 10 0) (svf-down -5 -10 0) (svf-down -10 -5 0)
        (svf-down 0 -1 0))
  ((10 0 0) (9 1 0) (10 1 0) (-11 3 0) (-10 0 0) (-2 1 0)))

(deftest svf.bounds-of-init-and-step
  (list (svf-big-k 1099511627776 3) (svf-big-k -1099511627776 3) (svf-big-k 1099511627776 0)
        (svf-too-big-k 5 3))
  (1099514773504 -1099508482048 1099511627776 3145736))

(deftest svf.out-of-bounds-variable-still-becomes-a-bignum
  (list (svf-wide (- (expt 2 63) 2) 2) (svf-wide (- (expt 2 63) 2) 1) (svf-wide (expt 2 70) 2)
        (typep (svf-wide (- (expt 2 63) 2) 2) 'bignum))
  (9223372036854775812 9223372036854775809 1180591620717411303430 t))

(deftest-emitting-only svf.the-steps-are-checked-int64-operations
  (let ((sil (%flat-text (dotcl:function-sil #'svf-fd)))
        (ref (%flat-text (dotcl:function-sil #'svf-fd-ref))))
    (list (and (search "Runtime.IsTrueGt" sil) t)
          (and (search "Runtime.Decrement" sil) t)
          (and (search "Runtime.IsTrueGt" ref) t)))
  (nil nil t))
