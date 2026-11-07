;;; (LOGAND X MASK) with a literal non-negative mask on a value of unknown type, (VALUES)
;;; in a single-value position, and LOOP's AND-joined counters with plain 1+ steps.

(defun %lmv-mask (x) (logand x 255))
(defun %lmv-store (v x) (declare (type (simple-array (unsigned-byte 8) (*)) v))
  (setf (aref v 0) (ldb (byte 8 0) (if (> x 0) x (%lmv-mask (- x))))) (values))
(defun %lmv-untyped (f x) (setf (aref (make-array 1 :element-type '(unsigned-byte 8)) 0)
                                (ldb (byte 8 0) (funcall f x))))

(deftest logand-mask-values-zero.mask
  (list (%lmv-mask 300) (%lmv-mask -1) (%lmv-mask (+ (expt 2 70) 5)) (%lmv-mask (- (expt 2 70)))
        (logand 255 (+ (expt 2 64) 77))
        (handler-case (%lmv-mask 1.5) (error () :error))
        (handler-case (%lmv-mask "a") (error () :error))
        (%lmv-untyped #'identity 513) (%lmv-untyped #'identity (expt 10 30))
        (let ((v (make-array 1 :element-type '(unsigned-byte 8))))
          (%lmv-store v 1000) (aref v 0)))
  (44 255 5 0 77 :error :error 1 0 232))

(deftest logand-mask-values-zero.values
  (list (list (values))
        (multiple-value-list (values))
        (let ((x (values))) x)
        (multiple-value-list (progn (values)))
        (multiple-value-list (funcall (lambda () (values))))
        (length (multiple-value-list (let ((y 1)) (declare (ignore y)) (values))))
        (if (values) :true :false)
        (progn (values) 5))
  ((nil) nil nil nil nil 0 :false 5))

(deftest logand-mask-values-zero.counters
  (list (loop for i from 0 below 3 and j from 5 by 2 collect (list i j))
        (loop for i fixnum from 0 below 3 and k fixnum from 10 collect (list i k))
        (loop for a from 0 below 3 and b = 10 then (1+ a) collect (list a b))
        (loop for a from 0 below 3 and b = 10 then (+ b 1) collect (list a b))
        (let ((c 0)) (loop for a from 0 below 3 and b = 10 then (progn (incf c) c) collect (list a b))))
  (((0 5) (1 7) (2 9)) ((0 10) (1 11) (2 12)) ((0 10) (1 1) (2 2)) ((0 10) (1 11) (2 12))
   ((0 10) (1 1) (2 2))))
