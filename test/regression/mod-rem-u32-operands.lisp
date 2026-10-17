;;; MOD and REM of two fixnums that both fit in 32 unsigned bits divide in 32
;;; bits. The answers around that boundary, on both the generic and the
;;; fixnum-typed entries, checked against FLOOR and TRUNCATE.

(defparameter *mru-values*
  (list 0 1 2 7 1000 #x7fffffff #x80000000 #xfffffffe #xffffffff #x100000000
        #x100000001 (expt 2 40) -1 -7 (- #xffffffff) (- (expt 2 32))
        most-positive-fixnum most-negative-fixnum))

(defun %mru-mod (a b) (mod a b))
(defun %mru-rem (a b) (rem a b))
(defun %mru-mod-fx (a b) (declare (fixnum a b)) (mod a b))
(defun %mru-rem-fx (a b) (declare (fixnum a b)) (rem a b))

(defun %mru-bad ()
  (let ((bad '()))
    (dolist (a *mru-values*)
      (dolist (b *mru-values*)
        (unless (zerop b)
          (let ((m (- a (* b (floor a b))))
                (r (- a (* b (truncate a b)))))
            (unless (and (= m (%mru-mod a b) (%mru-mod-fx a b))
                         (= r (%mru-rem a b) (%mru-rem-fx a b)))
              (push (list a b) bad))))))
    bad))

(deftest mod-rem-u32-operands.boundary-matrix
  (%mru-bad)
  nil)

(deftest mod-rem-u32-operands.values
  (list (%mru-mod-fx #xffffffff 1000) (%mru-rem #xffffffff #xfffffffe)
        (%mru-mod -1 #xffffffff) (%mru-rem-fx -1 #xffffffff)
        (%mru-mod #x100000000 #xffffffff))
  (295 1 #xfffffffe -1 1))

(deftest mod-rem-u32-operands.zero-divisor
  (list (handler-case (%mru-mod 5 0) (division-by-zero () :dbz))
        (handler-case (%mru-rem-fx 5 0) (division-by-zero () :dbz)))
  (:dbz :dbz))
