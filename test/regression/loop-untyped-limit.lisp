;;; LOOP counting from a constant towards a limit computed at run time keeps
;;; the counter and the limit in Int64 slots. The limit is evaluated once,
;;; before the loop, into the int64 at which the integer test changes: the
;;; ceiling (below, downto) or the floor (to, above) of a non-integer, the
;;; int64 at that end for a bignum. Before, the counter and the limit were
;;; boxed NUMBERs and every iteration called a generic comparison and 1+.
;;;
;;; What the tests pin: the shape (no generic test or step in the loop), and
;;; the answers for every kind of limit and direction, including the value of
;;; the variable after the loop and that the limit form runs once.

(setf dotcl:*save-sil* t)

(defun %lul-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %lul-xor (n)
  (let ((acc 0)) (declare (fixnum acc))
    (loop for i below n do (setq acc (logxor acc i)))
    acc))
(defun %lul-xor-fx (n)
  (declare (fixnum n))
  (let ((acc 0)) (declare (fixnum acc))
    (loop for i from 0 below n do (setq acc (logxor acc i)))
    acc))
(defun %lul-downto (n)
  (let ((acc 0)) (declare (fixnum acc))
    (loop for i from 100 downto n by 3 do (setq acc (logxor acc i)))
    acc))

(defun %lul-run (kind n)
  (let ((k 0))
    (list (ecase kind
            (:below (loop for i below n do (incf k) finally (return i)))
            (:to (loop for i from 1 to n do (incf k) finally (return i)))
            (:upto-by (loop for i from -3 upto n by 3 do (incf k) finally (return i)))
            (:below-by (loop for i from 2 below n by 4 do (incf k) finally (return i)))
            (:neg-below (loop for i from -10 below n do (incf k) finally (return i)))
            (:downto (loop for i from 10 downto n do (incf k) finally (return i)))
            (:above (loop for i from 10 above n by 2 do (incf k) finally (return i)))
            (:downfrom (loop for i downfrom 5 above n do (incf k) finally (return i))))
          k)))

(defun %lul-once (n)
  (let ((k 0)) (loop for i below (progn (incf k) n)) k))

(deftest-emitting-only loop-untyped-limit.shape
  (mapcar (lambda (fn)
            (let ((s (%lul-sil fn)))
              (list (and (search "IsTrue" s) t)
                    (and (search "Runtime.Increment" s) t)
                    (and (search "Runtime.Decrement" s) t))))
          (list #'%lul-xor #'%lul-xor-fx #'%lul-downto))
  ((nil nil nil) (nil nil nil) (nil nil nil)))

(deftest loop-untyped-limit.xor
  (list (%lul-xor 0) (%lul-xor 10) (%lul-xor -3) (%lul-xor 5/2) (%lul-xor 3.0d0)
        (%lul-xor (- (expt 2 80))) (%lul-xor-fx 10) (%lul-downto 90) (%lul-downto 7/2))
  (0 1 0 3 3 0 1 0 68))

(deftest loop-untyped-limit.up
  (mapcar (lambda (kind)
            (mapcar (lambda (n) (%lul-run kind n))
                    (list 0 5 -1 2.5 7/2 3.0 -2.5 (- (expt 2 80)))))
          '(:below :to :upto-by :below-by :neg-below))
  (((0 0) (5 5) (0 0) (3 3) (4 4) (3 3) (0 0) (0 0))
   ((1 0) (6 5) (1 0) (3 2) (4 3) (4 3) (1 0) (1 0))
   ((3 2) (6 3) (0 1) (3 2) (6 3) (6 3) (0 1) (-3 0))
   ((2 0) (6 1) (2 0) (6 1) (6 1) (6 1) (2 0) (2 0))
   ((0 10) (5 15) (-1 9) (3 13) (4 14) (3 13) (-2 8) (-10 0))))

(deftest loop-untyped-limit.down
  (mapcar (lambda (kind)
            (mapcar (lambda (n) (%lul-run kind n))
                    (list 0 5 -1 2.5 7/2 3.0 -2.5 (expt 2 80))))
          '(:downto :above :downfrom))
  (((-1 11) (4 6) (-2 12) (2 8) (3 7) (2 8) (-3 13) (10 0))
   ((0 5) (4 3) (-2 6) (2 4) (2 4) (2 4) (-4 7) (10 0))
   ((0 5) (5 0) (-1 6) (2 3) (3 2) (3 2) (-3 8) (5 0))))

(deftest loop-untyped-limit.once
  (list (%lul-once 0) (%lul-once 5) (%lul-once 2.5))
  (1 1 1))

(deftest loop-untyped-limit.not-a-number
  (handler-case (progn (%lul-run :below "abc") :no-error)
    (error () :error))
  :error)
