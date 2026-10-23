;;; The variable of a counting loop gets the integer range its start, limit and
;;; step allow: LOOP's FOR ... FROM / BELOW / TO / ABOVE / DOWNTO / BY, DO with
;;; a (1+ I) / (+ I K) step and a (>= I L) end test, and DOTIMES. With the range
;;; the index arithmetic, (+ I 1) or (* I 16), stays on the typed path without
;;; a declaration. These pin the values that must not change: the value after
;;; the loop (one step past the limit), negative steps, limits at the edge of
;;; the fixnum range and beyond it, and loops whose body assigns the variable
;;; itself, where no range may be assumed.

(setf dotcl:*save-sil* t)

(defun %lcr-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %lcr-generic-arith-p (fn)
  (let ((s (%lcr-sil fn)))
    (and (some (lambda (needle) (search needle s))
               '("Runtime.Add" "Runtime.Increment" "Runtime.Subtract"
                 "Runtime.Decrement" "Runtime.Multiply"))
         t)))

;;; Values after the loop: the first value past the limit (CLHS 6.1.2.1.1).

(deftest loop-counting-range.final-values
  (list (loop for x from 1 to 5 finally (return x))
        (loop for x from 1 below 5 finally (return x))
        (loop for x from 10 downto 0 finally (return x))
        (loop for x from 10 above 0 finally (return x))
        (loop for i from 0 below 16 by 4 finally (return i))
        (loop for i from 0 to 16 by 4 finally (return i))
        (loop for i downfrom 10 above 0 by 4 finally (return i))
        (loop for i from 3 to 3 finally (return i))
        (loop for i from 3 below 3 finally (return i)))
  (6 5 -1 0 16 20 -2 4 3))

(deftest loop-counting-range.sequences
  (list (loop for i from 0 below 16 by 4 collect i)
        (loop for i from 10 downto -10 by 3 collect i)
        (loop for i from 3 to 3 collect i)
        (loop for i from 3 below 3 collect i)
        (loop for i from 0 below 4 and j = 10 then i collect (list i j))
        (loop for x from 1 below 5.01 collect x)
        (loop for x from 1 to 4.0 collect x)
        (loop for x from 1/2 to 3 collect x)
        (loop for i of-type fixnum from 0 below 5 sum i)
        (loop for i of-type (integer 0 5) from 0 below 5 sum i))
  ((0 4 8 12) (10 7 4 1 -2 -5 -8) (3) nil ((0 10) (1 0) (2 1) (3 2))
   (1 2 3 4 5) (1 2 3 4) (1/2 3/2 5/2) 10 10))

;;; The body assigns the variable: the loop goes on from the assigned value,
;;; whatever it is.

(deftest loop-counting-range.body-assigns
  (list (loop for i from 0 below 10 collect i do (when (= i 2) (setq i 7)))
        (loop for i from 0 below 10 do (setq i (expt 2 70)) finally (return i))
        (loop for i below 5 collect i
              do (when (= i 1) (funcall (lambda () (setq i 3)))))
        (loop for i from 0 below 10 collect i do (incf i 3))
        (let ((r nil))
          (do ((i 0 (1+ i))) ((>= i 10) (nreverse r))
            (push i r)
            (when (= i 2) (setq i 7))))
        (let ((r nil))
          (dotimes (i 10 (nreverse r))
            (push i r)
            (when (= i 2) (setq i 7)))))
  ((0 1 2 8 9) #.(1+ (expt 2 70)) (0 1 4) (0 4 8) (0 1 2 8 9) (0 1 2 8 9)))

;;; Limits at and past the edge of the fixnum range. One step past the limit
;;; is a bignum here, so no int64 range may be assumed.

(defun %lcr-do-up (k n)
  (declare (fixnum k n))
  (do ((i k (1+ i))) ((>= i n) i)))

(defun %lcr-do-up-2 (k n)
  (declare (fixnum k n))
  (do ((i k (+ i 2))) ((>= i n) i)))

(defun %lcr-loop-to (k n)
  (declare (fixnum k n))
  (loop for i from k to n finally (return i)))

(defun %lcr-loop-downto (k n)
  (declare (fixnum k n))
  (loop for i from k downto n by 2 finally (return i)))

(deftest loop-counting-range.fixnum-edges
  (list (%lcr-do-up (- most-positive-fixnum 2) most-positive-fixnum)
        (= (%lcr-do-up-2 (- most-positive-fixnum 3) most-positive-fixnum)
           (1+ most-positive-fixnum))
        (= (%lcr-loop-to (- most-positive-fixnum 1) most-positive-fixnum)
           (1+ most-positive-fixnum))
        (= (%lcr-loop-downto (+ most-negative-fixnum 3) most-negative-fixnum)
           (- most-negative-fixnum 1))
        (loop for i from (- most-positive-fixnum 1) to most-positive-fixnum count t)
        (= (loop for i from (- most-positive-fixnum 1) to most-positive-fixnum
                 finally (return i))
           (1+ most-positive-fixnum)))
  (#.most-positive-fixnum t t t 2 t))

(deftest loop-counting-range.bignum-bounds
  (list (loop for i from (expt 2 64) below (+ (expt 2 64) 3) collect (- i (expt 2 64)))
        (loop for i from 0 below (expt 2 70) by (expt 2 68) count t)
        (let ((n (expt 2 70)))
          (do ((i (- n 2) (1+ i))) ((>= i n) (= i n))))
        (let ((c 0)) (dotimes (i (expt 2 70)) (when (= (incf c) 3) (return i)))))
  ((0 1 2) 4 t 2))

;;; DO: other steps read the old value; a limit the loop itself changes.

(deftest loop-counting-range.do-shapes
  (list (do ((i 0 (1+ i)) (acc nil (cons i acc))) ((>= i 3) acc))
        (do ((i 10 (- i 3)) (r nil (cons i r))) ((< i 0) r))
        (do ((i 0 (1+ i)) (n 3 (1+ n))) ((>= i n) :never) (when (> i 10) (return i)))
        (let ((n 3))
          (do ((i 0 (+ 2 i))) ((> i n) i) (setq n (if (< n 6) (+ n 1) n))))
        (let ((s 0)) (dotimes (i 8 s) (incf s (* i 16)))))
  ((2 1 0) (1 4 7 10) 11 8 448))

(defvar *lcr-k*)
(defun %lcr-read-k () *lcr-k*)

(deftest loop-counting-range.special-variable
  (list (loop for *lcr-k* from 0 below 3 collect (%lcr-read-k))
        (let ((r nil))
          (do ((*lcr-k* 0 (1+ *lcr-k*))) ((>= *lcr-k* 3) (nreverse r))
            (push (%lcr-read-k) r))))
  ((0 1 2) (0 1 2)))

;;; The index arithmetic of the counting loops is typed without a declaration.

(defun %lcr-loop-sum (v)
  (declare (type (simple-array fixnum (*)) v))
  (let ((s 0))
    (declare (fixnum s))
    (loop for i from 0 below 128 by 16
          do (setq s (logxor s (aref v i) (aref v (+ i 1)) (aref v (+ i 15)))))
    s))

(defun %lcr-do-sum (v)
  (declare (type (simple-array fixnum (*)) v))
  (let ((s 0))
    (declare (fixnum s))
    (do ((i 0 (+ i 16))) ((>= i 128))
      (setq s (logxor s (aref v i) (aref v (+ i 1)) (aref v (+ i 15)))))
    s))

(defun %lcr-dotimes-sum (v)
  (declare (type (simple-array fixnum (*)) v))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (j 8)
      (setq s (logxor s (aref v (* j 16)) (aref v (+ (* j 16) 15)))))
    s))

(defun %lcr-loop-down (v)
  (declare (type (simple-array fixnum (*)) v))
  (let ((s 0))
    (declare (fixnum s))
    (loop for i from 127 downto 1 by 2
          do (setq s (logxor s (aref v (- i 1)))))
    s))

(defparameter *lcr-v*
  (let ((v (make-array 128 :element-type 'fixnum)))
    (dotimes (i 128 v) (setf (aref v i) (* i i)))))

(deftest loop-counting-range.typed-values
  (list (%lcr-loop-sum *lcr-v*) (%lcr-do-sum *lcr-v*)
        (%lcr-dotimes-sum *lcr-v*) (%lcr-loop-down *lcr-v*))
  #.(let ((v (make-array 128)))
      (dotimes (i 128) (setf (aref v i) (* i i)))
      (list (let ((s 0))
              (loop for i from 0 below 128 by 16
                    do (setq s (logxor s (aref v i) (aref v (+ i 1)) (aref v (+ i 15)))))
              s)
            (let ((s 0))
              (loop for i from 0 below 128 by 16
                    do (setq s (logxor s (aref v i) (aref v (+ i 1)) (aref v (+ i 15)))))
              s)
            (let ((s 0))
              (dotimes (j 8) (setq s (logxor s (aref v (* j 16)) (aref v (+ (* j 16) 15)))))
              s)
            (let ((s 0))
              (loop for i from 127 downto 1 by 2 do (setq s (logxor s (aref v (- i 1)))))
              s))))

(deftest-emitting-only loop-counting-range.typed-path
  (mapcar #'%lcr-generic-arith-p
          (list #'%lcr-loop-sum #'%lcr-do-sum #'%lcr-dotimes-sum #'%lcr-loop-down))
  (nil nil nil nil))

;;; LOOP writes a constant numeric limit as itself, not quoted, so the end test
;;; is a typed comparison with a literal instead of the generic one.

(defun %lcr-loop-const-limit (v)
  (declare (type (simple-array fixnum (*)) v))
  (let ((s 0))
    (declare (fixnum s))
    (loop for i from 0 below 100 do (setq s (logxor s (aref v i))))
    (loop for i from 99 downto 1 do (setq s (logxor s (aref v i))))
    s))

(deftest loop-counting-range.const-limit-value
  (%lcr-loop-const-limit *lcr-v*)
  #.(let ((s 0))
      (loop for i from 1 below 100 do (setq s (logxor s (* i i))))
      (loop for i from 1 below 100 do (setq s (logxor s (* i i))))
      s))

(deftest-emitting-only loop-counting-range.const-limit-typed
  (let ((s (%lcr-sil #'%lcr-loop-const-limit)))
    (list (and (search "Runtime.IsTrue" s) t) (%lcr-generic-arith-p #'%lcr-loop-const-limit)))
  (nil nil))

;;; DO ended by (= I L): the variable is bounded only when it cannot step over
;;; L. Otherwise the loop runs past L (here until the body returns), and no
;;; range may be assumed.

(deftest loop-counting-range.do-until-equal
  (list (do ((i 0 (1+ i)) (s 0 (+ s i))) ((= i 10) (list i s)))
        (do ((i 0 (+ i 2)) (r nil (cons i r))) ((= i 8) (list i r)))
        (do ((i 10 (1- i)) (s 0 (+ s i))) ((= i 0) (list i s)))
        (let ((n 7)) (do ((i 0 (1+ i))) ((= i n) i)))
        (do ((i 0 (+ i 3))) ((= i 10) :never) (when (> i 20) (return i)))
        (do ((i 5 (1+ i))) ((= i 3) :never) (when (> i 8) (return i)))
        (do ((i 0 (1- i))) ((= i 3) :never) (when (< i -4) (return i)))
        (do ((i 0 (1+ i))) ((= i 10) :never) (when (= i 4) (setq i 12)) (when (> i 15) (return i))))
  ((10 45) (8 (6 4 2 0)) (0 55) 7 21 9 -5 16))

(defun %lcr-do-until-equal (v)
  (declare (type (simple-array fixnum (*)) v))
  (let ((s 0))
    (declare (fixnum s))
    (do ((j 0 (+ j 2))) ((= j 128)) (setq s (logxor s (aref v j) (aref v (+ j 1)))))
    (do ((j 127 (1- j))) ((= j 0)) (setq s (logxor s (ash (aref v j) 1))))
    s))

(deftest loop-counting-range.do-until-equal-value
  (%lcr-do-until-equal *lcr-v*)
  #.(let ((s 0))
      (dotimes (j 128) (setq s (logxor s (* j j))))
      (loop for j from 127 downto 1 do (setq s (logxor s (* 2 j j))))
      s))

(deftest-emitting-only loop-counting-range.do-until-equal-typed
  (%lcr-generic-arith-p #'%lcr-do-until-equal)
  nil)
