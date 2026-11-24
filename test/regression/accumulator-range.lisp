;;; An integer variable that a counted loop only adds to (or takes from) stays in the range
;;; the loop's trip count and the added value give, and lives in a raw int64 slot. Anything the
;;; compiler cannot count (another kind of loop, a closure, a count it does not know) keeps its
;;; generic, overflow-free arithmetic: the answers here are those of the generic functions.

(defun %acc-int64 ()
  (let ((s 0) (p 1))
    (dotimes (i 1000000)
      (setq s (+ s (* i 1000)))
      (setq p (logand (* p 31) #xFFFFFFFFFFF)))
    (+ s p)))
(defun %acc-int64-ref ()
  (let ((s 0) (p 1))
    (dotimes (i 1000000)
      (setq s (funcall #'+ s (funcall #'* i 1000)))
      (setq p (logand (funcall #'* p 31) #xFFFFFFFFFFF)))
    (+ s p)))
(defun %acc-nested ()
  (let ((s 0))
    (dotimes (i 300) (dotimes (j 200) (incf s (* i j))))
    s))
(defun %acc-decf ()
  (let ((s 100)) (dotimes (i 50) (decf s (* 3 i))) s))
(defun %acc-offset (c)
  (let ((s 5)) (dotimes (i 10) (incf s c)) s))
(defun %acc-big-step ()
  ;; 10 additions of 2^62: the sum is 10 * 2^62, beyond int64, a bignum.
  (let ((s 0)) (dotimes (i 10) (incf s (ash 1 62))) s))
(defun %acc-big-product ()
  (let ((s 0)) (dotimes (i 100000) (incf s (* i 100000000000000))) s))
(defun %acc-dolist (l)
  (let ((s 0)) (dotimes (i 3) (dolist (x l) (incf s x))) s))
(defun %acc-closure ()
  (let ((s 0) (f nil))
    (setq f (lambda () (incf s 1000000000000)))
    (dotimes (i 10) (funcall f))
    s))
(defun %acc-reset ()
  (let ((s 0)) (dotimes (i 100) (incf s i) (when (= (mod i 10) 9) (setq s 0))) s))
(defun %acc-unknown-count (n)
  (let ((s 0)) (dotimes (i n) (incf s 1000000000)) s))
(deftest accumulator-range.values
  (list (= (%acc-int64) (%acc-int64-ref)) (%acc-nested) (%acc-decf) (%acc-offset 3)
        (%acc-offset (expt 2 70)) (%acc-reset))
  (t 892515000 -3575 35 #.(+ 5 (* 10 (expt 2 70))) 0))

(deftest accumulator-range.beyond-int64-is-exact
  (list (%acc-big-step) (%acc-big-product) (%acc-dolist '(1 2 3)) (%acc-closure)
        (%acc-unknown-count 5))
  (#.(* 10 (expt 2 62)) #.(* 100000000000000 (/ (* 99999 100000) 2)) 18 10000000000000 5000000000))

(deftest-compiled-only accumulator-range.no-allocation
  (progn (%acc-int64)
         (list (< (min-allocated-bytes #'%acc-int64) 20000)
               (< (min-allocated-bytes #'%acc-nested) 20000)))
  (t t))

(defun %acc-go-loop ()
  ;; A GO that repeats the counted loop: the sum is 2000000 * 100000000000, beyond int64 / 2.
  (let ((s 0) (k 0))
    (tagbody top
       (dotimes (i 10) (incf s 100000000000))
       (incf k)
       (when (< k 20000) (go top)))
    s))
(deftest accumulator-range.go-around-the-loop
  (%acc-go-loop)
  #.(* 20000 10 100000000000))
