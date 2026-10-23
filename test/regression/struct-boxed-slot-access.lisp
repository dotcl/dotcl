;;; Boxed slots of a structure, read and written next to raw (typed) slots and in
;;; a structure without any, through compiled accessors.

(defstruct sbsa-mixed a (n 0 :type fixnum) b (d 0d0 :type double-float) c)
(defstruct sbsa-plain a b)

(defun %sbsa-walk (s)
  (list (sbsa-mixed-a s) (sbsa-mixed-n s) (sbsa-mixed-b s) (sbsa-mixed-d s) (sbsa-mixed-c s)))

(defun %sbsa-set (s)
  (list (setf (sbsa-mixed-a s) :a2)
        (setf (sbsa-mixed-n s) 7)
        (setf (sbsa-mixed-c s) '(c))
        (setf (sbsa-mixed-d s) 2.5d0)))

(deftest struct-boxed-slot-access.mixed
  (let ((s (make-sbsa-mixed :a :a :n 3 :b "b" :d 1.5d0 :c nil)))
    (list (%sbsa-walk s) (%sbsa-set s) (%sbsa-walk s)))
  ((:a 3 "b" 1.5d0 nil) (:a2 7 (c) 2.5d0) (:a2 7 "b" 2.5d0 (c))))

(defun %sbsa-plain-chain (n)
  (let ((head nil))
    (dotimes (i n) (setq head (make-sbsa-plain :a i :b head)))
    (let ((sum 0))
      (do ((p head (sbsa-plain-b p))) ((null p) sum)
        (setf (sbsa-plain-a p) (* 2 (sbsa-plain-a p)))
        (incf sum (sbsa-plain-a p))))))

(deftest struct-boxed-slot-access.plain-chain
  (%sbsa-plain-chain 100)
  9900)
