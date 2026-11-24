;;; A COND (or CASE) of many clauses compiles: the chain of clauses is not one more level of
;;; compile depth per clause (the depth limit is for runaway macro expansion), and the time grows
;;; about in proportion to the number of clauses, not with its square.

(defun %cmc-cond-form (n)
  `(lambda (x) (cond ,@(loop for i below n collect `((eql x ,i) ,i)) (t :none))))

(defun %cmc-case-form (n)
  `(lambda (x) (case x ,@(loop for i below n collect `((,i) ,i)) (t :none))))

(deftest-emitting-only cond-many-clauses.compiles
  (let ((c (compile nil (%cmc-cond-form 12000)))
        (k (compile nil (%cmc-case-form 12000))))
    (list (funcall c 0) (funcall c 11999) (funcall c 12000)
          (funcall k 0) (funcall k 11999) (funcall k 12000)))
  (0 11999 :none 0 11999 :none))

(deftest-emitting-only cond-many-clauses.small-shapes-unchanged
  (let ((f (compile nil '(lambda (x)
                          (cond ((eql x 1) :one)
                                ((eql x 2))
                                ((eql x 3) :three :3)
                                (t :other))))))
    (list (funcall f 1) (funcall f 2) (funcall f 3) (funcall f 4)))
  (:one t :3 :other))

(defun %cmc-compile-time (n)
  "The shortest of three compiles of an N-clause COND, in internal time units."
  (let ((form (%cmc-cond-form n)) (best nil))
    (dotimes (i 3 best)
      (let ((t0 (get-internal-real-time)))
        (compile nil form)
        (let ((dt (- (get-internal-real-time) t0)))
          (when (or (null best) (< dt best)) (setf best dt)))))))

;;; Twice the clauses should take about twice as long (a quadratic walk makes it four times).
;;; Wall time is noisy when other processes share the machine, so a run is judged by up to
;;; three attempts: a quadratic walk gives a ratio near 4 on every attempt, while a stall in one
;;; measurement only spoils that attempt.
(defun %cmc-growth-ok-p ()
  (%cmc-compile-time 500)               ; the first compile pays for loading the compiler paths
  (dotimes (attempt 3 nil)
    (let* ((small (max 1 (%cmc-compile-time 5000)))
           (large (%cmc-compile-time 10000)))
      (when (< (/ large small) 3.2) (return t)))))

(deftest-emitting-only cond-many-clauses.growth
  (%cmc-growth-ok-p)
  t)
