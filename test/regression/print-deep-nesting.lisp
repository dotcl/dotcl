;;; A deeply nested list or vector prints in full: the printer stops at the end of the
;;; stack, not at a small fixed depth (it used to cut at 256 levels with "(...)").

(defun %pdn-nest (n)
  (let ((l nil)) (dotimes (i n) (setq l (list l))) l))

(deftest print-deep-nesting.list
  (let ((s (prin1-to-string (%pdn-nest 300))))
    (list (length s) (search "..." s)))
  (603 nil))

(deftest print-deep-nesting.very-deep
  (let ((s (prin1-to-string (%pdn-nest 5000))))
    (list (length s) (search "..." s)))
  (10003 nil))

(deftest print-deep-nesting.vectors
  (let ((v nil))
    (dotimes (i 1000) (setq v (vector v)))
    (let ((s (prin1-to-string v)))
      (list (length s) (search "..." s))))
  (3003 nil))

(deftest print-deep-nesting.circular-structure-is-cut
  (let ((x (list 1)))
    (setf (car x) x)
    (let ((s (let ((*print-circle* nil)) (prin1-to-string x))))
      (list (< (length s) 100000) (and (search "..." s) t))))
  (t t))

(deftest print-deep-nesting.print-level-still-cuts
  (let ((*print-level* 3))
    (prin1-to-string (%pdn-nest 10)))
  "(((#)))")
