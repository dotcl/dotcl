;;; (VALUES X) where only the primary value is taken is X's primary value, and
;;; publishes nothing; in a tail or multiple-value position it still makes X's
;;; primary value the only value.

(setf dotcl:*save-sil* t)

(defun %vsc-let (a b) (let ((x (values (cons a b)))) (list x)))
(defun %vsc-tail () (values (floor 7 2)))
(defun %vsc-arg () (list (values (floor 7 2)) (values (values 1 2))))

(deftest values-single-context.values
  (list (%vsc-let 1 2)
        (multiple-value-list (%vsc-tail))
        (%vsc-arg)
        (multiple-value-list (values (floor 7 2)))
        (multiple-value-list (if (values (floor 7 2)) (values (floor 9 2)) nil))
        (let ((x (values (floor 7 2)))) (multiple-value-list (values x))))
  (((1 . 2)) (3) (3 1) (3) (4) (3)))

(deftest-compiled-only values-single-context.no-publish
  (and (search "MultipleValues.Primary" (%flat-text (dotcl:function-sil #'%vsc-let))) t)
  nil)

(setf dotcl:*save-sil* nil)
