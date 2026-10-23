;;; (IDENTITY X) is compiled as the primary value of X, not as a call.

(setf dotcl:*save-sil* t)

(defun %idi-add (a b) (identity (+ a b)))
(defun %idi-tail () (identity (values 1 2)))
(defun %idi-notinline (x)
  (declare (notinline identity))
  (identity x))

(deftest identity-inline.values
  (list (%idi-add 2 3)
        (multiple-value-list (%idi-tail))
        (multiple-value-list (identity (floor 7 2)))
        (multiple-value-list (identity (values)))
        (%idi-notinline :x)
        (funcall #'identity 4)
        (mapcar #'identity '(1 2)))
  (5 (1) (3) (nil) :x 4 (1 2)))

(deftest-compiled-only identity-inline.no-call
  (list (and (search "IDENTITY" (%flat-text (dotcl:function-sil #'%idi-add))) t)
        (and (search "IDENTITY" (%flat-text (dotcl:function-sil #'%idi-notinline))) t))
  (nil t))

(setf dotcl:*save-sil* nil)
