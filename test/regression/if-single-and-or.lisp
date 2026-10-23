;;; (IF (AND X) ...) and (IF (OR X) ...) branch on X directly, as they do with
;;; two or more forms, instead of building X's value and testing it.

(setf dotcl:*save-sil* t)

(defstruct %isao-s)
(defun %isao-and (x) (if (and x) :yes :no))
(defun %isao-or (x) (if (or x) :yes :no))
(defun %isao-typep (m) (cond ((and (typep m '%isao-s)) 1) (t 2)))
(defun %isao-not (x) (if (and (not x)) :yes :no))
(defun %isao-mv (x) (if (or (values x t)) :yes :no))
(defun %isao-effect (x) (let ((n 0)) (if (and (incf n) x) (incf n 10) (incf n 100)) n))

(deftest if-single-and-or.values
  (list (%isao-and 0) (%isao-and nil) (%isao-or :a) (%isao-or nil)
        (%isao-typep (make-%isao-s)) (%isao-typep 3)
        (%isao-not nil) (%isao-not 1) (%isao-mv nil) (%isao-mv 2)
        (%isao-effect t) (%isao-effect nil))
  (:yes :no :yes :no 1 2 :yes :no :no :yes 11 101))

(deftest-compiled-only if-single-and-or.fused
  (list (search "IsTruthy" (%flat-text (dotcl:function-sil #'%isao-typep))))
  (nil))
