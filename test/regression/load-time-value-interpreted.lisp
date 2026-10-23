;;; LOAD-TIME-VALUE under the tree-walk evaluator (every form of an emit-free
;;; build): the form is evaluated once, so a function returns the same object
;;; on every call, and a NIL value is not evaluated again.

(defvar *ltv-interp-count* 0)

(defun ltv-interp-cell () (load-time-value (list :cell)))

(defun ltv-interp-nil ()
  (load-time-value (progn (incf *ltv-interp-count*) nil)))

(deftest load-time-value-interpreted.top-level
  (load-time-value 7)
  7)

(deftest load-time-value-interpreted.same-object
  (list (ltv-interp-cell) (eq (ltv-interp-cell) (ltv-interp-cell)))
  ((:cell) t))

(deftest load-time-value-interpreted.nil-once
  (list (ltv-interp-nil) (ltv-interp-nil) *ltv-interp-count*)
  (nil nil 1))
