;;; Pretty printing a form whose car names a macro must not leave values in the
;;; multiple-values channel: a caller that then returns NIL reported them as its
;;; own, (NIL NIL) instead of (NIL).

(defun %pmfv-let (obj &rest args)
  (let ((s (apply #'write-to-string obj args)))
    (declare (ignore s))
    nil))

(defun %pmfv-loop (obj)
  (loop for s = (write-to-string obj :pretty t) repeat 0 collect s))

(deftest print-macro-form-values.1
  (list (multiple-value-list (%pmfv-let '(incf . x) :pretty t))
        (multiple-value-list (%pmfv-let '(incf x) :pretty t))
        (multiple-value-list (%pmfv-let '(with-slots a . b) :pretty t))
        (multiple-value-list (%pmfv-let '(when x y) :pretty t :circle t))
        (multiple-value-list (%pmfv-loop '(remf . x))))
  ((nil) (nil) (nil) (nil) (nil)))
