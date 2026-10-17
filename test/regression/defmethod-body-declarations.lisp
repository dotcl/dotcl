;;; The declarations at the head of a DEFMETHOD body are the method function's
;;; own, as for DEFUN: they sit in the lambda, not inside the BLOCK around the body.

(defun %dmbd-fn (s) (declare (simple-string s)) (length s))
(defmethod %dmbd-gf ((s t)) (declare (simple-string s)) (length s))
(defmethod %dmbd-cnm ((s string)) (declare (simple-string s)) (list (length s) (call-next-method)))
(defmethod %dmbd-cnm ((s t)) :t)

(defun %dmbd-adjustable ()
  (make-array 3 :element-type 'character :adjustable t :initial-element #\a))

(deftest defmethod-body-declarations.checked-like-defun
  (list (handler-case (%dmbd-fn (%dmbd-adjustable)) (type-error () :type-error))
        (handler-case (%dmbd-gf (%dmbd-adjustable)) (type-error () :type-error))
        (handler-case (%dmbd-cnm (%dmbd-adjustable)) (type-error () :type-error))
        (%dmbd-gf "abc")
        (%dmbd-cnm "abcd"))
  (:type-error :type-error :type-error 3 (4 :t)))

(defun %dmbd-read-x () (declare (special x)) x)
(defmethod %dmbd-special ((x t)) (declare (special x)) (%dmbd-read-x))

(deftest defmethod-body-declarations.special
  (%dmbd-special 7)
  7)
