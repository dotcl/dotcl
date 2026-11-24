;;; SBCL signals an error for a ~/ directive that has no closing slash; it does not call the function.

(defvar *fsu-called* nil)
(defun fsu-fn (s a c p &rest r) (declare (ignore c p r)) (setf *fsu-called* t) (princ a s))

(deftest format-slash-unterminated
  (list (handler-case (format nil "~/fsu-fn" 1) (error () :error))
        *fsu-called*
        (format nil "~/fsu-fn/" 1))
  (:error nil "1"))
