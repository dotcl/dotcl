;;; ~/name/ with no argument left.
;;;
;;; A directive that needs an argument when none is left is an error (CLHS 22.3).
;;; ~/name/ called the function with NIL instead. SBCL 2.6.8 signals the same
;;; "No more arguments" error as for ~A.

(defvar *fsma-called* nil)

(defun fsma-fn (s x &rest r)
  (declare (ignore r))
  (setf *fsma-called* t)
  (format s "<~A>" x))

(defun fsma-error-type (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (e) (type-of e))))

(deftest fsma-no-argument-signals
  (let ((*fsma-called* nil))
    (list (signals-error (format nil "~/fsma-fn/") error) *fsma-called*))
  (t nil))

(deftest fsma-same-error-as-tilde-a
  (let ((slash (fsma-error-type (lambda () (format nil "~/fsma-fn/"))))
        (aesthetic (fsma-error-type (lambda () (format nil "~A")))))
    (and slash (eq slash aesthetic)))
  t)

(deftest fsma-second-directive-runs-out
  (signals-error (format nil "~/fsma-fn/~/fsma-fn/" 1) error)
  t)

(deftest fsma-in-logical-block-runs-out
  (signals-error (format nil "~@<~/fsma-fn/~:>") error)
  t)

(deftest fsma-with-argument-unchanged
  (format nil "~/fsma-fn/" 1)
  "<1>")

(deftest fsma-iteration-unchanged
  (format nil "~{~/fsma-fn/~}" '(1 2))
  "<1><2>")
