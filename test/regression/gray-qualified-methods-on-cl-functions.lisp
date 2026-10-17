;;; DEFMETHOD on a CL stream function (CLOSE, OPEN-STREAM-P) turns it into a
;;; generic function that keeps the built-in as a fallback. With only :BEFORE
;;; methods applicable it signalled "No primary method", and CALL-NEXT-METHOD
;;; from a primary method had no next method; on SBCL both reach the built-in.
;;; nontrivial-gray-streams' tests record calls with :BEFORE methods like these.
(require "dotcl-gray")

(defclass %gqm-in (dotcl-gray:fundamental-character-input-stream)
  ((log :initform nil :accessor %gqm-log)))
(defmethod open-stream-p :before ((s %gqm-in)) (push :open-stream-p (%gqm-log s)))
(defmethod close :before ((s %gqm-in) &key abort)
  (declare (ignore abort))
  (push :close (%gqm-log s)))

(defclass %gqm-in-2 (%gqm-in) ())
(defmethod close ((s %gqm-in-2) &key abort)
  (declare (ignore abort))
  (list :own (next-method-p) (call-next-method)))

(deftest gray-qualified-methods.before-only-runs-built-in
  (let ((s (make-instance '%gqm-in)))
    (list (open-stream-p s) (close s) (%gqm-log s)))
  (t t (:close :open-stream-p)))

(deftest gray-qualified-methods.call-next-method-reaches-built-in
  (let ((s (make-instance '%gqm-in-2)))
    (list (close s) (%gqm-log s)))
  ((:own t t) (:close)))

(deftest gray-qualified-methods.other-streams-unchanged
  (list (open-stream-p *standard-input*)
        (close (make-string-input-stream "a")))
  (t t))

(defun %gqm-plain (x) (list :plain x))
(defvar *%gqm-seen* nil)
(defmethod %gqm-plain :after ((x integer)) (push x *%gqm-seen*))

(deftest gray-qualified-methods.ordinary-function-with-after-method
  (list (%gqm-plain 3) (%gqm-plain :k) *%gqm-seen*)
  ((:plain 3) (:plain :k) (3)))
