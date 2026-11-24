;;; regression: skip-under-interp
;;; A method on CLOSE for a class that is not a stream has no next method: the built-in CLOSE is the
;;; method for streams only (SBCL's CLOSE is a generic function with a STREAM-specialized method).
;;; iolib's datagram sockets close with (when (next-method-p) (call-next-method)).

(defclass %cmnn-thing () ())
(defmethod close ((x %cmnn-thing) &key abort)
  (declare (ignore abort))
  (if (next-method-p) (list :next (call-next-method)) :no-next))

(deftest close-method-no-next.next-method-p
  (close (make-instance '%cmnn-thing))
  :no-next)

(defmethod close :before ((x %cmnn-thing) &key abort)
  (declare (ignore abort))
  nil)

(deftest close-method-no-next.with-before-method
  (close (make-instance '%cmnn-thing))
  :no-next)

(deftest close-method-no-next.with-open-stream
  (with-open-stream (s (make-instance '%cmnn-thing)) 1)
  1)

;; Streams still reach the built-in method, and a non-stream with no method is still an error.
(deftest close-method-no-next.stream-closes
  (let ((s (make-string-input-stream "abc")))
    (close s)
    (open-stream-p s))
  nil)

(deftest close-method-no-next.non-stream-without-method
  (handler-case (close 5) (type-error () :type-error) (error () :other-error))
  :type-error)
