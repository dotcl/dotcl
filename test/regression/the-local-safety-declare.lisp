;;; A (declare (optimize (safety N))) in LOCALLY or LET sets the safety of THE checks in its body,
;;; whatever the DECLAIM or the enclosing function says. The &key function case is the one that
;;; failed: its body is re-wrapped in a LET*, which hid the function's own declaration.
(defun %tlsd-locally-0 (p)
  (declare (optimize (safety 3)))
  (locally (declare (optimize (safety 0))) (the (integer 0 5) p)))

(defun %tlsd-let-0 (p)
  (declare (optimize (safety 3)))
  (let ((q p)) (declare (optimize (safety 0))) (the (integer 0 5) q)))

(defun %tlsd-key-0 (&key p)
  (declare (optimize (safety 3)))
  (locally (declare (optimize (safety 0))) (the (integer 0 5) p)))

(defun %tlsd-locally-3 (p)
  (declare (optimize (safety 0)))
  (locally (declare (optimize (safety 3))) (the (integer 0 5) p)))

(deftest the-local-safety-declare.unchecked
  (list (%tlsd-locally-0 99) (%tlsd-let-0 99) (%tlsd-key-0 :p 99))
  (99 99 99))

(deftest-emitting-only the-local-safety-declare.checked
  (list (handler-case (%tlsd-locally-3 99) (type-error () :type-error))
        (%tlsd-locally-3 3))
  (:type-error 3))
