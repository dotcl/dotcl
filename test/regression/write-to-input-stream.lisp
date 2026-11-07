;;; Writing to something that is not an output stream is a TYPE-ERROR; the
;;; text used to go to the console.

(defun %wis-outcome (thunk)
  (handler-case (progn (funcall thunk) :ok)
    (type-error () :type-error)))

(deftest write-to-input-stream.string-input
  (list (%wis-outcome (lambda () (with-input-from-string (s "abc") (write-char #\a s))))
        (%wis-outcome (lambda () (with-input-from-string (s "abc") (write-string "a" s))))
        (%wis-outcome (lambda () (with-input-from-string (s "abc") (format s "x")))))
  (:type-error :type-error :type-error))

(deftest write-to-input-stream.not-a-stream
  (list (%wis-outcome (lambda () (write-char #\a 5)))
        (%wis-outcome (lambda () (terpri "abc"))))
  (:type-error :type-error))

(deftest write-to-input-stream.output-still-works
  (with-output-to-string (s) (write-char #\a s) (write-string "bc" s) (format s "~d" 1))
  "abc1")
