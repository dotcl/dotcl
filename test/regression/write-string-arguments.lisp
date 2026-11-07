;;; WRITE-STRING and WRITE-LINE take a string: anything else is a TYPE-ERROR
;;; (it was printed), and bad :START / :END bounds are a TYPE-ERROR naming
;;; them (it was a raw .NET message).

(defun %wsa-outcome (thunk)
  (handler-case (progn (funcall thunk) :ok)
    (type-error () :type-error)))

(deftest write-string-arguments.non-string
  (list (%wsa-outcome (lambda () (with-output-to-string (s) (write-string 5 s))))
        (%wsa-outcome (lambda () (with-output-to-string (s) (write-string 'abc s))))
        (%wsa-outcome (lambda () (with-output-to-string (s) (write-line 5 s)))))
  (:type-error :type-error :type-error))

(deftest write-string-arguments.bounds
  (list (%wsa-outcome (lambda () (with-output-to-string (s) (write-string "abc" s :start 10))))
        (%wsa-outcome (lambda () (with-output-to-string (s) (write-string "abc" s :end 10))))
        (%wsa-outcome (lambda () (with-output-to-string (s) (write-line "abc" s :start 2 :end 1)))))
  (:type-error :type-error :type-error))

(deftest write-string-arguments.valid
  (list (with-output-to-string (s) (write-string "abcdef" s :start 1 :end 3))
        (with-output-to-string (s) (write-string "abc" s :start 3))
        (with-output-to-string (s) (write-line (make-array 2 :element-type 'character :initial-element #\z) s)))
  ("bc" "" #.(format nil "zz~%")))
