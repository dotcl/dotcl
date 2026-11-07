;;; An unknown keyword argument is shown in the PROGRAM-ERROR message.

(deftest unknown-keyword-shown.write-sequence
  (handler-case (write-sequence "abc" *standard-output* :bogus 1)
    (program-error (e) (and (search ":BOGUS" (princ-to-string e)) t)))
  t)

(deftest unknown-keyword-shown.make-string-output-stream
  (handler-case (make-string-output-stream :bogus 1)
    (program-error (e) (and (search ":BOGUS" (princ-to-string e)) t)))
  t)
