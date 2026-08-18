;;; An out-of-range print base is a TYPE-ERROR naming the value.
;;;
;;; The radix check threw ArgumentException, which the CLR-exception mapping
;;; reports as PROGRAM-ERROR: passing 40 for :base was described as a broken
;;; program rather than a bad argument, and the condition carried no datum.

(defun %pbr-outcome (thunk)
  (handler-case (list :ok (funcall thunk))
    (type-error (e) (list :type-error (type-error-datum e)
                          (type-error-expected-type e)))
    (error (e) (list :other (type-of e)))))

(deftest print-base-radix.write-to-string-base-too-large
  (%pbr-outcome (lambda () (write-to-string 10 :base 40)))
  (:type-error 40 (integer 2 36)))

(deftest print-base-radix.write-to-string-base-too-small
  (%pbr-outcome (lambda () (write-to-string 10 :base 1)))
  (:type-error 1 (integer 2 36)))

(deftest print-base-radix.format-radix-directive
  (%pbr-outcome (lambda () (format nil "~40r" 10)))
  (:type-error 40 (integer 2 36)))

(deftest print-base-radix.print-base-variable
  (%pbr-outcome (lambda () (let ((*print-base* 40)) (princ-to-string 10))))
  (:type-error 40 (integer 2 36)))

;;; The whole valid range still prints, fixnum and bignum alike.

(deftest print-base-radix.valid-bases
  (list (write-to-string 255 :base 16)
        (write-to-string 255 :base 2)
        (write-to-string 255 :base 36)
        (write-to-string (expt 10 25) :base 36))
  ("FF" "11111111" "73" "198EXBVSHGPUNUK1S"))
