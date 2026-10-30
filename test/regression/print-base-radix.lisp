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

;;; The radix specifier is lowercase, and a base-ten ratio needs a leading
;;; #10r (an integer instead takes the trailing decimal point).

(deftest print-base-radix.lowercase-specifier
  (list (let ((*print-base* 3) (*print-radix* t)) (write-to-string 0))
        (let ((*print-base* 24) (*print-radix* t)) (write-to-string 23))
        (let ((*print-base* 36) (*print-radix* t)) (write-to-string 35)))
  ("#3r0" "#24rN" "#36rZ"))

(deftest print-base-radix.base-10-ratio
  (list (let ((*print-base* 10) (*print-radix* t)) (write-to-string 1/2))
        (let ((*print-base* 10) (*print-radix* t)) (write-to-string -3/7))
        (let ((*print-base* 10) (*print-radix* t))
          (write-to-string (/ (expt 10 25) 3))))
  ("#10r1/2" "#10r-3/7" "#10r10000000000000000000000000/3"))

(deftest print-base-radix.ratio-other-bases
  (list (let ((*print-base* 3) (*print-radix* t)) (write-to-string 1/2))
        (let ((*print-base* 3) (*print-radix* t)) (write-to-string -3/7))
        (let ((*print-base* 24) (*print-radix* t)) (write-to-string 1/2))
        (let ((*print-base* 10) (*print-radix* nil)) (write-to-string 1/2)))
  ("#3r1/2" "#3r-10/21" "#24r1/2" "1/2"))
