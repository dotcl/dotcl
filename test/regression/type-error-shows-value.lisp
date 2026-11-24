;;; A TYPE-ERROR whose text names what was expected also shows the value that was given
;;; (SBCL: "The value 5 is not of type ..."): the shapes that said only what was expected,
;;; "GCD: argument is not an integer", "RATIONAL: not real", "FORMAT: invalid destination", ...

(defun %tesv-message (thunk)
  (handler-case (progn (funcall thunk) nil)
    (type-error (e) (princ-to-string e))))

(defmacro %tesv-shows (form text)
  `(let ((m (%tesv-message (lambda () ,form))))
     (and m (search ,text m) t)))

(deftest type-error-shows-value.number-functions
  (list (%tesv-shows (gcd 'tesv-bad) "TESV-BAD")
        (%tesv-shows (lcm 'tesv-bad) "TESV-BAD")
        (%tesv-shows (rational 'tesv-bad) "TESV-BAD")
        (%tesv-shows (numerator 'tesv-bad) "TESV-BAD")
        (%tesv-shows (ash 1 'tesv-bad) "TESV-BAD")
        (%tesv-shows (logand 1 'tesv-bad) "TESV-BAD"))
  (t t t t t t))

(deftest type-error-shows-value.others
  (list (%tesv-shows (make-string 'tesv-bad) "TESV-BAD")
        (%tesv-shows (parse-integer "1" :start 'tesv-bad) "TESV-BAD")
        (%tesv-shows (apply #'+ 1 'tesv-bad) "TESV-BAD")
        (%tesv-shows (pathname 12345) "12345")
        (%tesv-shows (format 12345 "x") "12345"))
  (t t t t t))

;;; A message that already shows the value does not show it twice, and the datum is still there.
(deftest type-error-shows-value.no-repeat
  (let ((m (%tesv-message (lambda () (car 'tesv-twice)))))
    (and m (let ((a (search "TESV-TWICE" m))) (and a (not (search "TESV-TWICE" m :start2 (1+ a)))))))
  t)

(deftest type-error-shows-value.datum-and-expected-type
  (handler-case (gcd 'tesv-bad)
    (type-error (e) (list (type-error-datum e) (type-error-expected-type e))))
  (tesv-bad integer))
