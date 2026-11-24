;;; A fresh line (~&) or newline (~%) after a conditional newline ends its section, so a
;;; fill newline before it does not break when only the text up to that newline follows.

(define-condition ffl-nested (error) ()
  (:report (lambda (c s) (declare (ignore c))
             (format s "~@<The number was~@:_1.~@:>"))))

(defun ffl-cause (stream cause colon at)
  (declare (ignore colon at))
  (format stream "~@[ ~:_Caused by:~&~@<> ~@;~A~@:>~]" cause))

(deftest format-fresh-line-after-fill.cause-block
  (let ((*print-pretty* t))
    (format nil "~@<Foo.~/ffl-cause/~:>" (make-condition 'ffl-nested)))
  "Foo. Caused by:
> The number was
> 1.")

(deftest format-fresh-line-after-fill.inline-forms
  (let ((*print-pretty* t) (c (make-condition 'ffl-nested)))
    (list (format nil "~@<Foo.~@[ ~:_Caused by:~&~@<> ~@;~A~@:>~]~:>" c)
          (format nil "~@<Foo.~:_Caused by:~&Bar~:>")
          (format nil "~@<abcxyz~:_abc~2&de fgh~:>")))
  ("Foo. Caused by:
> The number was
> 1."
   "Foo.Caused by:
Bar"
   "abcxyzabc

de fgh"))
