;;; A keyword argument error from an anonymous lambda names the function as
;;; "lambda", as the argument count error does, instead of starting with ": ".

(defun %alke-message (thunk)
  (handler-case (progn (funcall thunk) nil)
    (program-error (e) (princ-to-string e))))

(deftest anonymous-lambda-keyword-error.unknown-keyword
  (let ((m (%alke-message (lambda () (funcall (lambda (&key a) a) :b 1)))))
    (list (and (search "unrecognized keyword argument :B" m) t)
          (and (> (length m) 0) (char/= (char m 0) #\:))))
  (t t))

(deftest anonymous-lambda-keyword-error.odd-count
  (let ((m (%alke-message (lambda () (funcall (lambda (&key a) a) :a)))))
    (and (search "odd number of keyword arguments" m)
         (char/= (char m 0) #\:)
         t))
  t)

(deftest anonymous-lambda-keyword-error.named-function-unchanged
  (progn (defun alke-named (&key a) a)
         (let ((m (%alke-message (lambda () (alke-named :b 1)))))
           (and (search "ALKE-NAMED" m) t)))
  t)
