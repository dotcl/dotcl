;;; Wording of runtime errors a user sees first.

(defun rew-message (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (c) (princ-to-string c))))

(deftest rew-anonymous-optional-too-many
  (let ((m (rew-message (lambda () (funcall (lambda (&optional a) a) 1 2)))))
    (and (search "too many arguments" m) (not (eql 0 (search ":" m)))))
  t)

(deftest rew-anonymous-optional-too-few
  (let ((m (rew-message (lambda () (funcall (lambda (a b &optional c) (list a b c)) 1)))))
    (and (search "too few arguments" m) (not (eql 0 (search ":" m)))))
  t)

(deftest rew-parse-integer-names-string
  (let ((m (rew-message (lambda () (parse-integer "abc")))))
    (and (search "\"abc\"" m) t))
  t)

(deftest rew-parse-integer-junk-names-string
  (let ((m (rew-message (lambda () (parse-integer "12x")))))
    (and (search "\"12x\"" m) t))
  t)

(deftest rew-position-non-sequence
  (let ((m (rew-message (lambda () (position 1 5)))))
    (and (search "POSITION" m) (not (search "LENGTH" m))))
  t)

(deftest rew-find-non-sequence
  (let ((m (rew-message (lambda () (find 1 5)))))
    (and (search "FIND" m) (not (search "LENGTH" m))))
  t)

(deftest rew-count-if-non-sequence
  (let ((m (rew-message (lambda () (count-if #'oddp 5)))))
    (and (search "COUNT-IF" m) (not (search "LENGTH" m))))
  t)

(deftest rew-position-non-sequence.type
  (handler-case (position 1 5) (type-error (e) (type-error-datum e)))
  5)

(deftest rew-open-directory
  (let ((m (rew-message (lambda () (open #-windows "/tmp" #+windows "C:/Windows")))))
    (and (search "directory" m) t))
  t)

(deftest-emitting-only rew-subseq-arity-names-function
  (let ((m (rew-message (lambda () (eval '(subseq))))))
    (and (search "SUBSEQ" m) t))
  t)

(deftest-emitting-only rew-concatenate-arity-names-function
  (let ((m (rew-message (lambda () (eval '(concatenate))))))
    (and (search "CONCATENATE" m) t))
  t)

(deftest rew-minus-arity-names-function
  (let ((m (rew-message (lambda () (apply #'- nil)))))
    (and (search "wrong number of arguments" m) t))
  t)
