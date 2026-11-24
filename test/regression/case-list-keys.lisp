;;; CASE with a list of keys tests them with EQL, inline, not through MEMBER and a :TEST function.

(defun case-keys-sample (x)
  (case x
    ((0) :zero)
    ((1 2) :small)
    ((#\a #\b #\c) :char)
    (((a . b)) :cons-never-eql)
    ((nil) :nil-key)
    ((:k1 :k2 :k3 :k4 :k5 :k6 :k7 :k8) :long)
    ((1.5d0) :double)
    ((10000000000000000000000) :bignum)
    (otherwise :other)))

(deftest case-list-keys.hits
  (list (case-keys-sample 0) (case-keys-sample 2) (case-keys-sample #\b)
        (case-keys-sample :k8) (case-keys-sample 1.5d0)
        (case-keys-sample 10000000000000000000000) (case-keys-sample nil))
  (:zero :small :char :long :double :bignum :nil-key))

(deftest case-list-keys.misses
  (list (case-keys-sample 3) (case-keys-sample "a") (case-keys-sample '(a . b))
        (case-keys-sample :k9))
  (:other :other :other :other))

(deftest case-list-keys.expansion-has-no-member
  (labels ((has-member (f) (cond ((eq f 'member) t)
                                 ((consp f) (or (has-member (car f)) (has-member (cdr f)))))))
    (list (has-member (macroexpand-1 '(case x ((0) a) ((1 2) b))))
          (has-member (macroexpand-1 '(case x ((1 2 3 4 5 6 7) a))))))
  (nil t))

(deftest case-list-keys.ecase
  (list (ecase 2 ((1 2) :ok)) (handler-case (ecase 9 ((1 2) :ok)) (type-error () :type-error)))
  (:ok :type-error))
