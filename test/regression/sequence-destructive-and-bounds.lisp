;;; NSUBSTITUTE changes a simple string in place; NTH / NTHCDR take any non-negative integer
;;; (past the end: NIL); a :START / :END outside the fixnum range is a TYPE-ERROR, not wrapped to
;;; 0 or read as NIL; REMOVE-DUPLICATES :FROM-END calls the test with the later element first.

(deftest sequence-destructive-and-bounds.nsubstitute-strings
  (list (let* ((s (copy-seq "abcabc")) (r (nsubstitute #\z #\a s))) (list s r (eq s r)))
        (let* ((s (copy-seq "abcabc")) (r (nsubstitute-if #\z (lambda (c) (char= c #\a)) s))) (list s r (eq s r)))
        (let* ((s (copy-seq "abcabc")) (r (nsubstitute-if-not #\z (lambda (c) (char= c #\a)) s))) (list s r (eq s r)))
        (let* ((s (copy-seq "abcabc")) (r (nsubstitute #\z #\a s :count 1 :start 1))) (list s (eq s r)))
        (let* ((s (copy-seq "abcabc")) (r (nsubstitute #\z #\a s :from-end t :count 1))) (list s (eq s r)))
        (let* ((s (copy-seq "abcABC")) (r (nsubstitute #\z #\a s :test #'char-equal))) (list s (eq s r)))
        (let* ((s (copy-seq "")) (r (nsubstitute #\z #\a s))) (list s (eq s r)))
        (let* ((s (copy-seq "abc")) (r (nsubstitute #\z #\q s))) (list s (eq s r)))
        (handler-case (nsubstitute 1 #\a (copy-seq "abc")) (type-error () :type-error)))
  (("zbczbc" "zbczbc" t) ("zbczbc" "zbczbc" t) ("azzazz" "azzazz" t) ("abczbc" t) ("abczbc" t)
   ("zbczBC" t) ("" t) ("abc" t) :type-error))

(deftest sequence-destructive-and-bounds.nsubstitute-others-unchanged
  (list (let* ((s (list 1 2 1)) (r (nsubstitute 9 1 s))) (list s (eq s r)))
        (let* ((s (vector 1 2 1)) (r (nsubstitute 9 1 s))) (list (coerce s 'list) (eq s r)))
        (nsubstitute 9 1 nil))
  (((9 2 9) t) ((9 2 9) t) nil))

(deftest sequence-destructive-and-bounds.nth-large-index
  (list (nth (expt 2 31) '(a b c)) (nthcdr (expt 2 31) '(1 2 3)) (nth (expt 2 100) '(a b c))
        (nthcdr (expt 2 100) '(1 2 3)) (nthcdr (expt 2 64) nil)
        (nth 2 '(a b c)) (nth 3 '(a b c)) (nthcdr 0 '(a)) (nthcdr 2 '(1 2 3 4))
        (handler-case (nth -1 '(a)) (type-error () :type-error))
        (handler-case (nth (- (expt 2 70)) '(a)) (type-error () :type-error))
        (handler-case (nthcdr 1.5 '(a)) (type-error () :type-error)))
  (nil nil nil nil nil c nil (a) (3 4) :type-error :type-error :type-error))

(deftest sequence-destructive-and-bounds.bignum-end
  (list (handler-case (find 2 '(1 2 3 2 1 4) :end 4294967296) (type-error () :type-error))
        (handler-case (find 2 '(1 2 3 2 1 4) :end 9223372036854775808) (type-error () :type-error))
        (handler-case (position 2 #(1 2 3) :end (expt 2 70)) (type-error () :type-error))
        (handler-case (count 1 "abc" :start (expt 2 40)) (type-error () :type-error))
        (find 2 '(1 2 3 2 1 4) :end 4)
        (find 2 '(1 2 3 2 1 4) :end nil)
        (find 4 '(1 2 3 2 1 4) :end 4)
        (count 2 '(1 2 3 2 1 4) :start 2 :end 6))
  (:type-error :type-error :type-error :type-error 2 2 nil 1))

(deftest sequence-destructive-and-bounds.remove-duplicates-from-end-order
  (let (log)
    (remove-duplicates '(1 2 1) :from-end t
                       :test (lambda (a b) (push (list a b) log) (eql a b)))
    (reverse log))
  ((2 1) (1 1)))
