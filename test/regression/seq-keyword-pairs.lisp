;;; Sequence and list functions called with one or two keyword pairs take a
;;; direct entry that parses the pairs from its parameters.  The tests pin
;;; that those calls allocate nothing, and that the keyword rules are the same
;;; as on the general path: the first of a repeated keyword wins,
;;; :allow-other-keys admits unknown keywords wherever it appears, and an
;;; unknown keyword or a non-symbol keyword is a PROGRAM-ERROR.

(defun %skp-calls (l v n)
  (let ((r nil))
    (dotimes (i n r)
      (setq r (find 3 l :key #'car :test #'eql))
      (setq r (find 3 l :key #'car))
      (setq r (find-if #'oddp l :key #'car :start 1))
      (setq r (find-if #'oddp l :key #'car))
      (setq r (position 3 l :key #'car :test #'eql))
      (setq r (position 3 v :test #'eql))
      (setq r (position-if #'oddp l :key #'car :from-end t))
      (setq r (position-if #'oddp v :start 1))
      (setq r (count 3 l :key #'car :test #'eql))
      (setq r (count 3 v :test #'eql))
      (setq r (count-if #'oddp l :key #'car :end 2))
      (setq r (count-if #'oddp v :start 1))
      (setq r (member 3 l :key #'car :test #'eql))
      (setq r (assoc 3 l :key #'1+ :test #'eql))
      (setq r (rassoc 'c l :key #'car :test #'eq))
      (setq r (rassoc 'c l :test #'eq))
      (setq r (mismatch v v :test #'eql :start1 1))
      (setq r (mismatch v v :test #'eql)))))

(deftest-emitting-only seq-keyword-pairs.no-allocation
  (let ((l (list (list 1 'a) (list 2 'b) (list 3 'c)))
        (v (vector 1 2 3)))
    (%skp-calls l v 100)
    (< (min-allocated-bytes (lambda () (%skp-calls l v 100000)))
       10000))
  t)

(defun %skp-err (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (program-error () :program-error)))

(deftest seq-keyword-pairs.first-wins
  (let ((l '((1 2) (2 1))))
    (list (find 1 l :key #'car :key #'cadr)
          (position 1 l :key #'cadr :key #'car)
          (count 1 '(1 2 1) :test #'eql :test #'/=)
          (remove 1 '(1 2 1) :count 1 :count nil)
          (remove-duplicates '(1 2 1) :from-end t :from-end nil)
          (substitute 9 1 '(1 2 1) :count 1 :count 2)
          (rassoc 2 '((a . 1) (b . 2)) :test #'eql :test #'/=)
          (mismatch '(1 2) '(1 3) :start1 0 :start1 1)))
  ((1 2) 1 2 (2 1) (1 2) (9 2 1) (b . 2) 1))

(deftest seq-keyword-pairs.allow-other-keys
  (let ((f #'find) (p #'position-if) (r #'remove-duplicates) (s #'substitute) (m #'mismatch))
    (list (funcall f 1 '(1) :foo 2 :allow-other-keys t)
          (funcall f 1 '(1) :allow-other-keys t :foo 2)
          (funcall p #'oddp '(2 3) :allow-other-keys nil :key #'identity)
          (funcall r '(1 1) :bar 0 :allow-other-keys t)
          (funcall s 0 1 '(1) :allow-other-keys t :zot 3)
          (funcall m '(1) '(1) :zot 3 :allow-other-keys t)
          (%skp-err (lambda () (funcall m '(1) '(1) :zot 3)))
          (%skp-err (lambda () (funcall f 1 '(1) :allow-other-keys nil :foo 2)))
          (%skp-err (lambda () (funcall f 1 '(1) :allow-other-keys nil :allow-other-keys t
                                        :foo 2)))))
  (1 1 1 (1) (0) nil :program-error :program-error :program-error))

(deftest seq-keyword-pairs.errors
  (let ((f #'find) (c #'count-if) (d #'delete) (r #'remove-duplicates))
    (list (%skp-err (lambda () (funcall f 1 '(1) :foo 2)))
          (%skp-err (lambda () (funcall f 1 '(1) :key #'identity :foo 2)))
          (%skp-err (lambda () (funcall c #'oddp '(1) 3 4)))
          (%skp-err (lambda () (funcall d 1 (list 1) :test)))
          (%skp-err (lambda () (funcall r '(1) :key)))
          (%skp-err (lambda () (funcall r '(1) :key #'identity :test)))))
  (:program-error :program-error :program-error :program-error :program-error
   :program-error))
