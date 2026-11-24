;;; FIND / POSITION (and the -IF / -IF-NOT forms) with three keyword pairs take
;;; a fixed-arity entry rather than the variadic one. Results and errors are
;;; those of the variadic path.

(deftest find-3kw.test-start-end
  (list (find #\Z "ab-zZ-q" :test #'char-equal :start 1 :end 6)
        (find #\z "ab-zZ-q" :test #'char-equal :start 4 :end 6)
        (find #\q "ab-zZ-q" :test #'char-equal :start 0 :end 6)
        (find 3 '(1 2 3 4) :test #'= :start 1 :end 4))
  (#\z #\Z nil 3))

(deftest find-3kw.key-from-end
  (list (find 2 '((1 a) (2 b) (2 c)) :key #'car :from-end t :start 0)
        (find 2 '((1 a) (2 b) (2 c)) :key #'car :test #'= :end 3)
        (find 5 #(1 2 3) :key #'1+ :test #'= :end 3))
  ((2 c) (2 b) nil))

(deftest find-3kw.if
  (list (find-if #'evenp '(1 3 4 6) :key #'identity :start 0 :end 4)
        (find-if #'evenp '(1 3 4 6) :from-end t :start 1 :end 4 :key #'identity)
        (find-if-not #'oddp #(1 3 4 6) :start 1 :end 4 :key #'identity))
  (4 6 4))

(deftest position-3kw.test-start-end
  (list (position #\Z "ab-zZ-q" :test #'char-equal :start 1 :end 6)
        (position #\z "ab-zZ-q" :test #'char-equal :from-end t :end 6)
        (position 9 '(1 9 9) :test #'= :start 2 :end 3)
        (position 9 '(1 9 9) :test #'= :start 0 :end 1))
  (3 4 2 nil))

(deftest position-3kw.if
  (list (position-if #'evenp '(1 3 4 6) :key #'identity :start 0 :end 4)
        (position-if #'evenp '(1 3 4 6) :from-end t :start 1 :end 4 :key #'identity)
        (position-if-not #'oddp #(1 3 4 6) :start 1 :end 4 :key #'identity))
  (2 3 2))

(deftest find-3kw.errors
  (list (handler-case (find 1 '(1) :test #'= :start 0 :bogus 1) (error () :error))
        (find 1 '(1) :test #'= :start 0 :bogus 1 :allow-other-keys t)
        (handler-case (position 1 '(1) :start 2 :end 1 :test #'=) (error () :error))
        (handler-case (find-if #'evenp '(1) :start 0 :bogus 1 :end 1) (error () :error)))
  (:error 1 :error :error))
