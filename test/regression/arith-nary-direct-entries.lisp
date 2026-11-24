;;; #'+ #'- #'* #'/ called with 3 or 4 arguments (funcall, apply, curry) take direct entries that
;;; fold left exactly as the n-ary wrapper does; errors and values are unchanged.

(defvar *adn-p* #'+)
(defvar *adn-m* #'-)
(defvar *adn-t* #'*)
(defvar *adn-d* #'/)

(defun adn-err (thunk)
  (handler-case (funcall thunk)
    (type-error (e) (list :type-error (type-error-datum e)))
    (division-by-zero () :division-by-zero)))

(deftest arith-nary-direct-entries.values
  (list (funcall *adn-p* 1 2 3) (funcall *adn-p* 1 2 3 4) (funcall *adn-p* 1 2 3 4 5) (funcall *adn-p* 1.5 2 3/2)
        (funcall *adn-m* 10 3 2) (funcall *adn-m* 10 3 2 1)
        (funcall *adn-t* 2 3 4) (funcall *adn-t* 2 3 4 5) (funcall *adn-t* 1/2 2/3 3/4)
        (funcall *adn-d* 100 5 2) (funcall *adn-d* 100 5 2 2) (funcall *adn-d* 7 2 1)
        (funcall *adn-p* 1.0d0 2 3) (funcall *adn-p* #c(1 2) 3 4)
        (apply *adn-p* '(1 2 3)) (apply *adn-m* '(5 1 1)) (apply *adn-d* '(8 2 2))
        (reduce *adn-p* '(1 2 3 4 5)) (mapcar *adn-p* '(1 2) '(3 4) '(5 6)))
  (6 10 15 5.0 5 4 24 120 1/4 10 5 7/2 6.0d0 #c(8 2) 6 3 2 15 (9 12)))

(deftest arith-nary-direct-entries.errors
  (list (adn-err (lambda () (funcall *adn-p* 1 2 'a)))
        (adn-err (lambda () (funcall *adn-p* 'a 2 3)))
        (adn-err (lambda () (funcall *adn-p* 1 'a 3 4)))
        (adn-err (lambda () (funcall *adn-p* 1 2 3 "x")))
        (adn-err (lambda () (funcall *adn-m* 10 3 'b)))
        (adn-err (lambda () (funcall *adn-t* 2 'z 4)))
        (adn-err (lambda () (funcall *adn-d* 1 0 2)))
        (adn-err (lambda () (funcall *adn-d* 1 2 0))))
  ((:type-error a) (:type-error a) (:type-error a) (:type-error "x") (:type-error b) (:type-error z)
   :division-by-zero :division-by-zero))

(deftest arith-nary-direct-entries.big-values
  (list (funcall *adn-p* (expt 10 30) 1 1) (funcall *adn-t* (expt 10 20) (expt 10 20) 2)
        (funcall *adn-m* (- (expt 10 30)) 1 1))
  (1000000000000000000000000000002 20000000000000000000000000000000000000000 -1000000000000000000000000000002))
