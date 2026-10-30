;;; + - 1+ 1- on arguments of unknown type: fixnums that stay fixnums, results that
;;; leave the fixnums at either end, and the other kinds of number.

(defun uab-add (a b) (+ a b))
(defun uab-sub (a b) (- a b))
(defun uab-inc (a) (1+ a))
(defun uab-dec (a) (1- a))

(deftest untyped-arith-boundaries.fixnum-edges
  (list (uab-add 1 2) (uab-sub 1 2) (uab-inc -1) (uab-dec 0)
        (uab-add most-positive-fixnum 1) (uab-add most-negative-fixnum -1)
        (uab-add most-positive-fixnum most-negative-fixnum)
        (uab-sub most-negative-fixnum 1) (uab-sub most-positive-fixnum -1)
        (uab-sub -1 most-negative-fixnum) (uab-sub 0 most-negative-fixnum)
        (uab-inc most-positive-fixnum) (uab-dec most-negative-fixnum)
        (uab-dec (1+ most-positive-fixnum)) (uab-inc (1- most-negative-fixnum)))
  (3 -1 0 -1
   #.(1+ most-positive-fixnum) #.(1- most-negative-fixnum) -1
   #.(1- most-negative-fixnum) #.(1+ most-positive-fixnum)
   #.most-positive-fixnum #.(- most-negative-fixnum)
   #.(1+ most-positive-fixnum) #.(1- most-negative-fixnum)
   #.most-positive-fixnum #.most-negative-fixnum))

(deftest untyped-arith-boundaries.other-numbers
  (list (uab-add 1.5d0 1) (uab-sub 1/2 1) (uab-inc 1/2) (uab-dec 2.5f0)
        (uab-add #c(1 2) 1) (uab-sub (expt 2 70) (expt 2 70))
        (typep (uab-sub (expt 2 70) (expt 2 70)) 'fixnum)
        (handler-case (uab-add 1 'a) (type-error () :type-error))
        (handler-case (uab-dec "x") (type-error () :type-error)))
  (2.5d0 -1/2 3/2 1.5f0 #c(2 2) 0 t :type-error :type-error))
