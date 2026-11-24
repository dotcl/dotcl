;;; A closure that forwards its &REST list with APPLY and some fixed arguments calls the target through its
;;; own fixed-arity entry when there are four arguments or fewer, and through an array above that: the same
;;; answers (including multiple values and the errors) for every split of fixed and rest arguments.

(defun %arf-0 (f) (lambda (&rest more) (apply f more)))
(defun %arf-1 (f a) (lambda (&rest more) (apply f a more)))
(defun %arf-2 (f a b) (lambda (&rest more) (apply f a b more)))
(defun %arf-3 (f a b c) (lambda (&rest more) (apply f a b c more)))
(defun %arf-mv (f a) (lambda (&rest more) (apply f a more)))

(deftest apply-rest-forward-small.counts
  (flet ((l (&rest xs) (copy-list xs)))
    (list (funcall (%arf-0 #'l)) (funcall (%arf-0 #'l) 1 2 3 4 5)
          (funcall (%arf-1 #'l :a)) (funcall (%arf-1 #'l :a) 1) (funcall (%arf-1 #'l :a) 1 2 3)
          (funcall (%arf-1 #'l :a) 1 2 3 4) (funcall (%arf-1 #'l :a) 1 2 3 4 5 6)
          (funcall (%arf-2 #'l :a :b)) (funcall (%arf-2 #'l :a :b) 1 2) (funcall (%arf-2 #'l :a :b) 1 2 3)
          (funcall (%arf-3 #'l :a :b :c) 1) (funcall (%arf-3 #'l :a :b :c) 1 2 3)
          (funcall (%arf-1 #'+ 1) 2) (funcall (%arf-1 #'+ 1) 2 3 4 5)
          (funcall (%arf-1 '+ 1) 2)))
  (() (1 2 3 4 5) (:a) (:a 1) (:a 1 2 3) (:a 1 2 3 4) (:a 1 2 3 4 5 6)
   (:a :b) (:a :b 1 2) (:a :b 1 2 3) (:a :b :c 1) (:a :b :c 1 2 3) 3 15 3))

(deftest apply-rest-forward-small.values-and-errors
  (list (multiple-value-list (funcall (%arf-mv #'floor 7) 2))
        (multiple-value-list (funcall (%arf-mv #'values 1) 2 3))
        (handler-case (funcall (%arf-1 #'car 1) 2) (error () :error))
        (handler-case (funcall (%arf-1 nil 1) 2) (error () :error))
        (let ((f (%arf-1 #'list 1))) (list (funcall f) (funcall f 2) (funcall f 2 3))))
  ((3 1) (1 2 3) :error :error ((1) (1 2) (1 2 3))))

(defun %arf-loop (f n) (declare (fixnum n)) (let ((x nil)) (dotimes (i n) (setq x (funcall f 5))) x))

(deftest-compiled-only apply-rest-forward-small.alloc
  (let ((f (%arf-1 #'+ 1)))
    (%arf-loop f 10)
    ;; the caller's argument array (32 bytes) and nothing else: no array for the fixed arguments, none
    ;; for the combined argument list
    (< (/ (min-allocated-bytes (lambda () (%arf-loop f 100000))) 100000.0) 48))
  t)

;;; (MULTIPLE-VALUE-CALL f (VALUES-LIST list) (VALUES-LIST rest)), the shape of alexandria's CURRY and
;;; RCURRY, takes the rest from the argument array, in either order.

(defun %arf-curry (f &rest args) (lambda (&rest more) (multiple-value-call f (values-list args) (values-list more))))
(defun %arf-rcurry (f &rest args) (lambda (&rest more) (multiple-value-call f (values-list more) (values-list args))))

(deftest apply-rest-forward-small.values-list-pair
  (flet ((l (&rest xs) (copy-list xs)))
    (list (funcall (%arf-curry #'l)) (funcall (%arf-curry #'l 1)) (funcall (%arf-curry #'l 1) 2)
          (funcall (%arf-curry #'l 1 2) 3 4) (funcall (%arf-curry #'l 1 2 3) 4 5 6)
          (funcall (%arf-rcurry #'l 1) 2) (funcall (%arf-rcurry #'l 1 2) 3 4 5)
          (funcall (%arf-rcurry #'l) 7) (funcall (%arf-curry #'+ 1) 2)
          (funcall (%arf-rcurry #'- 1) 10)
          (multiple-value-list (funcall (%arf-curry #'floor 7) 2))
          (handler-case (funcall (%arf-curry #'car) 1 2) (error () :error))))
  (nil (1) (1 2) (1 2 3 4) (1 2 3 4 5 6) (2 1) (3 4 5 1 2) (7) 3 9 (3 1) :error))

(defun %arf-curry-loop (f n) (declare (fixnum n)) (let ((x nil)) (dotimes (i n) (setq x (funcall f 5))) x))

(deftest-compiled-only apply-rest-forward-small.values-list-pair-alloc
  (let ((f (%arf-curry #'+ 1)))
    (%arf-curry-loop f 10)
    (< (/ (min-allocated-bytes (lambda () (%arf-curry-loop f 100000))) 100000.0) 48))
  t)
