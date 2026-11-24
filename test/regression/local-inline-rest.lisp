;;; A local function declared INLINE whose lambda list ends in &REST is substituted at a call: the
;;; extra arguments are bound to a list, or, where the body only tests the variable and walks it
;;; with DOLIST, kept as values (no list). Each case runs the function inline and as a plain
;;; closure (NOTINLINE); the two answers agree.

(defmacro %lir-pair (lambda-list body &rest calls)
  `(let ((a (flet ((f ,lambda-list ,@body)) (declare (inline f)) (list ,@calls)))
         (b (flet ((f ,lambda-list ,@body)) (declare (notinline f)) (list ,@calls))))
     (if (equal a b) a (list :differ a b))))

(deftest local-inline-rest.collector
  (%lir-pair (&rest xs)
             ((let ((acc '()))
                (if xs (dolist (x xs) (push x acc)) (setq acc (list :none)))
                (nreverse acc)))
             (f) (f 1) (f 1 2) (f 1 2 3) (f 1 2 3 4) (f 1 2 3 4 5))
  ((:none) (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)))

(deftest local-inline-rest.required-and-rest
  (%lir-pair (a b &rest r) ((list a b r (length r) (if r :some :none)))
             (f 1 2) (f 1 2 3) (f 1 2 3 4 5))
  ((1 2 nil 0 :none) (1 2 (3) 1 :some) (1 2 (3 4 5) 3 :some)))

(deftest local-inline-rest.the-list-is-used-otherwise
  (%lir-pair (&rest r) ((declare (dynamic-extent r)) (reduce #'+ r))
             (f) (f 4) (f 1 2 3))
  (0 4 6))

(deftest local-inline-rest.returned-and-captured
  (%lir-pair (&rest r) ((list r (lambda () r)))
             (length (f)) (length (f 1 2)))
  (2 2))

(deftest local-inline-rest.early-return-from-the-walk
  (%lir-pair (&rest r) ((dolist (x r :end) (when (evenp x) (return (list :even x)))))
             (f) (f 1 3) (f 1 4 6) (f 2))
  (:end :end (:even 4) (:even 2)))

(deftest local-inline-rest.dolist-result-and-variable
  (%lir-pair (&rest r) ((let ((n 0)) (dolist (x r (list n x)) (incf n x))))
             (f) (f 5) (f 1 2 3))
  ((0 nil) (5 nil) (6 nil)))

(deftest local-inline-rest.evaluation-order
  (let ((log '()))
    (flet ((f (a &rest r) (declare (inline f)) (let ((out '())) (dolist (x r) (push x out)) (list a (nreverse out)))))
      (declare (inline f))
      (let ((v (f (progn (push :a log) 1) (progn (push :b log) 2) (progn (push :c log) 3))))
        (list v (reverse log)))))
  ((1 (2 3)) (:a :b :c)))

(deftest local-inline-rest.a-variable-named-like-the-rest-list
  (%lir-pair (&rest xs) ((let ((xs 5)) (dolist (y '(1 2)) (incf xs y)) xs))
             (f) (f 1))
  (8 8))

(deftest local-inline-rest.shadowing-and-wrong-arity
  (flet ((f (a &rest r) (declare (inline f)) (cons a r)))
    (declare (inline f))
    (list (f 1) (f 1 2) (handler-case (eval '(flet ((g (a b &rest r) (list a b r))) (declare (inline g)) (g 1))) (error () :error))))
  ((1) (1 2) :error))

(defun %lir-collect-n (n)
  (let* ((head (list nil)) (tail head))
    (flet ((collect (&rest xs)
             (if xs (dolist (x xs) (setf (cdr tail) (setf tail (list x)))) (cdr head))))
      (declare (inline collect))
      (dotimes (i n) (collect i))
      (length (cdr head)))))

(deftest-compiled-only local-inline-rest.no-list-per-call
  (progn (%lir-collect-n 1000)
         ;; One cons (32 bytes) and one boxed fixnum (24 bytes) per element; the old closure
         ;; call built a second list and a rest array for each.
         (list (< (min-allocated-bytes (lambda () (%lir-collect-n 100000))) 6000000)))
  (t))

(defun %lir-in-closure (test items)
  (flet ((test (&rest args) (apply test args)))
    (declare (inline test))
    (let ((k 1))
      (list (find-if (lambda (e) (test k e)) items :key #'car)
            (test k 1) (test k 2) (test k 3) (test k 4) (test k 5) (test k 6)
            (test k 7) (test k 8) (test k 9) (test k 10) (test k 11) (test k 12)))))

(deftest local-inline-rest.called-from-a-closure
  (%lir-in-closure #'eql '((2) (1) (3)))
  ((1) t nil nil nil nil nil nil nil nil nil nil nil))

(defun %lir-apply-wrap (fn items)
  (flet ((test (&rest args) (apply fn args)))
    (declare (inline test))
    (let ((n 0))
      (dolist (x items) (when (test x 1) (incf n)))
      n)))

(defun %lir-apply-lead (fn)
  (flet ((g (&rest args) (apply fn 10 20 args)))
    (declare (inline g))
    (list (g) (g 1) (g 1 2))))

(deftest local-inline-rest.apply-of-the-rest-list
  (list (%lir-apply-wrap #'< '(0 1 2 3 4))
        (%lir-apply-lead #'list)
        (flet ((h (&rest a) (apply #'+ a))) (declare (inline h)) (list (h) (h 1) (h 1 2 3))))
  (1 ((10 20) (10 20 1) (10 20 1 2)) (0 1 6)))

(deftest-compiled-only local-inline-rest.apply-wrapper-no-list
  (progn (%lir-apply-wrap #'< '(1 2))
         (let ((l (loop for i below 100000 collect i)))
           (list (< (min-allocated-bytes (lambda () (%lir-apply-wrap #'< l))) 1000000))))
  (t))

(defmacro %lir-fbind-like (name fn &body body)
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,fn))
       (declare (function ,tmp))
       (flet ((,name (&rest args) (declare (dynamic-extent args)) (apply ,tmp args)))
         (declare (inline ,name))
         ,@body))))

(defun %lir-forwarder (g items)
  (%lir-fbind-like test g
    (let ((k 2) (n 0))
      (dolist (x items) (when (find-if (lambda (e) (test k (+ e x))) '(1 2 3)) (incf n)))
      (list n (test 1 1) (test 1 2) (multiple-value-list (test 5 5))))))

(deftest local-inline-rest.forwarder-called-in-a-closure
  (%lir-forwarder #'= '(0 1 2 3))
  (2 t nil (t)))

(deftest local-inline-rest.forwarder-named-by-sharp-quote-keeps-the-function
  (%lir-fbind-like test #'+
    (list (funcall #'test 1 2) (mapcar #'test '(1 2) '(10 20)) (test)))
  (3 (11 22) 0))

(defun %lir-forwarder-bytes (g n)
  (%lir-fbind-like test g
    (let ((c 0)) (dotimes (i n) (when (find-if (lambda (e) (test i e)) '(1 2 3)) (incf c))) c)))

(deftest-compiled-only local-inline-rest.forwarder-closure-call-no-rest-list
  (progn (%lir-forwarder-bytes #'= 10)
         (list (< (min-allocated-bytes (lambda () (%lir-forwarder-bytes #'= 100000))) 4000000)))
  (t))
