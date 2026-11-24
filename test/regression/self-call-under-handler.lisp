;;; A call of the function itself inside the body of HANDLER-CASE (or IGNORE-ERRORS) is not a
;;; tail call: it runs under the handler. Every expected value is what SBCL gives as well.

(defvar *sch-log* nil)

(defun sch-hc (n)
  (if (= n 0) (error "boom") (handler-case (sch-hc (1- n)) (error () (list :caught n)))))

(defun sch-hc-warn (n)
  (if (= n 0) (warn "w") (handler-case (sch-hc-warn (1- n)) (warning () (list :w n)))))

(defun sch-hc-type (n)
  (if (= n 0) (car 5) (handler-case (sch-hc-type (1- n)) (type-error () (list :te n)))))

(defun sch-hc-progn (n)
  (if (= n 0)
      (error "boom")
      (progn (push n *sch-log*)
             (handler-case (progn (sch-hc-progn (1- n))) (error () (list :c n))))))

(defun sch-hc-let (n)
  (if (= n 0)
      (error "boom")
      (let ((m (1- n))) (handler-case (sch-hc-let m) (error () (list :c n))))))

(defun sch-hc-cond (n)
  (cond ((= n 0) (error "boom"))
        (t (handler-case (sch-hc-cond (1- n)) (error () (list :c n))))))

(defun sch-hc-nested (n)
  (if (= n 0)
      (error "boom")
      (handler-case (handler-case (sch-hc-nested (1- n)) (type-error () :te))
        (error () (list :c n)))))

(defun sch-hc-noerror (n)
  (if (= n 0)
      7
      (handler-case (sch-hc-noerror (1- n)) (error () :c) (:no-error (v) (list :ne n v)))))

(defun sch-ie (n)
  (if (= n 0) (error "boom") (ignore-errors (sch-ie (1- n)))))

(defun sch-plain (n acc)
  (if (= n 0) acc (sch-plain (1- n) (+ acc n))))

(defun sch-hc-outside (n)
  (if (= n 0) (error "boom") (sch-hc-outside (1- n))))

(deftest self-call-under-handler.case
  (sch-hc 3)
  (:caught 1))

(deftest self-call-under-handler.warning
  (sch-hc-warn 3)
  (:w 1))

(deftest self-call-under-handler.type-error
  (sch-hc-type 3)
  (:te 1))

(deftest self-call-under-handler.progn
  (progn (setq *sch-log* nil) (list (sch-hc-progn 3) *sch-log*))
  ((:c 1) (1 2 3)))

(deftest self-call-under-handler.let
  (sch-hc-let 3)
  (:c 1))

(deftest self-call-under-handler.cond
  (sch-hc-cond 3)
  (:c 1))

(deftest self-call-under-handler.nested
  (sch-hc-nested 3)
  (:c 1))

(deftest self-call-under-handler.no-error
  (sch-hc-noerror 3)
  (:ne 3 (:ne 2 (:ne 1 7))))

(deftest self-call-under-handler.ignore-errors
  (null (sch-ie 3))
  t)

(deftest self-call-under-handler.plain-tail
  (sch-plain 100000 0)
  5000050000)

(deftest self-call-under-handler.outside
  (handler-case (sch-hc-outside 3) (error () :outer))
  :outer)

(deftest self-call-under-handler.labels
  (labels ((f (n) (if (= n 0) (error "boom") (handler-case (f (1- n)) (error () (list :c n))))))
    (f 3))
  (:c 1))
