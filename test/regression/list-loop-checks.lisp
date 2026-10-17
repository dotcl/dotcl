;;; The list loops (DOLIST, DO over CDR, LOOP FOR IN) keep their checks and
;;; bindings: a dotted tail or a non-list is a TYPE-ERROR, CAR / CDR of a
;;; non-list are TYPE-ERRORs, and DOLIST binds its variable afresh per element.

(defun %llc-dolist-sum (l)
  (let ((s 0)) (dolist (x l s) (incf s x))))

(defun %llc-closures (l)
  (let ((fs '())) (dolist (x l) (push (lambda () x) fs)) (mapcar #'funcall fs)))

(defun %llc-car (x) (declare (optimize (safety 1))) (car x))
(defun %llc-cdr (x) (declare (optimize (safety 1))) (cdr x))

(defun %llc-do-cars (l)
  (let ((r '())) (do ((p l (cdr p))) ((null p) (nreverse r)) (push (car p) r))))

(defun %llc-type-error-datum (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (c) (type-error-datum c))))

(deftest list-loop-checks.dolist
  (list (%llc-dolist-sum '(1 2 3)) (%llc-dolist-sum '()))
  (6 0))

(deftest list-loop-checks.dolist-dotted
  (%llc-type-error-datum (lambda () (%llc-dolist-sum '(1 2 . 3))))
  3)

(deftest list-loop-checks.dolist-not-a-list
  (%llc-type-error-datum (lambda () (%llc-dolist-sum 7)))
  7)

(deftest list-loop-checks.dolist-fresh-binding
  (%llc-closures '(1 2 3))
  (3 2 1))

(deftest list-loop-checks.car-cdr
  (list (%llc-car '(1 . 2)) (%llc-cdr '(1 . 2)) (%llc-car nil) (%llc-cdr nil)
        (%llc-type-error-datum (lambda () (%llc-car 5)))
        (%llc-type-error-datum (lambda () (%llc-cdr "s"))))
  (1 2 nil nil 5 "s"))

(deftest list-loop-checks.do-dotted
  (list (%llc-do-cars '(1 2 3))
        (%llc-type-error-datum (lambda () (%llc-do-cars '(1 2 . 3)))))
  ((1 2 3) 3))
