;;; A &rest variable declared DYNAMIC-EXTENT is replaced by the extra arguments exactly as one
;;; without the declaration is, and a use that needs the list still gets a list.

(defun %rde-apply-dx (g &rest r) (declare (dynamic-extent r)) (apply g r))
(defun %rde-apply (g &rest r) (apply g r))
(defun %rde-car-dx (&rest r) (declare (dynamic-extent r)) (car r))
(defun %rde-length-dx (&rest r) (declare (dynamic-extent r)) (length r))
(defun %rde-nth-dx (&rest r) (declare (dynamic-extent r)) (list (nth 1 r) (null r)))
(defun %rde-nested-dx (g h &rest r) (declare (dynamic-extent r)) (apply g (apply h r)))
(defun %rde-mixed-decls (&rest r) (declare (ignorable r) (dynamic-extent r)) (length r))
(defun %rde-escape-copy (&rest r) (declare (dynamic-extent r)) (copy-list r))
(defun %rde-escape-call (&rest r) (declare (dynamic-extent r)) (list (length r) (reverse r)))
(defun %rde-escape-map (f &rest r) (declare (dynamic-extent r)) (mapcar f r))
(defun %rde-escape-setq (&rest r) (declare (dynamic-extent r)) (setq r (cons 0 r)) (length r))
(defun %rde-required-dx (a b &rest r) (declare (dynamic-extent r)) (list a b (length r) (apply #'+ r)))

(deftest rest-dx.apply-arities
  (loop for n from 0 to 7
        collect (apply #'%rde-apply-dx #'+ (loop for i from 1 to n collect i)))
  (0 1 3 6 10 15 21 28))

(deftest rest-dx.apply-same-as-undeclared
  (loop for n from 0 to 7
        always (equal (apply #'%rde-apply-dx #'list (loop for i from 1 to n collect i))
                      (apply #'%rde-apply #'list (loop for i from 1 to n collect i))))
  t)

(deftest rest-dx.car (list (%rde-car-dx) (%rde-car-dx 7) (%rde-car-dx 7 8 9)) (nil 7 7))
(deftest rest-dx.length (list (%rde-length-dx) (%rde-length-dx 1) (%rde-length-dx 1 2 3 4 5 6))
  (0 1 6))
(deftest rest-dx.nth-null (list (%rde-nth-dx) (%rde-nth-dx 1) (%rde-nth-dx 1 2 3))
  ((nil t) (nil nil) (2 nil)))
(deftest rest-dx.nested (list (%rde-nested-dx #'list #'list)
                              (%rde-nested-dx #'+ #'list 1 2 3)
                              (%rde-nested-dx #'+ #'reverse '(1 2 3)))
  (nil 6 6))
(deftest rest-dx.mixed-declarations (list (%rde-mixed-decls) (%rde-mixed-decls 1 2)) (0 2))
(deftest rest-dx.escape-copy (list (%rde-escape-copy) (%rde-escape-copy 1 2 3)) (nil (1 2 3)))
(deftest rest-dx.escape-call (list (%rde-escape-call) (%rde-escape-call 1 2 3))
  ((0 nil) (3 (3 2 1))))
(deftest rest-dx.escape-map (list (%rde-escape-map #'1+) (%rde-escape-map #'1+ 1 2 3))
  (nil (2 3 4)))
(deftest rest-dx.escape-setq (list (%rde-escape-setq) (%rde-escape-setq 1 2)) (1 3))
(deftest rest-dx.required-and-rest
  (list (%rde-required-dx 1 2) (%rde-required-dx 1 2 3 4))
  ((1 2 0 0) (1 2 2 7)))

(defun %rde-bytes (thunk)
  (let ((before (nth 4 (dotcl:gc-stats))))
    (funcall thunk)
    (- (nth 4 (dotcl:gc-stats)) before)))

(defun %rde-run-apply (n) (let ((c 0)) (dotimes (i n) (when (%rde-apply-dx #'evenp i) (incf c))) c))
(defun %rde-run-car (n) (let ((c 0)) (dotimes (i n) (when (%rde-car-dx i) (incf c))) c))
(defun %rde-run-copy (n) (let ((c 0)) (dotimes (i n) (when (%rde-escape-copy i) (incf c))) c))

;; The substituted entries make no list: the allocation of 20000 calls is a small constant, not 32 bytes
;; a call; a form that needs the list still allocates it.
(deftest-compiled-only rest-dx.apply-makes-no-list
  (progn (%rde-run-apply 1000) (< (%rde-bytes (lambda () (%rde-run-apply 20000))) 100000)) t)
(deftest-compiled-only rest-dx.car-makes-no-list
  (progn (%rde-run-car 1000) (< (%rde-bytes (lambda () (%rde-run-car 20000))) 100000)) t)
(deftest-compiled-only rest-dx.escape-still-conses
  (progn (%rde-run-copy 1000) (> (%rde-bytes (lambda () (%rde-run-copy 20000))) 500000)) t)

