;;; UNINTERN takes a symbol (a string, character or number is a TYPE-ERROR), and a
;;; package argument of NIL to INTERN names the package "NIL", not the current one.

(defun %piu-error-type (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error () :type-error)
    (package-error () :package-error)))

(deftest package-intern-unintern.unintern-non-symbol
  (let ((p (make-package "PIU-A" :use nil)))
    (unwind-protect
         (progn (intern "X" p)
                (list (%piu-error-type (lambda () (unintern "X" p)))
                      (%piu-error-type (lambda () (unintern #\X p)))
                      (%piu-error-type (lambda () (unintern 42 p)))
                      (nth-value 1 (find-symbol "X" p))))
      (delete-package p)))
  (:type-error :type-error :type-error :internal))

(deftest package-intern-unintern.unintern-symbols
  (let ((p (make-package "PIU-B" :use nil)))
    (unwind-protect
         (let ((s (intern "Y" p)))
           (list (unintern s p) (find-symbol "Y" p) (unintern s p) (unintern nil p)))
      (delete-package p)))
  (t nil nil nil))

(deftest package-intern-unintern.intern-nil-package
  (let ((p (make-package "PIU-C" :use nil)))
    (unwind-protect
         (let ((*package* p))
           (list (%piu-error-type (lambda () (intern "X" nil)))
                 (%piu-error-type (lambda () (intern "X" t)))
                 (let ((r (multiple-value-list (intern "X"))))
                   (list (package-name (symbol-package (first r))) (second r)))
                 (let ((r (multiple-value-list (intern "X" "PIU-C"))))
                   (list (package-name (symbol-package (first r))) (second r)))))
      (delete-package p)))
  (:package-error :package-error ("PIU-C" nil) ("PIU-C" :internal)))
