;;; Package-name arguments are string designators: a number is a TYPE-ERROR, not a name made from its printed form.

(defun %pnd-class (thunk) (handler-case (progn (funcall thunk) :ok) (type-error () :type-error) (error () :error)))

(deftest package-name-designators.rejected
  (list (%pnd-class (lambda () (make-package 5)))
        (%pnd-class (lambda () (make-package "PND-A" :nicknames '(5))))
        (%pnd-class (lambda () (shadow 5)))
        (let ((p (make-package "PND-B"))) (%pnd-class (lambda () (rename-package p 7)))))
  (:type-error :type-error :type-error :type-error))

(deftest package-name-designators.accepted
  (let ((p (make-package :pnd-c :nicknames '("PND-D" #:pnd-e))))
    (prog1 (list (package-name p) (sort (copy-list (package-nicknames p)) #'string<)
                 (%pnd-class (lambda () (shadow '(pnd-f) p))))
      (delete-package p)))
  ("PND-C" ("PND-D" "PND-E") :ok))
