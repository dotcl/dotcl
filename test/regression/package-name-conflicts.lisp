;;; Name conflicts in USE-PACKAGE, EXPORT, IMPORT and UNINTERN.

(defvar *pnc-counter* 0)

(defun pnc-name (tag)
  (format nil "PNC-~A-~A" tag (incf *pnc-counter*)))

(defmacro pnc-let (bindings &body body)
  (let ((vars (mapcar #'car bindings)))
    `(let* ,(mapcar (lambda (b)
                      `(,(car b) (make-package (pnc-name ,(cadr b)) ,@(cddr b))))
                    bindings)
       (unwind-protect
            (progn ,@body)
         (dolist (p (reverse (list ,@vars)))
           (ignore-errors (delete-package p)))))))

(defun pnc-restart-names (condition)
  (sort (mapcar (lambda (r) (string (restart-name r)))
                (compute-restarts condition))
        #'string<))

(deftest package-use-conflict-offers-restarts
  (pnc-let ((a "A") (b "B"))
    (export (intern "X" a) a)
    (let ((old (intern "X" b))
          names)
      (handler-bind ((package-error
                       (lambda (c)
                         (setq names (pnc-restart-names c))
                         (invoke-restart (find-restart 'take-new c)))))
        (use-package a b))
      (list names
            (eq (find-symbol "X" a) (find-symbol "X" b))
            (eq old (find-symbol "X" b)))))
  (("CONTINUE" "KEEP-OLD" "RESOLVE-CONFLICT" "TAKE-NEW") t nil))

(deftest package-use-inherited-conflict-resolves
  (pnc-let ((a "A") (b "B") (c "C"))
    (export (intern "X" a) a)
    (let ((new (intern "X" b)))
      (export new b)
      (use-package a c)
      (handler-bind ((package-error
                       (lambda (c)
                         (invoke-restart (find-restart 'take-new c)))))
        (use-package b c))
      (list (eq new (find-symbol "X" c))
            (mapcar #'symbol-name (package-shadowing-symbols c)))))
  (t ("X")))

(deftest package-export-conflict-keep-old
  (pnc-let ((a "A") (b "B"))
    (use-package a b)
    (let ((s (intern "X" a))
          (old (intern "X" b))
          names)
      (handler-bind ((package-error
                       (lambda (c)
                         (setq names (pnc-restart-names c))
                         (invoke-restart (find-restart 'keep-old c)))))
        (export s a))
      (list names
            (eq old (find-symbol "X" b))
            (member "X" (mapcar #'symbol-name (package-shadowing-symbols b))
                    :test #'string=))))
  (("CONTINUE" "KEEP-OLD" "RESOLVE-CONFLICT" "TAKE-NEW") t ("X")))

(deftest package-import-conflict-keeps-existing
  (pnc-let ((a "A") (b "B"))
    (let ((new (intern "X" a))
          (old (intern "X" b))
          names)
      (handler-bind ((package-error
                       (lambda (c)
                         (setq names (pnc-restart-names c))
                         (invoke-restart (find-restart 'dont-import-it c)))))
        (import new b))
      (list names (eq old (find-symbol "X" b)))))
  (("CONTINUE" "DONT-IMPORT-IT" "RESOLVE-CONFLICT" "SHADOWING-IMPORT-IT") t))

(deftest package-import-conflict-shadowing-imports-new
  (pnc-let ((a "A") (b "B"))
    (let ((new (intern "X" a)))
      (intern "X" b)
      (handler-bind ((package-error
                       (lambda (c)
                         (invoke-restart
                          (find-restart 'shadowing-import-it c)))))
        (import new b))
      (list (eq new (find-symbol "X" b))
            (mapcar #'symbol-name (package-shadowing-symbols b)))))
  (t ("X")))

(deftest package-import-inherited-conflict-continues
  (pnc-let ((a "A") (b "B" :use '(:cl)))
    (let ((new (intern "CAR" a)))
      (handler-bind ((package-error
                       (lambda (c)
                         (invoke-restart (find-restart 'continue c)))))
        (import new b))
      (eq new (find-symbol "CAR" b))))
  t)

(deftest package-unintern-conflict-continues
  (pnc-let ((a "A") (b "B") (c "C"))
    (export (intern "X" a) a)
    (export (intern "X" b) b)
    (use-package a c)
    (shadow "X" c)
    (use-package b c)
    (let (names)
      (handler-bind ((package-error
                       (lambda (e)
                         (setq names (pnc-restart-names e))
                         (invoke-restart (find-restart 'continue e)))))
        (let ((removed (unintern (find-symbol "X" c) c)))
          (list names
                removed
                (mapcar #'symbol-name (package-shadowing-symbols c))
                (not (null (find-symbol "X" c))))))))
  (("CONTINUE" "RESOLVE-CONFLICT") t nil t))

(deftest package-unintern-uses-symbol-identity
  (pnc-let ((p "P" :use '(:cl)))
    (shadow 'car p)
    (let ((shadowing (find-symbol "CAR" p)))
      (list (eq shadowing 'car)
            (unintern 'car p)
            (eq shadowing (find-symbol "CAR" p)))))
  (nil nil t))

(deftest with-package-iterator-exhausted-returns-one-nil
  (pnc-let ((p "P"))
    (with-package-iterator (next (list p) :internal :external)
      (multiple-value-list (next))))
  (nil))

(deftest with-package-iterator-rejects-unknown-symbol-type
  (pnc-let ((p "P"))
    (handler-case
        (progn
          (with-package-iterator (next (list p) :bogus) (next))
          :no-error)
      (program-error () :program-error)
      (error () :other-error)))
  :program-error)
