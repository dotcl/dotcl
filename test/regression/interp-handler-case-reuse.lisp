;;; The tree-walk evaluator rewrites a HANDLER-CASE once per form and reuses
;;; the rewrite. Each evaluation still gets its own clause variable: a closure
;;; made in a clause keeps the condition of its own evaluation.

(deftest interp-handler-case-reuse.clause-variable-per-evaluation
  (let ((fns (loop for i below 3
                   collect (handler-case (error "c~d" i)
                             (error (e) (lambda () (princ-to-string e)))))))
    (mapcar #'funcall fns))
  ("c0" "c1" "c2"))

(deftest interp-handler-case-reuse.no-error-and-error-alternate
  (loop for i below 4
        collect (handler-case (if (oddp i) (error "odd") i)
                  (error () :caught)
                  (:no-error (v) (list :ok v))))
  ((:ok 0) :caught (:ok 2) :caught))

;; A recursive function whose HANDLER-CASE is entered again before the outer
;; one has finished.
(defun %ihcr-depth (n)
  (handler-case (if (zerop n) (error "bottom") (1+ (%ihcr-depth (1- n))))
    (error () 0)))

(deftest interp-handler-case-reuse.recursive
  (%ihcr-depth 5)
  5)
