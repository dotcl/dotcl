;;; A HANDLER-CASE clause no longer builds a handler function per entry: the
;;; binding carries the (tag, clause-index) pair and the signal walk transfers
;;; to the clause itself. Everything that reaches a clause has to keep working --
;;; the exception filter path (an error signalled inside the body), the
;;; HandlerClusterStack.Signal path (a condition signalled through handler-bind),
;;; and the interaction between the two.

(deftest handler-case-binding.catches-error
  (handler-case (error "boom") (error () :caught))
  :caught)

(deftest handler-case-binding.selects-by-type
  (handler-case (error 'type-error :datum 1 :expected-type 'string)
    (division-by-zero () :dbz)
    (type-error () :te)
    (error () :err))
  :te)

(deftest handler-case-binding.clause-variable-is-the-condition
  (handler-case (error "msg here") (error (c) (princ-to-string c)))
  "msg here")

(deftest handler-case-binding.no-error-clause
  (handler-case (values 1 2) (:no-error (a b) (list :ok a b)) (error () :err))
  (:ok 1 2))

(deftest handler-case-binding.body-values-pass-through
  (multiple-value-list (handler-case (values 1 2 3) (error () :e)))
  (1 2 3))

(deftest handler-case-binding.nested-inner-wins
  (handler-case (handler-case (error "x") (error () :inner)) (error () :outer))
  :inner)

(deftest handler-case-binding.nested-outer-when-inner-does-not-match
  (handler-case (handler-case (error "x") (division-by-zero () :inner)) (error () :outer))
  :outer)

(deftest handler-case-binding.declining-handler-bind-then-case
  ;; The handler-bind handler returns (declines); the enclosing handler-case
  ;; clause then takes it -- this is the Signal-walk path into a clause.
  (let ((log nil))
    (list (handler-case
              (handler-bind ((error (lambda (c) (declare (ignore c)) (push :declined log))))
                (error "x"))
            (error () :case))
          log))
  (:case (:declined)))

(deftest handler-case-binding.signal-not-error
  (handler-case (signal 'simple-condition) (condition () :signalled))
  :signalled)

(deftest handler-case-binding.warn-escalated-from-handler-bind
  (handler-case
      (handler-bind ((warning (lambda (c) (declare (ignore c)) (error "escalate"))))
        (warn "w"))
    (error () :escalated))
  :escalated)

(deftest handler-case-binding.raw-dotnet-error-maps-to-condition
  (handler-case (car 5) (type-error () :type-error) (error () :other))
  :type-error)

(deftest handler-case-binding.unwind-protect-cleanup-runs-first
  (let ((log nil))
    (handler-case (unwind-protect (error "u") (push :cleanup log))
      (error () (push :handler log)))
    (nreverse log))
  (:cleanup :handler))

(deftest handler-case-binding.ignore-errors
  (list (multiple-value-list (ignore-errors (+ 1 2)))
        (let ((r (multiple-value-list (ignore-errors (error "q")))))
          (list (first r) (typep (second r) 'error))))
  ((3) (nil t)))

(deftest handler-case-binding.deep-nesting-does-not-exhaust
  (labels ((f (n) (if (= n 0) (error "deep") (handler-case (f (1- n)) (division-by-zero () :nope)))))
    (handler-case (f 50) (error () :ok)))
  :ok)

(deftest handler-case-binding.many-clauses
  (list (handler-case (error 'division-by-zero)
          (type-error () :a) (parse-error () :b) (division-by-zero () :c) (error () :d))
        (handler-case (error "generic")
          (type-error () :a) (parse-error () :b) (division-by-zero () :c) (error () :d)))
  (:c :d))
