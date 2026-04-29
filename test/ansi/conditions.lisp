;;; conditions.lisp — ANSI condition system tests
;;; Phase 5.4a: handler-case, ignore-errors, error multi-arg, typep hierarchy

;;; ============================================================
;;; typep condition hierarchy
;;; ============================================================

(deftest typep-error-is-condition
  (handler-case (error "test")
    (error (c) (typep c 'condition)))
  t)

(deftest typep-error-is-error
  (handler-case (error "test")
    (error (c) (typep c 'error)))
  t)

(deftest typep-error-is-serious-condition
  (handler-case (error "test")
    (error (c) (typep c 'serious-condition)))
  t)

(deftest typep-type-error-is-error
  (handler-case (car 42)
    (error (c) (typep c 'error)))
  t)

(deftest typep-type-error-is-type-error
  (handler-case (car 42)
    (type-error (c) (typep c 'type-error)))
  t)

(deftest typep-type-error-is-condition
  (handler-case (car 42)
    (error (c) (typep c 'condition)))
  t)

;;; ============================================================
;;; handler-case basic
;;; ============================================================

(deftest handler-case-normal
  (handler-case 42
    (error (c) 99))
  42)

(deftest handler-case-catch-error
  (handler-case (error "boom")
    (error (c) 99))
  99)

(deftest handler-case-catch-with-var
  (handler-case (error "hello")
    (error (c) (typep c 'error)))
  t)

(deftest handler-case-catch-no-var
  (handler-case (error "hello")
    (error () 42))
  42)

;;; ============================================================
;;; handler-case type dispatch
;;; ============================================================

(deftest handler-case-dispatch-type-error
  (handler-case (car 42)
    (type-error (c) 'type-err)
    (error (c) 'generic-err))
  type-err)

(deftest handler-case-dispatch-generic-error
  (handler-case (error "general")
    (type-error (c) 'type-err)
    (error (c) 'generic-err))
  generic-err)

;;; ============================================================
;;; handler-case nested
;;; ============================================================

(deftest handler-case-nested
  (handler-case
      (handler-case (error "inner")
        (type-error (c) 'inner-type))
    (error (c) 'outer-error))
  outer-error)

(deftest handler-case-nested-catch-inner
  (handler-case
      (handler-case (car 42)
        (type-error (c) 'inner-type))
    (error (c) 'outer-error))
  inner-type)

;;; ============================================================
;;; handler-case + unwind-protect
;;; ============================================================

(deftest handler-case-unwind-protect
  (let ((cleanup-ran nil))
    (handler-case
        (unwind-protect
            (error "test")
          (setq cleanup-ran t))
      (error (c) cleanup-ran)))
  t)

;;; ============================================================
;;; handler-case body returns value
;;; ============================================================

(deftest handler-case-normal-value
  (handler-case (+ 1 2)
    (error (c) 99))
  3)

;;; ============================================================
;;; ignore-errors
;;; ============================================================

(deftest ignore-errors-no-error
  (ignore-errors 42)
  42)

(deftest ignore-errors-catches
  (multiple-value-bind (val cond)
      (ignore-errors (error "boom"))
    (and (null val) (typep cond 'error)))
  t)

;; ignore-errors returns the condition as second value
(deftest ignore-errors-condition-type
  (multiple-value-bind (val cond)
      (ignore-errors (error "boom"))
    (typep cond 'error))
  t)

;;; ============================================================
;;; error multi-arg (format string)
;;; ============================================================

(deftest error-format-basic
  (handler-case (error "~A is bad" 42)
    (error (c) t))
  t)

(deftest error-format-no-crash
  (handler-case (error "value ~A and ~S" 1 2)
    (error (c) 'caught))
  caught)

;;; ============================================================
;;; signals-error helper
;;; ============================================================

(deftest signals-error-true
  (signals-error (error "test") error)
  t)

(deftest signals-error-false
  (signals-error 42 error)
  nil)

(deftest signals-error-type-error
  (signals-error (car 42) type-error)
  t)

;;; ============================================================
;;; 5.4b: handler-bind
;;; ============================================================

(deftest handler-bind-no-error
  (handler-bind ((error (lambda (c) nil)))
    42)
  42)

(deftest handler-bind-called
  (let ((called nil))
    (handler-case
        (handler-bind ((error (lambda (c) (setq called t))))
          (error "test"))
      (error (c) called)))
  t)

(deftest handler-bind-decline
  ;; Handler returns normally → declines → error propagates to handler-case
  (handler-case
      (handler-bind ((error (lambda (c) nil)))
        (error "test"))
    (error (c) 'caught))
  caught)

(deftest handler-bind-with-handler-case
  ;; handler-bind is called before handler-case unwinds
  (let ((log nil))
    (handler-case
        (handler-bind ((error (lambda (c) (setq log 'noticed))))
          (error "test"))
      (error (c) log)))
  noticed)

;;; ============================================================
;;; 5.4b: signal
;;; ============================================================

;; signal is hard to test without make-condition; tested via handler-bind

;;; ============================================================
;;; 5.4b: warn
;;; ============================================================

(deftest warn-returns-nil
  (warn "test warning")
  nil)

(deftest warn-format
  (warn "~A is odd" 3)
  nil)

;;; ============================================================
;;; 5.4b: typep warning
;;; ============================================================

(deftest typep-warning-false
  (typep 42 'warning)
  nil)

;;; ============================================================
;;; 5.4c: restart-case basic
;;; ============================================================

(deftest restart-case-normal
  (restart-case 42
    (abort () 99))
  42)

(deftest restart-case-invoke
  (restart-case
      (invoke-restart 'use-value)
    (use-value () 77))
  77)

(deftest restart-case-invoke-with-arg
  (restart-case
      (invoke-restart 'use-value 42)
    (use-value (v) v))
  42)

(deftest restart-case-multiple-clauses
  (restart-case
      (invoke-restart 'second)
    (first () 1)
    (second () 2)
    (third () 3))
  2)

;;; ============================================================
;;; 5.4c: handler-bind + restart-case cooperation
;;; ============================================================

(deftest handler-bind-restart-case
  ;; handler-bind handler can invoke a restart established by restart-case
  (restart-case
      (handler-bind ((error (lambda (c) (invoke-restart 'use-value 99))))
        (error "test"))
    (use-value (v) v))
  99)

(deftest handler-bind-restart-case-abort
  (restart-case
      (handler-bind ((error (lambda (c) (invoke-restart 'abort))))
        (error "test"))
    (abort () 'aborted))
  aborted)

;;; ============================================================
;;; 5.4c: nested restart-case
;;; ============================================================

(deftest restart-case-nested
  (restart-case
      (restart-case
          (invoke-restart 'outer)
        (inner () 'inner-val))
    (outer () 'outer-val))
  outer-val)

(deftest restart-case-nested-inner
  (restart-case
      (restart-case
          (invoke-restart 'inner)
        (inner () 'inner-val))
    (outer () 'outer-val))
  inner-val)

;;; ============================================================
;;; 5.4c: find-restart
;;; ============================================================

(deftest find-restart-found
  (restart-case
      (notnot (find-restart 'use-value))
    (use-value () nil))
  t)

(deftest find-restart-not-found
  (find-restart 'nonexistent)
  nil)

;;; ============================================================
;;; 5.4c: cerror
;;; ============================================================

(deftest cerror-continue
  (handler-bind ((error (lambda (c) (invoke-restart 'continue))))
    (cerror "Continue anyway" "Something bad: ~A" 42)
    'continued)
  continued)

;;; ============================================================
;;; 5.4c: with-simple-restart
;;; ============================================================

(deftest with-simple-restart-normal
  (with-simple-restart (abort "Abort")
    42)
  42)

(deftest with-simple-restart-invoke
  (multiple-value-bind (val restarted)
      (with-simple-restart (abort "Abort")
        (invoke-restart 'abort))
    (and (null val) restarted))
  t)

;;; ============================================================
;;; 5.4c: restart-case + handler-case combined
;;; ============================================================

(deftest restart-handler-combined
  ;; Full CL idiom: restart-case wraps error, handler-bind invokes restart
  (handler-bind ((error (lambda (c)
                          (invoke-restart 'use-value 42))))
    (restart-case
        (error "need a value")
      (use-value (v) v)))
  42)

(do-tests-summary)
