;;; (STEP form) evaluates FORM and returns its values: CLHS lets an implementation with no
;;; stepper do that. It was a macro that expanded to itself, then an undefined function.

(deftest step-macro.value
  (step (+ 1 2))
  3)

(deftest step-macro.multiple-values
  (multiple-value-list (step (values 1 2 3)))
  (1 2 3))

(deftest step-macro.expands-to-the-form
  (values (macroexpand-1 '(step (+ 1 2))))
  (+ 1 2))

(deftest step-macro.side-effects-once
  (let ((n 0)) (step (incf n)) n)
  1)

(deftest step-macro.wrong-argument-count
  (list (handler-case (macroexpand-1 '(step)) (error () :error))
        (handler-case (macroexpand-1 '(step 1 2)) (error () :error)))
  (:error :error))
