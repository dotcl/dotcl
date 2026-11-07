;;; WARN takes a condition designator, as ERROR does: a condition, a condition
;;; type name, or a format control (a string or a function). Anything else is a
;;; type-error; a number or NIL used to be printed as the warning's text, and a
;;; function control printed as #<FUNCTION>.

(defun %wdd-warning-text (thunk)
  (handler-case (progn (funcall thunk) :no-warning)
    (warning (w) (princ-to-string w))
    (type-error () :type-error)))

(deftest warn-datum-designator.non-designators
  (mapcar (lambda (x) (%wdd-warning-text (lambda () (warn x))))
          (list 5 nil 1.5 #\a))
  (:type-error :type-error :type-error :type-error))

(deftest warn-datum-designator.function-control
  (list (%wdd-warning-text (lambda () (warn (lambda (s) (format s "fn ~A" 1)))))
        (%wdd-warning-text (lambda () (warn (formatter "n=~D") 7))))
  ("fn 1" "n=7"))

(deftest warn-datum-designator.string-control
  (list (%wdd-warning-text (lambda () (warn "plain")))
        (%wdd-warning-text (lambda () (warn "x~~y")))
        (%wdd-warning-text (lambda () (warn "n=~D" 3))))
  ("plain" "x~y" "n=3"))
