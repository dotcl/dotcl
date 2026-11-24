;;; A macro that takes &ENVIRONMENT may be given arguments that are patterns, not forms
;;; (SERAPEUM:CASE-OF clauses name a form shape in the car). The mutation analysis leaves
;;; such a call to code generation without walking its arguments as code.

(defmacro epa-shape-case (form &environment env &body clauses)
  (declare (ignore env))
  `(let ((x ,form))
     (cond ,@(loop for ((shape . rest) . body) in clauses
                   collect `((and (consp x) (eq (car x) ',shape)) ,@body)))))

(defun epa-classify (form)
  (let ((count 0))
    (flet ((bump () (incf count)))
      (epa-shape-case form
        ((destructuring-bind lambda-list expr &body body) (bump) :bind)
        (((flet labels) bindings &body body) (bump) :fun)
        ((multiple-value-bind vars expr &body body) (bump) :mvb)
        ((symbol-macrolet binds &body body) (bump) :symbol-macrolet)
        ((declaim &rest s) (bump) :declaim)
        ((defmacro name args &body body) (bump) :macro))
      count)))

(deftest env-macro-pattern-arguments.compile
  (list (epa-classify '(destructuring-bind a b))
        (epa-classify '(declaim x))
        (epa-classify '(other)))
  (1 1 0))

(deftest env-macro-pattern-arguments.closure-sees-assignment
  (let ((n 0))
    (let ((fn (lambda () n)))
      (epa-shape-case '(declaim)
        ((declaim) (setq n 5)))
      (funcall fn)))
  5)
