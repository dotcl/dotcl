;;; A dotted AND / OR / COND / PROGN / PROG1 / PROG2 / LOCALLY form signals
;;; PROGRAM-ERROR (EVAL used to crash or ignore the tail); the valid shapes,
;;; including the empty ones, still work.

(defmacro %sfd-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest special-form-shape-dotted.invalid
  (values (%sfd-pe (and t . :foo)) (%sfd-pe (and t . 1)) (%sfd-pe (or nil . :bar))
          (%sfd-pe (cond . 1)) (%sfd-pe (progn . 0)) (%sfd-pe (progn 1 . 2))
          (%sfd-pe (prog1 1 . 2)) (%sfd-pe (locally . 1)))
  :program-error :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error)

(deftest special-form-shape-dotted.valid
  (values (eval '(and)) (eval '(or)) (eval '(cond)) (eval '(cond (1)))
          (eval '(progn)) (eval '(progn 1 2)) (eval '(prog1 1)) (eval '(prog2 1 2))
          (eval '(locally)) (eval '(locally 3)) (eval '(and 1 2)) (eval '(or nil 5)))
  t nil nil 1 nil 2 1 2 nil 3 2 5)
