;;; BLOCK, RETURN-FROM, GO, THROW, CATCH, UNWIND-PROTECT with a bad
;;; shape signal PROGRAM-ERROR; every valid shape still works.

(defmacro %sfb-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest special-form-shape-block.invalid
  (values (%sfb-pe (block)) (%sfb-pe (block 1 2)) (%sfb-pe (block "a"))
          (%sfb-pe (return-from)) (%sfb-pe (return-from a 1 2)) (%sfb-pe (return-from 1 2))
          (%sfb-pe (tagbody (go))) (%sfb-pe (tagbody (go a b) a)) (%sfb-pe (tagbody (go done . foo) done))
          (%sfb-pe (throw)) (%sfb-pe (throw 'a)) (%sfb-pe (throw 'a 1 2))
          (%sfb-pe (catch)) (%sfb-pe (unwind-protect)))
  :program-error :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error)

(deftest special-form-shape-block.valid
  (values (eval '(block a)) (eval '(block nil 1 2)) (eval '(block a (return-from a)))
          (eval '(block a (return-from a 5)))
          (eval '(tagbody (go done) done))
          (eval '(tagbody 1 (go 2) 2))
          (eval '(catch 'x (throw 'x 3)))
          (eval '(catch 'x))
          (eval '(unwind-protect 4))
          (eval '(progn)) (eval '(progn 1 2)))
  nil 2 nil 5 nil nil 3 nil 4 nil 2)
