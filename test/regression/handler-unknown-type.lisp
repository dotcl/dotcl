;;; A handler clause naming a type that does not exist: signalling a condition
;;; finds the type unknown, and that error goes to the handlers outside the
;;; clause. It used to recurse until the process died of a stack overflow.

(deftest handler-unknown-type.outer-handler-sees-the-error
  (handler-case
      (handler-case (error "a") (no-such-type-hut () :inner))
    (error (e) (and (search "NO-SUCH-TYPE-HUT" (princ-to-string e)) :outer)))
  :outer)

(deftest handler-unknown-type.known-types-still-match
  (list (handler-case (handler-case (error "a") (no-such-type-hut () :inner) (error () :known))
          (error () :outer))
        (handler-case (error "b") (simple-error () :simple)))
  (:outer :simple))
