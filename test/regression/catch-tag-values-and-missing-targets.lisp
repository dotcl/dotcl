;;; The tag form of CATCH is one value (the first of several, NIL for none), and a
;;; RETURN-FROM / GO to a block or tag that does not exist is a PROGRAM-ERROR.

(deftest catch-tag-values-and-missing-targets.catch-tag
  (list (catch (values 'x 'y) (throw 'x 261))
        (catch (values) (throw nil 263))
        (catch 'a (throw 'a 1))
        (multiple-value-list (catch 'a (values 1 2 3)))
        (catch 'q (catch 'r (throw 'q 5)))
        (catch (values 'k 'ignored) (catch 'other (throw 'k 7))))
  (261 263 1 (1 2 3) 5 7))

(deftest catch-tag-values-and-missing-targets.throw-without-catch
  (handler-case (throw 'no-such-catch-tag 1) (control-error () :control-error))
  :control-error)

(deftest catch-tag-values-and-missing-targets.missing-targets
  (list (handler-case (eval '(block b (return-from so-no-block 1))) (program-error () :program-error))
        (handler-case (eval '(loop for x in '(1 2) do (go foo))) (program-error () :program-error))
        (handler-case (eval '(tagbody (go nope))) (program-error () :program-error))
        (eval '(block a (return-from a 3)))
        (eval '(let ((n 0)) (tagbody (go l) (setq n 1) l) n)))
  (:program-error :program-error :program-error 3 0))

(deftest-emitting-only catch-tag-values-and-missing-targets.compiled
  (list (handler-case (funcall (compile nil '(lambda () (block b (return-from nope 1)))))
          (program-error () :program-error))
        (handler-case (funcall (compile nil '(lambda () (tagbody (go nope)))))
          (program-error () :program-error)))
  (:program-error :program-error))
