;;; A LOOP variable initialized with (FUNCTION (LAMBDA ...)) used to throw to a tag
;;; nothing caught while LOOP estimated how large the initial form is.

(deftest loop-init-lambda.eval
  (list (eval '(loop for x = #'(lambda ()) for y = 10 then 20 return y))
        (eval '(loop for x = #'(lambda () 1) for y = 10 then 20 return y))
        (eval '(loop for x = #'(lambda (a) a) return (funcall x 5)))
        (eval '(loop for x = (function (lambda ())) repeat 1 collect (funcall x))))
  (10 10 5 (nil)))

(deftest-emitting-only loop-init-lambda.compiled
  (funcall (compile nil '(lambda () (loop for x = #'(lambda ()) for y = 10 then 20 return y))))
  10)
