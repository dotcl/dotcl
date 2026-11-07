;;; A MACROLET macro may have a dotted lambda list (a . rest), as DEFMACRO does.

(deftest macrolet-dotted-lambda-list.compile
  (let ((dotcl:*evaluator-mode* :compile))
    (eval '(macrolet ((m (a . b) `(quote ,(list a b)))) (m 1 2 3))))
  (1 (2 3)))

(deftest macrolet-dotted-lambda-list.interpret
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(macrolet ((m (a . b) `(quote ,(list a b)))) (m 1 2 3))))
  (1 (2 3)))

(deftest macrolet-dotted-lambda-list.no-tail
  (let ((dotcl:*evaluator-mode* :compile))
    (eval '(macrolet ((m (a . b) `(quote ,(list a b)))) (m 1))))
  (1 nil))

(deftest macrolet-dotted-lambda-list.optional-and-environment
  (let ((dotcl:*evaluator-mode* :compile))
    (eval '(macrolet ((m (&environment e a &optional (b 2) . c) `(quote ,(list a b c (and e t))))) (m 1 5 6))))
  (1 5 (6) t))
