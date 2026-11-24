;;; = < > <= >= and the character comparisons check every argument, also those
;;; after the point where the answer is known (CLHS 12.2, 13.2).

(defun %cce-lt (a b c) (< a b c))
(defun %cce-eq (a b c) (= a b c))

(deftest comparison-checks.compiled-chain
  (list (%cce-lt 1 2 3) (%cce-lt 3 2 1)
        (handler-case (%cce-lt 3 2 'x) (type-error () :type-error))
        (handler-case (%cce-lt 1 2 #c(1 2)) (type-error () :type-error))
        (handler-case (%cce-eq 1 2 'x) (type-error () :type-error))
        (%cce-eq 1 1 1) (%cce-eq 1 2 1))
  (t nil :type-error :type-error :type-error t nil))

(deftest comparison-checks.function-object
  (list (handler-case (apply #'< '(1 0 x)) (type-error () :type-error))
        (handler-case (apply #'= '(1 0 x)) (type-error () :type-error))
        (handler-case (apply #'>= '(1 3 x 1)) (type-error () :type-error))
        (apply #'< '(1 2 3)) (apply #'= '(1 1 1)))
  (:type-error :type-error :type-error t t))

(deftest comparison-checks.characters
  (list (handler-case (apply #'char< '(#\b #\a x)) (type-error () :type-error))
        (handler-case (funcall (lambda (a b c) (char< a b c)) #\b #\a 'x) (type-error () :type-error))
        (handler-case (apply #'char-equal '(#\b #\a x)) (type-error () :type-error))
        (char< #\a #\b #\c) (char-equal #\a #\A #\a))
  (:type-error :type-error :type-error t t))
