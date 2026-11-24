;;; DIGIT-CHAR of a weight past the fixnum range is NIL (a weight past the radix gives
;;; NIL), not a type error; a negative weight is still a type error.

(deftest digit-char-bignum.nil
  (list (digit-char 9461162448055937524 10) (digit-char (expt 10 30) 36) (digit-char (expt 2 64) 2)
        (digit-char 5 10) (digit-char 11 16) (digit-char 100 10))
  (nil nil nil #\5 #\B nil))

(deftest digit-char-bignum.negative-is-error
  (list (handler-case (digit-char -1 10) (type-error () :type-error))
        (handler-case (digit-char (- (expt 10 30)) 10) (type-error () :type-error)))
  (:type-error :type-error))
