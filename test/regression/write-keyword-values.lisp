;;; WRITE checks the values of its printer keywords: :CASE is :UPCASE, :DOWNCASE or :CAPITALIZE;
;;; :LEVEL :LENGTH :LINES :MISER-WIDTH :RIGHT-MARGIN are NIL or a non-negative integer. The values
;;; next to the bad ones (NIL, 0, a bignum, each :CASE) are accepted.

(defun %wkv-class (thunk) (handler-case (funcall thunk) (type-error () :type-error)))

(defun %wkv-write (object &rest keys)
  (with-output-to-string (s) (apply #'write object :stream s keys)))

(deftest write-keyword-values.rejected
  (list (%wkv-class (lambda () (%wkv-write 'abc :case :bogus)))
        (%wkv-class (lambda () (%wkv-write 'abc :case "UPCASE")))
        (%wkv-class (lambda () (%wkv-write '(1 2) :level -1)))
        (%wkv-class (lambda () (%wkv-write '(1 2) :length -1)))
        (%wkv-class (lambda () (%wkv-write '(1 2) :length "x")))
        (%wkv-class (lambda () (%wkv-write '(1 2) :length 1.5)))
        (%wkv-class (lambda () (%wkv-write 5 :right-margin -3)))
        (%wkv-class (lambda () (%wkv-write 5 :miser-width -1)))
        (%wkv-class (lambda () (%wkv-write 5 :lines -2)))
        (%wkv-class (lambda () (%wkv-write 5 :base 'a)))
        (%wkv-class (lambda () (%wkv-write 5 :base nil)))
        (%wkv-class (lambda () (%wkv-write 5 :base 1)))
        (%wkv-class (lambda () (%wkv-write 5 :base 37)))
        (%wkv-class (lambda () (%wkv-write 5 :base 2.0))))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error :type-error :type-error
   :type-error :type-error :type-error :type-error :type-error))

(deftest write-keyword-values.accepted
  (list (%wkv-write 'abc :case :upcase)
        (%wkv-write 'abc :case :downcase)
        (%wkv-write 'abc :case :capitalize)
        (%wkv-write '(1 2 3) :length nil)
        (%wkv-write '(1 2 3) :length 0)
        (%wkv-write '(1 2 3) :level 0)
        (%wkv-write '(1 2 3) :length (expt 10 30))
        (%wkv-write '(1 2 3) :length 2)
        (%wkv-write 5 :right-margin nil :miser-width nil :lines nil)
        (%wkv-write 5 :right-margin 0 :miser-width 0 :lines 0)
        (%wkv-write 10 :base 2)
        (%wkv-write 255 :base 16)
        (%wkv-write 35 :base 36))
  ("ABC" "abc" "Abc" "(1 2 3)" "(...)" "#" "(1 2 3)" "(1 2 ...)" "5" "5" "1010" "FF" "Z"))
