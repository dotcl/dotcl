;;; LOGCOUNT counts bits with a population count instead of a generic
;;; subtract-and-mask loop per bit (Coalton's HashMap counts the bits of every
;;; node bitmap on each insert and spent most of its time there). The answers
;;; are the CLHS ones: 1 bits of a non-negative integer, 0 bits of a negative
;;; one.

(deftest logcount-native.values
  (list (logcount 0) (logcount 13) (logcount -1) (logcount -13)
        (logcount most-positive-fixnum) (logcount most-negative-fixnum)
        (logcount (expt 2 100)) (logcount (- (expt 2 100)))
        (logcount (1- (expt 2 200))) (logcount (- (expt 2 200)))
        (funcall #'logcount 7) (mapcar #'logcount '(1 2 3)))
  (0 3 0 2 63 63 1 100 200 200 3 (1 1 2)))

(deftest logcount-native.type-error
  (handler-case (logcount 1.5) (type-error () :type-error))
  :type-error)
