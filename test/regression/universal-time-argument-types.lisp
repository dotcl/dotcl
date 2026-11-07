;;; ENCODE-UNIVERSAL-TIME takes integer fields and a time zone from -24 to 24, and
;;; DECODE-UNIVERSAL-TIME a non-negative integer and the same kind of zone (SBCL:
;;; TYPE-ERROR). Fractions, an out-of-range zone and a negative time were taken. Valid
;;; calls, including a fractional zone such as 5.5 or 1/2, are unchanged.

(defun %uta (thunk) (handler-case (funcall thunk) (type-error () :type-error)))

(deftest universal-time-argument-types.encode-errors
  (list (%uta (lambda () (encode-universal-time 0 0 0 1 1 2000 25)))
        (%uta (lambda () (encode-universal-time 0 0 0 1 1 2000 -25)))
        (%uta (lambda () (encode-universal-time 1.5 0 0 1 1 2000 0)))
        (%uta (lambda () (encode-universal-time 0 0 0 1 1 2000.5 0)))
        (%uta (lambda () (encode-universal-time 0 0 "0" 1 1 2000 0))))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest universal-time-argument-types.decode-errors
  (list (%uta (lambda () (decode-universal-time -1)))
        (%uta (lambda () (decode-universal-time 1.5)))
        (%uta (lambda () (decode-universal-time 3000000000 25))))
  (:type-error :type-error :type-error))

(deftest universal-time-argument-types.valid-calls
  (list (encode-universal-time 0 0 0 1 1 2000 0)
        (encode-universal-time 0 0 0 1 1 2000 8)
        (encode-universal-time 0 0 0 1 1 2000 1/2)
        (encode-universal-time 0 0 0 1 1 2000 -24)
        (encode-universal-time 0 0 0 1 1 2000 24)
        (nth-value 5 (decode-universal-time 3155673600 0))
        (nth-value 8 (decode-universal-time 3155673600 5.5))
        (nth-value 5 (decode-universal-time 0 0)))
  (3155673600 3155702400 3155675400 3155587200 3155760000 2000 5.5 1900))
