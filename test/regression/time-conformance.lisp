;;; APROPOS-LIST inherited symbols; ENCODE-UNIVERSAL-TIME pre-1900 rejection.

(deftest time-conformance.apropos-list-inherited-car
  (and (member 'car (apropos-list "CAR" "CL-USER")) t)
  t)

(deftest time-conformance.apropos-list-inherited-random
  (and (member 'random (apropos-list "RANDOM" "CL-USER")) t)
  t)

(deftest time-conformance.encode-before-base-type-error
  (handler-case (progn (encode-universal-time 0 0 0 1 1 1900 -9) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest time-conformance.encode-base
  (encode-universal-time 0 0 0 1 1 1900 0)
  0)

(deftest time-conformance.encode-before-base-nonnegative
  (encode-universal-time 0 0 20 31 12 1899 5)
  3600)

(deftest time-conformance.decode-explicit-zone
  (decode-universal-time 3900000000 0)
  0 20 21 2 8 2023 2 nil 0)

;;; Around the universal-time origin, as SBCL 2.6.8 answers: a result before
;;; 1900-01-01 00:00:00 GMT is a TYPE-ERROR, anything from it on encodes.

(deftest time-conformance.encode-origin-eastern-zones-type-error
  (loop for tz from -1 downto -24
        unless (handler-case (progn (encode-universal-time 0 0 0 1 1 1900 tz) nil)
                 (type-error () t))
          collect tz)
  nil)

(deftest time-conformance.encode-origin-western-zones-round-trip
  (loop for tz from 0 to 24
        for ut = (encode-universal-time 0 0 0 1 1 1900 tz)
        unless (and (= ut (* tz 3600))
                    (equal (multiple-value-list (decode-universal-time ut tz))
                           (list 0 0 0 1 1 1900 0 nil tz)))
          collect tz)
  nil)

(deftest time-conformance.encode-origin-boundary
  (list (encode-universal-time 0 0 1 1 1 1900 -1)
        (encode-universal-time 59 59 23 31 12 1899 1)
        (encode-universal-time 0 0 1 31 12 1899 23)
        (encode-universal-time 0 0 0 1 1 1900 1/2))
  (0 3599 0 1800))

(deftest time-conformance.encode-just-before-origin-type-error
  (loop for args in '((59 59 0 1 1 1900 -1)
                      (59 59 23 31 12 1899 0)
                      (0 0 0 31 12 1899 23)
                      (0 0 0 1 12 1899 0)
                      (0 0 0 1 1 1900 -1/2))
        unless (handler-case (progn (apply #'encode-universal-time args) nil)
                 (type-error () t))
          collect args)
  nil)
