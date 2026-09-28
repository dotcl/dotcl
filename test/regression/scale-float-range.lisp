;;; SCALE-FLOAT multiplies by 2^SCALE with one rounding. It was FLOAT times
;;; (EXPT 2 SCALE), so the answer was lost whenever that power of two did not
;;; fit the float format by itself: a single-float denormal came out as 0.0 and
;;; a result back in range from a denormal or a huge scale came out infinite.

(defun %sf-decode (x) (multiple-value-list (integer-decode-float x)))

(deftest scale-float-range.single-denormal
  (list (%sf-decode (scale-float 71.0 -149))
        (%sf-decode (scale-float 3.0 -149))
        (%sf-decode (scale-float 1.5 -130))
        (%sf-decode (scale-float -71.0 -149)))
  ((71 -149 1) (3 -149 1) (786432 -149 1) (71 -149 -1)))

(deftest scale-float-range.single-rounds-once
  ;; 3 * 2^-150 and 5 * 2^-150 are halfway cases: ties go to the even value.
  (list (%sf-decode (scale-float 3.0 -150))
        (%sf-decode (scale-float 5.0 -150)))
  ((2 -149 1) (2 -149 1)))

(deftest scale-float-range.back-into-range
  (list (scale-float least-positive-single-float 149)
        (scale-float least-positive-double-float 1074)
        (= (scale-float 0.5 128) (float (expt 2 127) 1.0))
        (%sf-decode (scale-float 1d0 -1074))
        (%sf-decode (scale-float 1d300 -2000)))
  (1.0 1.0d0 t (1 -1074 1) (6724873095247260 -1056 1)))

(deftest scale-float-range.underflow-to-zero
  (list (scale-float 1.0 -200) (scale-float 1d0 -1100) (scale-float 0.0 5))
  (0.0 0.0d0 0.0))

(deftest scale-float-range.type-errors
  (list (handler-case (scale-float 1 2) (type-error () :type-error))
        (handler-case (scale-float 1.0 1.5) (type-error () :type-error)))
  (:type-error :type-error))

;;; DECODE-FLOAT divided by a separately built 2^exponent as well, and 2^1024 is
;;; not a double: every double >= 2^1023 decoded to a 0.0 significand.
(deftest scale-float-range.decode-float-top-of-range
  (list (multiple-value-list (decode-float (scale-float 0.75d0 1024)))
        (multiple-value-list (decode-float most-positive-double-float))
        (multiple-value-list (decode-float (- most-positive-double-float)))
        (multiple-value-list (decode-float least-positive-double-float))
        (multiple-value-list (decode-float (scale-float 0.75d0 -1070)))
        (multiple-value-list (decode-float most-positive-single-float))
        (multiple-value-list (decode-float least-positive-single-float)))
  ((0.75d0 1024 1.0d0)
   (0.9999999999999999d0 1024 1.0d0)
   (0.9999999999999999d0 1024 -1.0d0)
   (0.5d0 -1073 1.0d0)
   (0.75d0 -1070 1.0d0)
   (0.99999994 128 1.0)
   (0.5 -148 1.0)))
