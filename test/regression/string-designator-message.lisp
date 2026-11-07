;;; STRING of a non-designator names the designator type.

(deftest string-non-designator
  (handler-case (string 5) (type-error (e) (type-error-datum e)))
  5)

(deftest string-non-designator.message
  (handler-case (string 5)
    (type-error (e) (and (search "string designator" (princ-to-string e)) t)))
  t)
