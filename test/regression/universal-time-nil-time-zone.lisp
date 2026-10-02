;;; DECODE-UNIVERSAL-TIME and ENCODE-UNIVERSAL-TIME take a NIL time zone as no
;;; time zone given (the current one), as SBCL does. Libraries pass their own
;;; optional TIME-ZONE argument straight through (net-telent-date's
;;; UNIVERSAL-TIME-TO-RFC2822-DATE does). NIL used to signal "Not a number".

(deftest universal-time-nil-time-zone.decode
  (equal (multiple-value-list (decode-universal-time 3900000000 nil))
         (multiple-value-list (decode-universal-time 3900000000)))
  t)

(deftest universal-time-nil-time-zone.encode
  (= (encode-universal-time 0 0 0 1 1 2000 nil)
     (encode-universal-time 0 0 0 1 1 2000))
  t)

(deftest universal-time-nil-time-zone.explicit-zone-unchanged
  (list (nth-value 2 (decode-universal-time 0 0))
        (encode-universal-time 0 0 0 1 1 1900 0))
  (0 0))
