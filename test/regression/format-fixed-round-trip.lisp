;;; The digit-count directives (~,dF and ~w,dF) take their digits from the
;;; shortest round-trip decimal too.
;;;
;;; The count-free ~F / ~G were fixed first. The branches that were GIVEN a digit
;;; count still rounded the widened double, so asking for more digits than the
;;; value carries exposed its binary expansion: (format nil "~,20F" 0.1d0) came
;;; out "0.10000000000000000555" and (format nil "~,10F" 1.5e12) came out
;;; "1500000026624.0000000000". SBCL pads the shortest decimal with zeros
;;; instead, which is also what this implementation's own count-free path does.
;;;
;;; Every expectation here matches SBCL 2.6.6.

(deftest format-fixed-round-trip.digit-count-pads-with-zeros
  (list (format nil "~,20F" 0.1d0)
        (format nil "~,20F" 0.1)
        (format nil "~,10F" 1.5e12)
        (format nil "~,2F" 1.0e30)
        (format nil "~,40F" 1.0e-30))
  ("0.10000000000000000000" "0.10000000000000000000"
   "1500000000000.0000000000"
   "1000000000000000000000000000000.00"
   "0.0000000000000000000000000000010000000000"))

(deftest format-fixed-round-trip.digit-count-still-rounds
  (list (format nil "~,3F" 16777216.0)
        (format nil "~,2F" 0.999)
        (format nil "~,1F" 9.99)
        (format nil "~,0F" 9.5)
        (format nil "~,25F" 1.0d0))
  ("16777216.000" "1.00" "10.0" "10." "1.0000000000000000000000000"))

;;; An exact tie rounds away from zero in BOTH directives. ~F already did;
;;; ~E rounded half to even, so ~G -- which picks between them -- disagreed with
;;; itself depending on which branch it took.
(deftest format-fixed-round-trip.ties-round-away-from-zero
  (list (format nil "~,1F" 1.25)
        (format nil "~,1F" 1.35)
        (format nil "~,3g" 1234.5)
        (format nil "~10,4g" 1234.5))
  ("1.3" "1.4" "1.235e+3" " 1235.    "))

;;; The width-only form chooses its own digit count; it reads from the same
;;; digits.
(deftest format-fixed-round-trip.width-only
  (list (format nil "~20F" 1.5e12)
        (format nil "~8F" 0.1))
  ("     1500000000000.0" "     0.1"))
