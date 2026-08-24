;;; ~F and ~G with no user-supplied digit count must print the shortest decimal
;;; that reads back to the same float, not the float's exact binary value.
;;;
;;; A single-float was widened to double before formatting, so the exact
;;; expansion came back out: (format nil "~G" 1.5e12) gave "1500000026624."
;;; where SBCL and ABCL give "1500000000000.". 1.5e12 IS 1500000026624 exactly,
;;; but 1500000000000 reads back to the same single-float and is shorter, which
;;; is what CLHS 22.1.3.1.3 asks the printer for.
;;;
;;; Only values past a single-float's precision show it; small ones agreed all
;;; along, and doubles were never affected.

(deftest format-g-shortest.single-float-large
  (list (format nil "~G" 1.5e12)
        (format nil "~F" 1.5e12)
        (format nil "~G" -1.5e12)
        (format nil "~G" 1.0e20)
        (format nil "~F" 1.0e20))
  ("1500000000000.    " "1500000000000.0" "-1500000000000.    "
   "100000000000000000000.    " "100000000000000000000.0"))

;;; Doubles were already right and must stay right.
(deftest format-g-shortest.double-float-unchanged
  (list (format nil "~G" 1.5d12)
        (format nil "~G" 1.0d20)
        (format nil "~F" 1.5d12))
  ("1500000000000.    " "100000000000000000000.    " "1500000000000.0"))

;;; Values a single-float represents exactly were never wrong; the fix must not
;;; move them.
(deftest format-g-shortest.small-values-unchanged
  (list (format nil "~G" 0.001)
        (format nil "~G" 15000000.0)
        (format nil "~G" 1.0e8)
        (format nil "~G" 1.0e10)
        (format nil "~G" 3.14159)
        (format nil "~G" 1.0)
        (format nil "~G" 0.0))
  ("1.000e-3" "15000000.    " "100000000.    " "10000000000.    "
   "3.14159    " "1.    " "0.    "))

;;; The exponential branch had the same defect: the exponent was read off the
;;; WIDENED double, and 1.0e-4 widens to 9.999999747378752e-5 -- one decade
;;; lower. Rounding those 9s used to carry back up and hide it.
(deftest format-g-shortest.exponential-branch-exponent
  (list (format nil "~G" 1.0e-4)
        (format nil "~G" 9.0e-4)
        (format nil "~G" 1.234e-5)
        (format nil "~E" 1.0e-4))
  ("1.0000e-4" "9.0000e-4" "1.23400000e-5" "1.0e-4"))

;;; In ~E a user-supplied d means "round the EXACT value to d digits":
;;; ansi-test FORMAT.E.26 checks ~,d,,0E against the float's rational, which is
;;; why that path rounds the rational here. SBCL prints the shortest
;;; representation there instead (0.150000000000000e+13) -- a deliberate
;;; divergence, kept because the conformance test drives it.
;;;
;;; ~F is the other way round: SBCL pads the shortest decimal with zeros, and
;;; so does this implementation's own count-free path, so the count-carrying one
;;; matches them rather than exposing the binary expansion.
(deftest format-g-shortest.explicit-d-in-e-rounds-exactly
  (format nil "~,15,,0E" 1.5e12)
  "0.150000002662400e+13")

(deftest format-g-shortest.explicit-d-in-f-uses-shortest
  (format nil "~,3F" 1.5e12)
  "1500000000000.000")
