;;; ~G without a digit count takes d from the shortest round-trip digits, so a number outside the
;;; fixed range prints those digits and zeros (as SBCL does), not the exact binary expansion.
;;; A d the user writes still rounds the exact value (FORMAT.E.26).

(deftest format-g-no-digit-count.double
  (list (format nil "~G" 1d-20)
        (format nil "~G" 1d-30)
        (format nil "~G" 5.983936418786249d-13)
        (format nil "~G" 1d-5)
        (format nil "~G" 1d-4)
        (format nil "~G" 1.234d-9))
  ("1.00000000000000000000d-20"
   "1.000000000000000000000000000000d-30"
   "5.9839364187862490000000000000d-13"
   "1.00000d-5"
   "1.0000d-4"
   "1.234000000000d-9"))

(deftest format-g-no-digit-count.single
  (list (format nil "~G" 1f-20)
        (format nil "~G" 1.5f-10)
        (format nil "~G" 1.234f-5))
  ("1.00000000000000000000e-20" "1.50000000000e-10" "1.23400000e-5"))

(deftest format-g-no-digit-count.very-small
  ;; 1d-300 used to print the 300+ digits of its binary expansion
  (length (format nil "~G" 1d-300))
  307)

(deftest format-g-no-digit-count.fixed-range-unchanged
  (list (format nil "~G" 123456789012345678d0) (format nil "~G" 0.5d0) (format nil "~G" 1.5d0))
  ("123456789012345680.    " "0.5    " "1.5    "))

(deftest format-g-no-digit-count.explicit-digits-unchanged
  (list (format nil "~,5G" 1d-20) (format nil "~,3,2G" 5.983936418786249d-13)
        (format nil "~12,5G" 1.5f-10))
  ("1.00000d-20" "5.984d-13" " 1.50000e-10"))
