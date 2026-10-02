;;; ~F (and ~$, and ~G through ~F) round the float's exact binary value when
;;; the digits dropped from its shortest decimal are exactly "5".
;;;
;;; 2.675d0 is really 2.67499999999999982236..., so (format nil "~,2F" 2.675d0)
;;; is "2.67", as in SBCL. dotcl rounded the shortest decimal "2.675" half-up
;;; and printed "2.68". cl-colors2 prints (* 255 0.203) = 51.765 (a single-float
;;; whose exact value is 51.76499938...) with ~,2F and expects "51.76".
;;; A true tie (exact in binary) still rounds away from zero, as SBCL does.
;;; Expected values below are SBCL's.

(defun %fxr (control &rest values)
  (mapcar (lambda (x) (format nil control x)) values))

(deftest format-fixed-exact-rounding.below-midpoint
  (%fxr "~,2F" 2.675d0 1.005d0 -2.675d0 -1.005d0 9.995d0 0.995d0)
  ("2.67" "1.00" "-2.67" "-1.00" "9.99" "0.99"))

(deftest format-fixed-exact-rounding.above-midpoint
  ;; 1.05d0 and 0.05d0 are a hair above their midpoints.
  (%fxr "~,1F" 1.05d0 0.05d0 0.05)
  ("1.1" "0.1" "0.1"))

(deftest format-fixed-exact-rounding.single-float
  (%fxr "~,2F" (* 255 0.203) 51.765 -51.765)
  ("51.76" "51.76" "-51.76"))

(deftest format-fixed-exact-rounding.true-ties
  (append (%fxr "~,2F" 0.125d0 0.125 -0.125d0 0.375d0)
          (%fxr "~,0F" 2.5d0 3.5d0 0.5d0 -2.5d0))
  ("0.13" "0.13" "-0.13" "0.38" "3." "4." "1." "-3."))

(deftest format-fixed-exact-rounding.width-and-general
  (list (format nil "~8,2F" 2.675d0) (format nil "~,3G" 2.675d0)
        (format nil "~,1F" 0.15d0) (format nil "~,1F" 0.35d0))
  ("    2.67" "2.67    " "0.1" "0.3"))

;; The scale factor shifts the decimal digits instead of multiplying the float,
;; and a tie is still decided on the unscaled float's exact value.
(deftest format-fixed-exact-rounding.scale-factor
  (list (format nil "~,2,1F" 1.2345678901234567d20)
        (format nil "~,2,1F" 0.0015d0)
        (format nil "~,2,1F" 2.675d0))
  ("1234567890123456700000.00" "0.02" "26.75"))

(deftest format-fixed-exact-rounding.large-values
  (%fxr "~,2F" 1.5e12 1d23)
  ("1500000000000.00" "100000000000000000000000.00"))

;; ~$ rounds the same way; .NET's "F" format printed a single-float's exact
;; binary value and rounded an exact tie to even.
(deftest format-fixed-exact-rounding.dollar
  (list (format nil "~$" 2.675d0) (format nil "~$" 0.125d0) (format nil "~@$" -0.125d0)
        (format nil "~$" 1.5e12) (format nil "~$" 51.765) (format nil "~,,10$" 1.005d0))
  ("2.67" "0.13" "-0.13" "1500000000000.00" "51.76" "      1.00"))
