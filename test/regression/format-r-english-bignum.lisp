;;; ~R and ~:R print English words for integers beyond the Int64 range.
;;;
;;; CLHS 22.3.2.1 places no upper bound on the argument of the cardinal and
;;; ordinal forms. dotcl converted it to a 64-bit integer, so anything larger
;;; signalled an overflow error instead of printing.
;;;
;;; Expected values are SBCL 2.6.8's.

(deftest format-r-english-bignum.cardinal
  (list (format nil "~R" (expt 10 20))
        (format nil "~R" (+ 1 (expt 10 20)))
        (format nil "~R" (* 123 (expt 10 18)))
        (format nil "~R" (- (expt 10 20)))
        (format nil "~R" (expt 10 30))
        (format nil "~R" (1+ (expt 10 30)))
        (format nil "~R" (expt 2 100)))
  ("one hundred quintillion"
   "one hundred quintillion one"
   "one hundred twenty-three quintillion"
   "negative one hundred quintillion"
   "one nonillion"
   "one nonillion one"
   "one nonillion two hundred sixty-seven octillion six hundred fifty septillion six hundred sextillion two hundred twenty-eight quintillion two hundred twenty-nine quadrillion four hundred one trillion four hundred ninety-six billion seven hundred three million two hundred five thousand three hundred seventy-six"))

(deftest format-r-english-bignum.ordinal
  (list (format nil "~:R" (expt 10 20))
        (format nil "~:R" (+ 1 (expt 10 20)))
        (format nil "~:R" (* 123 (expt 10 18)))
        (format nil "~:R" (- (expt 10 20)))
        (format nil "~:R" (expt 10 30))
        (format nil "~:R" (1+ (expt 10 30)))
        (format nil "~:R" (expt 2 100)))
  ("one hundred quintillionth"
   "one hundred quintillion first"
   "one hundred twenty-three quintillionth"
   "negative one hundred quintillionth"
   "one nonillionth"
   "one nonillion first"
   "one nonillion two hundred sixty-seven octillion six hundred fifty septillion six hundred sextillion two hundred twenty-eight quintillion two hundred twenty-nine quadrillion four hundred one trillion four hundred ninety-six billion seven hundred three million two hundred five thousand three hundred seventy-sixth"))
