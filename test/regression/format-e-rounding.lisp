;;; ~E must round the VALUE, not a rounded decimal of it.
;;;
;;; The digits used to come from ToString("G17") — enough to round-trip a double,
;;; not enough to round one. Rounding those 17 digits again at 15 is a double
;;; rounding, and the two disagree when the discarded part sits at the tie:
;;; G17 shows a trailing 5 with nothing after it, so a value just above the tie
;;; looks exactly on it. The values below are two such doubles, found by the same
;;; comparison the (randomised) ANSI test FORMAT.E.26 makes — which is why that
;;; test failed only on some runs.

;;; exact value 1051215004350025020997632 → 15 digits round UP
(deftest format-e-rounding.above-tie
  (format nil "~,15,,0e" 1.051215004350025d24)
  "0.105121500435003E+25")

;;; exact value 9.480733862209814993681932...e-226 → 15 digits round DOWN
(deftest format-e-rounding.below-tie
  (format nil "~,15,,0e" 9.480733862209815d-226)
  "0.948073386220981E-225")

;;; Fewer digits than the precision limit are unaffected.
(deftest format-e-rounding.fourteen-digits
  (format nil "~,14,,0e" 1.051215004350025d24)
  "0.10512150043500E+25")

(deftest format-e-rounding.short
  (list (format nil "~,3,,0e" 1.2345d0) (format nil "~,2e" 1234.5d0))
  ("0.123E+1" "1.23E+3"))

;;; A subnormal, where the exact expansion is hundreds of digits long.
(deftest format-e-rounding.subnormal
  (format nil "~,15,,0e" least-positive-double-float)
  "0.494065645841247E-323")

;;; The digit-count-free form still prints the round-trip representation.
(deftest format-e-rounding.default-digits
  (format nil "~e" 1.051215004350025d24)
  "1.051215004350025E+24")

;;; Direct check against exact rational arithmetic, for the two known values and a
;;; handful of ordinary ones: the printed digits must equal the exact value rounded
;;; to that many significant digits (ties may go either way, so both are accepted).
(defun fer-digits (x d)
  (let ((s (let ((*read-default-float-format* 'double-float))
             (format nil (format nil "~~,~d,,0e" d) x))))
    (subseq s (1+ (position #\. s)) (position #\e s :test #'char-equal))))

(defun fer-exact-digits (x d)
  "The first D significant digits of X's exact value, as (down . up)."
  (let* ((r (rational x))
         (scale (- d 1 (floor (log (abs x) 10))))
         (scaled (* r (expt 10 scale)))
         (down (floor scaled))
         (up (ceiling scaled)))
    ;; ceiling can carry to D+1 digits (999... → 1000...); the printer would then
    ;; re-normalise, so compare only when it does not.
    (cons (princ-to-string down)
          (princ-to-string (if (> up down) up down)))))

(defun fer-ok-p (x d)
  (let ((got (fer-digits x d))
        (exact (fer-exact-digits x d)))
    (or (string= got (car exact)) (string= got (cdr exact)))))

(deftest format-e-rounding.matches-exact-rational
  (let ((bad '()))
    (dolist (x (list 1.051215004350025d24 9.480733862209815d-226
                     1.2345d0 3.14159265358979d0 2.5d-5 1.0d100 7.0d0))
      (dolist (d '(1 3 7 14 15))
        (unless (fer-ok-p x d) (push (list x d) bad))))
    bad)
  nil)
