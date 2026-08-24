;;; The exponent marker: which letter, and in which case.
;;;
;;; PRIN1 already picked the letter by CLHS 22.1.3.1.3 -- omit it when the float's
;;; type is *READ-DEFAULT-FLOAT-FORMAT*, otherwise name the type (f / d) -- but
;;; wrote the default-type marker in upper case ("1.0E10") while writing the
;;; others in lower ("1.0d10"). ~E ignored the rule entirely and emitted a bare
;;; upper-case E for every float, so a double printed as 1.2345E+3 where the
;;; standard (and every other implementation) says 1.2345d+3.
;;;
;;; Every expectation below was checked against SBCL 2.6.6 and matches it exactly.

;;; PRIN1: marker present only when the type differs from the default, and the
;;; letter is lower case throughout.
(deftest float-exponent-marker.prin1-single-default
  (let ((*read-default-float-format* 'single-float))
    (list (prin1-to-string 1.0e10) (prin1-to-string 1.0d10)))
  ("1.0e10" "1.0d10"))

(deftest float-exponent-marker.prin1-double-default
  (let ((*read-default-float-format* 'double-float))
    (list (prin1-to-string 1.0e10) (prin1-to-string 1.0d10)))
  ("1.0f10" "1.0e10"))

;;; ~E takes the same letter as PRIN1 would, and always shows the exponent sign.
(deftest float-exponent-marker.tilde-e-single-default
  (let ((*read-default-float-format* 'single-float))
    (list (format nil "~e" 1234.5) (format nil "~e" 1234.5d0)))
  ("1.2345e+3" "1.2345d+3"))

(deftest float-exponent-marker.tilde-e-double-default
  (let ((*read-default-float-format* 'double-float))
    (list (format nil "~e" 1234.5) (format nil "~e" 1234.5d0)))
  ("1.2345f+3" "1.2345e+3"))

;;; An explicit exptchar parameter still wins over both.
(deftest float-exponent-marker.explicit-exptchar
  (list (format nil "~,,,,,,'EE" 1234.5)
        (format nil "~,,,,,,'qE" 1234.5d0))
  ("1.2345E+3" "1.2345q+3"))

;;; ~E with no digit count still prints the round-trip representation: the digits
;;; PRIN1 would use, not one fewer. (The lookup that finds the marker inside the
;;; printer's output reads it case-insensitively; when it missed, this dropped a
;;; digit.)
(deftest float-exponent-marker.tilde-e-keeps-round-trip-digits
  (let ((*read-default-float-format* 'double-float)
        (x 2.3356982399544044d296))
    (let* ((s (format nil "~e" x))
           (p (prin1-to-string x))
           (ep (1+ (position #\e p :test #'char-equal))))
      (list s (string= s (concatenate 'string (subseq p 0 ep) "+" (subseq p ep))))))
  ("2.3356982399544044e+296" t))

;;; What the reader does with it is unchanged: either case reads back.
(deftest float-exponent-marker.reads-back
  (list (= (read-from-string "1.0e10") (read-from-string "1.0E10"))
        (= (read-from-string (prin1-to-string 1.5e10)) 1.5e10)
        (= (read-from-string (prin1-to-string 1.5d10)) 1.5d10))
  (t t t))
