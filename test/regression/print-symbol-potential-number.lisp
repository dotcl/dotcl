;;; PRIN1 escapes a symbol name only when it would read back as a number or as
;;; a potential number (CLHS 2.3.1.1), not merely because it starts with a
;;; digit. 1!3 (a character that cannot be in a number) and 1AB / 3D-VECTORS
;;; (letters next to each other are never number markers) were printed as
;;; |1!3|, |1AB| and |3D-VECTORS|; mutility splits the printed name of 1!3 at
;;; the ! and read |1 back as an unterminated escape. Expected values are SBCL
;;; 2.6.8's answers.

(deftest print-symbol-potential-number.plain-symbols
  (mapcar #'prin1-to-string
          (list '|1!3| '|1AB| '|12AB3| '|3D-VECTORS| '|-.| '|1-| '|1+|))
  ("1!3" "1AB" "12AB3" "3D-VECTORS" "-." "1-" "1+"))

(deftest print-symbol-potential-number.escaped
  (mapcar #'prin1-to-string
          (list '|1A| '|1.2.3| '|1+2| '|1/2/3| '|1E5X| '|+1A| '|.5Q| '|1^2| '|1_2|
                '|1X2Y3| '|1.| '|1A.B| '|12| '|1/2| '|1.5|))
  ("|1A|" "|1.2.3|" "|1+2|" "|1/2/3|" "|1E5X|" "|+1A|" "|.5Q|" "|1^2|" "|1_2|"
   "|1X2Y3|" "|1.|" "|1A.B|" "|12|" "|1/2|" "|1.5|"))

;; What is printed reads back as the same symbol.
(deftest print-symbol-potential-number.round-trip
  (let ((*package* (find-package "CL-USER")))
    (loop for name in '("1!3" "1AB" "12AB3" "3D-VECTORS" "1A" "1.2.3" "1E5X" "1.")
          for sym = (intern name "CL-USER")
          always (eq sym (read-from-string (prin1-to-string sym)))))
  t)

;; In base 16 the letters A-F are digits, so 1AB is an integer there.
(deftest print-symbol-potential-number.base-16
  (let ((*print-base* 16))
    (mapcar #'prin1-to-string (list '|1AB| '|1G| '|1GH|)))
  ("|1AB|" "|1G|" "1GH"))
