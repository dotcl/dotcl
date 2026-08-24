;;; Two places where printed output disagreed with this implementation's own
;;; printer. Both found by running the same forms through dotcl and SBCL 2.6.6;
;;; every expectation below matches SBCL exactly.

;;; 1. (QUOTE X) prints as 'X under *PRINT-PRETTY* (CLHS 22.1.3.4), and so does
;;;    (FUNCTION CAR) as #'CAR. Neither was abbreviated, so every printed
;;;    macroexpansion and every quoted datum in a report read as (QUOTE ...).

(deftest printer-abbreviations.quote-and-function
  (list (prin1-to-string ''x)
        (prin1-to-string '#'car)
        (prin1-to-string ''''x))
  ("'X" "#'CAR" "'''X"))

;;; Only the exact two-element form: these are ordinary lists.
(deftest printer-abbreviations.only-the-two-element-form
  (list (prin1-to-string '(quote x y))
        (prin1-to-string '(quote))
        (prin1-to-string '(function car x))
        (prin1-to-string (cons 'quote 'x)))
  ("(QUOTE X Y)" "(QUOTE)" "(FUNCTION CAR X)" "(QUOTE . X)"))

;;; Nested, and inside other aggregates.
(deftest printer-abbreviations.nested
  (list (prin1-to-string (list 'list (list 'quote 'a) 'b))
        (prin1-to-string (list (list (list 'quote 'x))))
        (prin1-to-string (vector (list 'quote 'x))))
  ("(LIST 'A B)" "(('X))" "#('X)"))

;;; With *PRINT-PRETTY* off the full form comes back -- the abbreviation is the
;;; pretty printer's, not the reader's.
(deftest printer-abbreviations.pretty-nil-prints-the-list
  (let ((*print-pretty* nil))
    (list (prin1-to-string ''x)
          (prin1-to-string '#'car)
          (prin1-to-string (list 'list (list 'quote 'a) 'b))))
  ("(QUOTE X)" "(FUNCTION CAR)" "(LIST (QUOTE A) B)"))

;;; It reads back as the same object either way.
(deftest printer-abbreviations.round-trips
  (let ((x ''(1 2)))
    (list (equal (read-from-string (prin1-to-string x)) x)
          (equal (read-from-string (let ((*print-pretty* nil)) (prin1-to-string x))) x)))
  (t t))

;;; 2. ~@C must print what PRIN1 prints: #\a for a graphic character, the name
;;;    for one that needs it. It preferred the UCD name whenever one existed, so
;;;    #\a came out as #\LATIN_SMALL_LETTER_A -- readable here, but disagreeing
;;;    with this implementation's own printer and with every other one.

(deftest printer-abbreviations.tilde-at-c-matches-prin1
  (let ((chars (list #\a #\Z #\Space #\Newline #\Tab)))
    (list (mapcar (lambda (c) (format nil "~@c" c)) chars)
          (equal (mapcar (lambda (c) (format nil "~@c" c)) chars)
                 (mapcar #'prin1-to-string chars))))
  (("#\\a" "#\\Z" "#\\ " "#\\Newline" "#\\Tab") t))

;;; ~C and ~:C are unchanged: the bare character, and the name for the ones that
;;; are not textual.
(deftest printer-abbreviations.tilde-c-and-colon-c-unchanged
  (list (format nil "~c" #\a)
        (format nil "~:c" #\a)
        (format nil "~:c" #\Space)
        (format nil "~:c" #\Newline))
  ("a" "a" "Space" "Newline"))
