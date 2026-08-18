;;; NIL and T need a package prefix in a package that does not inherit them.
;;;
;;; CLHS 22.1.3.3 prints a symbol with a package prefix unless it is accessible
;;; in *PACKAGE*, and grants NIL and T no exemption. They are their own object
;;; types in this implementation, so they used to skip the accessibility check
;;; every other symbol goes through and printed bare. In a package that does not
;;; use COMMON-LISP that output reads back as a DIFFERENT symbol of the same
;;; name — print/read consistency broken with no error anywhere.
;;;
;;; Found through coalton: its (coalton ...) macro prints a form and re-reads
;;; it, and its test package uses only its own packages, so the () in
;;; (fn () ...) came back as a foreign NIL and coalton's parser rejected it as
;;; a malformed argument list.

(defpackage #:ntpp-bare (:use))          ; inherits nothing
(defpackage #:ntpp-cl (:use #:cl))

(defun %ntpp-print (pkg obj)
  (let ((*package* (find-package pkg)))
    (prin1-to-string obj)))

(deftest nil-t-package-prefix.qualified-when-not-inherited
  (list (%ntpp-print '#:ntpp-bare nil)
        (%ntpp-print '#:ntpp-bare t)
        (%ntpp-print '#:ntpp-bare '()))
  ("COMMON-LISP:NIL" "COMMON-LISP:T" "COMMON-LISP:NIL"))

(deftest nil-t-package-prefix.bare-when-inherited
  (list (%ntpp-print '#:ntpp-cl nil)
        (%ntpp-print '#:ntpp-cl t))
  ("NIL" "T"))

;;; The point of the prefix: the text has to read back as the same object.
(defun %ntpp-roundtrip (obj)
  (let ((*package* (find-package '#:ntpp-bare)))
    (let ((back (read-from-string (prin1-to-string obj))))
      (eq back obj))))

(deftest nil-t-package-prefix.round-trips
  (list (%ntpp-roundtrip nil) (%ntpp-roundtrip t))
  (t t))

;;; A whole form, which is how it actually shows up: an empty argument list
;;; inside a printed form must still be NIL after re-reading.
(defun %ntpp-form-roundtrip ()
  (let ((*package* (find-package '#:ntpp-bare)))
    (let ((back (read-from-string (prin1-to-string '(lambda () :x)))))
      (list (null (second back))
            (eq (second back) nil)))))

(deftest nil-t-package-prefix.empty-list-in-form
  (%ntpp-form-roundtrip)
  (t t))

;;; PRINC has no escapes, so no prefix either (CLHS 22.1.3.3: the prefix is
;;; printed only when *print-escape* or *print-readably* is true).
(deftest nil-t-package-prefix.princ-has-no-prefix
  (let ((*package* (find-package '#:ntpp-bare)))
    (list (princ-to-string nil) (princ-to-string t)))
  ("NIL" "T"))
