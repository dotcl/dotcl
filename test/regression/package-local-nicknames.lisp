;;; PACKAGE-LOCAL-NICKNAMES: printing, and the DEFPACKAGE clauses that name a
;;; package.
;;;
;;; FIND-PACKAGE and the reader already honoured local nicknames. Two places did
;;; not:
;;;
;;;   printing        a symbol printed its physical home package, so a name the
;;;                   nickname exists to avoid writing came back anyway and the
;;;                   output did not round-trip through the package that printed it
;;;   DEFPACKAGE      a clause naming the form's OWN nickname imported nothing
;;;                   and said nothing
;;;
;;; Printing now uses the nickname. The DEFPACKAGE half is refused rather than
;;; resolved -- see the section below for why, and for the spelling that does
;;; work in every implementation that has local nicknames.

(defpackage #:pln-physical-a (:use) (:export #:datum))
(defpackage #:pln-physical-b (:use) (:export #:datum))

(defpackage #:pln-user
  (:use #:cl)
  (:local-nicknames (#:pa #:pln-physical-a)))

;;; --- what already worked, kept so a change here is visible -----------------

(deftest pln-find-package-through-nickname
  (let ((*package* (find-package "PLN-USER")))
    (eq (find-package "PA") (find-package "PLN-PHYSICAL-A")))
  t)

;;; --- printing --------------------------------------------------------------

;;; Printed from the package that declares the nickname, the nickname is the
;;; prefix. This is the round-trip property: the text names, in this package,
;;; the symbol that was printed.
(deftest pln-prints-with-local-nickname
  (let ((*package* (find-package "PLN-USER")))
    (prin1-to-string (find-symbol "DATUM" "PLN-PHYSICAL-A")))
  "PA:DATUM")

;;; A package with no nickname for it still prints the physical name.
(deftest pln-prints-physical-name-without-nickname
  (let ((*package* (find-package "PLN-USER")))
    (prin1-to-string (find-symbol "DATUM" "PLN-PHYSICAL-B")))
  "PLN-PHYSICAL-B:DATUM")

;;; The nickname is a property of the printing package, not of the symbol: the
;;; same symbol printed from a package without the nickname is unaffected.
(deftest pln-nickname-is-per-package
  (let ((*package* (find-package "CL-USER")))
    (prin1-to-string (find-symbol "DATUM" "PLN-PHYSICAL-A")))
  "PLN-PHYSICAL-A:DATUM")

;;; What was printed reads back to the symbol it names.
(deftest pln-printed-form-reads-back
  (let ((*package* (find-package "PLN-USER")))
    (eq (read-from-string (prin1-to-string (find-symbol "DATUM" "PLN-PHYSICAL-A")))
        (find-symbol "DATUM" "PLN-PHYSICAL-A")))
  t)

;;; --- a clause cannot name the form's own nickname --------------------------
;;;
;;; A local nickname belongs to the package doing the READING: FIND-PACKAGE
;;; resolves nicknames against *PACKAGE*, which during a DEFPACKAGE is the
;;; package the form is read in and never the one being defined. So a nickname
;;; the form declares itself does not name a package in that form's own clauses.
;;; SBCL refuses it, and so does this.
;;;
;;; Resolving it instead would work, and is tempting -- the declaration is right
;;; there. It is refused because the difference would be invisible: source that
;;; every implementation with local nicknames accepts, meaning something else
;;; here. What it buys is an abbreviation inside one form, which is not
;;; something a library should be written against.

(deftest pln-import-from-own-nickname-is-refused
  (handler-case
      (progn (eval '(defpackage #:pln-importer
                      (:use #:cl)
                      (:local-nicknames (#:pb #:pln-physical-b))
                      (:import-from #:pb #:datum)))
             :no-error)
    (package-error () :package-error)
    (error () :other-error))
  :package-error)

(deftest pln-use-own-nickname-is-refused
  (handler-case
      (progn (eval '(defpackage #:pln-user2
                      (:use #:cl)
                      (:local-nicknames (#:pa #:pln-physical-a))
                      (:use #:pa)))
             :no-error)
    (package-error () :package-error)
    (error () :other-error))
  :package-error)

;;; --- but a nickname of the package being READ IN does work -----------------
;;;
;;; This is the half that is not a divergence: the nickname is on *PACKAGE*, so
;;; FIND-PACKAGE sees it, in SBCL as well. Anyone who wants the abbreviation in
;;; a DEFPACKAGE clause can have it this way, portably.

(defpackage #:pln-host
  (:use #:cl)
  (:local-nicknames (#:pb #:pln-physical-b)))

(deftest pln-clause-sees-nickname-of-the-reading-package
  (let ((*package* (find-package "PLN-HOST")))
    (eval '(defpackage #:pln-importer2 (:use #:cl) (:import-from #:pb #:datum)))
    (eq (find-symbol "DATUM" "PLN-IMPORTER2")
        (find-symbol "DATUM" "PLN-PHYSICAL-B")))
  t)

;;; --- a nickname naming nothing still fails ---------------------------------

;;; The refusal above must not be the only thing standing between a typo and a
;;; silent success: a nickname pointing at no package is still an error on its
;;; own, at the :LOCAL-NICKNAMES clause.
(deftest pln-nickname-to-missing-package-still-errors
  (handler-case
      (progn (eval '(defpackage #:pln-broken
                      (:use #:cl)
                      (:local-nicknames (#:gone #:pln-no-such-package))
                      (:import-from #:gone #:datum)))
             :no-error)
    (error () :error))
  :error)
