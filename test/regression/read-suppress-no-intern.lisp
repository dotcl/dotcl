;;; A skipped form must not leave its symbols behind.
;;;
;;; CLHS 23.2 (*READ-SUPPRESS*): while it is true the reader still parses a
;;; token, but "no symbols are interned". #+ and #- bind it when the feature
;;; expression is false, so a form written under #+nil -- the ordinary way to
;;; keep a worked-out definition next to the code that replaced it -- must have
;;; no effect on any package.
;;;
;;; dotcl interned them anyway. hu.dwim.def keeps this next to the expansion it
;;; actually uses:
;;;
;;;     #+nil
;;;     (def (class* e) namespace () ...)
;;;
;;; so reading the file interned HU.DWIM.DEF::CLASS*, and the later IMPORT of
;;; HU.DWIM.DEFCLASS-STAR:CLASS* into that package died on a name conflict with
;;; a symbol nothing had ever defined. hu.dwim.walker could not load.
;;;
;;; The checks ask whether the name exists in ANY package rather than in one
;;; named package: which package is current when a form is read depends on how
;;; the suite is run, and "nowhere at all" is the property actually wanted.

(defun rsni-interned-anywhere-p (name)
  (and (some (lambda (p) (nth-value 1 (find-symbol name p))) (list-all-packages))
       t))

;;; The names below appear ONLY inside suppressed forms, so a true answer can
;;; only have come from the reader.

#+nil (rsni-only-in-plus-nil 1 2)
#-(and) (rsni-only-in-minus-and)
#+(or) (rsni-only-in-plus-or)

(deftest read-suppress-plus-nil-interns-nothing
  (rsni-interned-anywhere-p "RSNI-ONLY-IN-PLUS-NIL")
  nil)

(deftest read-suppress-minus-and-interns-nothing
  (rsni-interned-anywhere-p "RSNI-ONLY-IN-MINUS-AND")
  nil)

(deftest read-suppress-plus-or-interns-nothing
  (rsni-interned-anywhere-p "RSNI-ONLY-IN-PLUS-OR")
  nil)

;;; The same holds when the variable is bound directly rather than by #+/#-.

(deftest read-suppress-bound-directly-interns-nothing
  (progn
    (let ((*read-suppress* t))
      (read-from-string "(rsni-bound-directly a b)"))
    (rsni-interned-anywhere-p "RSNI-BOUND-DIRECTLY"))
  nil)

;;; Keywords are symbols too, and a suppressed one must not reach the KEYWORD
;;; package either.

(deftest read-suppress-does-not-intern-keywords
  (progn
    (let ((*read-suppress* t))
      (read-from-string "(:rsni-suppressed-keyword)"))
    (nth-value 1 (find-symbol "RSNI-SUPPRESSED-KEYWORD" :keyword)))
  nil)

;;; A package that does not exist is not an error inside a form being skipped:
;;; the token is parsed, not resolved.

(deftest read-suppress-tolerates-unknown-package-marker
  (handler-case
      (let ((*read-suppress* t))
        (read-from-string "(rsni-no-such-package::whatever)")
        :no-error)
    (error () :error))
  :no-error)

;;; A suppressed read still returns NIL and still consumes exactly one form.

(deftest read-suppress-returns-nil-and-consumes-one-form
  (let ((*read-suppress* t))
    (multiple-value-bind (obj pos) (read-from-string "(a b c) tail")
      (list obj (subseq "(a b c) tail" pos))))
  (nil "tail"))

;;; And reading is unchanged when nothing is suppressed -- the guard must not
;;; leak into ordinary reads.

(deftest read-suppress-off-still-interns
  (progn
    (read-from-string "rsni-not-suppressed")
    (rsni-interned-anywhere-p "RSNI-NOT-SUPPRESSED"))
  t)

(deftest read-suppress-off-still-reads-the-form
  (let ((form (read-from-string "(rsni-a rsni-b . rsni-c)")))
    (list (symbol-name (first form))
          (symbol-name (second form))
          (symbol-name (cddr form))))
  ("RSNI-A" "RSNI-B" "RSNI-C"))
