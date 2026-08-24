;;; princ prints with BOTH *print-escape* and *print-readably* false (CLHS
;;; 22.1.3.4), and every directive defined as printing "as by princ" inherits
;;; that. Suppressing escape alone is not enough: the printer takes
;;; escape-or-readably, so under a true *print-readably* a princ-style directive
;;; silently prints like prin1.
;;;
;;; *print-readably* is true inside WITH-STANDARD-IO-SYNTAX (CLHS Figure 23-1),
;;; so this is not an exotic context -- it is the one every conforming program
;;; uses to print something back.

(deftest print-readably.princ-drops-readably
  (let ((*print-readably* t))
    (princ-to-string "x"))
  "x")

(deftest print-readably.tilde-a-drops-readably
  (let ((*print-readably* t))
    (format nil "~a" "x"))
  "x")

;;; ~S is prin1-style and must NOT drop it.

(deftest print-readably.tilde-s-keeps-escapes
  (let ((*print-readably* t))
    (format nil "~s" "x"))
  "\"x\"")

;;; ~D and ~R print a non-number "in ~A format" (CLHS 22.3.2.2), so they carry
;;; the same rule.

(deftest print-readably.tilde-d-non-integer-is-aesthetic
  (let ((*print-readably* t))
    (format nil "~d" "x"))
  "x")

(deftest print-readably.tilde-r-non-integer-is-aesthetic
  (let ((*print-readably* t))
    (format nil "~r" "x"))
  "x")

;;; The binding itself: WITH-STANDARD-IO-SYNTAX binds *print-readably* to T, and
;;; ~A inside it still prints aesthetically.

(deftest print-readably.with-standard-io-syntax-binds-it-true
  (let ((*print-readably* nil))
    (with-standard-io-syntax
      (list *print-readably* (format nil "~a" "x"))))
  (t "x"))
