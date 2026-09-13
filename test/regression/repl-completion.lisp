;;; TAB completion in the bundled REPL.
;;;
;;; The machinery was there (readline dispatches TAB to COMPLETE) but the hook
;;; was NIL, so TAB did nothing in a shipped image. These tests drive COMPLETE
;;; directly -- the terminal half cannot be exercised headlessly, the decision
;;; half can.

(require "dotcl-repl")

(defparameter *rc-builder* (dotnet:new "System.Text.StringBuilder"))

(defun rc-complete (text)
  "Return (new-text new-point item-count) for a TAB at the end of TEXT."
  (let ((r (dotcl-repl::complete (coerce text 'list) (length text))))
    (when r
      (list (coerce (first r) 'string) (second r) (length (third r))))))

;;; A completer ships wired: TAB in a released image does something.
(deftest rc-default-completer-installed
  (and dotcl-repl:*completer* t)
  t)

;;; A single candidate is inserted outright.
(deftest rc-unique-candidate-inserts
  (first (rc-complete "(dotnet:static \"System.Math\" \"Sqr"))
  "(dotnet:static \"System.Math\" \"Sqrt")

(deftest rc-unique-candidate-shows-no-list
  (third (rc-complete "(dotnet:static \"System.Math\" \"Sqr"))
  0)

;;; Several candidates: extend as far as they agree, then offer the list. The
;;; overloads share a name, so the listing is where the signatures show up.
(deftest rc-overloads-insert-common-and-list
  (first (rc-complete "(dotnet:invoke *rc-builder* \"AppendL"))
  "(dotnet:invoke *rc-builder* \"AppendLine")

(deftest rc-overloads-are-listed
  (plusp (third (rc-complete "(dotnet:invoke *rc-builder* \"AppendL")))
  t)

;;; The point lands after the inserted text.
(deftest rc-point-follows-insertion
  (let ((r (rc-complete "(dotnet:invoke *rc-builder* \"AppendL")))
    (= (second r) (length (first r))))
  t)

;;; Symbols complete the same way outside strings.
(deftest rc-symbol-completion
  (first (rc-complete "(make-insta"))
  "(make-instance")

;;; No candidates, no change.
(deftest rc-no-match-leaves-line-alone
  (rc-complete "(dotnet:invoke *rc-builder* \"Zzzz")
  nil)

;;; Longest common prefix, the piece the shell-like behaviour rests on.
(deftest rc-common-prefix-shared
  (dotcl-repl::common-prefix (list "AppendLine" "AppendLiteral"))
  "AppendLi")

(deftest rc-common-prefix-single
  (dotcl-repl::common-prefix (list "abc"))
  "abc")

(deftest rc-common-prefix-disjoint
  (dotcl-repl::common-prefix (list "abc" "xyz"))
  "")

(deftest rc-common-prefix-empty
  (dotcl-repl::common-prefix '())
  "")
