;;; dotcl-lsp-api:completions -- candidates for the cursor at an offset.
;;;
;;; The interesting cases are the ones where it must stay silent: completion
;;; runs on every keystroke, so a receiver that would have to be evaluated is
;;; left alone rather than guessed at.

(require "dotcl-lsp-api")

(defun lac-labels (text)
  (let ((r (dotcl-lsp-api:completions text (length text))))
    (mapcar (lambda (i) (getf i :label)) (getf r :items))))

(defun lac-has (text label)
  (and (member label (lac-labels text) :test #'string=) t))

(defparameter *lac-builder* (dotnet:new "System.Text.StringBuilder"))
(defparameter *lac-not-dotnet* "a plain string")

;;; A variable already bound to a .NET object answers for its runtime type.
(deftest lac-live-object
  (lac-has "(dotnet:invoke *lac-builder* \"AppendL" "AppendLine")
  t)

;;; A literal type name works the same way.
(deftest lac-literal-type
  (lac-has "(dotnet:invoke \"System.Text.StringBuilder\" \"AppendL" "AppendLine")
  t)

;;; Statics of a literal type.
(deftest lac-static
  (lac-has "(dotnet:static \"System.Math\" \"Sqr" "Sqrt")
  t)

;;; The range covers the text typed inside the string, not the quote.
(deftest lac-range
  (let* ((text "(dotnet:static \"System.Math\" \"Sqr")
         (r (dotcl-lsp-api:completions text (length text))))
    (subseq text (getf r :start) (getf r :end)))
  "Sqr")

;;; A receiver that would have to be evaluated gets no candidates: completing
;;; here would construct a FileStream on a keystroke.
(deftest lac-never-evaluates-a-form
  (let ((text "(dotnet:invoke (dotnet:new \"System.IO.FileStream\" p) \"Wri"))
    (dotcl-lsp-api:completions text (length text)))
  nil)

;;; A variable that is not a .NET object has nothing to offer.
(deftest lac-non-dotnet-variable
  (let ((text "(dotnet:invoke *lac-not-dotnet* \"App"))
    (dotcl-lsp-api:completions text (length text)))
  nil)

;;; An unbound variable likewise.
(deftest lac-unbound-variable
  (let ((text "(dotnet:invoke no-such-variable-at-all \"App"))
    (dotcl-lsp-api:completions text (length text)))
  nil)

;;; The member name is the third element; the string in receiver position is not
;;; a member name and must not be completed as one.
(deftest lac-argument-position-matters
  (let ((text "(dotnet:invoke \"App"))
    (dotcl-lsp-api:completions text (length text)))
  nil)

;;; Outside a string the candidates are ordinary symbols.
(deftest lac-symbols-outside-strings
  (lac-has "(defun f () (make-insta" "make-instance")
  t)

;;; Nesting does not confuse the scan: the enclosing form is the inner call.
(deftest lac-nested-form
  (lac-has "(let ((x 1)) (dotnet:invoke *lac-builder* \"AppendL" "AppendLine")
  t)

;;; A closed subform counts as one element, so the member is still element 2 --
;;; and since that element is a form rather than a variable, nothing is offered.
(deftest lac-closed-subform-counts-once
  (let ((text "(dotnet:invoke (identity *lac-builder*) \"AppendL"))
    (dotcl-lsp-api:completions text (length text)))
  nil)

;;; Comments and strings before point do not shift the element count.
(deftest lac-comment-before-point
  (lac-has (format nil "(dotnet:invoke *lac-builder* ; note~%\"AppendL") "AppendLine")
  t)

;;; Nothing to complete at all.
(deftest lac-empty-prefix-outside-string
  (dotcl-lsp-api:completions "(defun f () " 12)
  nil)

;;;; The package the text is read in
;;;;
;;;; A file says which package it is read in, and the answer depends on it: a
;;;; bare INVOKE names dotnet:invoke in a package that uses DOTNET and nothing
;;;; at all in CL-USER. The image's own *PACKAGE* is wherever the server sits,
;;;; which is nobody's file.

(defpackage :lac-app (:use :cl :dotnet))
(defpackage :lac-nick (:use :cl))
(ignore-errors
 (dotcl:add-package-local-nickname "NET" "DOTNET" (find-package :lac-nick)))

(deftest lac-in-package-makes-bare-operator-work
  (lac-has (format nil "(in-package :lac-app)~%(invoke \"System.Text.StringBuilder\" \"AppendL")
           "AppendLine")
  t)

;;; Without one, a bare INVOKE is not dotnet:invoke and nothing is offered.
(deftest lac-without-in-package-bare-operator-is-not-dotnet
  (let ((text "(invoke \"System.Text.StringBuilder\" \"AppendL"))
    (dotcl-lsp-api:completions text (length text)))
  nil)

;;; A package-local nickname is only a name inside the package that declares it.
(deftest lac-local-nickname-resolves
  (lac-has (format nil "(in-package :lac-nick)~%(net:invoke \"System.Text.StringBuilder\" \"AppendL")
           "AppendLine")
  t)

(deftest lac-cl-qualified-in-package-counts
  (lac-has (format nil "(cl:in-package #:lac-app)~%(invoke \"System.Text.StringBuilder\" \"AppendL")
           "AppendLine")
  t)

;;;; Package qualifiers are part of the token being completed

(deftest lac-package-name-is-a-candidate
  (lac-has "(dotn" "dotnet:")
  t)

(deftest lac-qualified-prefix-keeps-the-qualifier
  (lac-has "(dotnet:inv" "dotnet:invoke")
  t)

;;; A single colon offers the external symbols, two colons reach the rest. The
;;; property is checked rather than a chosen symbol: which names DOTNET exports
;;; is not this test's business, and it moves.
(deftest lac-single-colon-offers-only-externals
  (remove-if (lambda (label)
               (let ((name (string-upcase (subseq label (1+ (position #\: label))))))
                 (eq :external (nth-value 1 (find-symbol name "DOTNET")))))
             (lac-labels "(dotnet:%"))
  nil)

(deftest lac-double-colon-reaches-at-least-as-far
  (>= (length (lac-labels "(dotnet::%"))
      (length (lac-labels "(dotnet:%")))
  t)

(deftest lac-double-colon-keeps-both-colons
  (every (lambda (label) (search "::" label)) (lac-labels "(dotnet::%"))
  t)

;;; The label comes back in the case the token was typed in.
(deftest lac-case-follows-the-token
  (lac-has "(DOTNET:INV" "DOTNET:INVOKE")
  t)

;;; The whole token is replaced, qualifier included.
(deftest lac-qualified-replacement-covers-the-token
  (let* ((text "(dotnet:inv")
         (r (dotcl-lsp-api:completions text (length text))))
    (subseq text (getf r :start) (getf r :end)))
  "dotnet:inv")

;;;; Type names in the first argument
;;;;
;;;; (dotnet:new "System.Te needs no receiver resolved -- the question is which
;;;; types this image can name -- so the answer comes from the type index rather
;;;; than from an object.

(defun lac-type-labels (text)
  (mapcar (lambda (i) (getf i :label))
          (getf (dotcl-lsp-api:completions text (length text)) :items)))

(deftest lac-new-completes-type-names
  (and (member "System.Text.StringBuilder" (lac-type-labels "(dotnet:new \"System.Text.Str")
               :test #'string=)
       t)
  t)

;;; A namespace is offered one segment at a time: "System.Te" leads to
;;; "System.Text.", not to every type under it.
(deftest lac-namespace-is-one-step
  (lac-type-labels "(dotnet:new \"System.Te")
  ("System.Text."))

(deftest lac-namespace-kind
  (getf (first (getf (dotcl-lsp-api:completions "(dotnet:new \"System.Te" 22) :items)) :kind)
  :namespace)

;;; The literal-receiver spelling of a call asks the same question.
(deftest lac-invoke-first-argument-is-a-type
  (and (member "System.Text.StringBuilder"
               (lac-type-labels "(dotnet:invoke \"System.Text.Str") :test #'string=)
       t)
  t)

(deftest lac-static-first-argument-is-a-type
  (and (member "System.Math" (lac-type-labels "(dotnet:static \"System.Ma")
               :test #'string=)
       t)
  t)

;;; CAST and IS-INSTANCE-OF take the value first, so the type is the second
;;; argument there.
(deftest lac-cast-type-is-the-second-argument
  (and (member "System.IO.Stream" (lac-type-labels "(dotnet:cast x \"System.IO.Str")
               :test #'string=)
       t)
  t)

;;; The member position is unaffected.
(deftest lac-second-argument-still-members
  (lac-has "(dotnet:invoke *lac-builder* \"AppendL" "AppendLine")
  t)

;;; Generic types are named without their arity, which is how
;;; dotnet:make-generic-type takes them.
(deftest lac-generic-name-has-no-arity
  (and (member "System.Collections.Generic.List"
               (lac-type-labels "(dotnet:new \"System.Collections.Generic.Lis")
               :test #'string=)
       t)
  t)

;;;; Types that are available but not loaded
;;;;
;;;; The index answers first from loaded assemblies and fills in the rest from a
;;;; metadata scan running in the background, so an early answer can be partial.
;;;; It says so, and the caller passes that on -- an LSP client turns it into
;;;; isIncomplete and asks again.

(defun lac-wait-for-type-index (&optional (seconds 120))
  "Wait for the background scan, returning T if it finished.

The budget is a bound on a hang, not a statement about how long the scan should
take: it returns as soon as the scan is done, so a healthy run costs whatever the
scan costs and nothing more. It was 10 seconds, which is short enough to be a
deadline rather than a guard -- a run that had just done several builds missed it
and the two tests below failed together, on a scan that was merely slow."
  (loop repeat (ceiling seconds 0.05)
        when (nth-value 1 (dotnet:type-names "X")) return t
        do (sleep 0.05)))

(deftest lac-type-index-reports-completeness
  (lac-wait-for-type-index)
  t)

(deftest lac-complete-index-is-not-flagged
  (progn
    (lac-wait-for-type-index)
    (getf (dotcl-lsp-api:completions "(dotnet:new \"System.Te" 22) :incomplete))
  nil)

;;; Member completion asks an object, not the index, so it is never partial.
(deftest lac-member-completion-is-never-incomplete
  (let ((text "(dotnet:invoke *lac-builder* \"AppendL"))
    (getf (dotcl-lsp-api:completions text (length text)) :incomplete))
  nil)

;;; The scan reaches assemblies this image never loaded: a namespace under
;;; System. that no test has touched still has names under it.
(deftest lac-scan-reaches-unloaded-assemblies
  (progn
    (lac-wait-for-type-index)
    (and (plusp (length (dotnet:type-names "System." :limit 100000))) t))
  t)
