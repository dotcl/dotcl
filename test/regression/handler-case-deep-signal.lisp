;;; A signal that crosses many nested HANDLER-CASEs must not cost stack per level.
;;;
;;; HANDLER-CASE used to catch every LispErrorException and rethrow the ones no
;;; clause matched. A rethrow restarts exception dispatch from inside the handler
;;; funclet, and that funclet stays live for the rest of the exception's journey,
;;; so crossing N handler-cases stacked N dispatches. Recursion with a handler-case
;;; per level therefore died as an uncatchable .NET StackOverflowException at
;;; ~20000 frames — a depth the same recursion survives a hundredfold with no
;;; handler-case in it, so this was the rethrow chain and not the recursion.
;;; Matching the clause in a CIL exception FILTER lets a level that owns nothing
;;; decline without being entered.
;;;
;;; The deep cases are compiled-only for a reason that is not the bug: one
;;; interpreted level costs tens of .NET frames, so the recursion itself cannot
;;; reach these depths without a compiler. The interpreted path has the same defect
;;; in its own runtime primitive, and the last test here covers it at the depth
;;; that path actually reaches.

(defun %hcd-rec (n)
  (handler-case
      (if (zerop n) (error "bottom") (+ 1 (%hcd-rec (- n 1))))
    ;; deliberately not the condition signalled at the bottom: every level
    ;; declines, so the signal travels through all of them
    (type-error () :wrong-clause)))

(deftest-compiled-only handler-case-deep-signal.crosses-nested-handler-cases
  (handler-case (%hcd-rec 50000) (simple-error () :caught-outside))
  :caught-outside)

;;; The matching clause still wins at depth, with its variable bound.

(defun %hcd-rec2 (n)
  (handler-case
      (if (zerop n) (error "deep-bottom") (+ 1 (%hcd-rec2 (- n 1))))
    (type-error () :wrong-clause)))

(deftest-compiled-only handler-case-deep-signal.innermost-matching-clause-binds-condition
  (handler-case (%hcd-rec2 20000)
    (simple-error (e) (princ-to-string e)))
  "deep-bottom")

;;; A clause that DOES match at every level is taken by the INNERMOST one. Each
;;; level adds 1 to what it got back, and the level that handles contributes 0, so
;;; the answer counts the levels the value climbed: 1000 means the signalling level
;;; handled it, and any smaller number means an outer level stole it.

(defun %hcd-rec3 (n)
  (handler-case
      (if (zerop n) (error "innermost") (1+ (%hcd-rec3 (- n 1))))
    (error () 0)))

(deftest handler-case-deep-signal.matching-clause-handles-at-signalling-level
  (%hcd-rec3 1000)
  1000)

;;; A non-local exit crossing the same frames is not a condition and must pass
;;; through untouched (the filter must decline control-flow exceptions).

(defun %hcd-throw (n)
  (handler-case
      (if (zerop n) (throw :hcd-tag :thrown) (+ 1 (%hcd-throw (- n 1))))
    (error () :wrong-clause)))

(deftest-compiled-only handler-case-deep-signal.throw-passes-through
  (catch :hcd-tag (%hcd-throw 20000))
  :thrown)

;;; A raw .NET exception raised under many handler-cases is still wrapped and
;;; matched by type at the level that asks for it.

(defun %hcd-raw (n)
  (handler-case
      (if (zerop n) (dotnet:invoke "x" "Substring" 99) (+ 1 (%hcd-raw (- n 1))))
    (type-error () :wrong-clause)))

(deftest-compiled-only handler-case-deep-signal.raw-dotnet-exception-crosses
  (handler-case (%hcd-raw 5000) (error () :caught-outside))
  :caught-outside)

;;; The interpreted path builds its handler clusters in %CALL-WITH-HANDLER-CLUSTER,
;;; which had the identical shape in C#: catch every exception, let
;;; RewrapNonLispException rethrow the ones it does not convert. One of those
;;; frames sits under every interpreted HANDLER-CASE, so a condition escaping deep
;;; interpreted recursion accumulated a restarted dispatch per level and killed the
;;; process. Runaway recursion must instead arrive as a catchable STORAGE-CONDITION,
;;; the same as it does without a handler-case in the loop.

(deftest handler-case-deep-signal.interpreted-runaway-is-catchable
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(progn
            (defun %hcd-i-rec (n)
              (handler-case
                  (if (zerop n) 0 (+ 1 (%hcd-i-rec (- n 1))))
                (type-error () :wrong-clause)))
            (handler-case (%hcd-i-rec 10000000)
              (storage-condition () :caught-storage-condition)))))
  :caught-storage-condition)
