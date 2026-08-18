;;; Invoking a restart established many levels out must not cost stack per level.
;;;
;;; RESTART-CASE used to catch every RestartInvocationException and rethrow the
;;; ones whose tag belonged to an outer restart-case. A rethrow restarts exception
;;; dispatch from inside the handler funclet, and that funclet stays live for the
;;; rest of the exception's journey, so crossing N restart-cases stacked N
;;; dispatches: the crossing cost grew quadratically (20000 levels took 3.2s) and at
;;; 50000 the process died as an uncatchable .NET StackOverflowException — a depth
;;; the same recursion descends four times over when no restart is invoked.
;;; Matching the tag in a CIL exception FILTER lets a level that owns nothing
;;; decline without being entered.
;;;
;;; The deep cases are compiled-only for a reason that is not the bug: one
;;; interpreted level costs tens of .NET frames, so the recursion itself cannot
;;; reach these depths without a compiler. The last test covers the interpreted
;;; path at the depth it does reach.

(defun %rcd-rec (n)
  (restart-case
      (if (zerop n) (invoke-restart 'rcd-outer) (+ 1 (%rcd-rec (- n 1))))
    ;; deliberately not the restart invoked at the bottom: every level declines,
    ;; so the invocation travels through all of them
    (rcd-inner () :wrong-restart)))

(deftest-compiled-only restart-case-deep-invoke.crosses-nested-restart-cases
  (restart-case (%rcd-rec 50000) (rcd-outer () :caught-outside))
  :caught-outside)

;;; A restart that IS established at every level is found by the INNERMOST one.
;;; Each level adds 1 to what it got back and the handling level contributes 0, so
;;; the answer counts the levels the value climbed: 1000 means the invoking level
;;; handled it, and any smaller number means an outer level stole it.

(defun %rcd-innermost (n)
  (restart-case
      (if (zerop n) (invoke-restart 'rcd-any) (1+ (%rcd-innermost (- n 1))))
    (rcd-any () 0)))

(deftest restart-case-deep-invoke.innermost-restart-handles
  (%rcd-innermost 1000)
  1000)

;;; Arguments reach the clause after the crossing (they travel on the exception,
;;; which the filter now reads without catching it).

(defun %rcd-args (n)
  (restart-case
      (if (zerop n) (invoke-restart (find-restart 'rcd-outer-arg) 41 1)
          (+ 1 (%rcd-args (- n 1))))
    (rcd-inner-arg (x) (list :wrong x))))

(deftest-compiled-only restart-case-deep-invoke.arguments-survive-the-crossing
  (restart-case (%rcd-args 20000) (rcd-outer-arg (x y) (+ x y)))
  42)

;;; A non-local exit crossing the same frames is not a restart invocation and must
;;; pass through untouched (the filter must decline control-flow exceptions).

(defun %rcd-throw (n)
  (restart-case
      (if (zerop n) (throw :rcd-tag :thrown) (+ 1 (%rcd-throw (- n 1))))
    (rcd-inner () :wrong-restart)))

(deftest-compiled-only restart-case-deep-invoke.throw-passes-through
  (catch :rcd-tag (%rcd-throw 20000))
  :thrown)

;;; The interpreted path crosses its restart clusters in %CALL-WITH-RESTART-CLUSTER,
;;; which was moved to a C# exception filter for the same reason. An invocation
;;; crossing every interpreted level must still reach the level that owns it.

(deftest restart-case-deep-invoke.interpreted-invocation-crosses-levels
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(progn
            (defun %rcd-i-rec (n)
              (restart-case
                  (if (zerop n) (invoke-restart 'rcd-i-outer)
                      (+ 1 (%rcd-i-rec (- n 1))))
                (rcd-i-inner () :wrong-restart)))
            (restart-case (%rcd-i-rec 500)
              (rcd-i-outer () :caught-outside)))))
  :caught-outside)
