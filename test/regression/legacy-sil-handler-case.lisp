;;; A .sil emitted by an older dotcl must still assemble.
;;;
;;; The CIL directives in a .sil are looked up by NAME. Rebinding an existing name
;;; to a different shape -- a call that now takes two arguments where it took
;;; three, a constructor that lost a parameter -- makes every .sil that predates
;;; the change assemble into an invalid program. Nothing says so: the assembler
;;; happily emits a call whose arguments no longer match, and the CLR answers
;;; InvalidProgramException at run time, naming nothing.
;;;
;;; That is what happened to HANDLER-CASE. Identifying the invocation by the
;;; cluster array (rather than by a tag object and a separate specifier array)
;;; changed ControlFlowFilters.HandlerCaseClause from three arguments to two, and
;;; the name kept pointing at it. The old shapes have their old names back, and the
;;; cluster form has a name of its own.
;;;
;;; The fixture is a .sil compiled by dotcl 0.1.25 and checked in as that release
;;; emitted it. Loading it is the whole test: it assembles handler-case in the
;;; pre-cluster shape, and the functions it defines then have to behave.

(load "test/regression/legacy-sil/handler-case-0.1.25.sil")

;;; No condition signalled: the body's value comes back.
(deftest legacy-sil-hc.normal
  (cl-user::legacy-hc-normal)
  3)

;;; A condition the single clause takes.
(deftest legacy-sil-hc.caught
  (cl-user::legacy-hc-caught)
  (:caught simple-error))

;;; Two clauses: the filter has to match on type, in clause order.
(deftest legacy-sil-hc.clause-order
  (list (cl-user::legacy-hc-type-order :arith)
        (cl-user::legacy-hc-type-order :plain))
  (:div :err))

;;; Nested handler-cases decide by invocation identity: the inner one declines
;;; (wrong type) and the outer one has to take it.
(deftest legacy-sil-hc.nested
  (cl-user::legacy-hc-nested)
  :outer)
