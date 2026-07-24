;;; LOOP prologue ordering — ANSI CL 6.1.7.2 (prologue clauses run in source
;;; order) and SBCL compatibility.
;;;
;;; For "for var = form" WITHOUT then, the assignment runs in the loop body
;;; (first-step), so an :initially clause runs first regardless of textual
;;; order. dotcl inherited the CMUCL LOOP behavior, which stranded the
;;; assignment in the prologue AHEAD of :initially (produced (:FOR-STEP
;;; :INITIALLY) instead of (:INITIALLY :FOR-STEP)). That broke SBCL-compat and
;;; blocked named-readtables' define-api LOOP (which does
;;; `initially (assert ...) for opt-type = (pop ...)`). Fixed in
;;; loop-ansi-for-equals by emitting the assignment as PRE-LOOP-STEPS (psetq,
;;; matching the STEPS psetq) so loop-body merges it into the body. Values below
;;; are what SBCL produces.

;; initially textually BEFORE `for =` : initially runs first.
(deftest loop-prologue-initially-before-for-equals
  (let ((log '()))
    (loop initially (push :init log)
          for x in '(a)
          for y = (progn (push :step log) 1)
          collect y)
    (nreverse log))
  (:init :step))

;; initially textually AFTER `for =` : initially still runs first, because a
;; no-then `for =` initializes in the body, not the prologue.
(deftest loop-prologue-initially-after-for-equals
  (let ((log '()))
    (loop for x in '(a)
          for y = (progn (push :step log) 1)
          initially (push :init log)
          collect y)
    (nreverse log))
  (:init :step))

;; `for =` (no then) re-evaluates its form every iteration.
(deftest loop-for-equals-reevaluated-each-iteration
  (let ((n 0))
    (loop repeat 3 for y = (incf n) collect y))
  (1 2 3))

;; Multiple initially clauses run in source order, all before the body.
(deftest loop-multiple-initially-source-order
  (let ((log '()))
    (loop initially (push :a log)
          initially (push :b log)
          for x in '(1)
          do (push :body log))
    (nreverse log))
  (:a :b :body))

;; A `with` binding initializes before a following initially.
(deftest loop-with-init-before-initially
  (let ((log '()))
    (loop with z = (progn (push :with log) 0)
          initially (push :init log)
          for x in '(1)
          do (push :body log))
    (nreverse log))
  (:with :init :body))

;;; --- `for var = first then next` (WITH then) prologue ordering -------------
;;; The FIRST form runs once as a prologue variable-initialization at the
;;; for-clause's source position; the THEN form is the per-iteration step. So a
;;; textually-earlier :initially runs before FIRST (ANSI CL 6.1.7.2 source
;;; order); a textually-later :initially runs after it. dotcl previously stranded
;;; the FIRST assignment ahead of the whole prologue ((:FIRST :INIT ...)). Values
;;; below are what SBCL produces. (The step is evaluated once per iteration
;;; including a final wasted one — SBCL does the same, hence two :THEN for a
;;; two-element list.)

;; initially textually BEFORE `for = first then next` : initially runs first.
(deftest loop-prologue-initially-before-for-equals-then
  (let ((l '()))
    (loop initially (push :init l)
          for y = (progn (push :first l) 1) then (progn (push :then l) (1+ y))
          for x in '(a b) do (identity x))
    (nreverse l))
  (:init :first :then :then))

;; initially textually AFTER `for = first then next` : the FIRST init runs first.
(deftest loop-prologue-initially-after-for-equals-then
  (let ((l '()))
    (loop for y = (progn (push :first l) 1) then (progn (push :then l) (1+ y))
          initially (push :init l)
          for x in '(a b) do (identity x))
    (nreverse l))
  (:first :init :then :then))

;; `for = first then next` sits in source order between two initially clauses.
(deftest loop-prologue-for-equals-then-between-initially
  (let ((l '()))
    (loop initially (push :i1 l)
          for y = (progn (push :first l) 1) then (1+ y)
          initially (push :i2 l)
          for x in '(a) do (identity x))
    (nreverse l))
  (:i1 :first :i2))

;; The then-form steps the value each iteration.
(deftest loop-for-equals-then-steps
  (loop for y = 1 then (1+ y) for x in '(a b c) collect y)
  (1 2 3))
