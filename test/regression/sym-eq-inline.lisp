;;; Inline reference comparison for (EQ x 'SYM) / (EQL x 'SYM).
;;;
;;; The compiler lowers these to Primary + CEQ instead of a call to
;;; Runtime.IsTrueEq / IsTrueEql when one operand is a quoted symbol literal
;;; that is neither T nor NIL. The transform applies in branch position only
;;; (the fused-comparison path), so every test here puts the comparison inside
;;; an IF.
;;;
;;; Two things have to hold and neither is visible from the value alone:
;;;
;;; 1. T and NIL must be excluded. EQ bridges the T and NIL objects to the T
;;;    and NIL symbols; a plain reference comparison does not.
;;; 2. The operands must be evaluated left to right even though CEQ is
;;;    symmetric. Loading a symbol literal is not pure here -- it resolves (and
;;;    a keyword literal interns) at execution time -- so with the literal
;;;    first, compiling the other operand first was observable.
;;;
;;; The tests are differential: each compares the lowered form against the same
;;; comparison made through FUNCALL, which never takes the fast path. That way
;;; they keep testing agreement rather than freezing in how a literal happens to
;;; be loaded today. If literal loading ever becomes a constant-pool load, the
;;; ordering tests below stop being able to observe anything -- at that point
;;; they should be deleted, not adjusted to a new expected value.

(defun %se-probe-name () "SYM-EQ-ORDER-PROBE")

(defun %se-unintern ()
  "Remove the probe keyword, so the next literal load has to re-create it."
  (let ((s (find-symbol (%se-probe-name) "KEYWORD")))
    (when s (unintern s "KEYWORD")))
  nil)

(defun %se-fused ()
  (if (eq ':sym-eq-order-probe (find-symbol (%se-probe-name) "KEYWORD")) :same :differ))

(defun %se-generic ()
  (if (funcall #'eq ':sym-eq-order-probe (find-symbol (%se-probe-name) "KEYWORD"))
      :same :differ))

;;; ---- evaluation order, literal first ----

(deftest sym-eq.literal-first-order-matches-generic-call
  (let (fused generic)
    (%se-unintern) (setq fused (%se-fused))
    (%se-unintern) (setq generic (%se-generic))
    (eq fused generic))
  t)

;; Same shape with the literal second, which is what CASE emits.
(defun %se-fused-2 ()
  (if (eq (find-symbol (%se-probe-name) "KEYWORD") ':sym-eq-order-probe) :same :differ))

(defun %se-generic-2 ()
  (if (funcall #'eq (find-symbol (%se-probe-name) "KEYWORD") ':sym-eq-order-probe)
      :same :differ))

(deftest sym-eq.literal-second-order-matches-generic-call
  (let (fused generic)
    (%se-unintern) (setq fused (%se-fused-2))
    (%se-unintern) (setq generic (%se-generic-2))
    (eq fused generic))
  t)

;;; ---- T and NIL are not taken by the fast path ----

(deftest sym-eq.t-literal-matches-generic
  (let ((s (find-symbol "T" "CL")))
    (eq (if (eq s 't) :y :n)
        (if (funcall #'eq s 't) :y :n)))
  t)

(deftest sym-eq.nil-literal-matches-generic
  (let ((s (find-symbol "NIL" "CL")))
    (eq (if (eq s 'nil) :y :n)
        (if (funcall #'eq s 'nil) :y :n)))
  t)

(deftest sym-eq.t-object-against-t-literal
  (if (eq t 't) :y :n)
  :y)

(deftest sym-eq.nil-object-against-nil-literal
  (if (eq nil 'nil) :y :n)
  :y)

;;; ---- ordinary values ----

(deftest sym-eq.hit
  (let ((x 'alpha)) (if (eq x 'alpha) :y :n))
  :y)

(deftest sym-eq.miss
  (let ((x 'alpha)) (if (eq x 'beta) :y :n))
  :n)

(deftest sym-eq.hit-literal-first
  (let ((x 'alpha)) (if (eq 'alpha x) :y :n))
  :y)

(deftest sym-eq.eql-hit
  (let ((x 'alpha)) (if (eql x 'alpha) :y :n))
  :y)

(deftest sym-eq.keyword-hit
  (let ((x :alpha)) (if (eq x :alpha) :y :n))
  :y)

(deftest sym-eq.non-symbol-operand-misses
  (list (if (eq 5 'alpha) :y :n)
        (if (eq "alpha" 'alpha) :y :n)
        (if (eq #\a 'alpha) :y :n)
        (if (eq '(alpha) 'alpha) :y :n))
  (:n :n :n :n))

;; An uninterned symbol is EQ only to itself, whatever its name says.
(deftest sym-eq.uninterned-namesake
  (let ((u (make-symbol "ALPHA")))
    (list (if (eq u 'alpha) :y :n)
          (if (eq u u) :y :n)))
  (:n :y))

;; The comparison normalizes multiple values first: only the primary value is
;; compared, the same as the call it replaced.
(deftest sym-eq.multiple-values-operand
  (if (eq (values 'alpha 'beta) 'alpha) :y :n)
  :y)

(deftest sym-eq.no-values-operand
  (if (eq (values) 'alpha) :y :n)
  :n)

;;; ---- the CASE dispatch this was written for ----

(defun %se-dispatch (op)
  (case op
    (quote :quote) (if :if) (progn :progn) (setq :setq)
    (multiple-value-list :mvl)
    (t :default)))

(deftest sym-eq.case-dispatch
  (mapcar #'%se-dispatch '(quote if progn setq multiple-value-list car nil t))
  (:quote :if :progn :setq :mvl :default :default :default))

;; A key list goes through MEMBER, not the fused path, and must still work.
(defun %se-dispatch-list (op)
  (case op ((a b) :ab) ((c) :c) (t :other)))

(deftest sym-eq.case-key-list
  (mapcar #'%se-dispatch-list '(a b c d))
  (:ab :ab :c :other))

;; NOT / AND / OR recurse through the same branch compiler.
(deftest sym-eq.negated-and-combined
  (let ((x 'alpha) (y 'beta))
    (list (if (not (eq x 'alpha)) :y :n)
          (if (and (eq x 'alpha) (eq y 'beta)) :y :n)
          (if (or (eq x 'zzz) (eq y 'beta)) :y :n)))
  (:n :y :y))
