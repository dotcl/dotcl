;;; Regression tests for (DECLAIM (INLINE f)) actually substituting the
;;; definition at call sites.
;;;
;;; The proclamation used to be recorded and then ignored — every call still
;;; went through the full call sequence. A DEFUN compiled while its name is
;;; proclaimed inline now has its (lambda-list . body) kept, and a later call
;;; site with a matching argument count is rewritten to
;;;   ((lambda (params) decls... (block f body...)) args...)
;;; which the existing machinery already compiles correctly.
;;;
;;; INLINE is a request, not an order (CLHS 3.2.2.1.3), so every refusal below
;;; is allowed to be conservative — it costs speed, never correctness. What is
;;; NOT allowed is a substitution that changes the answer, which is what most of
;;; these tests are about.

(setf dotcl:*save-sil* t)

(defun %inl-calls-p (fn name)
  "True when FN's SIL still names NAME, i.e. still calls it instead of
   substituting its body."
  (and (search name (princ-to-string (dotcl:function-sil fn))) t))

;;; ---- it substitutes, and the answer is unchanged ----

(declaim (inline %inl-add2))
(defun %inl-add2 (a b) (+ a b))
(defun %inl-use-add2 (x) (%inl-add2 x 10))

(deftest inl-substituted
  (%inl-calls-p #'%inl-use-add2 "%INL-ADD2")
  nil)

(deftest inl-value
  (%inl-use-add2 5)
  15)

;; No proclamation → ordinary call.
(defun %inl-plain2 (a b) (+ a b))
(defun %inl-use-plain2 (x) (%inl-plain2 x 10))

;;; The substitution tests below are DEFTEST-COMPILED-ONLY: they ask whether the
;;; compiler DID or DID NOT substitute an inline body at a call site. Without a
;;; compiler nothing is ever substituted, so every one of them would report
;;; "not substituted" and pass or fail for a reason unrelated to what it tests.
;;;
;;; The capture tests are NOT compiled-only. They assert ordinary CL scoping —
;;; that a local FLET / LABELS / MACROLET does not reach into a separately
;;; defined function — which must hold under either evaluator.

(deftest-compiled-only inl-no-proclamation-not-substituted
  (%inl-calls-p #'%inl-use-plain2 "%INL-PLAIN2")
  t)

(deftest inl-no-proclamation-value
  (%inl-use-plain2 5)
  15)

;;; ---- the substituted body keeps its meaning ----

;; RETURN-FROM the function name must still work: the body goes inside a BLOCK
;; of that name.
(declaim (inline %inl-early))
(defun %inl-early (n)
  (when (> n 0) (return-from %inl-early :pos))
  :neg)

(defun %inl-use-early (n) (list (%inl-early n) (%inl-early (- n))))

(deftest inl-return-from
  (%inl-use-early 3)
  (:pos :neg))

;; Leading declarations belong to the LAMBDA's body, not inside the BLOCK.
;; A SPECIAL declaration on a parameter is the observable version: if the
;; declaration were dropped, the binding would be lexical and %INL-PEEK-V
;; could not see it.
(declaim (inline %inl-dynbind))
(defun %inl-dynbind (v) (declare (special v)) (%inl-peek-v))
(defun %inl-peek-v () (locally (declare (special v)) v))

(deftest inl-declarations-survive
  (%inl-dynbind 42)
  42)

;; Arguments are evaluated exactly once, left to right — the substitution is an
;; application, not a textual splice of each argument into every use.
(defvar *inl-log* nil)
(defun %inl-note (x) (push x *inl-log*) x)

(declaim (inline %inl-swap))
(defun %inl-swap (a b) (list b a))

(deftest inl-argument-evaluation
  (progn
    (setq *inl-log* nil)
    (list (%inl-swap (%inl-note 1) (%inl-note 2)) (reverse *inl-log*)))
  ((2 1) (1 2)))

;; A parameter used twice in the body still evaluates its argument once.
(declaim (inline %inl-twice))
(defun %inl-twice (a) (+ a a))

(deftest inl-argument-evaluated-once
  (progn
    (setq *inl-log* nil)
    (list (%inl-twice (%inl-note 7)) *inl-log*))
  (14 (7)))

;;; ---- hygiene: the body's free names keep meaning what they meant ----

;; The classic inline bug. %INL-CALLS-HELPER was written at top level, so its
;; HELPER is the global one. Dropped inside a caller's FLET of the same name it
;; would silently become the caller's — a wrong answer. The substitution is
;; refused instead.
(defun %inl-helper () :global)
(declaim (inline %inl-calls-helper))
(defun %inl-calls-helper () (%inl-helper))

(defun %inl-flet-shadow ()
  (flet ((%inl-helper () :local))
    (list (%inl-helper) (%inl-calls-helper))))

(deftest inl-flet-does-not-capture
  (%inl-flet-shadow)
  (:local :global))

;; LABELS is the same question.
(defun %inl-labels-shadow ()
  (labels ((%inl-helper () :labels))
    (list (%inl-helper) (%inl-calls-helper))))

(deftest inl-labels-does-not-capture
  (%inl-labels-shadow)
  (:labels :global))

;; MACROLET can rebind any name at all, so an active macrolet scope refuses the
;; substitution outright.
(defun %inl-macrolet-shadow ()
  (macrolet ((%inl-helper () :macro))
    (list (%inl-helper) (%inl-calls-helper))))

(deftest inl-macrolet-does-not-capture
  (%inl-macrolet-shadow)
  (:macro :global))

;; An FLET that shadows a name the body does NOT use must not block the
;; substitution — the guard is per-name, not "any flet in scope".
(defun %inl-unrelated-flet (x)
  (flet ((%inl-unrelated () :unused))
    (declare (ignorable #'%inl-unrelated))
    (%inl-add2 x 1)))

(deftest inl-unrelated-flet-still-substitutes
  (%inl-calls-p #'%inl-unrelated-flet "%INL-ADD2")
  nil)

(deftest inl-unrelated-flet-value
  (%inl-unrelated-flet 1)
  2)

;; The callee's own name is shadowed by an FLET: that is a different function
;; and must be called, not substituted.
(defun %inl-self-shadow (x)
  (flet ((%inl-add2 (a b) (list :flet a b)))
    (%inl-add2 x 10)))

(deftest inl-shadowed-callee
  (%inl-self-shadow 5)
  (:flet 5 10))

;;; ---- NOTINLINE turns it off ----

(declaim (inline %inl-notinlinable))
(defun %inl-notinlinable (a) (* a 2))

;; Lexically declared NOTINLINE at the call site.
(defun %inl-local-notinline (x)
  (declare (notinline %inl-notinlinable))
  (%inl-notinlinable x))

(deftest-compiled-only inl-local-notinline-not-substituted
  (%inl-calls-p #'%inl-local-notinline "%INL-NOTINLINABLE")
  t)

(deftest inl-local-notinline-value
  (%inl-local-notinline 4)
  8)

;; And without the declaration the same call site does substitute.
(defun %inl-no-local-notinline (x) (%inl-notinlinable x))

(deftest inl-without-notinline-substituted
  (%inl-calls-p #'%inl-no-local-notinline "%INL-NOTINLINABLE")
  nil)

;;; ---- recursion terminates ----

(declaim (inline %inl-fact))
(defun %inl-fact (n) (if (= n 0) 1 (* n (%inl-fact (- n 1)))))
(defun %inl-use-fact () (%inl-fact 6))

(deftest inl-recursive-value
  (%inl-use-fact)
  720)

(declaim (inline %inl-even %inl-odd))
(defun %inl-even (n) (if (= n 0) t (%inl-odd (- n 1))))
(defun %inl-odd (n) (if (= n 0) nil (%inl-even (- n 1))))

(deftest inl-mutual-recursion-value
  (list (%inl-even 6) (%inl-odd 6))
  (t nil))

;;; ---- shapes that are refused, but still correct ----

;; &optional / &key / &rest lambda lists are not substituted (the expansion
;; would have to replicate defaulting), but they still work.
(declaim (inline %inl-opt))
(defun %inl-opt (a &optional (b 3)) (list a b))
(defun %inl-use-opt () (list (%inl-opt 1) (%inl-opt 1 2)))

(deftest-compiled-only inl-optional-not-substituted
  (%inl-calls-p #'%inl-use-opt "%INL-OPT")
  t)

(deftest inl-optional-value
  (%inl-use-opt)
  ((1 3) (1 2)))

(declaim (inline %inl-key))
(defun %inl-key (a &key (b 5)) (list a b))
(defun %inl-use-key () (list (%inl-key 1) (%inl-key 1 :b 2)))

(deftest inl-key-value
  (%inl-use-key)
  ((1 5) (1 2)))

(declaim (inline %inl-rest))
(defun %inl-rest (a &rest more) (cons a more))
(defun %inl-use-rest () (%inl-rest 1 2 3))

(deftest inl-rest-value
  (%inl-use-rest)
  (1 2 3))

;; A body over the size limit stays a call.
(declaim (inline %inl-big))
(defun %inl-big (a)
  (list a a a a a a a a a a a a a a a a a a a a
        a a a a a a a a a a a a a a a a a a a a
        a a a a a a a a a a a a a a a a a a a a
        a a a a a a a a a a a a a a a a a a a a
        a a a a a a a a a a a a a a a a a a a a))
(defun %inl-use-big () (length (%inl-big 1)))

(deftest-compiled-only inl-oversize-not-substituted
  (%inl-calls-p #'%inl-use-big "%INL-BIG")
  t)

(deftest inl-oversize-value
  (%inl-use-big)
  100)

;;; ---- redefinition ----

;; CLHS 3.2.2.1.3 licenses an already-substituted call site to keep the old
;; body. What must hold is that a call site compiled AFTER the redefinition
;; sees the new one.
(declaim (inline %inl-versioned))
(defun %inl-versioned () :v1)
(defun %inl-site-v1 () (%inl-versioned))

(deftest inl-redef-first-site
  (%inl-site-v1)
  :v1)

(defun %inl-versioned () :v2)
(defun %inl-site-v2 () (%inl-versioned))

(deftest inl-redef-new-site
  (%inl-site-v2)
  :v2)

;; Proclaiming NOTINLINE drops the recorded body, so a later site calls.
(declaim (notinline %inl-versioned))
(defun %inl-versioned () :v3)
(defun %inl-site-v3 () (%inl-versioned))

(deftest-compiled-only inl-notinline-proclaim-drops-body
  (%inl-calls-p #'%inl-site-v3 "%INL-VERSIONED")
  t)

(deftest inl-notinline-proclaim-value
  (%inl-site-v3)
  :v3)

;;; ---- the motivating case: crc-division-step ----

(declaim (inline %inl-crc-step))
(defun %inl-crc-step (bit rmdr poly msb-mask)
  (declare (type (signed-byte 56) rmdr poly msb-mask) (type bit bit))
  (let ((new-rmdr (logior bit (* rmdr 2))))
    (if (zerop (logand msb-mask new-rmdr))
        new-rmdr
        (logxor new-rmdr poly))))

(defun %inl-crc-adjust (poly n)
  (declare (type (signed-byte 56) poly) (fixnum n))
  (let* ((mask (ash 1 (1- (integer-length poly))))
         (rmdr (%inl-crc-step 1 0 poly mask)))
    (dotimes (k (- n 1))
      (setf rmdr (%inl-crc-step 0 rmdr poly mask)))
    rmdr))

(defun %inl-crc-step-generic (bit rmdr poly msb-mask)
  (let ((new-rmdr (logior bit (* rmdr 2))))
    (if (zerop (logand msb-mask new-rmdr))
        new-rmdr
        (logxor new-rmdr poly))))

(defun %inl-crc-adjust-generic (poly n)
  (let* ((mask (ash 1 (1- (integer-length poly))))
         (rmdr (%inl-crc-step-generic 1 0 poly mask)))
    (dotimes (k (- n 1))
      (setf rmdr (%inl-crc-step-generic 0 rmdr poly mask)))
    rmdr))

(deftest inl-crc-substituted
  (%inl-calls-p #'%inl-crc-adjust "%INL-CRC-STEP")
  nil)

(deftest inl-crc-matches-generic
  (equal (%inl-crc-adjust 1099587256329 30000)
         (%inl-crc-adjust-generic 1099587256329 30000))
  t)

(setf dotcl:*save-sil* nil)
