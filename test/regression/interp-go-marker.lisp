;;; GO out of the tree-walk interpreter travels as a return value where it can.
;;;
;;; It used to be a CL THROW, i.e. a .NET exception, always. Unwinding one costs
;;; far more from the interpreter than from compiled code, because the .NET stack
;;; between the GO and its TAGBODY holds a dozen interpreter frames per level of
;;; Lisp nesting: the same throw measured ~1.0 us from compiled code and ~4.8 us
;;; from here, which made one GO 3.1 us -- about 45% of an interpreted DOTIMES
;;; iteration.
;;;
;;; Now a GO whose value can reach its TAGBODY by simply being returned does that
;;; instead. Only positions whose caller checks take part: the forms of a TAGBODY
;;; body, the forms of a PROGN/LET body under one, and the arms of an IF (and so
;;; WHEN / UNLESS / COND, which macroexpand into IF). Everywhere else GO still
;;; throws, which is what the cases below are mostly about -- the marker must
;;; never surface as a value.
;;;
;;; Both evaluator paths are asserted by binding dotcl:*evaluator-mode* around the
;;; EVAL, so this runs under the ordinary compiled harness too.

(defun %gm (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

(defun %gm-both (form)
  "Both evaluators must agree; the answer is returned once."
  (let ((i (%gm :interpret form))
        (c (%gm :compile form)))
    (if (equal i c) i (list :disagree i c))))

;;; ---- the shapes the marker path handles ----

(deftest go-marker.plain-loop
  (%gm-both '(let ((n 0))
               (tagbody
                top
                  (setq n (+ n 1))
                  (if (< n 5) (go top)))
               n))
  5)

(deftest go-marker.through-when
  (%gm-both '(let ((n 0))
               (tagbody top (setq n (+ n 1)) (when (< n 4) (go top)))
               n))
  4)

(deftest go-marker.through-nested-progn-and-let
  (%gm-both '(let ((n 0))
               (tagbody
                top
                  (progn
                    (let ((step 1))
                      (setq n (+ n step))
                      (if (< n 3) (go top)))))
               n))
  3)

;; A GO from a non-last form must abandon the rest of the body.
(deftest go-marker.skips-rest-of-body
  (%gm-both '(let ((log '()) (once nil))
               (tagbody
                top
                  (push :a log)
                  (unless once (setq once t) (go top))
                  (push :b log))
               (reverse log)))
  (:a :a :b))

;; Backward and forward jumps, and a tag reached only by falling through.
(deftest go-marker.forward-jump
  (%gm-both '(let ((log '()))
               (tagbody
                  (push :start log)
                  (go skip)
                  (push :never log)
                skip
                  (push :end log))
               (reverse log)))
  (:start :end))

;;; ---- nesting: the marker must find the right TAGBODY ----

(deftest go-marker.inner-tagbody-does-not-catch-outer-tag
  (%gm-both '(let ((log '()))
               (tagbody
                outer
                  (push :o log)
                  (tagbody
                   inner
                     (push :i log)
                     (when (< (length log) 4) (go outer)))
                  (push :after log))
               (reverse log)))
  (:o :i :o :i :after))

(deftest go-marker.inner-tag-shadows-outer-of-same-name
  (%gm-both '(let ((hits 0))
               (tagbody
                same
                  (tagbody
                   same
                     (setq hits (+ hits 1))
                     (when (< hits 3) (go same))))
               hits))
  3)

;;; ---- positions the marker cannot travel through: must still work ----

;; Inside a function called from the body: the callee's frames are between the
;; GO and the TAGBODY, so this can only be a throw.
(deftest go-marker.from-inside-a-called-function
  (%gm-both '(let ((n 0))
               (flet ((bump () (setq n (+ n 1)) (< n 3)))
                 (tagbody top (if (bump) (go top))))
               n))
  3)

;; Inside an argument to a call.
(deftest go-marker.from-an-argument
  (%gm-both '(let ((log '()) (once nil))
               (tagbody
                top
                  (push (if once :b (progn (setq once t) (go top))) log))
               (reverse log)))
  (:b))

;; Inside a LET init form (evaluated before the body, not a marker position).
(deftest go-marker.from-an-init-form
  (%gm-both '(let ((n 0))
               (tagbody
                top
                  (let ((x (progn (setq n (+ n 1)) (if (< n 3) (go top) n))))
                    (setq n x)))
               n))
  3)

;; Inside a handler.
(deftest go-marker.from-a-handler
  (%gm-both '(let ((n 0))
               (tagbody
                top
                  (handler-case (error "x")
                    (error () (setq n (+ n 1)) (if (< n 3) (go top)))))
               n))
  3)

;;; ---- unwinding obligations still met ----

(deftest go-marker.unwind-protect-cleanup-runs
  (%gm-both '(let ((log '()) (once nil))
               (tagbody
                top
                  (unwind-protect
                       (unless once (setq once t) (go top))
                    (push :cleanup log)))
               (reverse log)))
  (:cleanup :cleanup))

(deftest go-marker.special-binding-unwinds
  (%gm-both '(let ((seen '()) (once nil))
               (declare (special %gm-v))
               (setq %gm-v :outer)
               (tagbody
                top
                  (let ((%gm-v :inner))
                    (declare (special %gm-v))
                    (push %gm-v seen)
                    (unless once (setq once t) (go top)))
                  (push %gm-v seen))
               (reverse seen)))
  (:inner :inner :outer))

;;; ---- the marker must never be visible as a value ----

(deftest go-marker.tagbody-returns-nil
  (%gm-both '(tagbody (go done) done))
  nil)

(deftest go-marker.value-of-enclosing-let-is-not-a-marker
  (%gm-both '(let ((r (let ((n 0))
                        (tagbody top (setq n (+ n 1)) (if (< n 2) (go top)))
                        n)))
               (list r (consp r))))
  (2 nil))

(deftest go-marker.go-to-a-missing-tag-still-errors
  (let ((r (%gm :interpret '(tagbody (go no-such-tag)))))
    (and (consp r) (eq (car r) :error)))
  t)
