;;; Single labels self-tail-recursion TCO regression tests.
;;;
;;; A single self-recursive labels function is speculatively compiled through
;;; the direct+TCO path (compile-labels-boxed): a self-reference that is a real
;;; tail call becomes a loop branch instead of overflowing the .NET stack. The
;;; speculation is accepted only when the generated body is provably
;;; self-contained (no box-key reference, no orphan code after a TCO branch);
;;; otherwise it falls back to the unchanged closure path. So every non-tail /
;;; captured / guarded shape must still return the correct value — the fix may
;;; never trade correctness for the optimization.

;; Core: single self-tail-recursion at depth 3M must TCO (not overflow).
(deftest-compiled-only labels-self-tco.deep-single
  (labels ((g (n acc) (if (zerop n) acc (g (1- n) (1+ acc))))) (g 3000000 0))
  3000000)

;; Multiple tail sites via cond, deep.
(deftest-compiled-only labels-self-tco.deep-cond
  (labels ((g (n acc)
             (cond ((zerop n) acc)
                   ((evenp n) (g (1- n) (+ acc 2)))
                   (t (g (1- n) (1+ acc))))))
    (g 3000000 0))
  4500000)

;; Non-tail self-reference: must fall back to the closure path and stay correct.
(deftest labels-self-tco.non-tail-fallback
  (labels ((g (n) (if (zerop n) 0 (+ 1 (g (1- n)))))) (g 100))
  100)

;; #'g as a value in the body: box is genuinely captured -> closure fallback.
(deftest labels-self-tco.function-ref-fallback
  (labels ((g (n acc) (if (zerop n) acc (funcall #'g (1- n) (1+ acc))))) (g 100 0))
  100)

;; The box must stay usable by the body AND by #'g even when the fn also
;; self-tail-recurses: box allocation/population is unconditional.
(deftest labels-self-tco.external-and-body-calls
  (labels ((g (n a) (if (zerop n) a (g (1- n) (1+ a)))))
    (list (g 10 0) (funcall #'g 3 0)))
  (10 3))

;; A sibling calls g; g self-tail-recurses at depth 3M. Mixed arity (h/1, g/2)
;; forces the boxed (non-mutual) path, and g must still TCO.
(deftest-compiled-only labels-self-tco.mixed-arity-sibling
  (labels ((g (n a) (if (zerop n) a (g (1- n) (1+ a))))
           (h (x) (g x 0)))
    (h 3000000))
  3000000)

;; Guard for the intrinsic-argument TCO hazard: a tail self-call sitting in an
;; intrinsic argument (print) must NOT be accepted as a bare direct TCO that
;; strands the print as dead code — it falls back and the print runs.
(deftest labels-self-tco.intrinsic-arg-guard
  (let* ((out (make-string-output-stream))
         (r (let ((*standard-output* out))
              (labels ((g (n) (if (zerop n) 7 (print (g (1- n)))))) (g 3)))))
    (list r (if (plusp (length (get-output-stream-string out))) t nil)))
  (7 t))

;; special-declared param disables TCO (needs try/finally) -> closure fallback.
(deftest labels-self-tco.special-param-fallback
  (labels ((g (n acc)
             (declare (special acc))
             (if (zerop n) acc (g (1- n) (1+ acc)))))
    (g 50 0))
  50)

;; &optional param: not simple-required-only, speculation skipped -> correct.
(deftest labels-self-tco.optional-fallback
  (labels ((g (n &optional (acc 0)) (if (zerop n) acc (g (1- n) (1+ acc))))) (g 50))
  50)

;; Tail self-call inside handler-case (leave, not br): deep, must TCO+catch-clean.
(deftest-compiled-only labels-self-tco.handler-case-tail
  (labels ((g (n acc)
             (handler-case
                 (if (zerop n) acc (g (1- n) (1+ acc)))
               (error () :err))))
    (g 1000000 0))
  1000000)
