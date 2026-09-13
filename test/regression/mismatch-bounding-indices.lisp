;;; MISMATCH checks its bounding indices.
;;;
;;; SEARCH, SUBSTITUTE, REPLACE and FILL all call CheckBoundingIndices; MISMATCH
;;; was the one taker that did not. A reversed or out-of-range range produced an
;;; answer instead of an error, and the answer could be nonsense:
;;; (mismatch "hi" "hello" :start2 4 :end2 2) returned -2, an index no caller can
;;; use. CLHS 17.1 (bounding index designator) and Issue
;;; SUBSEQ-OUT-OF-BOUNDS:IS-AN-ERROR make these an error.
;;;
;;; Expected values are SBCL's; dotcl now agrees on all 17 probes below and on the
;;; 35 in MISMATCH-CONSING.

(defmacro %mbi-try (form)
  `(handler-case (progn ,form :no-error) (error () :error)))

;;; SEQ1's range.

(deftest mismatch-bounding-indices.seq1-out-of-range
  (list (%mbi-try (mismatch "hello" "hi" :start1 4 :end1 2))
        (%mbi-try (mismatch "hello" "hi" :end1 9))
        (%mbi-try (mismatch "hello" "hi" :start1 9))
        (%mbi-try (mismatch "hello" "hi" :start1 -1)))
  (:error :error :error :error))

;;; SEQ2's range is checked too -- this is the one that used to answer -2.

(deftest mismatch-bounding-indices.seq2-out-of-range
  (list (%mbi-try (mismatch "hi" "hello" :start2 4 :end2 2))
        (%mbi-try (mismatch "hi" "hello" :end2 9))
        (%mbi-try (mismatch "hi" "hello" :start2 -1))
        (%mbi-try (mismatch "hello" "hi" :end1 9 :end2 9)))
  (:error :error :error :error))

;;; Every sequence type, and the :FROM-END direction, go through the same check.

(deftest mismatch-bounding-indices.all-sequence-types
  (list (%mbi-try (mismatch '(1 2) '(1 2) :end1 9))
        (%mbi-try (mismatch #(1 2) #(1 2) :end1 9))
        (%mbi-try (mismatch nil nil :end1 1))
        (%mbi-try (mismatch "hello" "hi" :start1 4 :end1 2 :from-end t)))
  (:error :error :error :error))

;;; The edges are legal and must keep answering: START at the length, END at the
;;; length, an empty range, and an explicit NIL end.

(deftest mismatch-bounding-indices.legal-edges
  (list (mismatch "hello" "hello" :start1 5)
        (mismatch "hello" "hello" :end1 5)
        (mismatch "hello" "hello" :start1 2 :end1 2)
        (mismatch "hello" "hello" :end1 nil)
        (mismatch "xhelloy" "hello" :start1 1 :end1 6))
  (5 nil 2 nil nil))

;;; The two-argument entry has no indices to get wrong, and must not have picked
;;; up a cost from the check.
