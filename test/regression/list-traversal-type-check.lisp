;;; Regression: NTH and LAST must reject a non-list instead of answering NIL.
;;;
;;; Both walked the list without ever checking what they were walking, and both
;;; ended by handing back a default: (nth 1 7) was NIL, (nth 2 '(1 . 2)) was NIL,
;;; (last 7) was 7. A wrong answer, not an error -- the caller sees a plausible
;;; NIL and the mistake surfaces somewhere else entirely. The rest of the
;;; traversal family (CDR, NTHCDR, BUTLAST, COPY-LIST) already signalled, and so
;;; does SBCL for all of these; NTH and LAST were the two holes.
;;;
;;; A dotted list stays legal where CLHS says it is: LAST accepts one (the whole
;;; point of (last '(a b . c)) => (b . c)), while NTH walking into the non-list
;;; tail of one is an error.

(defun %ltc-classify (thunk)
  "Value if THUNK returns, else (datum expected-type)."
  (handler-case (funcall thunk)
    (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))
    (error (e) (list :other (type-of e)))))

(deftest list-traversal-type-check.nth-non-list
  (%ltc-classify (lambda () (nth 1 7)))
  (7 list))

(deftest list-traversal-type-check.nth-index-zero-non-list
  (%ltc-classify (lambda () (nth 0 7)))
  (7 list))

(deftest list-traversal-type-check.nth-walks-into-dotted-tail
  (%ltc-classify (lambda () (nth 2 (cons 1 2))))
  (2 list))

(deftest list-traversal-type-check.nth-string-is-not-a-list
  (%ltc-classify (lambda () (nth 2 "abc")))
  ("abc" list))

(deftest list-traversal-type-check.last-non-list
  (%ltc-classify (lambda () (last 7)))
  (7 list))

(deftest list-traversal-type-check.last-string-is-not-a-list
  (%ltc-classify (lambda () (last "ab")))
  ("ab" list))

;;; What must keep working: running off the end of a real list is still NIL, a
;;; dotted list is still readable up to its tail, and LAST still accepts one.
(deftest list-traversal-type-check.valid-uses-unchanged
  (list (nth 1 (list 1 2 3))
        (nth 9 (list 1 2))
        (nth 0 nil)
        (nth 0 (cons 1 2))
        (last nil)
        (last (cons 1 2))
        (last (list 1 2 3))
        (last (list 1 2 3) 2)
        (last (list 1 2 3) 0))
  (2 nil nil 1 nil (1 . 2) (3) (2 3) nil))

;;; The interpreter shares the check.
(deftest list-traversal-type-check.interpreted
  (let ((dotcl:*evaluator-mode* :interpret))
    (list (eval '(handler-case (nth 1 7) (type-error (e) (type-error-datum e))))
          (eval '(handler-case (last 7) (type-error (e) (type-error-datum e))))
          (eval '(nth 1 (list 1 2)))
          (eval '(last (list 1 2)))))
  (7 7 2 (2)))
