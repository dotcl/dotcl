;;; Stale *tco-self-symbol* regression tests.
;;;
;;; A labels function G (non-capturing, same arity as the enclosing defun F)
;;; containing a tail call to F must re-enter F — not falsely match the
;;; self-TCO identity check against the enclosing defun's symbol and branch
;;; to G's own TCO loop. Two facets: labels directly inside the defun, and
;;; labels inside a closure inside the defun (closure-boundary reset).

(defvar *tss-count* 0)

(defun tss-f (x)
  (incf *tss-count*)
  (labels ((g (y)
             (if (zerop y)
                 (tss-f 1)              ; tail call to OUTER defun from g
                 (list y *tss-count*))))
    (g x)))

(deftest tco-self-symbol.labels-in-defun
  (progn (setq *tss-count* 0)
         (tss-f 0))
  (1 2))

(defun tss-h (x)
  (incf *tss-count*)
  (let ((captured (* x 10)))
    (funcall
     (lambda ()
       (list captured
             (labels ((g (y)
                        (if (zerop y)
                            (tss-h 1)   ; tail call to OUTER defun through closure
                            (list y *tss-count*))))
               (g x)))))))

(deftest tco-self-symbol.labels-in-closure
  (progn (setq *tss-count* 0)
         (tss-h 0))
  (0 (10 (1 2))))

;;; Legitimate labels self-TCO still works (and now matches by the labels
;;; function's own symbol): deep self-recursion must not overflow the stack.
(defun tss-deep (n)
  (labels ((g (y acc)
             (if (zerop y)
                 acc
                 (g (1- y) (1+ acc)))))
    (g n 0)))

(deftest-compiled-only tco-self-symbol.labels-self-tco-depth
  (tss-deep 200000)
  200000)
