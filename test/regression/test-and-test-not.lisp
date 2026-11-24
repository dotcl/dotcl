;;; CLHS 17.2.1: giving both :TEST and :TEST-NOT to a sequence or list function is an error (SBCL signals a
;;; SIMPLE-ERROR). A NIL value counts as not given, so a wrapper that forwards both, one of them NIL, works.
;;; The expected value of the first test is SBCL's.

(defmacro %ttn-errors-p (form)
  `(handler-case (progn ,form nil) (simple-error () t) (error () :other)))

(deftest test-and-test-not.both-given-is-an-error
  (let ((no-error nil))
    (macrolet ((chk (name form) `(unless (eq (%ttn-errors-p ,form) t) (push ',name no-error))))
      (chk find (find 1 '(1 2 3) :test #'eql :test-not #'eql))
      (chk position (position 1 '(1 2 3) :test #'eql :test-not #'eql))
      (chk count (count 1 '(1 2 3) :test #'eql :test-not #'eql))
      (chk remove (remove 1 '(1 2 3) :test #'eql :test-not #'eql))
      (chk delete (delete 1 (list 1 2 3) :test #'eql :test-not #'eql))
      (chk substitute (substitute 9 1 '(1 2 3) :test #'eql :test-not #'eql))
      (chk nsubstitute (nsubstitute 9 1 (list 1 2 3) :test #'eql :test-not #'eql))
      (chk remove-duplicates (remove-duplicates '(1 1 2) :test #'eql :test-not #'eql))
      (chk delete-duplicates (delete-duplicates (list 1 1 2) :test #'eql :test-not #'eql))
      (chk member (member 1 '(1 2 3) :test #'eql :test-not #'eql))
      (chk assoc (assoc 1 '((1 . a)) :test #'eql :test-not #'eql))
      (chk rassoc (rassoc 1 '((a . 1)) :test #'eql :test-not #'eql))
      (chk adjoin (adjoin 1 '(1 2) :test #'eql :test-not #'eql))
      (chk subst (subst 1 2 '(1 2) :test #'eql :test-not #'eql))
      (chk nsubst (nsubst 1 2 (list 1 2) :test #'eql :test-not #'eql))
      (chk sublis (sublis '((1 . 2)) '(1 2) :test #'eql :test-not #'eql))
      (chk union (union '(1 2) '(2 3) :test #'eql :test-not #'eql))
      (chk intersection (intersection '(1 2) '(2 3) :test #'eql :test-not #'eql))
      (chk set-difference (set-difference '(1 2) '(2 3) :test #'eql :test-not #'eql))
      (chk set-exclusive-or (set-exclusive-or '(1 2) '(2 3) :test #'eql :test-not #'eql))
      (chk subsetp (subsetp '(1) '(1 2) :test #'eql :test-not #'eql))
      (chk tree-equal (tree-equal '(1) '(1) :test #'eql :test-not #'eql))
      (chk search (search '(1) '(1 2) :test #'eql :test-not #'eql))
      (chk mismatch (mismatch '(1) '(1 2) :test #'eql :test-not #'eql)))
    (nreverse no-error))
  nil)

;;; A wrapper that passes both keywords on, one of them NIL (hu.dwim.util's SUBSTITUTE/TREE does). dotcl takes a
;;; NIL keyword value as "not given"; SBCL calls it (an error), so this is dotcl's own behaviour, kept as it was.
(defun %ttn-forward (item list test test-not)
  (list (position item list :test test :test-not test-not)
        (remove item list :test test :test-not test-not)
        (member item list :test test :test-not test-not)
        (count item list :test test :test-not test-not)))

(deftest test-and-test-not.forwarding-one-nil
  (list (%ttn-forward 2 '(1 2 3) #'eql nil)
        (%ttn-forward 2 '(1 2 3) nil #'eql)
        (%ttn-forward 2 '(1 2 3) nil nil))
  ((1 (1 3) (2 3) 1)
   (0 (2) (1 2 3) 2)
   (1 (1 3) (2 3) 1)))
