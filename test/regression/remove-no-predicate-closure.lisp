;;; REMOVE and its family cost their result conses and nothing else.
;;;
;;; Two things were being paid before the walk even started:
;;;
;;;   - the element test was a Func<LispObject, bool>, so every call allocated a
;;;     display class (the lambda captures the item or the predicate plus the
;;;     keyword struct, copied into it) and a delegate on top;
;;;   - one of the local helpers took `i => vec[i]`, and that lambda captured a
;;;     local of the enclosing method, which forced the WHOLE method's closure
;;;     onto the heap -- including for the list path, which never indexes.
;;;
;;; And the list fast path copied the entire list before noticing that nothing
;;; matched, then threw the copy away.
;;;
;;; (remove 2 '(1 2 3)) cost 247.9 bytes where SBCL costs 15.7; (remove 99 ...)
;;; cost the same 247.9 where SBCL costs nothing at all.

(deftest remove-npc.basic
  (list (remove 2 (list 1 2 3 2 4)) (remove 1 (list 1 1 1)) (remove 9 nil))
  ((1 3 4) nil nil))

;;; Nothing removed: the original list, by identity. Libraries rely on this --
;;; (setq x (delete .. x)) has to leave x's conses in place so a following NCONC
;;; still mutates the shared list.
(deftest remove-npc.nothing-removed-is-eq
  (let ((l (list 1 2 3))) (eq l (remove 9 l)))
  t)

(deftest remove-npc.count
  (list (remove 2 (list 1 2 3 2 4) :count 1)
        (remove 2 (list 1 2 3 2 4) :count 0)
        (remove 2 (list 1 2 3 2 4) :count 5))
  ((1 3 2 4) (1 2 3 2 4) (1 3 4)))

(deftest remove-npc.from-end
  (list (remove 2 (list 1 2 3 2 4) :from-end t :count 1)
        (remove 2 (list 1 2 3 2 4) :from-end t))
  ((1 2 3 4) (1 3 4)))

(deftest remove-npc.bounds
  (list (remove 2 (list 1 2 3 2 4) :start 2)
        (remove 2 (list 1 2 3 2 4) :end 2)
        (remove 2 (list 1 2 3 2 4) :start 1 :end 4))
  ((1 2 3 4) (1 3 2 4) (1 3 4)))

(deftest remove-npc.key-and-test
  (list (remove 1 (list (cons 1 :a) (cons 2 :b) (cons 1 :c)) :key #'car)
        (remove "a" (list "a" "b" "A") :test #'string=)
        (remove "a" (list "a" "b" "A") :test #'string-equal)
        (remove 2 (list 1 2 3) :test-not #'eql))
  (((2 . :b)) ("b" "A") ("b") (2)))

(deftest remove-npc.vector-and-string
  ;; Vectors coerced to lists: DEFTEST compares with EQUAL, which does not look
  ;; inside a vector.
  (list (coerce (remove 2 (vector 1 2 3 2)) 'list)
        (coerce (remove 9 (vector 1 2)) 'list)
        (remove #\a "banana") (remove #\z "banana"))
  ((1 3) (1 2) "bnn" "banana"))

(deftest remove-npc.remove-if
  (list (remove-if #'evenp (list 1 2 3 4))
        (remove-if-not #'evenp (list 1 2 3 4))
        (remove-if #'evenp (list 1 2 3 4) :count 1)
        (coerce (remove-if #'evenp (vector 1 2 3 4)) (quote list)))
  ((1 3) (2 4) (1 3 4) (1 3)))

(deftest remove-npc.substitute
  (list (substitute 9 2 (list 1 2 3 2))
        (substitute 9 2 (list 1 2 3 2) :count 1)
        (substitute-if 9 #'evenp (list 1 2 3 4)))
  ((1 9 3 9) (1 9 3 2) (1 9 3 9)))

(deftest remove-npc.delete
  (list (delete 2 (list 1 2 3 2)) (delete 9 (list 1 2))
        (delete-if #'evenp (list 1 2 3 4)))
  ((1 3) (1 2) (1 3)))

;;; The predicate sees every element, in order, exactly once.
(defvar *rnpc-side* nil)
(defun %rnpc-note (x) (push x *rnpc-side*) x)

(deftest remove-npc.predicate-order
  (progn
    (setq *rnpc-side* nil)
    (let ((r (remove-if (lambda (x) (evenp (%rnpc-note x))) (list 1 2 3))))
      (list r (reverse *rnpc-side*))))
  ((1 3) (1 2 3)))

;;; REMOVE-IF yields exactly one value even when the predicate returned more.
(deftest remove-npc.single-value
  (multiple-value-list (remove-if (lambda (x) (values (evenp x) :extra)) (list 1 2 3)))
  ((1 3)))

;;; --- the point ---

(defun %rnpc-bytes () (nth 4 (dotcl:gc-stats)))

(defvar *rnpc-l* (list 1 2 3))

(defmacro %rnpc-per-call (name &body body)
  `(progn
     (defun ,name (n)
       (declare (fixnum n))
       (let ((r nil))
         (do ((i 0 (1+ i))) ((= i n) r)
           (declare (fixnum i))
           (setq r (progn ,@body)))))
     (,name 2000)
     (let ((best nil))
       (dotimes (r 5 best)
         (let ((before (%rnpc-bytes)))
           (,name 50000)
           (let ((used (- (%rnpc-bytes) before)))
             (when (or (null best) (< used best)) (setq best used))))))))

;;; Removing nothing allocates nothing: the answer is the argument.
(deftest-compiled-only remove-npc.no-match-allocates-nothing
  (= 0 (%rnpc-per-call %rnpc-none (remove 99 *rnpc-l*)))
  t)

;;; Removing one of three costs the two surviving conses and no more. Compared
;;; against building those two conses by hand, so the test keeps meaning if a
;;; cons changes size; within one byte per call.
(deftest-compiled-only remove-npc.costs-only-its-conses
  (<= (abs (- (%rnpc-per-call %rnpc-rm (remove 2 *rnpc-l*))
              (%rnpc-per-call %rnpc-hand (cons (car *rnpc-l*) (cons (caddr *rnpc-l*) nil)))))
      50000)
  t)

;;; DELETE on a list splices in place and allocates nothing of its own. Measured
;;; against building the same list and leaving it alone, so what is compared is
;;; DELETE's own cost.
(deftest-compiled-only remove-npc.delete-allocates-nothing
  (<= (abs (- (%rnpc-per-call %rnpc-del (delete 2 (list 1 2 3)))
              (%rnpc-per-call %rnpc-mklist (list 1 2 3))))
      50000)
  t)

;;; SUBSTITUTE costs its result conses and no more.
(deftest-compiled-only remove-npc.substitute-costs-only-its-conses
  (<= (abs (- (%rnpc-per-call %rnpc-sub (substitute 9 2 *rnpc-l*))
              (%rnpc-per-call %rnpc-sub-hand
                              (cons (car *rnpc-l*) (cons 9 (cons (caddr *rnpc-l*) nil))))))
      50000)
  t)

;;; DELETE really is destructive: the caller's chain is spliced, not copied.
(deftest remove-npc.delete-splices-in-place
  (let* ((l (list 1 2 3 2 4))
         (r (delete 2 l)))
    (list r l (eq r l)))
  ((1 3 4) (1 3 4) t))

;;; ...and when the head matches, the result starts later in the same chain.
(deftest remove-npc.delete-drops-head
  (let* ((l (list 2 1 2 3))
         (r (delete 2 l)))
    (list r (eq r l)))
  ((1 3) nil))
