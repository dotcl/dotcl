;;; A LET* special binding is popped when a later init form exits non-locally.
;;;
;;; LET* evaluates each init form in the scope of the bindings before it, so by
;;; the time the second init runs the first binding is already live. The dynamic
;;; binding of a special is established by pushing an entry and removed by the
;;; cleanup of a protected region -- and that region used to open only after the
;;; LAST binding, with every init form outside it. An init that exited
;;; non-locally (THROW, RETURN-FROM, GO, an error handled further out) therefore
;;; escaped before the region was entered, and the entries pushed before it were
;;; never popped.
;;;
;;; The damage outlives the form. A leaked entry sits on the thread's dynamic
;;; stack for good: every later read of that variable sees the abandoned value,
;;; and every later pop of it removes somebody else's entry instead, so the next
;;; binding of the same name leaks in turn. This surfaced as a compiler failing
;;; on correct source -- the compiler's own LET* of a compile-state variable
;;; leaked when a nested speculation unwound, and the next compile ran in the
;;; abandoned state.
;;;
;;; Each region now opens right after the push that established the binding it
;;; cleans up, one per special binding, so the cleanup runs exactly on the paths
;;; where the push did. Popping is by symbol, which is why "pop it anyway" is not
;;; an option: popping a binding that was never pushed takes an ENCLOSING
;;; binding of the same name.

(defvar *lssu-a* :outer)
(defvar *lssu-b* :outer)
(defvar *lssu-c* :outer)

(defun %lssu-boom () (throw 'lssu-tag :thrown))

;;; ---- the leak, in each shape that can produce it ----

;; A plain lexical init after the special binding.
(defun %lssu-later-lexical ()
  (let ((r (catch 'lssu-tag
             (let* ((*lssu-a* :inner) (q (%lssu-boom)))
               (list *lssu-a* q)))))
    (list r *lssu-a*)))

(deftest let-star-special-unwind.later-lexical-init
  (%lssu-later-lexical)
  (:thrown :outer))

;; A second SPECIAL binding whose init throws: the first must be popped, and
;; the second was never pushed, so it must be left alone.
(defun %lssu-later-special ()
  (let ((r (catch 'lssu-tag
             (let* ((*lssu-a* :inner) (*lssu-b* (%lssu-boom)))
               (list *lssu-a* *lssu-b*)))))
    (list r *lssu-a* *lssu-b*)))

(deftest let-star-special-unwind.later-special-init
  (%lssu-later-special)
  (:thrown :outer :outer))

;; Three bindings, the last one throwing: both live bindings come back.
(defun %lssu-three ()
  (let ((r (catch 'lssu-tag
             (let* ((*lssu-a* :inner) (*lssu-b* :inner) (*lssu-c* (%lssu-boom)))
               (list *lssu-a* *lssu-b* *lssu-c*)))))
    (list r *lssu-a* *lssu-b* *lssu-c*)))

(deftest let-star-special-unwind.three-bindings
  (%lssu-three)
  (:thrown :outer :outer :outer))

;; RETURN-FROM out of a later init, not just THROW.
(defun %lssu-return-from ()
  (let ((r (block out
             (let* ((*lssu-a* :inner) (q (return-from out :returned)))
               (list *lssu-a* q)))))
    (list r *lssu-a*)))

(deftest let-star-special-unwind.return-from-in-init
  (%lssu-return-from)
  (:returned :outer))

;; An error signalled by a later init and handled outside.
(defun %lssu-error ()
  (let ((r (handler-case
               (let* ((*lssu-a* :inner) (q (error "lssu")))
                 (list *lssu-a* q))
             (error () :handled))))
    (list r *lssu-a*)))

(deftest let-star-special-unwind.error-in-init
  (%lssu-error)
  (:handled :outer))

;; A leak used to be permanent, and the cheapest way to see that is to run the
;; same form twice: with a leak the SECOND call already starts from the
;; abandoned value.
(deftest let-star-special-unwind.no-residue-across-calls
  (list (%lssu-later-lexical) (%lssu-later-lexical) *lssu-a*)
  ((:thrown :outer) (:thrown :outer) :outer))

;;; ---- shapes that were already correct, pinned so they stay so ----

;; Parallel LET evaluates every init before binding anything, so a throwing init
;; leaves nothing to pop.
(deftest let-star-special-unwind.parallel-let-unaffected
  (let ((r (catch 'lssu-tag
             (let ((*lssu-a* :inner) (q (%lssu-boom)))
               (list *lssu-a* q)))))
    (list r *lssu-a*))
  (:thrown :outer))

;; The ordinary exit path still pops, and the value is visible inside.
(deftest let-star-special-unwind.normal-exit-still-pops
  (let ((inside (let* ((*lssu-a* :inner) (q 1))
                  (list *lssu-a* q))))
    (list inside *lssu-a*))
  ((:inner 1) :outer))

;; A throw from the BODY (rather than an init) always worked; keep it working.
(deftest let-star-special-unwind.throw-from-body
  (let ((r (catch 'lssu-tag
             (let* ((*lssu-a* :inner) (q 1))
               (declare (ignorable q))
               (%lssu-boom)))))
    (list r *lssu-a*))
  (:thrown :outer))

;; Nesting: an inner LET* of the same variable unwinding must leave the outer
;; binding, not the global value.
(deftest let-star-special-unwind.nested-same-variable
  (let* ((*lssu-a* :mid))
    (let ((r (catch 'lssu-tag
               (let* ((*lssu-a* :inner) (q (%lssu-boom)))
                 (list *lssu-a* q)))))
      (list r *lssu-a*)))
  (:thrown :mid))

;;; ---- what the leak did to the compiler (issue reproducer) ----
;;;
;;; The compiler speculates that a local function's captured variables can be
;;; passed as extra arguments, and abandons the speculation by unwinding when a
;;; call site turns out not to be able to reach the original binding -- a call
;;; from inside a closure that also returns from an enclosing block. The unwind
;;; crossed a LET* of a compile-state variable, so the retry compiled in the
;;; abandoned state, found the speculation still recorded there and unwound a
;;; second time -- to a tag whose CATCH was gone.

(defun %lssu-lifted-call-in-closure (test)
  (let ((test test))
    (flet ((g (x) (list x test)))
      (block nil
        (funcall (lambda (x) (or (g x) (return))) 1)))))

(deftest let-star-special-unwind.lifted-flet-called-from-closure
  (%lssu-lifted-call-in-closure :t)
  (1 :t))

;; The same shape as the library function that found this: LABELS with a single
;; self-free function (which is compiled as an FLET), reduced over a sequence,
;; with the early exit in the reducing closure.
(defun %lssu-gcp (seqs &key (test #'eql))
  (if (null seqs)
      nil
      (let ((test test))
        (labels ((gcp (x y)
                   (let ((miss (mismatch x y :test test)))
                     (cond ((not miss) x)
                           ((> miss 0) (subseq x 0 miss))
                           (t nil)))))
          (block nil
            (reduce (lambda (x y) (or (gcp x y) (return))) seqs))))))

(deftest let-star-special-unwind.greatest-common-prefix
  (list (%lssu-gcp '("foobar" "foobaz" "foo"))
        (%lssu-gcp '("abc" "xyz"))
        (%lssu-gcp '()))
  ("foo" nil nil))
