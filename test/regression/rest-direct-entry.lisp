;;; A &rest function gets typed entries for the call shapes that pass few enough
;;; extra arguments.
;;;
;;; A variadic LispFunction has one entry, taking LispObject[], so every call to
;;; a &rest function built an array -- including (F 1), where the rest list is
;;; NIL and there is nothing to collect. On a typed arity the extra arguments
;;; arrive as real parameters and a LET* binds the rest parameter to a fresh list
;;; of exactly them, which is what the array entry consed anyway. (LIST) with no
;;; arguments is NIL, so the required-only arity allocates nothing at all.
;;;
;;; The arities stop at required + 2, the same place the &key entry stops: each
;;; further one is another copy of the body in the image. Longer calls, and
;;; APPLY, still go through the array XEP.
;;;
;;; Only the --asm load path installs these; the fasl path stays array-only.
;;;
;;; Every expected value here was taken from SBCL.

(defun %rde-r0 (&rest r) r)
(defun %rde-r1 (a &rest r) (list a r))
(defun %rde-r2 (a b &rest r) (list a b r))

;;; Across the typed arities and past them into the array entry.

(deftest rest-direct-entry.no-required
  (list (%rde-r0) (%rde-r0 1) (%rde-r0 1 2) (%rde-r0 1 2 3) (%rde-r0 1 2 3 4))
  (nil (1) (1 2) (1 2 3) (1 2 3 4)))

(deftest rest-direct-entry.one-required
  (list (%rde-r1 1) (%rde-r1 1 2) (%rde-r1 1 2 3) (%rde-r1 1 2 3 4))
  ((1 nil) (1 (2)) (1 (2 3)) (1 (2 3 4))))

(deftest rest-direct-entry.two-required
  (list (%rde-r2 1 2) (%rde-r2 1 2 3))
  ((1 2 nil) (1 2 (3))))

;;; The array entry still backs FUNCALL and APPLY, and answers the same.

(deftest rest-direct-entry.funcall-and-apply
  (list (funcall #'%rde-r1 1 2)
        (apply #'%rde-r1 1 '(2))
        (apply #'%rde-r1 '(1 2 3)))
  ((1 (2)) (1 (2)) (1 (2 3))))

;;; The rest list is a fresh, mutable list per call -- the property callers rely
;;; on when they NCONC it or write through it. A shared or read-only list would
;;; pass every test above and fail here.

(defun %rde-mutate (a &rest r) (declare (ignore a)) (when r (setf (car r) :mutated)) r)
(defun %rde-nconc (&rest r) (nconc r (list :tail)))

(deftest rest-direct-entry.rest-list-is-fresh
  (let ((x (%rde-r0 1 2)))
    (setf (car x) :x)
    (list x (%rde-r0 1 2) (eq (%rde-r0 1) (%rde-r0 1))))
  ((:x 2) (1 2) nil))

(deftest rest-direct-entry.rest-list-is-mutable
  (list (%rde-mutate 0 1 2) (%rde-nconc 1 2))
  ((:mutated 2) (1 2 :tail)))

;;; Recursion: a self-call reaches the typed arity too, and APPLY of the same
;;; function reaches the array entry -- both in one call chain here.

(defun %rde-sum (&rest r) (if (null r) 0 (+ (car r) (apply #'%rde-sum (cdr r)))))
(defun %rde-count (a &rest r) (if (null r) a (apply #'%rde-count (1+ a) (cdr r))))

(deftest rest-direct-entry.recursion
  (list (%rde-sum 1 2 3 4 5) (%rde-count 0 :a :b :c))
  (15 3))

;;; The rest variable captured by a closure outlives the call.

(defun %rde-close (a &rest r) (lambda () (list a r)))

(deftest rest-direct-entry.closure-over-rest
  (funcall (%rde-close 1 2 3))
  (1 (2 3)))

;;; Only the primary value of an argument form is passed, as for any call.

(deftest rest-direct-entry.multiple-values-in-arguments
  (%rde-r0 (values 1 2) 3)
  (1 3))

;;; A rest parameter declared SPECIAL must keep its dynamic binding. The typed
;;; entry binds with a LET*, which cannot see a declaration sitting inside the
;;; implicit block, so such a function does not get one -- the array entry is
;;; correct and this is the check that it is still chosen.

(defun %rde-special (a &rest r)
  (declare (special r) (ignore a))
  (list r (symbol-value 'r)))

(deftest rest-direct-entry.special-rest-keeps-dynamic-binding
  (list (%rde-special 1) (%rde-special 1 2))
  ((nil nil) ((2) (2))))

;;; The point of the change.

(defun %rde-noop (a &rest r) (declare (ignore r)) a)

(defun %rde-call-none (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (%rde-noop 1)))))

(defun %rde-call-one (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (%rde-noop 1 2)))))

;; (F 1) has an empty rest list, so it can reach zero; the bound is above the
;; noise of two counter reads and well under the 32 B array it used to build.
;; Compiled-only: this is a statement about the entries the compiler emits, and
;; an emit-free build has none.
(deftest-compiled-only rest-direct-entry.empty-rest-allocates-nothing
  (< (bytes-per-op #'%rde-call-none) 1)
  t)

;; (F 1 2) must still cons its one-element rest list -- 32 B, and nothing else.
;; The bound sits between that and the 72 B it cost with the array.
(deftest-compiled-only rest-direct-entry.one-rest-costs-only-the-cons
  (< (bytes-per-op #'%rde-call-one) 50)
  t)
