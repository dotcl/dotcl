;;; Backquote templates: what they build, not how.
;;;
;;; The reader expands a template at read time. It used to wrap every element in
;;; its own (LIST x) and fold the lot with binary APPEND, which costs a call and
;;; a fresh cons per element and compiles APPEND's variadic call to an args
;;; array. Runs of ordinary elements are now one LIST (or LIST* before a dotted
;;; tail) and only ,@ forces an APPEND.
;;;
;;; CLHS 2.4.6 leaves the expansion to the implementation, so these tests assert
;;; the value a template builds and never its shape. What is easy to get wrong
;;; when rewriting the expander is the joins: a splice at either end, a dotted
;;; tail after a splice, and a template that is nothing but a splice.

(defparameter *bqe-a* 1)
(defparameter *bqe-b* 2)
(defparameter *bqe-l* '(x y))
(defparameter *bqe-tl* 'z)
(defparameter *bqe-empty* '())

;;; ---- no splices: one LIST ----

(deftest backquote-expansion.plain
  `(a b c)
  (a b c))

(deftest backquote-expansion.unquotes
  `(foo ,*bqe-a* ,*bqe-b*)
  (foo 1 2))

(deftest backquote-expansion.nested-template
  `(a (b ,*bqe-a*) (c (d ,*bqe-b*)))
  (a (b 1) (c (d 2))))

;;; ---- dotted tails ----

(deftest backquote-expansion.dotted-constant
  `(,*bqe-a* . b)
  (1 . b))

(deftest backquote-expansion.dotted-unquote
  `(a . ,*bqe-b*)
  (a . 2))

(deftest backquote-expansion.dotted-after-several
  `(a ,*bqe-a* b . ,*bqe-tl*)
  (a 1 b . z))

;;; ---- splices ----

(deftest backquote-expansion.splice-middle
  `(foo ,*bqe-a* ,@*bqe-l* bar)
  (foo 1 x y bar))

(deftest backquote-expansion.splice-only
  `(,@*bqe-l*)
  (x y))

(deftest backquote-expansion.splice-leading
  `(,@*bqe-l* tail)
  (x y tail))

(deftest backquote-expansion.splice-trailing
  `(head ,@*bqe-l*)
  (head x y))

(deftest backquote-expansion.two-splices
  `(,@*bqe-l* ,@*bqe-l*)
  (x y x y))

(deftest backquote-expansion.splice-of-empty
  `(a ,@*bqe-empty* b)
  (a b))

(deftest backquote-expansion.splice-then-dotted
  `(a ,@*bqe-l* . ,*bqe-tl*)
  (a x y . z))

;;; A splice that is not last is copied — APPEND copies all but its final
;;; argument — so mutating the source afterwards must not reach the result.
;;; (A trailing ,@ is the final argument and does share, which is APPEND's
;;; contract, so it is not asserted here.)
(deftest backquote-expansion.splice-copies
  (let* ((src (list 1 2))
         (built `(0 ,@src 3)))
    (setf (car src) :changed)
    built)
  (0 1 2 3))

;;; ---- vectors (CLHS 2.4.6) ----

(deftest backquote-expansion.vector
  (coerce `#(1 ,*bqe-a* ,@*bqe-l*) 'list)
  (1 1 x y))

;;; ---- empty and single ----

(deftest backquote-expansion.empty
  `()
  nil)

(deftest backquote-expansion.single-unquote
  `(,*bqe-a*)
  (1))

;;; ---- nested backquote ----

;;; The inner template is not built by the outer one; evaluating the outer
;;; result must yield it.
(deftest backquote-expansion.nested-backquote
  (eval (eval '`(list 1 `(2 ,,*bqe-a*))))
  (1 (2 1)))

;;; ---- templates as macro bodies, the real consumer ----

(defmacro %bqe-when2 (test &body body)
  `(if ,test (progn ,@body) nil))

(deftest backquote-expansion.macro-body
  (list (%bqe-when2 t 1 2 3) (%bqe-when2 nil 1 2 3))
  (3 nil))

(defmacro %bqe-bind (pairs &body body)
  `(let ,(mapcar (lambda (p) `(,(first p) ,(second p))) pairs) ,@body))

(deftest backquote-expansion.macro-with-mapped-template
  (%bqe-bind ((p 10) (q 20)) (+ p q))
  30)
