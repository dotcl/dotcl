;;; (declare (inline f)) on an FLET/LABELS function.
;;;
;;; The declaration used to be read and thrown away. Honouring it is worth more
;;; than the call it saves: a local function is its own .NET method, so a
;;; RETURN-FROM out of it can only leave that method by throwing, and unwinding
;;; costs hundreds of bytes. Substituted into the caller, the same RETURN-FROM
;;; is a jump.
;;;
;;; Substitution is only sound while every name the body uses still means the
;;; binding it meant where the body was written, so most of what is tested here
;;; is the REFUSALS: a call site that rebinds one of those names has to get an
;;; ordinary call and the definition's answer, not the call site's.

;;; --- The substitution itself ---

(deftest local-inline.value
  (flet ((f (n) (* n 2)))
    (declare (inline f))
    (f 21))
  42)

(deftest local-inline.labels-value
  (labels ((f (n) (* n 2)))
    (declare (inline f))
    (list (f 1) (f 2)))
  (2 4))

;;; Arguments are evaluated once each, left to right, in the caller's scope --
;;; the LET the expansion is made of, not the body it wraps.
(deftest local-inline.argument-evaluation
  (let ((log '()))
    (flet ((f (a b) (list a b)))
      (declare (inline f))
      (let ((r (f (progn (push :a log) 1)
                  (progn (push :b log) 2))))
        (list r (reverse log)))))
  ((1 2) (:a :b)))

;;; An argument that names one of the parameters still reads the caller's
;;; binding: LET binds in parallel.
(deftest local-inline.parameter-named-argument
  (let ((n 5))
    (flet ((f (n) (* n 2)))
      (declare (inline f))
      (f (+ n 1))))
  12)

(deftest local-inline.multiple-values
  (flet ((f (n) (values n (* n 2))))
    (declare (inline f))
    (multiple-value-list (f 3)))
  (3 6))

;;; The implicit block the binding form establishes has to survive the move.
(deftest local-inline.return-from-own-block
  (flet ((f (n) (when (evenp n) (return-from f :even)) (* n 2)))
    (declare (inline f))
    (list (f 3) (f 4)))
  (6 :even))

;;; The case the whole thing is for: an escape to a block OUTSIDE the local
;;; function. Inlined, it is a jump within one method.
(deftest local-inline.escape-to-outer-block
  (list (block out
          (labels ((f (n) (when (evenp n) (return-from out :even)) (* n 2)))
            (declare (inline f))
            (+ (f 3) 1)))
        (block out
          (labels ((f (n) (when (evenp n) (return-from out :even)) (* n 2)))
            (declare (inline f))
            (+ (f 4) 1))))
  (7 :even))

;;; Several call sites each take their own copy.
(deftest local-inline.two-call-sites
  (block out
    (flet ((f (n) (when (evenp n) (return-from out :even)) (* n 2)))
      (declare (inline f))
      (+ (f 3) (f 5))))
  16)

;;; Past *LOCAL-INLINE-CALL-LIMIT* the remaining sites are ordinary calls. The
;;; answers cannot tell the difference, which is the point.
(deftest local-inline.past-the-call-limit
  (let ((x 1))
    (flet ((f (n) (+ n x)))
      (declare (inline f))
      (list (f 1) (f 2) (f 3) (f 4) (f 5) (f 6))))
  (2 3 4 5 6 7))

;;; --- Refusals: the body has to keep meaning what it meant ---

;;; A variable the body reads, rebound at the call site. Inlining it blindly
;;; would read the call site's binding: a wrong answer, not a slow one.
(deftest local-inline.shadowed-variable
  (let ((x 1))
    (flet ((f () x))
      (declare (inline f))
      (list (f)
            (let ((x 2)) (declare (ignorable x)) (f)))))
  (1 1))

;;; Same, through a symbol macro rather than a binding.
(deftest local-inline.shadowed-by-symbol-macro
  (let ((y 1))
    (flet ((f () y))
      (declare (inline f))
      (list (f)
            (symbol-macrolet ((y 99)) (f)))))
  (1 1))

;;; A function the body calls, rebound at the call site.
(deftest local-inline.shadowed-function
  (flet ((g () :outer))
    (flet ((f () (g)))
      (declare (inline f))
      (list (f)
            (flet ((g () :inner)) (f)))))
  (:outer :outer))

;;; A block the body returns from, rebound at the call site. Refused, the call
;;; returns from the OUTER block, which is the one the definition names.
(deftest local-inline.shadowed-block
  (block out
    (flet ((f () (return-from out :from-definition)))
      (declare (inline f))
      (block out (f))
      :not-reached))
  :from-definition)

;;; A LABELS function that calls itself is not substitutable at all: inside the
;;; body the name means the binding, and where the body was written it did not.
(deftest local-inline.recursive
  (labels ((fact (n) (if (<= n 1) 1 (* n (fact (1- n))))))
    (declare (inline fact))
    (fact 5))
  120)

;;; A lambda list with defaulting is left to the ordinary call.
(deftest local-inline.optional-parameter
  (flet ((f (a &optional (b 10)) (+ a b)))
    (declare (inline f))
    (list (f 1) (f 1 2)))
  (11 3))

(deftest local-inline.rest-parameter
  (flet ((f (&rest args) (length args)))
    (declare (inline f))
    (list (f) (f 1 2 3)))
  (0 3))

;;; The wrong number of arguments is not a call this can rewrite; the ordinary
;;; call path reports it as it always did.
(deftest local-inline.arity-mismatch-still-errors
  (handler-case
      (funcall (lambda ()
                 (flet ((f (a b) (list a b)))
                   (declare (inline f))
                   (funcall #'f 1))))
    (error () :error))
  :error)

;;; #'f is still the function the user wrote, and having taken it means the
;;; binding is still built.
(deftest local-inline.function-value
  (let ((x 3))
    (flet ((f (n) (+ n x)))
      (declare (inline f))
      (list (f 1) (mapcar #'f (list 1 2)))))
  (4 (4 5)))

;;; A call from inside a closure: the closure's own scope is not the scope the
;;; body was written in, so a body with a free variable is called, not copied.
(deftest local-inline.called-from-closure
  (let ((x 10))
    (flet ((f (n) (+ n x)))
      (declare (inline f))
      (list (f 1) (funcall (lambda () (f 2))))))
  (11 12))

;;; A body with no free variables, called from inside a nested LAMBDA, and
;;; escaping to a block outside that lambda. Substituting it there would move it
;;; into a different method, which changes which frame the RETURN-FROM leaves --
;;; so the block has to be one of the names that refuses.
(deftest local-inline.escape-from-inside-closure
  (block out
    (flet ((f (n) (when (evenp n) (return-from out :even)) (* n 2)))
      (declare (inline f))
      (list (funcall (lambda () (f 3)))
            (funcall (lambda () (f 4))))))
  :even)

;;; Declaring one of several functions inline leaves the others alone.
(deftest local-inline.one-of-several
  (flet ((f (n) (* n 2))
         (g (n) (* n 3)))
    (declare (inline f))
    (list (f 2) (g 2)))
  (4 6))

;;; NOTINLINE at the call site refuses the substitution; the answer is the same.
(deftest local-inline.notinline-at-call-site
  (flet ((f (n) (* n 2)))
    (declare (inline f))
    (list (f 1)
          (locally (declare (notinline f)) (f 2))))
  (2 4))

;;; --- What it costs ---
;;;
;;; The escape is the measurement that motivated the feature: leaving a .NET
;;; frame means throwing, and the throw is what allocates. Inlined, there is no
;;; frame to leave. Compiled mode only: the interpreter does not substitute
;;; anything, and its allocation says nothing about this.

(defun %li-bytes () (nth 4 (dotcl:gc-stats)))

(defun %li-escape-plain (x)
  (block out
    (labels ((f (n) (when (evenp n) (return-from out :even)) (* n 2)))
      (+ (f x) 1))))

(defun %li-escape-inline (x)
  (block out
    (labels ((f (n) (when (evenp n) (return-from out :even)) (* n 2)))
      (declare (inline f))
      (+ (f x) 1))))

;;; The shape the feature was built for: a body that returns from its OWN block
;;; as well as from one outside it -- cl-ppcre's ADVANCE-FN does both. Naming
;;; itself in a RETURN-FROM must not be read as calling itself, or every local
;;; function that returns early declines and this measures the same as PLAIN.
(defun %li-both-plain (x)
  (block out
    (labels ((f (n)
               (when (evenp n) (return-from out :even))
               (when (> n 100) (return-from f :big))
               (* n 2)))
      (list (f x)))))

(defun %li-both-inline (x)
  (block out
    (labels ((f (n)
               (when (evenp n) (return-from out :even))
               (when (> n 100) (return-from f :big))
               (* n 2)))
      (declare (inline f))
      (list (f x)))))

(defun %li-loop (f n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (funcall f 4)))))

(defun %li-per-call (f)
  "Bytes allocated per escaping call of F, smallest of five runs."
  (%li-loop f 2000)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (%li-bytes)))
        (%li-loop f 20000)
        (let ((used (floor (- (%li-bytes) before) 20000)))
          (when (or (null best) (< used best)) (setq best used)))))))

;;; Not a fixed number on either side: the point is the difference between
;;; "throws to get out" (hundreds of bytes, all of it the unwind) and "jumps to
;;; get out" (nothing at all).
(deftest-compiled-only local-inline.escape-allocation
  (list (> (%li-per-call #'%li-escape-plain) 100)
        (= (%li-per-call #'%li-escape-inline) 0))
  (t t))

(deftest-compiled-only local-inline.escape-allocation-with-own-return
  (list (> (%li-per-call #'%li-both-plain) 100)
        (= (%li-per-call #'%li-both-inline) 0)
        (%li-both-inline 3)
        (%li-both-inline 4)
        (%li-both-inline 101))
  (t t (6) :even (:big)))
