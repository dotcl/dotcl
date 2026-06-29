;;; Closure and free-variable capture regression tests

;;; Basic closure over immutable variable
(deftest closure-basic
  (let ((x 10))
    (funcall (lambda () x)))
  10)

;;; Closure captures mutable variable (setq)
(deftest closure-mutation
  (let ((n 0))
    (let ((inc (lambda () (setq n (+ n 1)))))
      (funcall inc)
      (funcall inc)
      (funcall inc)
      n))
  3)

;;; Multiple closures share mutable state
(deftest closure-shared-state
  (let ((n 0))
    (let ((inc (lambda () (setq n (+ n 1))))
          (get (lambda () n)))
      (funcall inc)
      (funcall inc)
      (funcall get)))
  2)

;;; Closure returned from function (upward funarg)
(defun make-adder (x)
  (lambda (y) (+ x y)))

(deftest closure-upward-funarg
  (funcall (make-adder 5) 3)
  8)

;;; Closure over loop variable captured per-iteration (do loop)
(deftest closure-do-loop-capture
  (let ((fns nil))
    (do ((i 0 (+ i 1)))
        ((= i 3))
      (let ((captured i))
        (push (lambda () captured) fns)))
    (mapcar #'funcall (reverse fns)))
  (0 1 2))

;;; Nested closures
(deftest closure-nested
  (let ((x 1))
    (let ((f (lambda ()
               (let ((y 2))
                 (lambda () (+ x y))))))
      (funcall (funcall f))))
  3)

;;; Closure in labels
(deftest closure-labels
  (labels ((counter (n)
             (lambda () n)))
    (funcall (counter 42)))
  42)

;;; (setf (the type place) value) must correctly assign through type annotation
(deftest setf-the-basic
  (let ((x 0))
    (setf (the fixnum x) 42)
    x)
  42)

;;; (incf (the fixnum var)) must correctly mutate variable
(deftest incf-the-basic
  (let ((n 5))
    (incf (the fixnum n))
    n)
  6)

;;; (decf (the fixnum var)) must correctly mutate variable
(deftest decf-the-basic
  (let ((n 5))
    (decf (the fixnum n))
    n)
  4)

;;; (incf (the fixnum n)) in a closure must mutate captured var.
;;; Bug: mutation analysis didn't unwrap (the type var), so n wasn't boxed.
(deftest incf-the-in-closure
  (let ((n 0))
    (let ((incr (lambda () (incf (the fixnum n)))))
      (funcall incr)
      (funcall incr)
      (funcall incr)
      n))
  3)

;;; (setf (the fixnum n) val) in a closure must mutate captured var
(deftest setf-the-in-closure
  (let ((x 0))
    (funcall (lambda () (setf (the fixnum x) 99)))
    x)
  99)

;;; Lisp-2: a local variable that is mutated + captured (and therefore lives
;;; in a boxed LispObject[1] cell) must NOT shadow a same-named global function
;;; when that name appears in operator position. Regression for the
;;; quicklisp http.lisp `(or (url *proxy-url*) url)` miscompile, where `url`
;;; was both the http-fetch parameter (mutated, captured by with-connection's
;;; lambda) and a global function: the call `(url ...)` wrongly funcalled the
;;; boxed variable cell, yielding "cannot cast <value> to LispFunction".
(defun l2box-fn (thing) (list :fn thing))

(deftest lisp2-boxed-var-shadows-global-fn
  (flet ((caller (l2box-fn)
           (setf l2box-fn (list :merged l2box-fn))   ; mutate -> needs a cell
           (flet ((capture () l2box-fn))              ; capture -> boxes the cell
             (declare (ignorable #'capture))
             (l2box-fn 99))))                          ; operator: must call global
    (caller 42))
  (:fn 99))

(deftest lisp2-boxed-var-shadow-or-form
  (flet ((caller (l2box-fn)
           (setf l2box-fn (list :merged l2box-fn))
           (let ((proxy (or (l2box-fn 7) l2box-fn)))   ; fn-call + value-ref same form
             (flet ((capture () (list l2box-fn proxy)))
               (funcall #'capture)))))
    (caller 42))
  ((:merged 42) (:fn 7)))

;;; (setf (slot-value obj slot) VALUE) must pre-evaluate obj/slot into temps so
;;; the evaluation stack is empty when VALUE is compiled. If VALUE contains a
;;; try block (a handler-case, or a LOOP `being each hash-value` which expands
;;; with one), pushing obj+slot inline first left the stack non-empty at the
;;; try-block entry → InvalidProgramException. Regression for quicklisp
;;; install-dist (slot-unbound for PROVIDED-SYSTEMS does exactly this).
(defclass ssv-holder () ((s :accessor ssv-s :initform nil)))

(deftest setf-slot-value-loop-hash-value
  (let ((h (make-hash-table)) (o (make-instance 'ssv-holder)))
    (setf (gethash :a h) 1 (gethash :b h) 2)
    (setf (slot-value o 's)
          (loop for v being each hash-value of h collect v))
    (sort (copy-list (slot-value o 's)) #'<))
  (1 2))

(deftest setf-slot-value-handler-case-value
  (let ((o (make-instance 'ssv-holder)))
    (setf (slot-value o 's)
          (handler-case (error "x") (error () :caught)))
    (slot-value o 's))
  :caught)

;;; A cond clause whose TEST is a symbol that is BOTH a captured local variable
;;; and a global function name (Lisp-2) must capture the variable. Free-var
;;; analysis treated the clause (sap ...) as a function call (car = fn name) and
;;; dropped `sap` as a free-var ref, so it wasn't captured → "Unbound variable"
;;; at run time. Surfaced via cl-ppcre create-scanner-aux's `start-anchored-p`
;;; (a defgeneric) reused as a closure-captured parameter in (cond (start-anchored-p ...)).
(defun cap-anchored-p (x) (declare (ignore x)) :global-fn)

(deftest cond-test-captures-var-shadowing-global-fn
  (funcall (funcall (lambda (cap-anchored-p)
                      (lambda (n)
                        (declare (ignore n))
                        (cond (cap-anchored-p :anchored) (t :unanchored))))
                    t)
           0)
  :anchored)

(deftest cond-test-captures-var-false-branch
  (funcall (funcall (lambda (cap-anchored-p)
                      (lambda () (cond (cap-anchored-p :y) (t :n))))
                    nil))
  :n)

;; a lambda parameter whose name also names a global generic function, when
;; captured ONLY inside a nested closure built via a memoizing constructor and read
;; under fixnum declarations, must resolve to the captured lexical (not the GF) and
;; come out bound. cl-ppcre's create-scanner-aux / START-ANCHORED-P shape; was
;; "Unbound variable: START-ANCHORED-P" at scan time on 0.1.8.
(defgeneric i337-start-anchored-p (regex &optional in-seq-p))
(defmethod i337-start-anchored-p ((r t) &optional in-seq-p)
  (declare (ignore in-seq-p)) :gf)

(defun i337-cache (fn)
  (let ((tbl (make-hash-table :test #'equalp)))
    (lambda (k) (or (gethash k tbl) (setf (gethash k tbl) (funcall fn k))))))

(defun i337-make-scanner (i337-start-anchored-p)
  (funcall (i337-cache
            (lambda (key)
              (declare (ignore key))
              (lambda (start)
                (declare (type fixnum start))
                (cond (i337-start-anchored-p (list :anchored start))
                      (t (list :free start))))))
           :k))

(deftest i337-param-shadows-gf-captured-in-nested-closure
  (list (funcall (i337-make-scanner t) 0)
        (funcall (i337-make-scanner nil) 5))
  ((:anchored 0) (:free 5)))

;; Closure body methods are stored per compilation unit and resolved at runtime
;; when MakeClosure runs. Each call to a closure-returning function must mint a
;; fresh, independent closure (distinct captured env), and repeated invocation
;; of the same closure body must keep resolving the same body method.
(defun s3-adder (n) (lambda (x) (+ x n)))

(deftest s3-distinct-closures-independent-env
  (let ((a (s3-adder 10)) (b (s3-adder 100)))
    (list (funcall a 5) (funcall b 5) (funcall a 1)))
  (15 105 11))

;; A counter closure mutating its captured binding across many calls — exercises
;; the same body method resolved repeatedly from the unit store.
(defun s3-counter ()
  (let ((c 0)) (lambda () (incf c))))

(deftest s3-counter-state-persists
  (let ((k (s3-counter)))
    (list (funcall k) (funcall k) (funcall k)))
  (1 2 3))

;; Nested closures: outer returns a closure that itself builds inner closures.
;; Both body methods come from the same unit; each inner capture is independent.
(defun s3-nested (a)
  (lambda (b) (lambda (c) (+ a b c))))

(deftest s3-nested-closures
  (let* ((f (s3-nested 100))
         (g (funcall f 20))
         (h (funcall f 30)))
    (list (funcall g 3) (funcall h 3)))
  (123 133))

;; S4: closure body DynamicMethods now live in a weakly-rooted per-unit
;; store, reclaimed once no function can still call into the unit. Correctness
;; requirement: any closure that is still reachable must stay callable across a
;; full GC (the unit holder is pinned via the enclosing fn / the closure itself).
(defun s4-make-adder (n) (lambda (x) (+ x n)))

(deftest s4-defun-closure-survives-gc
  (let ((a (s4-make-adder 10)) (b (s4-make-adder 100)))
    (dotcl:gc)                          ; force full collect + finalizers
    (list (funcall a 5) (funcall b 5)))
  (15 105))

;; Nested closures resolve a SIBLING body DM when the outer closure is called
;; after a GC — the outer closure must keep its unit alive for the inner build.
(defun s4-nested (a) (lambda (b) (lambda (c) (+ a b c))))

(deftest s4-nested-closure-survives-gc
  (let ((g (funcall (s4-nested 100) 20)))
    (dotcl:gc)
    (funcall g 3))
  123)

;; A closure produced by a transient EVAL unit, retained in a binding, must
;; remain callable after GC (the closure pins its unit even though the toplevel
;; code that built it is gone).
(deftest s4-eval-transient-closure-survives-gc
  (let ((f (eval '(let ((k 7)) (lambda (x) (* x k))))))
    (dotcl:gc)
    (funcall f 6))
  42)

;; S5: non-capturing lambdas (MAKE-FUNCTION / MAKE-FUNCTION-DIRECT) are now
;; stored per compilation unit and reclaimed with it, instead of pinned in the
;; global constant pool forever. Correctness: a non-capturing lambda returned by
;; a rooted defun must stay callable across a full GC (the defun pins its unit).
(defun s5-gen () (lambda (x) (* x x)))

(deftest s5-rooted-noncapturing-lambda-survives-gc
  (let ((g (s5-gen)))
    (dotcl:gc)
    (funcall g 7))
  49)

;; A non-capturing lambda whose body builds a further lambda must keep its unit
;; alive so the inner one resolves when the outer is later called after a GC.
(defun s5-outer () (lambda () (funcall (lambda (z) (+ z 1)) 41)))

(deftest s5-nested-noncapturing-survives-gc
  (let ((o (s5-outer)))
    (dotcl:gc)
    (funcall o))
  42)
