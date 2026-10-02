;;; A call site that consumes a result at once tells the callee (primary only,
;;; or values read from the thread state), and a function whose tail returns
;;; multiple values then builds no MvReturn. These pin the cases where the
;;; values travel further than the immediate caller, or where something runs
;;; between the callee's return and the consumer. Expected values are SBCL's.

(defun %mcm-p2 (x) (values x (1+ x)))
(defun %mcm-p3 (x) (values x (1+ x) (+ x 2)))
(defun %mcm-p4 (x) (values x (1+ x) (+ x 2) (+ x 3)))
(defun %mcm-tail (x) (%mcm-p2 x))
(defun %mcm-branch (x) (if (evenp x) (values x :even) (let ((y (* x 10))) (values y :odd))))
(defun %mcm-loop (n acc) (if (= n 0) (values acc :done) (%mcm-loop (1- n) (+ acc n))))

(deftest mv-call-site-mode.single
  (list (%mcm-p2 1) (%mcm-p3 1) (%mcm-p4 1) (%mcm-tail 5) (1+ (%mcm-p4 7)))
  (1 1 1 5 8))

(deftest mv-call-site-mode.bind
  (list (multiple-value-bind (a b) (%mcm-p2 1) (list a b))
        (multiple-value-bind (a b c) (%mcm-p3 1) (list a b c))
        (multiple-value-bind (a b c d) (%mcm-p4 1) (list a b c d))
        (multiple-value-bind (a b) (%mcm-p4 1) (list a b))
        (multiple-value-bind (a b c) (%mcm-p2 1) (list a b c))
        (multiple-value-bind (a b) (%mcm-tail 5) (list a b)))
  ((1 2) (1 2 3) (1 2 3 4) (1 2) (1 2 nil) (5 6)))

(deftest mv-call-site-mode.list-and-nth
  (list (multiple-value-list (%mcm-p3 1)) (multiple-value-list (%mcm-tail 2))
        (nth-value 1 (%mcm-p4 1)) (nth-value 3 (%mcm-p4 1))
        (multiple-value-call #'list (%mcm-p2 1) (%mcm-p3 10)))
  ((1 2 3) (2 3) 2 4 (1 2 10 11 12)))

(deftest mv-call-site-mode.branches-and-tco
  (list (multiple-value-list (%mcm-branch 2)) (multiple-value-list (%mcm-branch 3))
        (%mcm-branch 3) (multiple-value-list (%mcm-loop 10 0)) (%mcm-loop 4 0))
  ((2 :even) (30 :odd) 30 (55 :done) 10))

;; Something that publishes values runs between the return and the consumer.
(deftest mv-call-site-mode.unwind-protect
  (list (multiple-value-list (unwind-protect (%mcm-p2 1) (%mcm-p3 100)))
        (multiple-value-bind (a b c) (unwind-protect (%mcm-p3 1) (%mcm-p4 100)) (list a b c))
        (multiple-value-bind (a b) (unwind-protect (%mcm-tail 7) (%mcm-p2 100)) (list a b)))
  ((1 2) (1 2 3) (7 8)))

(deftest mv-call-site-mode.handler-case
  (list (multiple-value-bind (a b) (handler-case (%mcm-p2 1) (error () (values :e :e))) (list a b))
        (multiple-value-list (handler-case (%mcm-p3 4) (error () :e)))
        (multiple-value-list (ignore-errors (%mcm-p2 3))))
  ((1 2) (4 5 6) (3 4)))

;; A generic function: the primary method's values survive the :AFTER method,
;; which returns values of its own.
(defgeneric %mcm-gf (x))
(defmethod %mcm-gf ((x integer)) (%mcm-p2 x))
(defmethod %mcm-gf :after ((x integer)) (%mcm-p3 1000))
(deftest mv-call-site-mode.gf-after
  (list (multiple-value-list (%mcm-gf 1)) (multiple-value-bind (a b) (%mcm-gf 5) (list a b)) (%mcm-gf 9))
  ((1 2) (5 6) 9))

;; A wrapper installed on the symbol (TRACE) calls the function and prints the
;; result before returning it; the values must survive the printing.
(defun %mcm-traced (x) (values x (* x 2) (* x 3)))
(deftest mv-call-site-mode.trace
  (let ((*trace-output* (make-broadcast-stream)))
    (unwind-protect
         (progn (trace %mcm-traced)
                (list (multiple-value-list (%mcm-traced 2))
                      (multiple-value-bind (a b c) (%mcm-traced 3) (list a b c))
                      (%mcm-traced 4)))
      (untrace %mcm-traced)))
  ((2 4 6) (3 6 9) 4))

;; A call that fails before the body runs (wrong number of arguments) must not
;; leave its expectation for a later call of the same function.
(defun %mcm-keep (f) (let ((r (funcall f 1))) (%mcm-p3 50) r))
(deftest mv-call-site-mode.after-arity-error
  (progn (ignore-errors (multiple-value-bind (a b) (%mcm-p2) (list a b)))
         (list (multiple-value-list (%mcm-keep #'%mcm-p2))
               (multiple-value-list (funcall #'%mcm-p2 8))
               (multiple-value-list (apply #'%mcm-p3 '(1)))))
  ((1) (8 9) (1 2 3)))

;; Values handed through a closure and through APPLY.
(deftest mv-call-site-mode.closure-and-apply
  (let ((f (lambda (x) (%mcm-p2 x))))
    (list (multiple-value-list (funcall f 3))
          (multiple-value-bind (a b) (apply #'%mcm-p2 '(4)) (list a b))
          (multiple-value-list (funcall (lambda () (%mcm-p4 0))))))
  ((3 4) (4 5) (0 1 2 3)))

;; RETURN-FROM the function's own block with values, from inside nested forms,
;; and the block's natural end: both answer the caller's mode.
(defun %mcm-lookup (key alist)
  (let ((hit (assoc key alist)))
    (when hit
      (let ((v (cdr hit)))
        (return-from %mcm-lookup (values v t)))))
  (let ((d (list :default key)))
    (values d nil)))
(deftest mv-call-site-mode.return-from
  (let ((al '((:a . 1) (:b . 2))))
    (list (%mcm-lookup :a al) (%mcm-lookup :z al)
          (multiple-value-list (%mcm-lookup :b al))
          (multiple-value-bind (v f) (%mcm-lookup :z al) (list v f))
          (multiple-value-bind (v f) (the (values t boolean) (%mcm-lookup :a al)) (list v f))
          (multiple-value-list (the (values t boolean) (%mcm-lookup :a al)))
          (let ((x (the (values t boolean) (%mcm-lookup :b al)))) x)))
  (1 (:default :z) (2 t) ((:default :z) nil) (1 t) (1 t) 2))

;; THE returns all the values of its form in the tree-walk evaluator too (the
;; only evaluator of the emit-free build), also a FIXNUM THE, whose check
;; reads the primary value only.
(deftest mv-call-site-mode.the-values-interpreted
  (let ((dotcl:*evaluator-mode* :interpret))
    (list (eval '(multiple-value-list (the (values t boolean) (values 1 t))))
          (eval '(multiple-value-bind (v f) (the (values t boolean) (values 2 nil)) (list v f)))
          (eval '(multiple-value-list (the t (values 3 4 5))))
          (eval '(multiple-value-list (the fixnum (values 6 7))))))
  ((1 t) (2 nil) (3 4 5) (6 7)))

;; A block in the tail whose RETURN-FROM crosses an UNWIND-PROTECT whose
;; cleanup returns values of its own.
(defun %mcm-cleanup (x)
  (block nil
    (unwind-protect (return (values x (* 2 x)))
      (%mcm-p3 100))))
(deftest mv-call-site-mode.return-across-cleanup
  (list (%mcm-cleanup 3) (multiple-value-list (%mcm-cleanup 4))
        (multiple-value-bind (a b) (%mcm-cleanup 5) (list a b)))
  (3 (4 8) (5 10)))

;; Closures (a LABELS function handed out with #') called through FUNCALL
;; under MULTIPLE-VALUE-BIND, the shape of a type-test combinator.
(defun %mcm-any (op thing list)
  (let ((certain t))
    (dolist (i list (values nil certain))
      (multiple-value-bind (v c) (funcall op thing i)
        (if c (when v (return (values t t))) (setf certain nil))))))
(defun %mcm-typep (obj types tag)
  (labels ((recurse (o ty)
             (cond ((eq ty :unknown) (values nil nil))
                   ((consp ty) (%mcm-any #'recurse o ty))
                   (t (values (and (typep o ty) tag) t)))))
    (recurse obj types)))
(deftest mv-call-site-mode.closure-funcall
  (list (multiple-value-list (%mcm-typep 1 '(string integer) :yes))
        (multiple-value-list (%mcm-typep 1 '(string :unknown) :yes))
        (multiple-value-list (%mcm-typep "a" '(integer (symbol string)) :in))
        (%mcm-typep 1 'integer :p))
  ((t t) (nil nil) (t t) :p))

;; &optional and &key functions, whose entries are built per arity.
(defun %mcm-opt (x &optional (d 2)) (values (floor x d) (mod x d)))
(defun %mcm-key (x &key (d 3) (e 0)) (values (+ x d) (+ x e) :k))
(deftest mv-call-site-mode.optional-and-key
  (list (%mcm-opt 7) (multiple-value-list (%mcm-opt 7)) (multiple-value-list (%mcm-opt 7 3))
        (%mcm-key 1) (multiple-value-list (%mcm-key 1 :e 5))
        (multiple-value-bind (a b c) (%mcm-key 1 :d 10) (list a b c)))
  (3 (3 1) (2 1) 4 (4 6 :k) (11 1 :k)))

;; A body that returns only its primary value must still leave the thread
;; state saying "one value": something it called earlier may have published
;; values whose first is that same object, and a caller further up that reads
;; the state (MULTIPLE-VALUE-LIST of a function returning the value as is)
;; would take those for its own.
(defun %mcm-g2 (x) (floor x 1) (values x t))
(defun %mcm-f2 (x) (let ((r (%mcm-g2 x))) r))
(deftest mv-call-site-mode.primary-collapses
  (list (multiple-value-list (%mcm-f2 3))
        (multiple-value-bind (a b) (%mcm-f2 4) (list a b))
        (nth-value 1 (%mcm-f2 5)))
  ((3) (4 nil) nil))

;; A RETURN-FROM out of a closure hands its values to another body's block:
;; the mode the closure itself was called with says nothing about them.
(defun %mcm-search (list)
  (multiple-value-list
   (block search
     (labels ((rec (l)
                (cond ((null l) (values nil nil))
                      ((evenp (car l)) (return-from search (values (car l) :found)))
                      (t (rec (cdr l))))))
       (multiple-value-bind (a b) (rec list) (list :inner a b))))))
(defun %mcm-search2 (list)
  (let ((r (block search
             (labels ((rec (l)
                        (cond ((null l) nil)
                              ((evenp (car l)) (return-from search (values (car l) :found)))
                              (t (rec (cdr l))))))
               (rec list)))))
    r))
(deftest mv-call-site-mode.nonlocal-return-from
  (list (%mcm-search '(1 3 4)) (%mcm-search '(1 3))
        (multiple-value-list (%mcm-search2 '(5 6))))
  ((4 :found) ((:inner nil nil)) (6)))

;; The same with the closure called where only its primary value is used.
(defun %mcm-search3 (list)
  (multiple-value-list
   (block search
     (flet ((probe (x)
              (if (evenp x) (return-from search (values x :found)) (values nil x))))
       (dolist (x list) (let ((r (probe x))) r))
       (values nil :none)))))
(deftest mv-call-site-mode.nonlocal-return-from-primary
  (list (%mcm-search3 '(1 4 5)) (%mcm-search3 '(1 3)))
  ((4 :found) (nil :none)))

;; A MULTIPLE-VALUE-BIND, NTH-VALUE or MULTIPLE-VALUE-LIST whose form ends in
;; its own (VALUES ...) or call: the values reach the consumer directly.
(defun %mcm-local (x)
  (multiple-value-bind (a b)
      (let ((y (* x 2)))
        (if (> y 5) (values y :big) (%mcm-p2 y)))
    (list a b)))
(defun %mcm-local-block (x)
  (multiple-value-bind (a b c)
      (block found
        (dolist (i '(1 2 3))
          (when (= i x) (return-from found (values i (* i 10) :hit))))
        (values nil nil :miss))
    (list a b c)))
(defun %mcm-local-cleanup (x)
  (multiple-value-bind (a b)
      (block b1
        (unwind-protect (return-from b1 (values x :protected))
          (%mcm-p3 100)))
    (list a b)))
(deftest mv-call-site-mode.local-capture
  (list (%mcm-local 1) (%mcm-local 4)
        (%mcm-local-block 2) (%mcm-local-block 9)
        (%mcm-local-cleanup 7)
        (nth-value 1 (let ((z 3)) (values z (+ z 1))))
        (multiple-value-list (let ((z 3)) (if (oddp z) (values z :odd) z)))
        (multiple-value-bind (a b) (let ((z 5)) z) (list a b)))
  ((2 3) (8 :big) (2 20 :hit) (nil nil :miss) (7 :protected) 4 (3 :odd) (5 nil)))
