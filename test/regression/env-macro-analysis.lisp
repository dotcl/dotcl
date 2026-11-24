;;; A macro that takes &ENVIRONMENT makes every variable around it a boxed cell, except one
;;; declared transparent (it only picks among its own argument forms): the mutation walk walks
;;; its arguments. The answers must stay those of the generic path.

(dotcl.cil-compiler::%register-transparent-env-macro '%ema-id)
(dotcl.cil-compiler::%register-transparent-env-macro '%ema-pick)

(defmacro %ema-id (x &environment env)
  (declare (ignorable env))
  x)

(defmacro %ema-pick (flag then &optional else &environment env)
  (declare (ignorable env))
  (if (eval flag) then else))

(defmacro %ema-flag-p (name &environment env)
  ;; Expands to what the enclosing SYMBOL-MACROLET makes NAME, or :none.
  (multiple-value-bind (exp expanded-p) (macroexpand-1 name env)
    (if expanded-p exp :none)))

(defmacro %ema-bump (var &environment env)
  (declare (ignorable env))
  `(setq ,var (+ ,var 1)))

(defmacro %ema-typed-p (var &environment env)
  ;; Asks for lexical type information: must keep the generic path.
  (multiple-value-bind (kind local-p decls) (dotcl-cltl2:variable-information var env)
    (declare (ignore kind local-p))
    (if (assoc 'type decls) :typed :untyped)))

(defun %ema-batches (seq n)
  (loop while seq
        collect (loop for i below n for (elt . rest) on seq
                      collect elt
                      finally (setf seq rest)
                              (%ema-id (when (> i n) (error "x"))))))

(deftest env-macro-analysis.loop-result
  (%ema-batches (loop for i below 25 collect i) 10)
  ((0 1 2 3 4 5 6 7 8 9) (10 11 12 13 14 15 16 17 18 19) (20 21 22 23 24)))

(defun %ema-bytes (n) (%ema-batches (loop for i below n collect i) 100))

(deftest-compiled-only env-macro-analysis.loop-variables-stay-unboxed
  (progn (%ema-bytes 1000)
         (let ((l (loop for i below 200000 collect i)))
           (list (< (min-allocated-bytes (lambda () (%ema-batches l 100))) 6700000))))
  (t))

(deftest env-macro-analysis.symbol-macrolet-seen-by-the-macro
  (list (symbol-macrolet ((flag 1)) (let ((a 0)) (loop for i below 3 do (setq a (+ a (%ema-flag-p flag)))) a))
        (symbol-macrolet ((flag (+ 1 1))) (%ema-flag-p flag))
        (%ema-flag-p flag))
  (3 2 :none))

(deftest env-macro-analysis.assignment-made-by-the-expansion
  ;; The macro's expansion assigns X; a closure also reads it.
  (let ((x 0) (fns '()))
    (dotimes (i 3)
      (%ema-bump x)
      (push (lambda () x) fns))
    (list x (mapcar #'funcall fns)))
  (3 (3 3 3)))

(deftest env-macro-analysis.expansion-in-a-closure
  (let ((n 0))
    (let ((f (lambda () (%ema-bump n) (%ema-bump n))))
      (funcall f) (funcall f))
    n)
  4)

(deftest-emitting-only env-macro-analysis.type-information-keeps-the-generic-path
  (list (let ((x 1)) (declare (fixnum x)) (%ema-typed-p x))
        (let ((x 1)) (%ema-typed-p x)))
  (:typed :untyped))

(defmacro %ema-bump-if-flag (var name &environment env)
  ;; Assigns VAR only where NAME is a symbol macro, so the walk must see the SYMBOL-MACROLET.
  (if (nth-value 1 (macroexpand-1 name env))
      `(setq ,var (+ ,var 1))
      nil))

(deftest env-macro-analysis.assignment-depends-on-the-scope
  (list (symbol-macrolet ((flag t))
          (let ((x 0))
            (let ((f (lambda () x)))
              (%ema-bump-if-flag x flag)
              (funcall f))))
        (let ((x 0))
          (let ((f (lambda () x)))
            (%ema-bump-if-flag x flag)
            (funcall f))))
  (1 0))

(defun %ema-pick-box (n)
  (let ((x 0) (fns '()))
    (dotimes (i n)
      (%ema-pick t (incf x) (push (lambda () x) fns)))
    (list x (length fns))))

(deftest env-macro-analysis.a-transparent-macro-with-an-assignment-and-a-closure
  ;; Both argument forms are walked: X is assigned in one and read by a closure in the other.
  (%ema-pick-box 3)
  (3 0))
