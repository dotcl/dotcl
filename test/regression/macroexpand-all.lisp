;;; dotcl-cltl2:macroexpand-all — the code walker.
;;;
;;; A structural walker (expand, recurse on the result) gets the easy cases but
;;; cannot handle scope: it has no way to put a MACROLET definition in scope for
;;; the body, and no way to notice that a LET binding shadows a symbol macro.
;;; These tests pin the scope behaviour, which is the reason the walker exists.

(defmacro mea-twice (x) (list 'progn x x))
(define-symbol-macro mea-global 42)

(defun mea (form) (dotcl-cltl2:macroexpand-all form))

(deftest mea-global-macro
  (mea '(mea-twice (foo)))
  (progn (foo) (foo)))

(deftest mea-expands-below-the-top
  (mea '(let ((a 1)) (mea-twice (bar a))))
  (let ((a 1)) (progn (bar a) (bar a))))

(deftest mea-global-symbol-macro
  (mea '(list mea-global))
  (list 42))

;;; MACROLET: the definition must be in scope for the body, and the macrolet
;;; form itself disappears from the expansion.
(deftest mea-macrolet
  (mea '(macrolet ((m (x) (list 'quote x))) (m hello)))
  (quote hello))

(deftest mea-macrolet-inside-let
  (mea '(let ((y 1)) (macrolet ((m (x) (list '+ x 1))) (m y))))
  (let ((y 1)) (+ y 1)))

;;; A macrolet body still sees symbol macros from an enclosing scope.
(deftest mea-macrolet-sees-outer-symbol-macro
  (mea '(symbol-macrolet ((s 7)) (macrolet ((m () 's)) (list (m)))))
  (list 7))

(deftest mea-symbol-macrolet
  (mea '(symbol-macrolet ((s (compute))) (list s s)))
  (list (compute) (compute)))

;;; Shadowing: a lexical variable hides a symbol macro of the same name, and a
;;; local function hides a global macro of the same name.
(deftest mea-let-shadows-symbol-macro
  (mea '(symbol-macrolet ((s (bad))) (let ((s 1)) (list s))))
  (let ((s 1)) (list s)))

(deftest mea-flet-shadows-macro
  (mea '(flet ((mea-twice (x) x)) (mea-twice 9)))
  (flet ((mea-twice (x) x)) (mea-twice 9)))

;;; QUOTE contents are data, not code.
(deftest mea-quote-untouched
  (mea '(quote (mea-twice x)))
  (quote (mea-twice x)))

;;; Declarations stay put; tagbody tags are not forms.
(deftest mea-declare-kept
  (mea '(lambda (a) (declare (ignore a)) (mea-twice 1)))
  (lambda (a) (declare (ignore a)) (progn 1 1)))

(deftest mea-tagbody-tags-kept
  (mea '(tagbody top (mea-twice 1) (go top)))
  (tagbody top (progn 1 1) (go top)))

(deftest mea-lambda-body-walked
  (mea '(lambda (a) (mea-twice a)))
  (lambda (a) (progn a a)))

;;; SETQ on a symbol macro becomes SETF of the expansion (CLHS 5.1.2.4), so the
;;; result must no longer be a SETQ.
(deftest mea-setq-on-symbol-macro-becomes-setf
  (let ((r (mea '(symbol-macrolet ((s (slot o))) (setq s 5)))))
    (and (consp r) (not (eq (car r) 'setq)) t))
  t)

(deftest mea-setq-on-plain-variable-stays-setq
  (mea '(let ((v 0)) (setq v (mea-twice 1))))
  (let ((v 0)) (setq v (progn 1 1))))

;;; Compiler-internal macros (cond/when/and/or) expand too — walkers rely on it.
(deftest mea-cond-expands
  (let ((r (mea '(cond (a 1) (t 2)))))
    (eq (car r) 'if))
  t)

;;; An explicit NIL environment is accepted (trivial-macroexpand-all passes one).
(deftest mea-accepts-nil-env
  (dotcl-cltl2:macroexpand-all '(mea-twice 1) nil)
  (progn 1 1))

;;; Atoms and self-evaluating forms come back unchanged.
(deftest mea-atoms
  (list (mea 5) (mea "s") (mea :k) (mea t) (mea nil))
  (5 "s" :k t nil))
