;;; DOTCL-CLTL2:MAP-ENVIRONMENT-VARIABLES and MAP-ENVIRONMENT-FUNCTIONS list what
;;; the &ENVIRONMENT of a macro binds: the variables, symbol macros, local functions
;;; and local macros around the call, innermost binding first, each name once,
;;; with the kind and declarations VARIABLE-INFORMATION / FUNCTION-INFORMATION give.
;;; An environment that carries no lexical state (NIL) binds nothing, and nothing
;;; is called. Compiled code only: the interpreter's environments carry no frames.

(defmacro me-vars (&environment env)
  (let ((out '()))
    (dotcl-cltl2:map-environment-variables
     (lambda (name kind decls expansion) (push (list name kind decls expansion) out))
     env)
    `',(sort out #'string< :key (lambda (e) (symbol-name (car e))))))

(defmacro me-funs (&environment env)
  (let ((out '()))
    (dotcl-cltl2:map-environment-functions
     (lambda (name kind decls macro-function)
       (push (list name kind decls (and macro-function (functionp macro-function) :function)) out))
     env)
    `',(sort out #'string< :key (lambda (e) (symbol-name (car e))))))

(defvar *me-special* 1)

(deftest-compiled-only cltl2-map-environment.let-variables
  (let ((x 1) (y 2))
    (declare (fixnum x) (ignore y))
    (me-vars))
  ((x :lexical ((type . fixnum)) nil)
   (y :lexical ((ignore . t)) nil)))

(deftest-compiled-only cltl2-map-environment.shadowed-name-once
  (let ((x 1))
    (declare (ignorable x))
    (let ((x 2))
      (declare (ignorable x) (fixnum x))
      (me-vars)))
  ((x :lexical ((type . fixnum)) nil)))

(deftest-compiled-only cltl2-map-environment.lambda-list-and-special
  (funcall (lambda (a &optional (b 1 bp) &rest r)
             (declare (ignorable a b bp r))
             (let ((*me-special* 2))
               (me-vars)))
           1)
  ((*me-special* :special nil nil)
   (a :lexical nil nil)
   (b :lexical nil nil)
   (bp :lexical nil nil)
   (r :lexical nil nil)))

(deftest-compiled-only cltl2-map-environment.symbol-macro
  (let ((z '(1 2)))
    (declare (ignorable z))
    (symbol-macrolet ((sm (car z)))
      (me-vars)))
  ((sm :symbol-macro nil (car z))
   (z :lexical nil nil)))

(deftest-compiled-only cltl2-map-environment.functions-and-macros
  (flet ((f (x) x))
    (declare (ignorable (function f)))
    (macrolet ((m () 1))
      (me-funs)))
  ((f :function nil nil)
   (m :macro nil :function)))

(deftest-compiled-only cltl2-map-environment.nothing-around
  (list (me-vars) (me-funs))
  (nil nil))

(deftest-compiled-only cltl2-map-environment.nil-environment-calls-nothing
  (let ((calls 0))
    (dotcl-cltl2:map-environment-variables (lambda (&rest args) (declare (ignore args)) (incf calls)) nil)
    (dotcl-cltl2:map-environment-functions (lambda (&rest args) (declare (ignore args)) (incf calls)) nil)
    calls)
  0)

;;; Environments made by AUGMENT-ENVIRONMENT are listed too.
(deftest-compiled-only cltl2-map-environment.augmented
  (let ((env (dotcl-cltl2:augment-environment
              nil
              :variable '(q)
              :symbol-macro '((s 7))
              :function '(g)
              :macro (list (list 'mm (lambda (form env) (declare (ignore form env)) 1)))
              :declare '((special sp)))) 
        (vars '()) (funs '()))
    (dotcl-cltl2:map-environment-variables
     (lambda (name kind decls expansion) (declare (ignore decls)) (push (list name kind expansion) vars))
     env)
    (dotcl-cltl2:map-environment-functions
     (lambda (name kind decls macro-function) (declare (ignore decls))
       (push (list name kind (functionp macro-function)) funs))
     env)
    (list (sort vars #'string< :key (lambda (e) (symbol-name (car e))))
          (sort funs #'string< :key (lambda (e) (symbol-name (car e))))))
  (((q :lexical nil) (s :symbol-macro 7))
   ((g :function nil) (mm :macro t))))

;; Inside one binding frame the last bound comes first, the order of SBCL's lexical
;; environment (a code walker reads it back in that order).
(defmacro me-funs-in-order (&environment env)
  (let ((out '()))
    (dotcl-cltl2:map-environment-functions
     (lambda (name kind decls macro-function) (declare (ignore kind decls macro-function)) (push name out))
     env)
    `',(nreverse out)))

(defmacro me-vars-in-order (&environment env)
  (let ((out '()))
    (dotcl-cltl2:map-environment-variables
     (lambda (name kind decls expansion) (declare (ignore kind decls expansion)) (push name out))
     env)
    `',(nreverse out)))

(deftest-compiled-only cltl2-map-environment.order-inside-a-frame
  (flet ((f1 () 1) (f2 () 2))
    (let* ((x 1) (y 2))
      (declare (ignorable x y))
      (list (me-funs-in-order) (me-vars-in-order) (list (f1) (f2)))))
  ((f2 f1) (y x) (1 2)))

;; SBCL's order by kind of frame: LET / LET* / FLET / LABELS newest first, a lambda
;; list last to first, SYMBOL-MACROLET and MACROLET as written.
(deftest-compiled-only cltl2-map-environment.order-lambda-list
  (funcall (lambda (p q r s) (declare (ignorable p q r s)) (me-vars-in-order)) 1 2 3 4)
  (s r q p))

(deftest-compiled-only cltl2-map-environment.order-let-and-labels
  (let ((a 1) (b 2))
    (declare (ignorable a b))
    (labels ((l1 () 1) (l2 () 2))
      (declare (ignorable #'l1 #'l2))
      (list (me-vars-in-order) (me-funs-in-order))))
  ((b a) (l2 l1)))

(deftest-compiled-only cltl2-map-environment.order-symbol-macrolet-and-macrolet
  (symbol-macrolet ((sm1 1) (sm2 2))
    (macrolet ((m1 () 1) (m2 () 2))
      (list (me-vars-in-order) (me-funs-in-order))))
  ((sm1 sm2) (m1 m2)))
