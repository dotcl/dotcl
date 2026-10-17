;;; The &ENVIRONMENT a macro gets while code is compiled carries the lexical
;;; bindings and declarations around the macro call, and the DOTCL-CLTL2
;;; queries read them: VARIABLE-INFORMATION, FUNCTION-INFORMATION and
;;; DECLARATION-INFORMATION OPTIMIZE. AUGMENT-ENVIRONMENT, PARSE-MACRO and
;;; ENCLOSE work on such environments. Expected values are SBCL 2.6.8's
;;; SB-CLTL2 answers (SBCL normalizes an FTYPE's result, so that test looks at
;;; the argument types). Compiled code only: the interpreter's environments do
;;; not carry declarations.

(defmacro le-var (&rest names &environment env)
  `',(mapcar (lambda (n) (multiple-value-list (dotcl-cltl2:variable-information n env)))
             names))

(defmacro le-fun (&rest names &environment env)
  `',(mapcar (lambda (n) (multiple-value-list (dotcl-cltl2:function-information n env)))
             names))

(defmacro le-optimize (&environment env)
  `',(remove-if-not (lambda (e) (member (car e) '(speed space safety debug compilation-speed)))
                    (dotcl-cltl2:declaration-information 'optimize env)))

(defmacro le-global-macro () nil)

(deftest-compiled-only cltl2-lexical.let-declarations
  (let ((x 1) (y 'a) (z 2) (w 3))
    (declare (fixnum x) (symbol y) (ignorable y) (ignore z) (special w))
    (le-var x y z w))
  ((:lexical t ((type . fixnum)))
   (:lexical t ((type . symbol)))
   (:lexical t ((ignore . t)))
   (:special nil nil)))

(deftest-compiled-only cltl2-lexical.inner-binding-hides-declaration
  (let ((x 1))
    (declare (fixnum x) (ignorable x))
    (let ((x 2))
      (declare (ignorable x))
      (le-var x)))
  ((:lexical t nil)))

(deftest-compiled-only cltl2-lexical.free-declarations
  (let ((x 1))
    (declare (ignorable x))
    (locally (declare (type fixnum x) (special free-le))
      (le-var x free-le)))
  ((:lexical t ((type . fixnum)))
   (:special nil nil)))

(deftest-compiled-only cltl2-lexical.unbound-name
  (let ((x 1))
    (declare (ignorable x))
    (le-var not-bound-le))
  ((nil nil nil)))

(deftest-compiled-only cltl2-lexical.lambda-parameters
  ((lambda (a &optional (b 1) &key c)
     (declare (fixnum a) (ignore b c))
     (le-var a b c))
   1)
  ((:lexical t ((type . fixnum)))
   (:lexical t ((ignore . t)))
   (:lexical t ((ignore . t)))))

(deftest-compiled-only cltl2-lexical.seen-from-a-closure
  (let ((x 1))
    (declare (fixnum x) (ignorable x))
    (funcall (lambda () (le-var x))))
  ((:lexical t ((type . fixnum)))))

(deftest-compiled-only cltl2-lexical.symbol-macrolet
  (symbol-macrolet ((sm-le 1))
    (le-var sm-le))
  ((:symbol-macro t nil)))

(deftest-compiled-only cltl2-lexical.local-functions
  (flet ((lf () 1) (lg () 2))
    (declare (ftype (function () symbol) lf) (inline lg) (ignorable #'lf #'lg))
    (let ((info (le-fun lf lg)))
      (list (destructuring-bind (kind local-p decls) (first info)
              (let ((ftype (cdr (assoc 'ftype decls))))
                (list kind local-p (car ftype) (cadr ftype))))
            (second info))))
  ((:function t function ())
   (:function t ((inline . inline)))))

(deftest-compiled-only cltl2-lexical.labels-and-macrolet
  (list (labels ((lh () 1))
          (declare (notinline lh) (ignorable #'lh))
          (le-fun lh))
        (macrolet ((mm () 1))
          (le-fun mm))
        (flet ((le-global-macro () 1))
          (declare (ignorable #'le-global-macro))
          (le-fun le-global-macro)))
  (((:function t ((inline . notinline))))
   ((:macro t nil))
   ((:function t nil))))

(deftest-compiled-only cltl2-lexical.optimize
  (locally (declare (optimize (debug 3) (speed 2) (space 0)))
    (le-optimize))
  ((speed 2) (space 0) (safety 1) (debug 3) (compilation-speed 1)))

(deftest cltl2-lexical.augment-environment
  (let* ((env (dotcl-cltl2:augment-environment
               nil
               :variable '(av)
               :symbol-macro '((asm 42))
               :function '(af)
               :macro (list (list 'am (lambda (form env)
                                        (declare (ignore env))
                                        `(quote ,(cdr form)))))
               :declare '((type fixnum av))))
         (inner (dotcl-cltl2:augment-environment env :variable '(asm))))
    (list (multiple-value-list (dotcl-cltl2:variable-information 'av env))
          (multiple-value-list (dotcl-cltl2:variable-information 'asm env))
          (multiple-value-list (dotcl-cltl2:function-information 'af env))
          (multiple-value-list (dotcl-cltl2:function-information 'am env))
          (macroexpand 'asm env)
          (macroexpand '(am 1 2) env)
          (multiple-value-list (dotcl-cltl2:variable-information 'asm inner))
          (macroexpand-1 'asm inner)))
  ((:lexical t ((type . fixnum)))
   (:symbol-macro t nil)
   (:function t nil)
   (:macro t nil)
   42
   '(1 2)
   (:lexical t nil)
   asm))

(deftest cltl2-lexical.parse-macro
  (let ((expander (coerce (dotcl-cltl2:parse-macro
                           'pm-le '(&whole w a &optional (b 2))
                           '((declare (ignorable w))
                             (when (eq a 'out) (return-from pm-le :early))
                             `(list ',(car w) ,a ,b)))
                   'function)))
    (list (funcall expander '(pm-le 1) nil)
          (funcall expander '(pm-le 1 3) nil)
          (funcall expander '(pm-le out) nil)))
  ((list 'pm-le 1 2) (list 'pm-le 1 3) :early))

(deftest cltl2-lexical.enclose
  (let ((env (dotcl-cltl2:augment-environment
              nil
              :macro (list (list 'twice-le (lambda (form env)
                                             (declare (ignore env))
                                             `(* 2 ,(second form)))))
              :symbol-macro '((sm42-le 42)))))
    (list (funcall (dotcl-cltl2:enclose '(lambda () (twice-le 21)) env))
          (funcall (dotcl-cltl2:enclose '(lambda (x) (+ x sm42-le)) env) 1)))
  (42 43))
