;;; The declarations at the head of the body of DEFSETF (long form),
;;; DEFINE-SETF-EXPANDER, DEFINE-COMPILER-MACRO and DEFTYPE apply to the
;;; parameters, as for DEFMACRO. Each body here declares its parameter SPECIAL
;;; and reports whether a function outside sees it bound.

(defun dbd-read ()
  (if (boundp 'dbd-x) :special :lexical))

(defun dbd-get (o) o)

(defsetf dbd-get (dbd-x) (v)
  (declare (special dbd-x))
  `(progn ,v ',(dbd-read)))

(deftest definer-body-declarations.defsetf
  (setf (dbd-get 7) 1)
  :special)

(defsetf dbd-get-doc (dbd-x) (v)
  "Documented."
  (declare (special dbd-x))
  `(progn ,v ',(dbd-read)))

(deftest definer-body-declarations.defsetf-docstring
  (setf (dbd-get-doc 7) 1)
  :special)

(define-setf-expander dbd-place (dbd-x)
  (declare (special dbd-x))
  (let ((s (gensym)))
    (values '() '() (list s) s `',(dbd-read))))

(deftest definer-body-declarations.define-setf-expander
  (nth-value 4 (get-setf-expansion '(dbd-place 1)))
  ':special)

(defun dbd-cm (dbd-x) dbd-x)

(define-compiler-macro dbd-cm (dbd-x)
  (declare (special dbd-x))
  `',(dbd-read))

(deftest definer-body-declarations.define-compiler-macro
  (funcall (compiler-macro-function 'dbd-cm) '(dbd-cm 1) nil)
  ':special)

(defun dbd-cm-env (dbd-x) dbd-x)

(define-compiler-macro dbd-cm-env (dbd-x &environment env)
  (declare (special dbd-x) (ignore env))
  `',(dbd-read))

(deftest definer-body-declarations.define-compiler-macro-environment
  (funcall (compiler-macro-function 'dbd-cm-env) '(dbd-cm-env 1) nil)
  ':special)

(deftype dbd-type (dbd-x)
  (declare (special dbd-x))
  (if (eq (dbd-read) :special) 'integer 'string))

(deftest definer-body-declarations.deftype
  (typep 5 '(dbd-type 1))
  t)
