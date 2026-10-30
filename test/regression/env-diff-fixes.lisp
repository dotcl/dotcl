;;; Evaluation-environment fixes found by the SBCL differential report.

(defun edf-condition (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error () :type-error)
    (undefined-function () :undefined-function)
    (unbound-variable () :unbound-variable)
    (error () :error)))

(deftest-compiled-only edf-nontoplevel-defmacro-has-no-compile-time-effect
  (progn
    (eval '(defun edf-nested-macro-fn ()
             (defmacro edf-nested-macro () 1)
             (edf-nested-macro)))
    (edf-condition #'edf-nested-macro-fn))
  :undefined-function)

(deftest-compiled-only edf-nontoplevel-symbol-macro-has-no-compile-time-effect
  (progn
    (eval '(defun edf-nested-sm-fn ()
             (define-symbol-macro edf-nested-sm 10)
             edf-nested-sm))
    (list (nth-value 1 (macroexpand-1 'edf-nested-sm))
          (edf-condition #'edf-nested-sm-fn)))
  (nil :unbound-variable))

(deftest-compiled-only edf-nontoplevel-defmacro-not-registered-while-compiling
  (edf-condition
   (lambda ()
     (funcall (compile nil
                       '(lambda ()
                          (defmacro edf-compiled-nested () 1)
                          (fmakunbound 'edf-compiled-nested)
                          (edf-compiled-nested))))))
  :undefined-function)

(deftest-compiled-only edf-compile-file-nontoplevel-symbol-macro
  (let ((src "edf-sm-tmp.lisp")
        (fasl "edf-sm-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(when nil (define-symbol-macro edf-file-sm 1))" s))
           (compile-file src :output-file fasl)
           (nth-value 1 (macroexpand-1 'edf-file-sm)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  nil)

(deftest-compiled-only edf-nontoplevel-symbol-macro-still-runs
  (list (edf-condition
         (lambda ()
           (funcall (compile nil
                             '(lambda ()
                                (define-symbol-macro edf-runtime-sm 11)
                                edf-runtime-sm)))))
        (nth-value 0 (macroexpand-1 'edf-runtime-sm)))
  (:unbound-variable 11))

(deftest edf-macro-function-setf-rejects-invalid-values
  (let ((name (gensym)))
    (setf (macro-function name)
          (lambda (form environment)
            (declare (ignore form environment))
            1))
    (list (edf-condition (lambda () (setf (macro-function name) nil)))
          (edf-condition (lambda () (setf (macro-function name) 42)))
          (functionp (macro-function name))))
  (:type-error :type-error t))

(deftest edf-compiler-macro-function-setf-rejects-a-non-function
  (let ((name (gensym)))
    (list (edf-condition (lambda () (setf (compiler-macro-function name) 42)))
          (compiler-macro-function name)))
  (:type-error nil))

(deftest edf-macroexpand-hook-returning-the-form-is-still-expanded
  (let ((*macroexpand-hook*
          (lambda (expander form environment)
            (declare (ignore expander environment))
            form)))
    (list (nth-value 1 (macroexpand-1 '(when a b)))
          (nth-value 1 (macroexpand '(when a b)))))
  (t t))

(defun edf-nil-hook-probe ()
  (let ((*macroexpand-hook* nil))
    (macroexpand-1 '(when a b))))

(deftest edf-nil-macroexpand-hook-is-a-type-error
  (edf-condition #'edf-nil-hook-probe)
  :type-error)

(defun edf-cm-env (x) x)
(define-compiler-macro edf-cm-env (x &environment environment)
  (declare (ignore x))
  `(quote ,(not (null environment))))

;; An interpreted call does not apply compiler macros.
(deftest-compiled-only edf-compiler-macro-receives-an-environment
  (funcall (compile nil '(lambda () (edf-cm-env 1))))
  t)

;; An interpreted call does not apply compiler macros.
(deftest-compiled-only edf-setf-compiler-macro-receives-an-environment
  (let ((name (gensym)))
    (setf (symbol-function name) (lambda (x) x))
    (setf (compiler-macro-function name)
          (lambda (form environment)
            (declare (ignore form))
            `(quote ,(not (null environment)))))
    (funcall (compile nil (list 'lambda '() (list name 1)))))
  t)

(defun edf-cm-doc (x) "function-doc" x)
(define-compiler-macro edf-cm-doc (x) "compiler-macro-doc" x)

(deftest edf-compiler-macro-docstring
  (list (documentation 'edf-cm-doc 'function)
        (documentation 'edf-cm-doc 'compiler-macro))
  ("function-doc" "compiler-macro-doc"))

;; An interpreted call does not apply compiler macros.
(deftest-compiled-only edf-compiler-macro-docstring-applied
  (funcall (compile nil '(lambda () (edf-cm-doc 5))))
  5)

(defstruct edf-doc-struct "structure-doc" slot)

(deftest edf-defstruct-docstring-reaches-type-and-class
  (list (documentation 'edf-doc-struct 'structure)
        (documentation 'edf-doc-struct 'type)
        (documentation (find-class 'edf-doc-struct) 'type)
        (documentation (find-class 'edf-doc-struct) t))
  ("structure-doc" "structure-doc" "structure-doc" "structure-doc"))

(deftest edf-defining-macros-macroexpand
  (list (nth-value 1 (macroexpand-1 '(defun edf-expanded-fn (x) x)))
        (nth-value 1 (macroexpand-1 '(defmacro edf-expanded-macro (x) x)))
        (nth-value 1 (macroexpand-1 '(defvar *edf-expanded-var* 1)))
        (nth-value 1 (macroexpand-1 '(defparameter *edf-expanded-param* 1)))
        (nth-value 1 (macroexpand-1 '(defconstant +edf-expanded-const+ 1))))
  (t t t t t))

(deftest edf-defining-macro-expansions-evaluate
  (progn
    (eval (macroexpand-1 '(defun edf-expanded-call (x) (* x 2))))
    (eval (macroexpand-1 '(defmacro edf-expanded-twice (x) `(list ,x ,x))))
    (eval (macroexpand-1 '(defvar *edf-expanded-value* 9)))
    (list (edf-expanded-call 4)
          (eval '(edf-expanded-twice 5))
          (symbol-value '*edf-expanded-value*)))
  (8 (5 5) 9))
