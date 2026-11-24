;;; An EVAL called from a macro expander that an analysis walk is running (here
;;; through MACROEXPAND-1 from another macro) compiled its form with the walk's
;;; bound-variable list still in place: a lexical variable of the outer code
;;; hid a SYMBOL-MACROLET of the same name inside the evaluated form.
;;; cl-environments' AUGMENT-ENVIRONMENT does exactly this, and cl-form-types
;;; then typed a symbol macro as T.

(defmacro %nesw-inner ()
  `',(eval '(let (x)
              (symbol-macrolet ((x 'a))
                (macrolet ((e (&environment env)
                             `',(multiple-value-list (macroexpand-1 'x env))))
                  (e))))))

(defmacro %nesw-via-macroexpand (form &environment env)
  (macroexpand-1 form env))

(defun %nesw-compiled ()
  (funcall (compile nil '(lambda ()
                          (let ((x 1050))
                            (declare (type number x) (ignorable x))
                            (%nesw-via-macroexpand (%nesw-inner)))))))

(deftest-compiled-only nested-eval-symbol-macro-walker.compiled
  (%nesw-compiled)
  ((quote a) t))

(deftest nested-eval-symbol-macro-walker.eval
  (eval '(let ((x 1050))
          (declare (type number x) (ignorable x))
          (%nesw-via-macroexpand (%nesw-inner))))
  ((quote a) t))
