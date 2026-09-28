;;; A compiler macro called on a (FUNCALL #'NAME . args) form destructures ARGS
;;; (CLHS 3.2.2.1.3), and a local function of the same name hides the global
;;; compiler macro from COMPILER-MACRO-FUNCTION with the lexical environment.

(defun %cmff-memb (item list &key (test #'eql))
  (member item list :test test))

(define-compiler-macro %cmff-memb (&whole whole item list &key test)
  (if (equal test '(function eq))
      `(%cmff-memq ,item ,list)
      whole))

(deftest compiler-macro-funcall-form.plain-call
  (funcall (compiler-macro-function '%cmff-memb)
           '(%cmff-memb x (foo) :test #'eq) nil)
  (%cmff-memq x (foo)))

(deftest compiler-macro-funcall-form.funcall-call
  (funcall (compiler-macro-function '%cmff-memb)
           '(funcall #'%cmff-memb x (foo) :test #'eq) nil)
  (%cmff-memq x (foo)))

(deftest compiler-macro-funcall-form.whole-is-whole-form
  (let ((form '(funcall #'%cmff-memb x (foo))))
    (eq form (funcall (compiler-macro-function '%cmff-memb) form nil)))
  t)

(defmacro %cmff-cmf-here (&environment env)
  `',(and (compiler-macro-function '%cmff-memb env) t))

(deftest compiler-macro-funcall-form.global
  (%cmff-cmf-here)
  t)

(deftest compiler-macro-funcall-form.flet-shadows
  (flet ((%cmff-memb (&rest args) args))
    (declare (ignorable #'%cmff-memb))
    (%cmff-cmf-here))
  nil)

(deftest compiler-macro-funcall-form.macrolet-shadows
  (macrolet ((%cmff-memb (&rest args) `',args))
    (%cmff-cmf-here))
  nil)

(deftest compiler-macro-funcall-form.flet-call-uses-local
  (flet ((%cmff-memb (&rest args) (length args)))
    (%cmff-memb 1 '(1 2) :test #'eq))
  4)
