;;; MACROLET must shadow a global macro even when the compiler lowers that name
;;; through its own handler table (CLHS 3.1.2.1.2.2).
;;;
;;; COMPILE-FORM dispatches on *COMPILE-FORM-HANDLERS* before it macroexpands,
;;; which is right for special forms but wrong for the CL macros lowered the same
;;; way — WHEN, UNLESS, AND, OR, INCF, DOLIST and friends. A MACROLET binding for
;;; one of those was ignored and the built-in compiled instead, silently, while
;;; the tree-walk interpreter (which consults its lexical macros first) returned
;;; the shadowed expansion. The two evaluators disagreed on the same source.
;;;
;;; Every case is asserted under both evaluators, since the bug was exactly that
;;; they diverged.

(defun %msh-eval (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (eval form)))

(defmacro def-msh-test (name form expected)
  `(progn
     (deftest ,(intern (format nil "MACROLET-SHADOWS-HANDLER.~a-COMPILE" name))
       (%msh-eval :compile ',form)
       ,expected)
     (deftest ,(intern (format nil "MACROLET-SHADOWS-HANDLER.~a-INTERPRET" name))
       (%msh-eval :interpret ',form)
       ,expected)))

;;; The reported shapes: names the handler table lowers as if they were special
;;; forms.
(def-msh-test "WHEN"
  (macrolet ((when (&rest r) (declare (ignore r)) :shadowed)) (when t 1))
  :shadowed)

(def-msh-test "UNLESS"
  (macrolet ((unless (&rest r) (declare (ignore r)) :shadowed)) (unless nil 1))
  :shadowed)

(def-msh-test "AND"
  (macrolet ((and (&rest r) (declare (ignore r)) :shadowed)) (and t 1))
  :shadowed)

(def-msh-test "OR"
  (macrolet ((or (&rest r) (declare (ignore r)) :shadowed)) (or nil 1))
  :shadowed)

(def-msh-test "INCF"
  (macrolet ((incf (&rest r) (declare (ignore r)) :shadowed))
    (let ((x 0)) (declare (ignorable x)) (incf x)))
  :shadowed)

(def-msh-test "DOLIST"
  (macrolet ((dolist (&rest r) (declare (ignore r)) :shadowed))
    (dolist (x '(1 2 3)) x))
  :shadowed)

;;; The shadow is lexical: the built-in is back in force outside the MACROLET,
;;; and an argument form inside still sees the real WHEN through a nested scope
;;; that does not rebind it.
(def-msh-test "SCOPE-ENDS"
  (list (macrolet ((when (&rest r) (declare (ignore r)) :shadowed)) (when t 1))
        (when t 1))
  (:shadowed 1))

;;; An inner MACROLET rebinds the same name, and the outer binding is restored
;;; when that scope ends.
(def-msh-test "NESTED"
  (macrolet ((when (&rest r) (declare (ignore r)) :outer))
    (list (macrolet ((when (&rest r) (declare (ignore r)) :inner)) (when t 1))
          (when t 1)))
  (:inner :outer))

;;; The expansion is compiled in the macrolet's lexical environment: it can use
;;; the enclosing variable, and the free-variable analysis walk must see that use
;;; through the shadowed operator (the analysis pass walks the same form).
(def-msh-test "EXPANSION-USES-LEXICAL"
  (let ((x 41))
    (macrolet ((when (&rest r) (declare (ignore r)) '(1+ x)))
      (when nil nil)))
  42)

;;; Same, through a closure: the shadowed form's expansion captures a variable
;;; that is also mutated, which is what the boxing analysis keys on.
(def-msh-test "EXPANSION-CAPTURES"
  (let ((n 0))
    (macrolet ((when (&rest r) (declare (ignore r)) '(lambda () (incf n))))
      (let ((f (when t t)))
        (funcall f)
        (funcall f))))
  2)

;;; A name the handler table does NOT carry was always shadowable; keep it here
;;; so a regression that breaks ordinary MACROLET shows up in the same file.
(def-msh-test "PLAIN-NAME"
  (macrolet ((%msh-not-a-builtin (&rest r) (declare (ignore r)) :shadowed))
    (%msh-not-a-builtin))
  :shadowed)
