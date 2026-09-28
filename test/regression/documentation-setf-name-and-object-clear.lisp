;;; DOCUMENTATION of a (SETF name) DEFUN, and a docstring set or cleared
;;; through the function object as seen through the name.
;;;
;;; - DEFUN of a (SETF FOO) name dropped its docstring: only symbol names had
;;;   theirs stored.
;;; - A docstring set through the function object (#'f) did not reach the
;;;   name: (setf (documentation #'f 'function) nil) left
;;;   (documentation 'f 'function) answering the old docstring.
;;; SBCL keeps one docstring per function, so both spellings always agree.

(defun (setf doc-snc-place) (v x) "setf doc" (list v x))

(deftest documentation-of-a-setf-defun
  (list (documentation '(setf doc-snc-place) 'function)
        (documentation #'(setf doc-snc-place) 'function))
  ("setf doc" "setf doc"))

;;; The DEFUN form's value is still the name.
(deftest setf-defun-with-docstring-returns-the-name
  (defun (setf doc-snc-place2) (v x) "d" (list v x))
  (setf doc-snc-place2))

(deftest documentation-setf-nil-through-the-object-clears-the-name
  (progn
    (defun doc-snc-f () "f doc" 1)
    (setf (documentation #'doc-snc-f 'function) nil)
    (list (documentation 'doc-snc-f 'function)
          (documentation #'doc-snc-f 'function)))
  (nil nil))

(deftest documentation-set-through-the-object-reaches-the-name
  (progn
    (defun doc-snc-g () "g doc" 1)
    (setf (documentation #'doc-snc-g t) "new")
    (list (documentation 'doc-snc-g 'function)
          (documentation #'doc-snc-g 'function)))
  ("new" "new"))

(deftest documentation-setf-nil-through-a-gf-object-clears-the-name
  (progn
    (defgeneric doc-snc-gf (x) (:documentation "gf doc"))
    (setf (documentation #'doc-snc-gf t) nil)
    (documentation 'doc-snc-gf 'function))
  nil)

(deftest documentation-setf-name-object-clear-and-set
  (progn
    (defun (setf doc-snc-place3) (v x) "p3" (list v x))
    (setf (documentation #'(setf doc-snc-place3) 'function) nil)
    (let ((cleared (documentation '(setf doc-snc-place3) 'function)))
      (setf (documentation '(setf doc-snc-place3) 'function) "p3 new")
      (list cleared
            (documentation '(setf doc-snc-place3) 'function)
            (documentation #'(setf doc-snc-place3) 'function))))
  (nil "p3 new" "p3 new"))

;;; --- over-fix guards ---------------------------------------------------

;;; A macro keeps its docstring under the name; it has no function object.
(deftest documentation-of-a-macro-is-unchanged
  (progn
    (defmacro doc-snc-mac () "mac doc" 1)
    (documentation 'doc-snc-mac 'function))
  "mac doc")

;;; The interpreter's DEFMACRO records it too (the only DEFMACRO an emit-free
;;; build has); a lone string is the expansion, not a docstring.
(deftest documentation-of-an-interpreted-macro
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(progn
            (defmacro doc-snc-imac () "imac doc" 1)
            (defmacro doc-snc-imac2 () "only body")
            (list (documentation 'doc-snc-imac 'function)
                  (documentation 'doc-snc-imac2 'function)
                  (macroexpand-1 '(doc-snc-imac2))))))
  ("imac doc" nil "only body"))

;;; A docstring set on a name before it is defined is still answered.
(deftest documentation-of-an-unbound-name
  (progn
    (setf (documentation 'doc-snc-unbound 'function) "unbound doc")
    (documentation 'doc-snc-unbound 'function))
  "unbound doc")

;;; Clearing one function object does not touch another's docstring.
(deftest documentation-clear-is-per-function
  (progn
    (defun doc-snc-a () "a doc" 1)
    (defun doc-snc-b () "b doc" 2)
    (setf (documentation #'doc-snc-a 'function) nil)
    (documentation 'doc-snc-b 'function))
  "b doc")

;;; A list that is not a function name answers NIL instead of signalling.
(deftest documentation-of-a-non-function-name-list
  (documentation '(not a name) 'function)
  nil)

;;; The same through EVAL under the tree-walk interpreter.
(deftest documentation-setf-name-and-object-clear-interpret
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(progn
            (defun (setf doc-snc-iplace) (v x) "iplace" (list v x))
            (defun doc-snc-if () "if doc" 1)
            (setf (documentation #'doc-snc-if 'function) nil)
            (list (documentation '(setf doc-snc-iplace) 'function)
                  (documentation #'(setf doc-snc-iplace) 'function)
                  (documentation 'doc-snc-if 'function)))))
  ("iplace" "iplace" nil))

;;; A redefinition without a docstring has none: the docstring belonged to the
;;; old definition. Before, a docstring set on a bound name was also filed under
;;; the name, and the name answered it for every later definition. SBCL answers
;;; NIL in each case below.
(deftest documentation-redefinition-without-docstring
  (progn
    (defun doc-snc-h () "h doc" 1)
    (defun doc-snc-h () 2)
    (defun (setf doc-snc-hplace) (v x) "hplace doc" (list v x))
    (defun (setf doc-snc-hplace) (v x) (list v x))
    (defmacro doc-snc-hmacro () "hmacro doc" 1)
    (defmacro doc-snc-hmacro () 2)
    (defun doc-snc-h3 () "h3 doc" 1)
    (setf (symbol-function 'doc-snc-h3) (lambda () 2))
    (list (documentation 'doc-snc-h 'function)
          (documentation #'doc-snc-h t)
          (documentation '(setf doc-snc-hplace) 'function)
          (documentation 'doc-snc-hmacro 'function)
          (documentation 'doc-snc-h3 'function)))
  (nil nil nil nil nil))

;;; A docstring set while the name was unbound stays with the name across a
;;; later DEFUN without one (as in SBCL); the function object itself has none.
(deftest documentation-set-while-unbound-survives-defun
  (progn
    (setf (documentation 'doc-snc-u 'function) "u doc")
    (defun doc-snc-u () 3)
    (list (documentation 'doc-snc-u 'function)
          (documentation #'doc-snc-u t)))
  ("u doc" nil))

;;; A macro's docstring is on its expander, as SBCL has it.
(deftest documentation-of-a-macro-through-its-expander
  (progn
    (defmacro doc-snc-m () "m doc" 1)
    (list (documentation 'doc-snc-m 'function)
          (documentation (macro-function 'doc-snc-m) t)))
  ("m doc" "m doc"))

(deftest documentation-redefinition-without-docstring-interpret
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(progn
            (defun doc-snc-ih () "ih doc" 1)
            (defun doc-snc-ih () 2)
            (defmacro doc-snc-imacro () "imacro doc" 1)
            (defmacro doc-snc-imacro () 2)
            (list (documentation 'doc-snc-ih 'function)
                  (documentation 'doc-snc-imacro 'function)))))
  (nil nil))
