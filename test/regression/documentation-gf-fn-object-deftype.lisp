;;; DOCUMENTATION of a DEFGENERIC, of a function OBJECT, and of a DEFTYPE.
;;;
;;; Three writers did not reach the place the reader looks:
;;; - DEFGENERIC checked its :documentation option for duplicates and then
;;;   dropped it, so neither the name nor the generic function had a docstring.
;;; - DEFUN files its docstring under the NAME. (documentation #'f 'function)
;;;   looked the function object up and found nothing.
;;; - DEFTYPE left the docstring in the expander body, where it was evaluated
;;;   and discarded, so (documentation 'ty 'type) was NIL.
;;; SBCL answers the docstring in all four spellings below.

(defgeneric doc-gfo-gf (x) (:documentation "gf doc"))
(defgeneric doc-gfo-gf-undocumented (x))
(defun doc-gfo-fn () "fn doc" 1)
(defun doc-gfo-fn-undocumented () 1)
(deftype doc-gfo-ty () "type doc" 'integer)
(deftype doc-gfo-ty-body-only () 'integer)

(deftest documentation-of-a-defgeneric-by-name
  (documentation 'doc-gfo-gf 'function)
  "gf doc")

(deftest documentation-of-a-generic-function-object
  (list (documentation #'doc-gfo-gf 'function)
        (documentation #'doc-gfo-gf t))
  ("gf doc" "gf doc"))

(deftest documentation-of-a-defun-function-object
  (list (documentation #'doc-gfo-fn 'function)
        (documentation #'doc-gfo-fn t))
  ("fn doc" "fn doc"))

(deftest documentation-of-a-deftype
  (list (documentation 'doc-gfo-ty 'type)
        (typep 3 'doc-gfo-ty)
        (typep "x" 'doc-gfo-ty))
  ("type doc" t nil))

;;; --- over-fix guards ---------------------------------------------------

;;; Nothing documented stays NIL, by name and by object.
(deftest documentation-undocumented-stays-nil
  (list (documentation 'doc-gfo-gf-undocumented 'function)
        (documentation #'doc-gfo-gf-undocumented 'function)
        (documentation #'doc-gfo-fn-undocumented 'function)
        (documentation 'doc-gfo-ty-body-only 'type))
  (nil nil nil nil))

;;; A lone string in a DEFTYPE is the expansion, not a docstring.
(deftest deftype-lone-string-is-the-body
  (progn
    (deftype doc-gfo-ty-lone () "not a doc")
    (documentation 'doc-gfo-ty-lone 'type))
  nil)

;;; A redefinition's docstring belongs to the new function; the old object
;;; keeps the docstring it was given.
(deftest documentation-follows-the-redefined-function
  (let ((old (progn (defun doc-gfo-redef () "first" 1) #'doc-gfo-redef)))
    (defun doc-gfo-redef () "second" 2)
    (list (documentation old 'function)
          (documentation #'doc-gfo-redef 'function)
          (documentation 'doc-gfo-redef 'function)))
  ("first" "second" "second"))

;;; Clearing through the object clears what the object answers.
(deftest documentation-setf-nil-through-the-function-object
  (progn
    (defun doc-gfo-clear () "to clear" 1)
    (setf (documentation #'doc-gfo-clear 'function) nil)
    (documentation #'doc-gfo-clear 'function))
  nil)

;;; The same through EVAL under the tree-walk interpreter.
(deftest documentation-gf-fn-deftype-interpret
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(progn
            (defgeneric doc-gfo-igf (x) (:documentation "igf"))
            (defun doc-gfo-ifn () "ifn" 1)
            (deftype doc-gfo-ity () "ity" 'integer)
            (list (documentation 'doc-gfo-igf 'function)
                  (documentation #'doc-gfo-igf 'function)
                  (documentation #'doc-gfo-ifn 'function)
                  (documentation 'doc-gfo-ity 'type)))))
  ("igf" "igf" "ifn" "ity"))

;;; ENSURE-GENERIC-FUNCTION's :documentation replaces what DEFGENERIC stored.
;;; Before DEFGENERIC stored anything, ANSI ENSURE-GENERIC-FUNCTION.12 passed
;;; vacuously on NIL; once it did, the old "foo" came back instead of "bar".
(deftest documentation-ensure-generic-function-replaces
  (progn
    (defgeneric doc-gfo-egf (x) (:documentation "foo"))
    (ensure-generic-function 'doc-gfo-egf :lambda-list '(x) :documentation "bar")
    (list (documentation 'doc-gfo-egf 'function)
          (documentation #'doc-gfo-egf 'function)))
  ("bar" "bar"))
