;;; Docstrings that the runtime attaches to its own built-in functions
;;; (DOTCL:SAVE-APPLICATION, DOTNET:MEMBERS and friends) belong to those
;;; symbols only. A symbol of the same name in another package -- shadowing,
;;; uninterned, or simply defined by the user -- must not answer them.

(deftest lispdoc-on-the-builtin-itself
  (list (stringp (documentation 'dotcl:save-application 'function))
        (stringp (documentation 'dotnet:members 'function))
        (stringp (documentation 'dotnet:type-names 'function)))
  (t t t))

(defpackage "LISPDOC-SHADOW"
  (:use "COMMON-LISP" "DOTCL")
  (:shadow "SAVE-APPLICATION"))

(defpackage "LISPDOC-OTHER" (:use "COMMON-LISP"))

(deftest lispdoc-not-for-a-shadowing-symbol
  (documentation 'lispdoc-shadow::save-application 'function)
  nil)

(deftest lispdoc-not-for-an-uninterned-symbol
  (list (documentation (make-symbol "SAVE-APPLICATION") 'function)
        (documentation (make-symbol "MEMBERS") 'function))
  (nil nil))

(defun lispdoc-other::members (x) x)
(defun lispdoc-other::save-application () nil)

(deftest lispdoc-not-for-a-user-function-of-the-same-name
  (list (documentation 'lispdoc-other::members 'function)
        (documentation 'lispdoc-other::save-application 'function))
  (nil nil))

;;; A user function of the same name keeps its own docstring.

(defun lispdoc-other::type-names () "Mine." nil)

(deftest lispdoc-user-docstring-of-the-same-name
  (documentation 'lispdoc-other::type-names 'function)
  "Mine.")

;;; Inheriting the symbol is not a different symbol: the docstring follows it.

(defpackage "LISPDOC-USER" (:use "COMMON-LISP" "DOTCL"))

(deftest lispdoc-through-use-package
  (let ((sym (find-symbol "SAVE-APPLICATION" "LISPDOC-USER")))
    (list (eq sym 'dotcl:save-application)
          (stringp (documentation sym 'function))))
  (t t))
