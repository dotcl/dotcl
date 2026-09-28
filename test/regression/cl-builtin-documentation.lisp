;;; Built-in docstrings for the COMMON-LISP package.
;;;
;;; Every external CL symbol that names a function, macro or special operator
;;; answers a docstring for (documentation sym 'function), and every one that
;;; names a variable or constant answers one for (documentation sym 'variable).
;;; The coverage tests enumerate the package rather than a fixed list, so a
;;; symbol that gains a definition without gaining a docstring turns them red
;;; instead of rotting quietly.

(require "dotcl-repl")

(defun cl-doc-missing (predicate doc-type)
  (let ((missing '()))
    (do-external-symbols (s "COMMON-LISP")
      (when (and (funcall predicate s)
                 (not (stringp (documentation s doc-type))))
        (push s missing)))
    (sort missing #'string< :key #'symbol-name)))

(deftest cl-doc-every-operator-has-a-docstring
  (cl-doc-missing (lambda (s) (or (fboundp s) (special-operator-p s))) 'function)
  nil)

(deftest cl-doc-every-variable-has-a-docstring
  (cl-doc-missing #'boundp 'variable)
  nil)

;;; The text is shipped in the core, so it must stay plain ASCII.

(deftest cl-doc-docstrings-are-ascii
  (let ((bad '()))
    (do-external-symbols (s "COMMON-LISP")
      (dolist (kind '(function variable))
        (let ((doc (documentation s kind)))
          (when (and (stringp doc)
                     (or (zerop (length doc))
                         (find-if (lambda (c) (>= (char-code c) 128)) doc)))
            (push (list s kind) bad)))))
    bad)
  nil)

;;; One of each kind.

(deftest cl-doc-function
  (stringp (documentation 'car 'function))
  t)

(deftest cl-doc-macro
  (stringp (documentation 'when 'function))
  t)

(deftest cl-doc-special-operator
  (stringp (documentation 'let 'function))
  t)

(deftest cl-doc-special-variable
  (stringp (documentation '*print-base* 'variable))
  t)

(deftest cl-doc-constant
  (stringp (documentation 'most-positive-fixnum 'variable))
  t)

;;; The table belongs to the CL symbols, not to their names.

(defpackage "CL-DOC-SHADOW" (:use "COMMON-LISP") (:shadow "CAR" "*PRINT-BASE*"))

(defun cl-doc-shadow::car (x) x)
(defvar cl-doc-shadow::*print-base* 10)

(deftest cl-doc-not-for-a-same-named-function
  (documentation 'cl-doc-shadow::car 'function)
  nil)

(deftest cl-doc-not-for-a-same-named-variable
  (documentation 'cl-doc-shadow::*print-base* 'variable)
  nil)

(deftest cl-doc-not-for-an-uninterned-symbol
  (documentation (make-symbol "CAR") 'function)
  nil)

;;; ,doc in the REPL prints it.

(deftest cl-doc-repl-doc-command
  (let* ((out (make-string-output-stream))
         (*standard-output* out))
    (dotcl-repl:dispatch ",doc car")
    (not (null (search (documentation 'car 'function)
                       (get-output-stream-string out)))))
  t)
