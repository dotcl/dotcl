;;; Load the tree's COMPILE-DEFMACRO into a dotcl host before it compiles the tree.
;;;
;;; The tree's COMPILE-DEFMACRO calls helpers the older host does not have (it records which
;;; macros take &ENVIRONMENT), and the top-level forms of the tree that register macros call them
;;; too, so the definitions of those helpers come first. They are taken from the tree's source,
;;; not copied here, so the host cannot drift from the tree.

(defparameter cl-user::*seed-host-helpers*
  '(*macro-takes-environment* *macro-expander-takes-environment*
    %macro-lambda-list-takes-environment-p %note-macro-takes-environment
    %note-macro-expander-takes-environment %macro-expander-takes-environment-p
    %macro-body-asks-declarations-p %body-walkable-p))

(defun cl-user::seed-eval-tree-forms (file names kinds)
  "Evaluate in the tree's compiler package the top-level forms of FILE that are a definition
   (one of KINDS) of a name in NAMES."
  (let ((*package* (find-package "DOTCL.CIL-COMPILER")))
    (with-open-file (in file)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            when (and (consp form) (member (car form) kinds) (symbolp (cadr form))
                      (member (symbol-name (cadr form)) names :test #'string=))
              do (eval form)))))

(let ((names (mapcar #'symbol-name cl-user::*seed-host-helpers*)))
  (cl-user::seed-eval-tree-forms
   (merge-pathnames "../compiler/cil-compiler.lisp" *load-pathname*) names
   '(cl:defvar cl:defparameter cl:defun)))

(cl-user::seed-eval-tree-forms
 (merge-pathnames "../compiler/cil-forms-defun.lisp" *load-pathname*)
 '("COMPILE-DEFMACRO") '(cl:defun))
