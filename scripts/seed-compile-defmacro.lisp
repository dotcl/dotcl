;;; Load the tree's COMPILE-DEFMACRO into a dotcl host before it compiles the tree.

(let ((*package* (find-package "DOTCL.CIL-COMPILER"))
      (source (merge-pathnames "../compiler/cil-forms-defun.lisp" *load-pathname*)))
  (with-open-file (in source)
    (loop for form = (read in nil :eof)
          until (eq form :eof)
          when (and (consp form)
                    (eq (car form) 'cl:defun)
                    (string= (symbol-name (cadr form)) "COMPILE-DEFMACRO"))
            do (eval form))))
