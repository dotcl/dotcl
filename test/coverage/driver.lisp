;;; driver.lisp — compile lib.lisp to a fasl, load it, and call some of its
;;; functions. Run under a .NET coverage collector (see check.sh): with
;;; DOTCL_FASL_LOADFROM=1 the fasl is file-backed, so the collector attributes
;;; the executed lines to lib.lisp through the PDB's document table.
(in-package :cl-user)

(let* ((src (or (dotcl:getenv "COVERAGE_SRC") "lib.lisp"))
       (fasl (compile-file src)))
  (load fasl)
  (format t "~&;;COVERAGE result=~s~%" (funcall (intern "COV-RUN" :cl-user)))
  (force-output))
