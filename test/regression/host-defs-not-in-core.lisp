;;; What compiler/cil-host.lisp defines is for the Lisp that hosts the
;;; cross-compile only. The compiler's sources are read with the host's features,
;;; so the same definition written in them under #-dotcl is compiled into the
;;; core, where it defines a second function under the compiler's package next to
;;; the runtime builtin. None of those names may carry a definition of their own
;;; in DOTCL.CIL-COMPILER.

(defvar *hdc-host-file*
  (merge-pathnames "../../compiler/cil-host.lisp" (or *load-truename* *default-pathname-defaults*)))

(defun hdc-host-names ()
  "The names cil-host.lisp defines with a top-level DEFUN or DEFVAR."
  (with-open-file (in *hdc-host-file*)
    (loop for line = (read-line in nil)
          while line
          nconc (loop for prefix in '("(defun " "(defvar ")
                      when (and (> (length line) (length prefix))
                                (string= prefix line :end2 (length prefix)))
                        collect (let ((rest (subseq line (length prefix))))
                                  (string-upcase
                                   (subseq rest 0 (position-if (lambda (c) (member c '(#\Space #\( #\))))
                                                               rest))))))))

(deftest host-defs-not-in-core.names-found
  (not (null (member "%MACROEXPAND-1-OR-SELF" (hdc-host-names) :test #'string=)))
  t)

(deftest host-defs-not-in-core.no-second-definition
  (let ((builtin-pkg (find-package "DOTCL-INTERNAL"))
        (compiler-pkg (find-package "DOTCL.CIL-COMPILER")))
    (loop for name in (hdc-host-names)
          for s = (find-symbol name compiler-pkg)
          when (and s
                    (not (eq s (find-symbol name builtin-pkg)))
                    (or (fboundp s) (boundp s)))
            collect name))
  nil)
