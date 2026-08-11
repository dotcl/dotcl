;;; A symbol that appears only inside a macro's backquote template must exist in
;;; its package as soon as the .fasl is loaded — before anything expands the
;;; macro. SBCL interns the whole constant pool at load time; dotcl emitted the
;;; constant's Startup.SymInPkg call inline in the macro body, so the symbol only
;;; came into existence when the macro was first expanded.
;;;
;;; Symptom: a later file's (defpackage ... (:import-from other-pkg #:sym)) could
;;; not find the symbol — lparallel's TIME-REMAINING (defined only inside the
;;; WITH-COUNTDOWN template) was unresolvable from cons-queue / vector-queue,
;;; which the lparallel fork worked around by exporting it.
;;;
;;; Fix: FaslAssembler emits Startup.PreinternSymbol calls for every symbol the
;;; fasl names, at the END of ModuleInit (after the file's own defpackage forms).

(defun %fasl-template-sym-case ()
  (let* ((tmp (format nil "~a/dotcl-tmplsym-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defpackage #:tmplsym-pkg (:use :cl))~%")
      (format s "(in-package #:tmplsym-pkg)~%")
      ;; SECRET-SYM occurs ONLY inside the backquote template.
      (format s "(defmacro mac () `(flet ((secret-sym () 42)) (secret-sym)))~%"))
    (compile-file src)
    (let ((fasl (concatenate 'string (subseq src 0 (- (length src) 5)) ".fasl")))
      (load fasl))
    ;; Interned by the load itself — the macro has not been expanded yet.
    (list (not (null (find-symbol "SECRET-SYM" "TMPLSYM-PKG")))
          ;; ...and a later defpackage can import it, the reported symptom.
          (not (null (eval (read-from-string
                            "(defpackage #:tmplsym-user (:use :cl)
                               (:import-from #:tmplsym-pkg #:secret-sym))"))))
          ;; The macro still expands and runs.
          (eval (read-from-string "(tmplsym-pkg::mac)")))))

(deftest-compiled-only fasl-template-symbol-intern.load-time
  (%fasl-template-sym-case)
  (t t 42))
