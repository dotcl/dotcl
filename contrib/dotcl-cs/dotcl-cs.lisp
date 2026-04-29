;;; dotcl-cs.lisp — Compile and embed C# code in dotcl via Roslyn.
;;;
;;; Usage: (require "dotcl-cs")
;;;
;;; Provides:
;;;   dotcl-cs:disassemble-cs body-string
;;;     → list of (:OP operand) instructions, dotcl-SIL-shaped.
;;;   dotcl-cs:inline-cs (bindings &key returns body)
;;;     → macro that splices C# IL inline into the enclosing dotcl function.
;;;
;;; Implementation (D686 + D875, unified in D903):
;;;   Loads the contrib's own lib/ directory (Roslyn + a small C# helper
;;;   DotCL.Contrib.DotclCs.RoslynCompiler) via dotnet:load-assembly and
;;;   calls CompileAndDisassemble in-process. inline-cs uses disassemble-cs
;;;   at macro-expansion time and expands to %inline-cs-spliced (the
;;;   internal special form in DOTCL-INTERNAL).
;;;
;;; MVP scope for inline-cs: only `long` parameters and `long` return.
;;; Future: float, double, references, branches that respect dotcl's label
;;; namespace.

(defpackage :dotcl-cs
  (:use :cl)
  (:export #:disassemble-cs
           #:inline-cs))

(in-package :dotcl-cs)

;;; ---------------------------------------------------------------------------
;;; disassemble-cs (Phase 1 — D680/D681/D686)

;; Captured at load time; *load-pathname* is only bound during Load.
(defvar *contrib-dir*
  (when *load-pathname*
    (make-pathname :directory (pathname-directory *load-pathname*)
                   :defaults *load-pathname*)))

(defvar *lib-loaded* nil)

(defun ensure-lib-loaded ()
  (unless *lib-loaded*
    (unless *contrib-dir*
      (error "dotcl-cs: *contrib-dir* not captured — contrib loaded outside of Load?"))
    (let ((lib-dir (make-pathname
                    :directory (append (pathname-directory *contrib-dir*)
                                       (list "lib"))
                    :defaults *contrib-dir*)))
      ;; Load order matters: Roslyn.dll first, then CSharp.dll (depends on core),
      ;; then our helper (depends on both + DotCL runtime).
      (dolist (name '("Microsoft.CodeAnalysis.dll"
                      "Microsoft.CodeAnalysis.CSharp.dll"
                      "DotCL.Contrib.DotclCs.dll"))
        (dotnet:load-assembly
         (namestring (merge-pathnames name lib-dir)))))
    (setf *lib-loaded* t)))

(defun disassemble-cs (body)
  "Wrap BODY (a C# string with one or more public static method definitions)
  in an implicit class, compile in-memory via Roslyn, and disassemble the
  first public static method's IL into a dotcl-SIL-shaped list like
  ((:LDARG-0) (:LDC-I4 42) (:ADD) (:RET))."
  (check-type body string)
  (ensure-lib-loaded)
  (dotnet:static "DotCL.Contrib.DotclCs.RoslynCompiler"
                 "CompileAndDisassemble" body))

;;; ---------------------------------------------------------------------------
;;; inline-cs (Phase 2 — D875)

(defmacro inline-cs (bindings &key returns body)
  "Compile BODY (a C# string) at macro-expansion time and splice the IL
   into the enclosing dotcl function. BINDINGS is a list of (cs-name
   cs-type lisp-form). LISP-FORM is evaluated to provide the value of
   cs-name in the C# body. Currently only `long` cs-type is supported,
   and RETURNS must be `long`.

   Example:
     (inline-cs ((x long a) (y long b)) :returns long
                :body \"return x + y;\")"
  (when (and (stringp returns) (null body))
    ;; Old-style call: (inline-cs bindings \"body\") with no :returns/:body kw
    (setf body returns returns 'long))
  (unless (and (symbolp returns)
               (string-equal (symbol-name returns) "LONG"))
    (error "inline-cs: only :returns long is supported (got ~S)" returns))
  (unless (and (stringp body) (> (length body) 0))
    (error "inline-cs: :body must be a non-empty C# string"))
  (let* ((cs-params
           (with-output-to-string (out)
             (loop for b in bindings
                   for first = t then nil do
                     (unless first (write-string ", " out))
                     (format out "~A ~A"
                             (string-downcase (symbol-name (second b)))
                             (string-downcase (symbol-name (first b)))))))
         (cs-fn (format nil "public static ~A __dotcl_inline_cs__(~A) { ~A }"
                        (string-downcase (symbol-name returns))
                        cs-params body))
         (sil (disassemble-cs cs-fn))
         (lisp-vals (mapcar #'third bindings)))
    ;; %INLINE-CS-SPLICED is in DOTCL-INTERNAL (intern'd at compiler
    ;; bootstrap, see cil-compiler.lisp). Reference it via find-symbol
    ;; to get the same Symbol instance the special-form dispatcher uses.
    `(,(or (find-symbol "%INLINE-CS-SPLICED" "DOTCL-INTERNAL")
           (intern "%INLINE-CS-SPLICED" "DOTCL-INTERNAL"))
       ,lisp-vals :returns ,returns ,sil)))

;; Intentionally NO (provide "dotcl-cs") here — D688 regression test relies
;; on this contrib being a module that forgets to call provide, so that
;; require's auto-push behaviour can be exercised. If you add provide here,
;; update test/regression/recent-fixes.lisp:d688-* to use a different
;; provide-less contrib.
