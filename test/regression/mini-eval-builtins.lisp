;;; The compiler recognises a set of internal lowering targets by SYMBOL-NAME
;;; (%MAKE-PACKAGE, %PACKAGE-EXPORT, %SET-ELT, ... — the forms DEFPACKAGE, the
;;; package functions and the SETF expanders macroexpand into) and emits a direct
;;; runtime call for each. For most of them that name test was the *only*
;;; definition: the symbol carried no function. The tree-walk interpreter
;;; resolves an operator through SYMBOL-FUNCTION, so every interpreted DEFPACKAGE
;;; died on "Undefined function: %MAKE-PACKAGE" — and on emit-free
;;; (netstandard2.0) builds the interpreter is the only evaluator there is.
;;;
;;; These tests drive %MINI-EVAL directly, so they run under the ordinary
;;; compiled harness and still fail if a lowering target loses its binding.

(defun %meb-eval (form) (dotcl.cil-compiler::%mini-eval form nil))

;;; --- DEFPACKAGE: %MAKE-PACKAGE / %PACKAGE-USE / %PACKAGE-EXPORT / %PACKAGE-NICKNAME

(deftest mini-eval-builtins.defpackage
  (progn
    (%meb-eval '(defpackage "MEB-A" (:use "CL") (:nicknames "MEB-A-NICK")
                  (:export "MEB-FOO")))
    (list (packagep (find-package "MEB-A"))
          (eq (find-package "MEB-A-NICK") (find-package "MEB-A"))
          (nth-value 1 (find-symbol "MEB-FOO" "MEB-A"))
          (not (null (member (find-package "CL") (package-use-list "MEB-A"))))))
  (t t :external t))

;;; --- EXPORT / UNEXPORT / IMPORT / SHADOW / SHADOWING-IMPORT / UNUSE-PACKAGE
;;; Each expands into the matching %-lowering, so an interpreted call reaches it.

(deftest mini-eval-builtins.package-functions
  (progn
    (%meb-eval '(defpackage "MEB-B" (:use "CL")))
    (%meb-eval '(export (intern "MEB-BAR" "MEB-B") "MEB-B"))
    (let ((exported (nth-value 1 (find-symbol "MEB-BAR" "MEB-B"))))
      (%meb-eval '(unexport (intern "MEB-BAR" "MEB-B") "MEB-B"))
      (%meb-eval '(shadow "CAR" "MEB-B"))
      (%meb-eval '(import (intern "MEB-IMPORTED" "CL-USER") "MEB-B"))
      (%meb-eval '(shadowing-import (intern "LIST" "CL-USER") "MEB-B"))
      (%meb-eval '(unuse-package "CL" "MEB-B"))
      (list exported
            (nth-value 1 (find-symbol "MEB-BAR" "MEB-B"))
            (not (null (member (find-symbol "CAR" "MEB-B")
                               (package-shadowing-symbols "MEB-B"))))
            (eq (find-symbol "MEB-IMPORTED" "MEB-B")
                (find-symbol "MEB-IMPORTED" "CL-USER"))
            (package-use-list "MEB-B"))))
  (:external :internal t t nil))

;;; --- SETF expanders: %SET-ELT / %SET-CHAR / %SET-SUBSEQ / %PUTF

(deftest mini-eval-builtins.setf-elt
  (%meb-eval '(let ((v (vector 1 2 3))) (setf (elt v 1) 99) (coerce v 'list)))
  (1 99 3))

(deftest mini-eval-builtins.setf-char
  (%meb-eval '(let ((s (copy-seq "abc"))) (setf (char s 0) #\z) s))
  "zbc")

(deftest mini-eval-builtins.setf-subseq
  (%meb-eval '(let ((s (copy-seq "abcdef"))) (setf (subseq s 1 3) "XY") s))
  "aXYdef")

(deftest mini-eval-builtins.setf-getf
  (%meb-eval '(let ((pl (list :a 1))) (setf (getf pl :a) 2) (getf pl :a)))
  2)

;;; --- LOOP over a package: %PACKAGE-ALL-SYMBOLS / %PACKAGE-EXTERNAL-SYMBOLS

(deftest mini-eval-builtins.loop-symbols-of
  (progn
    (%meb-eval '(defpackage "MEB-C" (:use) (:export "MEB-X")))
    (intern "MEB-Y" "MEB-C")
    (list (sort (mapcar #'symbol-name
                        (%meb-eval '(loop for s being the symbols of "MEB-C"
                                          collect s)))
                #'string<)
          (mapcar #'symbol-name
                  (%meb-eval '(loop for s being the external-symbols of "MEB-C"
                                    collect s)))))
  (("MEB-X" "MEB-Y") ("MEB-X")))

;;; --- %DOTNET-CALL-DIRECT: a compiler intrinsic whose third argument is a
;;; literal list of parameter types, so it cannot be given a function binding —
;;; the interpreter needs its own special-form case. Dropping the overload hint
;;; and going through DOTNET:INVOKE is the same call the assembler itself falls
;;; back to when it cannot resolve the overload.

(deftest mini-eval-builtins.dotnet-call-direct
  (%meb-eval '(let ((sb (dotnet:new "System.Text.StringBuilder")))
                (%dotnet-call-direct "System.Text.StringBuilder" "Append"
                                     ("System.String") sb "ab")
                (%dotnet-call-direct "System.Text.StringBuilder" "Append"
                                     ("System.Int32") sb 7)
                (%dotnet-call-direct "System.Text.StringBuilder" "ToString" () sb)))
  "ab7")
