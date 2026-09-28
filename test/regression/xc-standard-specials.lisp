;;; While cross-compiling the core, a LET of a standard special variable
;;; (*PACKAGE*, *PRINT-LENGTH*, *READTABLE*, ...) binds it dynamically, with no
;;; (declare (special ...)) needed.
;;;
;;; Bug: the compiler learns that a variable is special from DEFVAR/PROCLAIM
;;; (*global-specials*) or by asking the running image. While cross-compiling it
;;; cannot ask the image, and nothing in the core DEFVARs the variables the
;;; standard defines, so (let ((*package* ...)) ...) in the core bound a lexical
;;; variable that no callee could see. The compiler's own
;;; (let ((*print-length* 6) (*print-level* 3)) (format nil ...)) in the
;;; "Illegal function call" diagnostic was bound that way and never truncated.
;;;
;;; Fix: the standard special variables are special by name while
;;; cross-compiling.

(defun xss-compile-cross (form)
  "The instruction list the compiler produces for FORM in cross-compile mode,
as a string."
  (let ((flag (find-symbol "*CROSS-COMPILING*" "DOTCL-INTERNAL"))
        (compile-toplevel (find-symbol "COMPILE-TOPLEVEL" "DOTCL.CIL-COMPILER")))
    (progv (list flag) '(t)
      (prin1-to-string (funcall compile-toplevel form)))))

(defun xss-binds-dynamically-p (var)
  "T when a LET of VAR, compiled in cross-compile mode, pushes a dynamic binding."
  (and (search "DynamicBindings.Push"
               (xss-compile-cross
                `(defun xss-probe (value) (let ((,var value)) (xss-callee)))))
       t))

(deftest xc-standard-specials.package
  (xss-binds-dynamically-p '*package*)
  t)

(deftest xc-standard-specials.printer-variables
  (mapcar #'xss-binds-dynamically-p
          '(*print-length* *print-level* *print-gensym* *print-readably*))
  (t t t t))

(deftest xc-standard-specials.reader-and-streams
  (mapcar #'xss-binds-dynamically-p
          '(*readtable* *read-base* *standard-output* *error-output*))
  (t t t t))

(deftest xc-standard-specials.repl-variables
  (mapcar #'xss-binds-dynamically-p '(* ** *** + ++ +++ - / // ///))
  (t t t t t t t t t t))

;;; Only the standard's own variables: an earmuffed name in another package is
;;; still lexical unless something declares it special.
(deftest xc-standard-specials.other-earmuffs-stay-lexical
  (xss-binds-dynamically-p (intern "*XSS-NOT-SPECIAL*" "CL-USER"))
  nil)

;;; The compiler itself is cross-compiled: its "Illegal function call" message
;;; binds *PRINT-LENGTH* 6 / *PRINT-LEVEL* 3, which now reaches FORMAT.
(deftest-compiled-only xc-standard-specials.compiler-binding-reaches-format
  (handler-case (progn (funcall (compile nil '(lambda () ((0 1 2 3 4 5 6 7 8 9))))) "")
    (error (e) (princ-to-string e)))
  "Illegal function call: (0 1 2 3 4 5 ...)")
