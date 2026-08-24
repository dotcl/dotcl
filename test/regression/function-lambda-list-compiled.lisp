;;; The compiled half of DOTCL:FUNCTION-LAMBDA-LIST. The generic-function and
;;; interpreted sources are pinned in function-lambda-list.lisp, which
;;; deliberately left the compiled case unasserted because recording it meant
;;; emitting a per-definition constant -- one leaked constant-pool entry per
;;; redefinition, which DEFUN-REDEFINE-DOES-NOT-LEAK-CONSTANTS exists to prevent.
;;;
;;; The way out was to stop emitting anything: the lambda list rides the
;;; :defmethod / :defmethod-direct / :defmethod-native directive, so the
;;; assembler stores it on the function object and no code (and no pool entry) is
;;; produced. That is what makes the compiled case assertable at all, so these
;;; live with the redefinition test rather than beside the older contract tests.

(defun flc-simple (a b) (list a b))                  ; :defmethod-direct path
(defun flc-opt (a &optional b &key c) (list a b c))  ; :defmethod (array XEP)
(defun flc-none () :v)
(defun flc-rest (a &rest r) (cons a r))
(defmacro flc-mac (x &body body) (list* 'progn x body))

(defun %flc (name) (multiple-value-list (dotcl:function-lambda-list name)))

(deftest function-lambda-list-compiled.defun-paths
  (list (%flc 'flc-simple) (%flc 'flc-opt) (%flc 'flc-none) (%flc 'flc-rest))
  (((a b) t) ((a &optional b &key c) t) (nil t) ((a &rest r) t)))

;;; A macro shows the lambda list the user wrote, not the (form env) pair its
;;; expander actually takes.
(deftest function-lambda-list-compiled.macro
  (%flc 'flc-mac)
  ((x &body body) t))

;;; Recording must survive redefinition -- and must not accumulate, which is
;;; what carrying it on the directive instead of in emitted code buys.
(deftest function-lambda-list-compiled.survives-redefinition
  (progn (eval '(defun flc-redef (p) p))
         (eval '(defun flc-redef (p q &key r) (list p q r)))
         (%flc 'flc-redef))
  ((p q &key r) t))

;;; The recorded lambda list must not carry a package. The SIL is compared byte
;;; for byte across self-host generations (SELFHOST-CHECK), and a parameter's
;;; package is not stable across them: a DEFSTRUCT accessor's parameter interns
;;; in DOTCL-INTERNAL under an SBCL cross-compile and in the current package
;;; under self-host, so the directive printed (DOTCL-INTERNAL::OBJ) in one
;;; generation and (OBJ) in the next. The compiler now writes the names
;;; uninterned and the assembler resolves them against the definition's package.
;;;
;;; Resolving must use FIND-SYMBOL and never INTERN: creating a symbol as a side
;;; effect of recording display data made (defstruct (s (:conc-name nil))
;;; otherpkg::a) leave A interned in the reading package, which
;;; DEFSTRUCT-CONC-NAME.NIL-KEEPS-THE-SLOT-SYMBOL exists to catch.
(deftest function-lambda-list-compiled.names-are-plain-symbols
  (let ((ll (dotcl:function-lambda-list 'flc-simple)))
    (list (mapcar #'symbol-name ll)
          ;; not gensyms: a tool prints these, and #:A is not what a user wants
          (notnot (every #'symbol-package ll))))
  (("A" "B") t))

(deftest function-lambda-list-compiled.recording-interns-nothing
  (progn
    (eval (read-from-string "(defpackage #:flc-slotpkg (:use))"))
    (eval (read-from-string
           "(defstruct (flc-s (:conc-name nil)) flc-slotpkg::flc-slot)"))
    ;; the slot symbol stays where it was read; nothing new in this package
    (values (find-symbol "FLC-SLOT" *package*)))
  nil)

;;; The interpreter has its own DEFMACRO, which builds a (whole env) expander
;;; closure -- and recording a lambda list for interpreted closures means that
;;; expander records ITS OWN parameters. DOTCL:FUNCTION-LAMBDA-LIST then answered
;;; (#:MWHOLE #:MENV) instead of what the user wrote.
;;;
;;; This only showed on an emit-free build, where the interpreter is the only
;;; DEFMACRO path there is; with the compiler present the compiled path had
;;; already recorded the right list. Asserting it under :INTERPRET pins it on
;;; every build.
(deftest function-lambda-list-compiled.interpreted-macro
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(defmacro flc-interp-mac (a &body body) (list* 'progn a body)))
    (%flc 'flc-interp-mac))
  ((a &body body) t))

;;; &whole and &environment are part of what the user wrote and stay visible.
(deftest function-lambda-list-compiled.interpreted-macro-whole-and-env
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(defmacro flc-interp-we (&whole w a &environment e)
            (declare (ignore w e)) a))
    (%flc 'flc-interp-we))
  ((&whole w a &environment e) t))
