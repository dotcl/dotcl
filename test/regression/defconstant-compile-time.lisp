;;; DEFCONSTANT has to take effect while the rest of the file compiles.
;;;
;;; CLHS 3.2.3.1 lists DEFCONSTANT among the forms COMPILE-FILE evaluates for
;;; their compile-time side effects: the name must be a constant variable for
;;; the remainder of the compilation. Every implementation also makes the VALUE
;;; readable there, which is what a macro expander in the same file needs -- and
;;; reading it is the whole reason to put a table in a constant.
;;;
;;; dotcl's compile-time form list had DEFCLASS, DEFTYPE, DEFMACRO and the rest
;;; but not DEFCONSTANT, so the constant existed only after the fasl was loaded.
;;; A macro a few hundred lines further down the same file got "Unbound
;;; variable". series does exactly this -- it keeps the special operators it
;;; cannot walk in a DEFCONSTANT and reads it from an expander later in
;;; s-code.lisp -- and could not be compiled at all.
;;;
;;; DEFVAR is deliberately NOT in that list: CLHS says the compiler must
;;; proclaim the name special but must NOT evaluate the initial value, and SBCL
;;; agrees (compiling a file that reads a DEFVAR's value at expansion time
;;; reports failure-p true).

(deftest-compiled-only compile-file-defconstant-is-visible-to-later-macros
  (let ((src "cfdc-src-tmp.lisp")
        (fasl "cfdc-src-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             ;; The BOUNDP guard is not decoration. Once the constant takes
             ;; effect at compile time, loading the fasl in the same image runs
             ;; DEFCONSTANT again with a freshly consed list, which is not EQL to
             ;; the first -- and SBCL signals "the constant is being redefined"
             ;; for exactly this. Real code that puts a list in a constant writes
             ;; the guard for that reason (series calls it DEFCONST-ONCE).
             (format s "(defconstant cfdc-table~%")
             (format s "  (if (boundp 'cfdc-table) (symbol-value 'cfdc-table) '(:a :b :c)))~%")
             (format s "(defmacro cfdc-expand () (list 'quote (length cfdc-table)))~%")
             (format s "(defun cfdc-count () (cfdc-expand))~%"))
           ;; The compile is the test: expanding CFDC-EXPAND has to read the
           ;; constant. Loading afterwards proves the value read was the right
           ;; one rather than something that merely failed to error.
           (compile-file src :output-file fasl)
           (load fasl)
           (funcall (intern "CFDC-COUNT")))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))
      (ignore-errors (fmakunbound (intern "CFDC-COUNT")))
      (ignore-errors (fmakunbound (intern "CFDC-EXPAND")))))
  3)

;;; The same thing one level down: a constant whose value is computed from an
;;; earlier constant, both read at expansion time.

(deftest-compiled-only compile-file-defconstant-chains
  (let ((src "cfdc2-src-tmp.lisp")
        (fasl "cfdc2-src-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (format s "(defconstant cfdc2-base 7)~%")
             (format s "(defconstant cfdc2-derived (* 3 cfdc2-base))~%")
             (format s "(defmacro cfdc2-expand () (list 'quote cfdc2-derived))~%")
             (format s "(defun cfdc2-value () (cfdc2-expand))~%"))
           (compile-file src :output-file fasl)
           (load fasl)
           (funcall (intern "CFDC2-VALUE")))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))
      (ignore-errors (fmakunbound (intern "CFDC2-VALUE")))
      (ignore-errors (fmakunbound (intern "CFDC2-EXPAND")))))
  21)

;;; ---- redefinition ----
;;;
;;; Once DEFCONSTANT takes effect at compile time, a file compiled and loaded in
;;; one image evaluates it TWICE. For a number or a symbol the second value is
;;; EQL to the first and nothing happens. For a list or a string it is a fresh
;;; object. SBCL reports it ("The constant X is being redefined (from (A B C)
;;; to (A B C))"), and libraries carry #+sbcl workarounds for exactly that.
;;; dotcl is not in those feature lists, so when one of the two evaluations is
;;; the compile-time one and the values are similar it keeps the first object
;;; quietly (see the compile-file tests at the end). Every other non-EQL
;;; redefinition still signals, with a CONTINUE restart as in SBCL.

(defconstant dcr-eql-value 42)

;; Re-evaluating a DEFCONSTANT whose value is EQL to the old one is quiet. This
;; is the case that must not become noisy: it is what loading a file twice does.
(deftest defconstant-redefinition.eql-value-is-a-no-op
  (progn (eval '(defconstant dcr-eql-value 42))
         (list dcr-eql-value (eval 'dcr-eql-value)))
  (42 42))

(defconstant dcr-list-value '(a b))

;; A value that is not EQL is still refused.
(deftest defconstant-redefinition.non-eql-signals
  (handler-case (progn (eval '(defconstant dcr-list-value '(a b))) :accepted)
    (error () :error))
  :error)

;; ... and CONTINUE goes ahead and installs the new value, as in SBCL.
(defconstant dcr-continue-value '(old))

(deftest defconstant-redefinition.continue-restart-installs-the-value
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (let ((r (find-restart 'continue)))
                            (when r (invoke-restart r))))))
    (eval '(defconstant dcr-continue-value '(new)))
    (eval 'dcr-continue-value))
  (new))

;; The shape that started this: a list constant in a file that is compiled and
;; then loaded in the same image. The load-time evaluation meets the value the
;; compile-time evaluation installed; the two lists are similar, so the load
;; keeps the first object quietly. SBCL signals here (cl-who, clx and
;; cl-environments carry #+sbcl workarounds for it); CCL and ECL do not.
;; Loading the same fasl a second time is two load-time evaluations, and that
;; is still refused, as in SBCL.
(deftest-compiled-only defconstant-redefinition.compile-file-then-load-keeps-the-value
  (let ((src "dcr-tmp.lisp")
        (fasl "dcr-tmp.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede)
            (format s "(defconstant dcr-from-file '(a b c))~%")
            (format s "(defconstant dcr-string-from-file (make-string 1 :initial-element #\\Newline))~%")
            (format s "(defconstant dcr-vector-from-file #(1 (2 \"x\") #(3)))~%")
            (format s "(defun dcr-from-file-value () dcr-from-file)~%"))
          (compile-file src :output-file fasl)
          (let ((ct-list (symbol-value (intern "DCR-FROM-FILE")))
                (ct-string (symbol-value (intern "DCR-STRING-FROM-FILE"))))
            (list (handler-case (progn (load fasl) :loaded)
                    (error () :error))
                  (eq ct-list (symbol-value (intern "DCR-FROM-FILE")))
                  (eq ct-string (symbol-value (intern "DCR-STRING-FROM-FILE")))
                  (funcall (intern "DCR-FROM-FILE-VALUE"))
                  (handler-case (progn (load fasl) :loaded)
                    (error () :error)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))
      (ignore-errors (fmakunbound (intern "DCR-FROM-FILE-VALUE")))))
  (:loaded t t (a b c) :error))

;; A value that differs between compile time and load time is a real
;; redefinition and is still reported.
(defvar *dcr-phase* :load)

(deftest-compiled-only defconstant-redefinition.compile-file-then-load-different-value-reports
  (let ((src "dcr2-tmp.lisp")
        (fasl "dcr2-tmp.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede)
            (format s "(defconstant dcr2-phase (list cl-user::*dcr-phase*))~%"))
          (let ((*dcr-phase* :compile))
            (compile-file src :output-file fasl))
          (handler-case (progn (load fasl) :loaded)
            (error () :error)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  :error)
