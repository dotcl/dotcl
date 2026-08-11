;;; An interpreted DEFVAR / DEFPARAMETER / DEFCONSTANT dropped the docstring.
;;;
;;; COMPILE-DEFVAR emitted the Runtime.SetVariableDocumentation call, but the
;;; three %MINI-EVAL cases never read the form's fourth element, so a variable
;;; defined through EVAL always answered NIL to
;;; (documentation name 'variable) — ansi-test DEFVAR.4/5, DEFPARAMETER.4/5.
;;;
;;; The documentation is updated INDEPENDENTLY of the value: DEFVAR skips the init
;;; form when the variable is already bound, but still rewrites the documentation
;;; (CLHS). DEFVAR.5 turns on exactly that.
;;;
;;; Note: this family HIDES if a symbol is reused across modes. The compiled run
;;; goes first and sets the documentation, so the interpreted run can read that
;;; value while doing nothing itself. Use a different symbol per mode.

(defun %dvd (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

;;; --- DEFVAR

(deftest interp-defvar-documentation.defvar-compile
  (%dvd :compile '(progn (defvar *dvd-c1* 200 "Whatever.")
                   (documentation '*dvd-c1* 'variable)))
  "Whatever.")

(deftest interp-defvar-documentation.defvar-interpret
  (%dvd :interpret '(progn (defvar *dvd-i1* 200 "Whatever.")
                     (documentation '*dvd-i1* 'variable)))
  "Whatever.")

;;; --- DEFPARAMETER

(deftest interp-defvar-documentation.defparameter-compile
  (%dvd :compile '(progn (defparameter *dvd-c2* 200 "Doc-P.")
                   (documentation '*dvd-c2* 'variable)))
  "Doc-P.")

(deftest interp-defvar-documentation.defparameter-interpret
  (%dvd :interpret '(progn (defparameter *dvd-i2* 200 "Doc-P.")
                     (documentation '*dvd-i2* 'variable)))
  "Doc-P.")

;;; --- DEFCONSTANT

(deftest interp-defvar-documentation.defconstant-interpret
  (%dvd :interpret '(progn (defconstant +dvd-i3+ 1 "Doc-K.")
                     (documentation '+dvd-i3+ 'variable)))
  "Doc-K.")

;;; --- redefinition: the value stays, the documentation is updated (ansi DEFVAR.5)

(deftest interp-defvar-documentation.redefinition-interpret
  (%dvd :interpret '(let ((x 0))
                     (defvar *dvd-i4* 200 "Whatever.")
                     (list (documentation '*dvd-i4* 'variable)
                           *dvd-i4*
                           (progn (defvar *dvd-i4* (incf x) "And ever.")
                                  (documentation '*dvd-i4* 'variable))
                           *dvd-i4*
                           x)))
  ("Whatever." 200 "And ever." 200 0))

;;; --- over-fix guards ---------------------------------------------------

;;; a DEFVAR with no docstring stays NIL (rejects a fix that stores something)
(deftest interp-defvar-documentation.no-docstring-interpret
  (%dvd :interpret '(progn (defvar *dvd-i5* 1) (documentation '*dvd-i5* 'variable)))
  nil)

;;; a non-string fourth element is left alone
(deftest interp-defvar-documentation.non-string-fourth-interpret
  (%dvd :interpret '(progn (defvar *dvd-i6* 1 nil) (documentation '*dvd-i6* 'variable)))
  nil)

;;; value, special proclamation and return value are unchanged
(deftest interp-defvar-documentation.value-and-special-interpret
  (%dvd :interpret '(list (defvar *dvd-i7* 42 "d")
                     *dvd-i7*
                     (flet ((%f () *dvd-i7*)) (let ((*dvd-i7* 7)) (%f)))
                     (progn (defvar *dvd-i7* 99 "d2") *dvd-i7*)))
  (*dvd-i7* 42 7 42))
