;;; Regression: a *PACKAGE* that is not a package must report as a TYPE-ERROR.
;;;
;;; The operations that default their package argument to *PACKAGE* cast the
;;; variable's value straight to the runtime Package type, so a NIL there
;;; surfaced as the .NET cast message ("Unable to cast object of type
;;; 'DotCL.Nil' to type 'DotCL.Package'") wrapped in a PROGRAM-ERROR -- no
;;; datum, no expected type, and no hint that *PACKAGE* was the problem.
;;; Ordinary code reaches this: (let ((*package* (find-package "NO-SUCH"))) ...)
;;; binds NIL without complaint, exactly as RT's do-tests does.
;;;
;;; The binding itself is still allowed (SBCL declaims the variable's type and
;;; rejects it there); this only fixes what the readers report.

(defun %pv-classify (thunk)
  "Run THUNK and describe how it failed, as (datum expected-type names-var-p)."
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e)
      (list (type-error-datum e)
            (type-error-expected-type e)
            (and (search "*PACKAGE*" (princ-to-string e)) t)))
    (error (e) (list :other (type-of e) (princ-to-string e)))))

(deftest package-var-type-error.intern
  (%pv-classify (lambda () (let ((*package* nil)) (intern "X"))))
  (nil package t))

;;; FIND-SYMBOL and UNINTERN reach the check by one of two routes, so both
;;; shapes are accepted:
;;;
;;;   - the call reaches the runtime with the package argument omitted, and the
;;;     defaulting above reports the TYPE-ERROR (compiled builds, where the call
;;;     goes straight to the builtin);
;;;   - the call goes through the CIL-STDLIB wrapper, whose (pkg *package*)
;;;     default forwards the NIL as an explicit package designator, and NIL
;;;     designates the (nonexistent) package "NIL" -- a PACKAGE-ERROR, which is
;;;     what CLHS says about that designator (emit-free builds, which have no
;;;     builtin-direct route).
;;;
;;; What both must do is fail, naming the operation and the offending value.
;;; INTERN is not in this pair: its builtin already treats a NIL package
;;; argument as "not supplied", so both routes land on the TYPE-ERROR.
(defun %pv-reports-p (thunk op)
  (handler-case (progn (funcall thunk) nil)
    (error (e)
      (let ((s (princ-to-string e)))
        (and (search op s) (search "NIL" s) t)))))

(deftest package-var-type-error.find-symbol
  (%pv-reports-p (lambda () (let ((*package* nil)) (find-symbol "CAR")))
                 "FIND-SYMBOL")
  t)

(deftest package-var-type-error.unintern
  (%pv-reports-p (lambda () (let ((*package* nil)) (unintern 'no-such-symbol-here)))
                 "UNINTERN")
  t)

(deftest package-var-type-error.print
  (%pv-classify (lambda () (let ((*package* nil)) (prin1-to-string 'car))))
  (nil package t))

;;; The value need not be NIL: any non-package reports the same way, carrying
;;; the offending value as the datum. Checked on the printer, which requires an
;;; actual package. (INTERN reaches its own package-designator check first, so a
;;; string or symbol value gets resolved there instead of reported here.)
(deftest package-var-type-error.non-nil-value
  (list (%pv-classify (lambda () (let ((*package* 7)) (prin1-to-string 'car))))
        (%pv-classify (lambda () (let ((*package* "CL-USER")) (prin1-to-string 'car)))))
  ((7 package t) ("CL-USER" package t)))

;;; The normal paths are unchanged.
(deftest package-var-type-error.valid-package-still-works
  (let ((*package* (find-package "CL-USER")))
    (list (symbol-name (intern "PV-STILL-WORKS"))
          (nth-value 1 (find-symbol "CAR" "CL"))
          (prin1-to-string 'car)))
  ("PV-STILL-WORKS" :external "CAR"))
