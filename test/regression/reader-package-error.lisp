;;; A token naming a package that does not exist, or a symbol that is not external, signals
;;; an error that is both a READER-ERROR and a PACKAGE-ERROR (SBCL does the same). Other
;;; reader errors stay READER-ERROR only.

(defun %rpe (string)
  (handler-case (progn (read-from-string string) :no-error)
    (error (e) (list (typep e 'reader-error) (typep e 'package-error)))))

(deftest reader-package-error.missing-package
  (list (%rpe "no-such-package::foo") (%rpe "no-such-package:foo"))
  ((t t) (t t)))

(deftest reader-package-error.not-external
  (%rpe "cl:no-such-symbol-xyz")
  (t t))

(deftest reader-package-error.package-slot
  (handler-case (read-from-string "no-such-pkg-q:foo")
    (package-error (e) (let ((p (package-error-package e))) (if (packagep p) (package-name p) p))))
  "NO-SUCH-PKG-Q")

(deftest reader-package-error.other-reader-errors-unchanged
  (list (%rpe ")") (%rpe "#<foo>") (%rpe "cl:car") (%rpe "common-lisp-user::zzz-ok"))
  ((t nil) (t nil) :no-error :no-error))

(deftest reader-package-error.read-suppress
  (let ((*read-suppress* t)) (%rpe "no-such-package::foo"))
  :no-error)
