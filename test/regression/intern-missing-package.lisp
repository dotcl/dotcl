;;; INTERN into a package that does not exist signals a PACKAGE-ERROR.

(deftest intern-missing-package.string
  (handler-case (intern "A" "NO-SUCH-PACKAGE-INTERN") (package-error () :package-error))
  :package-error)

(deftest intern-missing-package.symbol
  (handler-case (intern "A" 'no-such-package-intern) (package-error () :package-error))
  :package-error)

(deftest intern-missing-package.existing
  (symbol-package (intern "A" "KEYWORD"))
  #.(find-package "KEYWORD"))
