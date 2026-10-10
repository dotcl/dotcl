;;; A symbol that only shares its name with a COMMON-LISP class does not name
;;; that class. FIND-CLASS of a symbol named LIST in another package returned
;;; the class LIST: cl-l10n asks (find-class 'ldml:list nil) for each element
;;; of the CLDR files and falls back to its own node class on NIL, so it made
;;; instances of LIST and its loading stopped on an assertion. Expected values
;;; are SBCL 2.6.8's answers.

(defpackage :find-class-other-pkg (:use) (:export #:list #:hash-table #:stream #:foo))

(deftest find-class-other-package-cl-name.not-found
  (list (find-class 'find-class-other-pkg:list nil)
        (find-class 'find-class-other-pkg:hash-table nil)
        (find-class 'find-class-other-pkg:stream nil)
        (find-class 'find-class-other-pkg:foo nil))
  (nil nil nil nil))

(deftest find-class-other-package-cl-name.signals
  (handler-case (progn (find-class 'find-class-other-pkg:list) :no-error)
    (error () :error))
  :error)

;; The COMMON-LISP symbols themselves still name their classes.
(deftest find-class-other-package-cl-name.cl-symbols
  (mapcar (lambda (s) (class-name (find-class s))) '(list hash-table stream))
  (list hash-table stream))

;; Once the other package's symbol names a class of its own, that is the one found.
(deftest find-class-other-package-cl-name.own-class
  (progn
    (eval '(defclass find-class-other-pkg:list () ()))
    (list (eq (find-class 'find-class-other-pkg:list) (find-class 'list))
          (class-name (find-class 'find-class-other-pkg:list))))
  (nil find-class-other-pkg:list))
