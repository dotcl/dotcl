;;; Regression: DotclHost.Call — the host API a C# program uses to call into
;;; Lisp — must reach a function defined in a USER package.
;;;
;;; It resolved through the internal name-based symbol bridge, which was later
;;; narrowed to dotcl's own packages (a correct change: a name is not an
;;; identity). That silently broke every host entry point, including the one the
;;; `dotnet new dotcl-app` template's Program.cs calls: the app defines
;;; APP-MAIN in package APP, and Call could no longer find it.
;;;
;;; Call now resolves an unqualified name through any package holding an fbound
;;; symbol of that name, and accepts "PKG:NAME" to say which one.

(defpackage :hostcall-a (:use :cl))
(defpackage :hostcall-b (:use :cl))

(defun hostcall-a::entry (x) (format nil "a:~a" x))
(defun hostcall-b::other (x) (format nil "b:~a" x))

(defun %host-call (name &rest args)
  (apply #'dotnet:static "DotCL.DotclHost" "Call" name args))

;;; Unqualified name, defined only in a user package.
(deftest host-call-user-package
  (%host-call "ENTRY" "x")
  "a:x")

;;; Package-qualified names, both single and double colon.
(deftest host-call-qualified
  (list (%host-call "HOSTCALL-A:ENTRY" 1)
        (%host-call "HOSTCALL-B::OTHER" 2))
  ("a:1" "b:2"))

;;; A CL function still resolves (the normal resolver runs first).
(deftest host-call-cl-function
  (%host-call "STRING-UPCASE" "abc")
  "ABC")

;;; Ambiguity is an error, not a coin flip — the caller must qualify.
(deftest host-call-ambiguous
  (progn
    (setf (symbol-function 'hostcall-a::shared) (lambda (x) x))
    (setf (symbol-function 'hostcall-b::shared) (lambda (x) x))
    (handler-case (progn (%host-call "SHARED" 1) :no-error)
      (error () :error)))
  :error)

;;; A name nothing defines still reports the missing binding.
(deftest host-call-undefined
  (handler-case (progn (%host-call "NO-SUCH-HOST-ENTRY-POINT") :no-error)
    (error () :error))
  :error)
