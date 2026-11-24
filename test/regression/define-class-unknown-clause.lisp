;;; A clause DOTNET:DEFINE-CLASS does not know is an error at expansion, with the
;;; usual singular spelling named. It used to be dropped, so (:field ...) and
;;; (:method ...) made a class without the member, and the only complaint was a
;;; .NET TypeLoadException at load.

(require "dotnet-class")

(defun %dcuc-message (form)
  (handler-case (progn (macroexpand-1 form) nil)
    (error (e) (princ-to-string e))))

(deftest define-class-unknown-clause.singular-spellings-name-the-plural
  (let ((f (%dcuc-message '(dotnet:define-class "Dcuc.A" () (:field "v" "System.Int32"))))
        (m (%dcuc-message '(dotnet:define-class "Dcuc.B" () (:method "M" () "System.Int32" 1))))
        (i (%dcuc-message '(dotnet:define-class "Dcuc.C" () (:interface "System.IDisposable")))))
    (list (and f (search "did you mean :FIELDS" f) t)
          (and m (search "did you mean :METHODS" m) t)
          (and i (search "did you mean :IMPLEMENTS" i) t)))
  (t t t))

(deftest define-class-unknown-clause.nonsense-clause-lists-the-clauses
  (let ((m (%dcuc-message '(dotnet:define-class "Dcuc.D" () (:bogus 1)))))
    (and m (search "unknown clause" m) (search ":FIELDS" m) (search ":IMPLEMENTS" m) t))
  t)

(deftest define-class-unknown-clause.known-clauses-still-expand
  (null (%dcuc-message '(dotnet:define-class "Dcuc.E" () (:fields ("v" Int32))
                                              (:methods ("M" () :returns Int32 1)))))
  t)
