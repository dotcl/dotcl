;;; (typep <.NET object> 'class-name) must agree with the class-object form and
;;; with dispatch. It used to answer NIL for every .NET class name — only a class
;;; OBJECT worked — so (typep sb 'stringbuilder) was NIL while
;;; (typep sb (dotnet:class-for-type "System.Text.StringBuilder")) was T.
;;;
;;; The lookup also has to register the object's type first: .NET classes are
;;; created lazily, so before this the answer depended on whether something had
;;; already touched that type in the session.

(deftest dntp-simple-name-first-touch
  ;; StringBuilder has not been used earlier in this file; the very first call
  ;; must already answer T.
  (typep (dotnet:new "System.Text.StringBuilder") 'stringbuilder)
  t)

(deftest dntp-class-object-agrees
  (let ((o (dotnet:new "System.Text.StringBuilder")))
    (list (typep o 'stringbuilder)
          (typep o (dotnet:class-for-type "System.Text.StringBuilder"))))
  (t t))

(deftest dntp-base-class-name
  (typep (dotnet:new "System.InvalidOperationException" "boom") 'exception)
  t)

(deftest dntp-interface-name
  (let* ((lst (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                    (list "System.Int32"))))
         (iface (class-name (dotnet:class-for-type "System.Collections.IEnumerable"))))
    (typep lst iface))
  t)

(deftest dntp-unrelated-class-name-is-nil
  (typep (dotnet:new "System.Text.StringBuilder") 'uri)
  nil)

;;; A plain Lisp object must not be dragged into the .NET registry lookup.
(deftest dntp-lisp-object-unaffected
  (list (typep "abc" 'string) (typep 5 'stringbuilder))
  (t nil))
