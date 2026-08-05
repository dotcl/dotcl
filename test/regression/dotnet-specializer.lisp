;;; DEFMETHOD parameter specializers may name a .NET type directly: a type-name
;;; string, a form evaluating to a System.Type, or a symbol naming a type that has
;;; no CLOS class registered yet. Before this, the CLOS class for a .NET type only
;;; appeared once an instance had been seen (or class-for-type called), so a
;;; specializer on a not-yet-instantiated type failed with "no class named X",
;;; and same-simple-name types could not be told apart at the specializer.

(defgeneric dnsp-kind (x))
(defmethod dnsp-kind (x) (declare (ignore x)) :other)

;;; A type-name string. Nothing of this type has been constructed at defmethod time.
(defmethod dnsp-kind ((x "System.Text.StringBuilder")) (declare (ignore x)) :sb)
(deftest dnsp-string-specializer
  (dnsp-kind (dotnet:new "System.Text.StringBuilder"))
  :sb)

;;; A form that evaluates to a System.Type.
(defmethod dnsp-kind ((x (dotnet:resolve-type "System.Uri"))) (declare (ignore x)) :uri)
(deftest dnsp-resolve-type-specializer
  (dnsp-kind (dotnet:new "System.Uri" "http://example.com/"))
  :uri)

;;; A symbol naming a .NET type with no class registered yet (barred: the reader
;;; upcases unbarred symbols, and .NET names are case-sensitive).
(defmethod dnsp-kind ((x |System.Guid|)) (declare (ignore x)) :guid)
(deftest dnsp-symbol-specializer
  (dnsp-kind (dotnet:static "System.Guid" "NewGuid"))
  :guid)

;;; Closed generic types are distinct classes, so they dispatch separately.
(defmethod dnsp-kind ((x (dotnet:make-generic-type "System.Collections.Generic.List"
                                                   (list "System.Int32"))))
  (declare (ignore x)) :list-int)
(defmethod dnsp-kind ((x (dotnet:make-generic-type "System.Collections.Generic.List"
                                                   (list "System.String"))))
  (declare (ignore x)) :list-str)
(deftest dnsp-closed-generic-dispatch
  (list (dnsp-kind (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                         (list "System.Int32"))))
        (dnsp-kind (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                         (list "System.String")))))
  (:list-int :list-str))

;;; Dispatch still walks the BaseType chain: a method on the base type applies to
;;; a derived instance.
(defmethod dnsp-kind ((x "System.Exception")) (declare (ignore x)) :exn)
(deftest dnsp-base-type-dispatch
  (dnsp-kind (dotnet:new "System.InvalidOperationException" "boom"))
  :exn)

;;; A more derived .NET method wins over the base one.
(defmethod dnsp-kind ((x "System.InvalidOperationException")) (declare (ignore x)) :ioe)
(deftest dnsp-most-specific-wins
  (list (dnsp-kind (dotnet:new "System.InvalidOperationException" "boom"))
        (dnsp-kind (dotnet:new "System.Exception" "boom")))
  (:ioe :exn))

;;; Ordinary CLOS specializers are unaffected.
(defclass dnsp-plain () ())
(defmethod dnsp-kind ((x dnsp-plain)) (declare (ignore x)) :plain)
(deftest dnsp-lisp-class-specializer
  (list (dnsp-kind (make-instance 'dnsp-plain)) (dnsp-kind 42))
  (:plain :other))

;;; A name that is neither a class nor a resolvable .NET type still fails.
(deftest dnsp-unknown-specializer-errors
  (handler-case (progn (eval '(defmethod dnsp-kind ((x "No.Such.Type.Here")) :nope)) :no-error)
    (error () :error))
  :error)

(deftest dnsp-unknown-symbol-specializer-errors
  (handler-case (progn (eval '(defmethod dnsp-kind ((x no-such-lisp-class)) :nope)) :no-error)
    (error () :error))
  :error)

;;; .NET generic variance. List<String> implements IEnumerable<String>, and
;;; IEnumerable<out T> is covariant, so the CLR says it is also an
;;; IEnumerable<Object> — an assignability no class precedence list can
;;; enumerate (it would need every instantiation over every supertype of every
;;; type argument). Dispatch asks the CLR for the specializers a generic
;;; function actually has, and ranks such a match just ahead of T.
(defgeneric dnsp-var (x)
  (:method (x) (declare (ignore x)) :other))

(defmethod dnsp-var ((x (dotnet:make-generic-type "System.Collections.Generic.IEnumerable"
                                                  (list "System.Object"))))
  (declare (ignore x))
  :ienum-object)

(deftest dnsp-variance-covariant
  (dnsp-var (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                  (list "System.String"))))
  :ienum-object)

;;; An instantiation the CPL does carry beats the variance-only match...
(defmethod dnsp-var ((x (dotnet:make-generic-type "System.Collections.Generic.IEnumerable"
                                                  (list "System.String"))))
  (declare (ignore x))
  :ienum-string)

;;; ...and the concrete class beats both.
(defmethod dnsp-var ((x (dotnet:make-generic-type "System.Collections.Generic.List"
                                                  (list "System.String"))))
  (declare (ignore x))
  :list-string)

(deftest dnsp-variance-specificity
  (let ((ls (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                  (list "System.String")))))
    (list (dnsp-var ls)
          ;; the covariant method still answers for an instantiation that has no
          ;; closer method
          (dnsp-var (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                          (list "System.Uri"))))))
  (:list-string :ienum-object))

;;; Contravariance: Action<Object> is assignable to Action<String>, so a method
;;; on Action<String> applies to an Action<Object>. An unrelated instantiation
;;; must still fall through to the catch-all.
(defgeneric dnsp-var2 (x)
  (:method (x) (declare (ignore x)) :other))
(defmethod dnsp-var2 ((x (dotnet:make-generic-type "System.Action" (list "System.String"))))
  (declare (ignore x))
  :action-string)

(deftest dnsp-variance-contravariant
  (list (dnsp-var2 (dotnet:make-delegate
                    (dotnet:make-generic-type "System.Action" (list "System.Object"))
                    (lambda (o) (declare (ignore o)) nil)))
        (dnsp-var2 (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                         (list "System.Int32")))))
  (:action-string :other))

;;; TYPEP follows the same rule, so class membership and method applicability
;;; do not disagree. (SUBTYPEP on two .NET classes is name/CType-driven and does
;;; not know about variance yet.)
(deftest dnsp-variance-typep
  (let ((ls (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                  (list "System.String"))))
        (ienum-obj (dotnet:make-generic-type "System.Collections.Generic.IEnumerable"
                                             (list "System.Object")))
        (ienum-int (dotnet:make-generic-type "System.Collections.Generic.IEnumerable"
                                             (list "System.Int32"))))
    (list (typep ls (dotnet:class-for-type ienum-obj))
          (typep ls (dotnet:class-for-type ienum-int))))
  (t nil))
