;;; .NET interfaces take part in CLOS dispatch: EnsureDotNetTypeClass makes every
;;; implemented interface a superclass, and orders the class precedence list
;;; concrete-classes → interfaces (most derived first) → T. Before this only the
;;; BaseType chain was mapped, so a method on IEnumerable never applied.

(defun dnid-list-of (elt-type)
  (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List" (list elt-type))))

(defgeneric dnid-kind (x))
(defmethod dnid-kind (x) (declare (ignore x)) :other)

(defmethod dnid-kind ((x "System.Collections.IEnumerable")) (declare (ignore x)) :enumerable)
(deftest dnid-interface-dispatch
  (dnid-kind (dnid-list-of "System.Int32"))
  :enumerable)

;;; A method on the concrete class outranks the interface one.
(defmethod dnid-kind ((x (dotnet:make-generic-type "System.Collections.Generic.List"
                                                   (list "System.Int32"))))
  (declare (ignore x)) :list-int)
(deftest dnid-concrete-beats-interface
  (list (dnid-kind (dnid-list-of "System.Int32"))     ; concrete method exists
        (dnid-kind (dnid-list-of "System.Double")))   ; only the interface one does
  (:list-int :enumerable))

;;; A derived interface outranks the interface it extends.
(defmethod dnid-kind ((x (dotnet:make-generic-type "System.Collections.Generic.IEnumerable"
                                                   (list "System.String"))))
  (declare (ignore x)) :ienum-str)
(defmethod dnid-kind ((x (dotnet:make-generic-type "System.Collections.Generic.IList"
                                                   (list "System.String"))))
  (declare (ignore x)) :ilist-str)
(deftest dnid-derived-interface-wins
  (dnid-kind (dnid-list-of "System.String"))
  :ilist-str)

;;; CPL shape: the class itself first, T last, and every interface after every
;;; concrete class. (Names, not identity, so the check reads in the failure output.)
(defun dnid-cpl-names (type-designator)
  (mapcar #'class-name (class-precedence-list (dotnet:class-for-type type-designator))))

(deftest dnid-cpl-starts-with-class-ends-with-t
  (let ((names (dnid-cpl-names (dotnet:make-generic-type "System.Collections.Generic.List"
                                                         (list "System.Int32")))))
    (list (symbol-name (first names)) (symbol-name (car (last names)))))
  ("List<Int32>" "T"))

(deftest dnid-derived-interface-precedes-base-in-cpl
  (let* ((names (mapcar #'symbol-name
                        (dnid-cpl-names (dotnet:make-generic-type
                                         "System.Collections.Generic.List"
                                         (list "System.Int32")))))
         (ilist (position "IList<Int32>" names :test #'string=))
         (ienum (position "IEnumerable<Int32>" names :test #'string=)))
    (and ilist ienum (< ilist ienum) t))
  t)

;;; Inherited interfaces reach a derived .NET class too: a stream subclass is
;;; still IDisposable.
(deftest dnid-inherited-interface
  (let ((names (mapcar #'symbol-name (dnid-cpl-names "System.IO.MemoryStream"))))
    (and (member "Stream" names :test #'string=)
         (member "IDisposable" names :test #'string=)
         t))
  t)

;;; typep against the interface's class object, and subtypep between the names.
(deftest dnid-typep-interface-class
  (typep (dnid-list-of "System.Int32")
         (dotnet:class-for-type "System.Collections.IEnumerable"))
  t)

(deftest dnid-subtypep-through-interface
  (let ((concrete (class-name (dotnet:class-for-type
                               (dotnet:make-generic-type "System.Collections.Generic.List"
                                                         (list "System.Int32")))))
        (iface (class-name (dotnet:class-for-type "System.Collections.IEnumerable"))))
    (and (subtypep concrete iface) t))
  t)

;;; An interface class can be obtained with no implementor instantiated, and is
;;; itself rooted at T.
(deftest dnid-interface-class-standalone
  (let ((names (mapcar #'symbol-name (dnid-cpl-names "System.IFormatProvider"))))
    (list (first names) (car (last names))))
  ("IFormatProvider" "T"))
