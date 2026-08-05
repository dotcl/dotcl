;;; A closed generic type's class has the OPEN definition as a superclass, so a
;;; method can be specialized on List`1 and apply to every instantiation, while a
;;; method on List<Int32> still wins for that one. Same for generic interfaces.

(defun dnog-list-of (elt-type)
  (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List" (list elt-type))))
(defun dnog-list-class (elt-type)
  (dotnet:class-for-type (dotnet:make-generic-type "System.Collections.Generic.List"
                                                   (list elt-type))))

(defgeneric dnog-kind (x))
(defmethod dnog-kind (x) (declare (ignore x)) :other)

(defmethod dnog-kind ((x (dotnet:resolve-type "System.Collections.Generic.List`1")))
  (declare (ignore x)) :any-list)
(deftest dnog-wildcard-applies-to-every-instantiation
  (list (dnog-kind (dnog-list-of "System.Int32"))
        (dnog-kind (dnog-list-of "System.String")))
  (:any-list :any-list))

(defmethod dnog-kind ((x (dotnet:make-generic-type "System.Collections.Generic.List"
                                                   (list "System.Int32"))))
  (declare (ignore x)) :list-int)
(deftest dnog-closed-beats-open
  (list (dnog-kind (dnog-list-of "System.Int32"))
        (dnog-kind (dnog-list-of "System.Double")))
  (:list-int :any-list))

;;; Open generic INTERFACE specializer: a string array is IReadOnlyCollection<string>.
(defgeneric dnog-coll (x))
(defmethod dnog-coll (x) (declare (ignore x)) :other)
(defmethod dnog-coll ((x (dotnet:resolve-type "System.Collections.Generic.IReadOnlyCollection`1")))
  (declare (ignore x)) :any-rocoll)
(deftest dnog-open-interface-specializer
  (dnog-coll (dotnet:new-array "System.String" "a" "b"))
  :any-rocoll)

;;; ...and the closed interface outranks the open one.
(defmethod dnog-coll ((x (dotnet:make-generic-type "System.Collections.Generic.IReadOnlyCollection"
                                                   (list "System.String"))))
  (declare (ignore x)) :rocoll-str)
(deftest dnog-closed-interface-beats-open-interface
  (list (dnog-coll (dotnet:new-array "System.String" "a"))
        (dnog-coll (dotnet:new-array "System.Int32" 1)))
  (:rocoll-str :any-rocoll))

;;; CPL: the open definition sits immediately behind its closed instantiation.
(deftest dnog-open-follows-closed-in-cpl
  (let ((names (mapcar (lambda (c) (symbol-name (class-name c)))
                       (class-precedence-list (dnog-list-class "System.Int32")))))
    (list (first names) (second names)))
  ("List<Int32>" "List<T>"))

;;; An open-constructed interface (IList<T> as List<T> declares it) must collapse to
;;; the generic definition — otherwise a second, nameless class with the same display
;;; name gets registered and the wildcard specializer would miss it.
(deftest dnog-open-constructed-collapses-to-definition
  (let ((from-cpl (find "IList<T>" (class-precedence-list (dnog-list-class "System.Int32"))
                        :key (lambda (c) (symbol-name (class-name c)))
                        :test #'string=))
        (from-name (dotnet:class-for-type "System.Collections.Generic.IList`1")))
    (eq from-cpl from-name))
  t)

;;; Non-generic types are unaffected.
(deftest dnog-non-generic-unaffected
  (let ((names (mapcar (lambda (c) (symbol-name (class-name c)))
                       (class-precedence-list (dotnet:class-for-type "System.Text.StringBuilder")))))
    (list (first names) (car (last names))))
  ("StringBuilder" "T"))
