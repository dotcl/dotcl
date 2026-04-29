;;; dotnet:define-class — user-facing macro wrapping DOTNET:%DEFINE-CLASS.
;;;
;;; Part of the defclass-cil roadmap:
;;;   D776 = Step 5a runtime primitive
;;;   D777 = Step 5b macro
;;;   D778 = Step 5c short-name resolution + (require :dotnet-class)
;;;   D783 = Step 7a (:ctor ...) option in macro
;;;   D785 = Step 7b (:properties ...) auto-properties
;;;   D786 = Step 7c :override keyword on method spec
;;;   D787 = Step 7d (:implements ...) for interface implementations
;;;   D788 = Step 7e (:events ...) for events (delegate field + add_/remove_)
;;;   D790 = Step 7g :notify keyword on property spec (auto-fires PropertyChanged)
;;;
;;; Loaded via `(require :dotnet-class)` (module-provide-contrib finds
;;; contrib/dotnet-class/dotnet-class.lisp) or explicit (load ...).
;;;
;;; Syntax:
;;;
;;;   (dotnet:define-class "Full.TypeName" (Base)
;;;     (:fields
;;;       ("FieldName" Int32)
;;;       ("OtherField" "System.String"))
;;;     (:attributes
;;;       ("System.ObsoleteAttribute" "message"))
;;;     (:ctor ()
;;;       ;; runs after base.ctor; `self' is bound to the new instance
;;;       body-forms...)
;;;     (:methods
;;;       ("MethodName" ((param Int32)) :returns Int32
;;;         body-forms...)
;;;       ("NoArgs" () :returns Void
;;;         body-forms...)
;;;       ("ToString" () :returns String :override t
;;;         body-forms...)))
;;;
;;; After `:returns TYPE' a method spec may carry `:override t` to emit the
;;; method as an override of a matching virtual method on the base class
;;; hierarchy.
;;;
;;; (:implements IFoo IBar ...) declares interface implementations. Any method
;;; in (:methods ...) whose name+signature matches an interface method is
;;; emitted as the implicit implementation of that slot. Interface type specs
;;; accept symbols (resolved via *type-aliases*) or strings (used verbatim).
;;;
;;; (:events ("Name" DelegateType) ...) emits each event as a private delegate
;;; field + public add_Name/remove_Name accessor pair + EventBuilder. When the
;;; type also implements a matching interface (e.g. INotifyPropertyChanged),
;;; the accessors are wired as implicit interface impls automatically.
;;;
;;; Property spec accepts optional :notify keyword after the type:
;;;   (:properties ("Title" String :notify t))
;;; When :notify is truthy, the setter calls OnPropertyChanged automatically
;;; after updating the backing field. Requires a PropertyChanged event to be
;;; declared via (:events ...).
;;;
;;; Type names are either strings (used verbatim) or symbols (looked up in
;;; DOTNET::*TYPE-ALIASES* — a hash-table keyed by symbol-name, containing
;;; common BCL short-names). Unknown symbols signal a compile-time error;
;;; users extend the table to add MAUI / ASP.NET / user types.
;;;
;;; Inside a :methods spec, param names are symbols (lexical vars in body),
;;; method name is a string, and the body is an implicit progn whose final
;;; value is converted to the declared return type (void discards it).

(export 'dotnet::define-class (find-package :dotnet))
(export 'dotnet::*type-aliases* (find-package :dotnet))

(defvar dotnet::*type-aliases*
  (let ((h (make-hash-table :test 'equal)))
    ;; BCL primitives
    (setf (gethash "OBJECT" h)   "System.Object")
    (setf (gethash "STRING" h)   "System.String")
    (setf (gethash "VOID" h)     "System.Void")
    (setf (gethash "BOOLEAN" h)  "System.Boolean")
    (setf (gethash "BOOL" h)     "System.Boolean")
    (setf (gethash "BYTE" h)     "System.Byte")
    (setf (gethash "SBYTE" h)    "System.SByte")
    (setf (gethash "CHAR" h)     "System.Char")
    (setf (gethash "INT16" h)    "System.Int16")
    (setf (gethash "UINT16" h)   "System.UInt16")
    (setf (gethash "INT32" h)    "System.Int32")
    (setf (gethash "UINT32" h)   "System.UInt32")
    (setf (gethash "INT64" h)    "System.Int64")
    (setf (gethash "UINT64" h)   "System.UInt64")
    (setf (gethash "INT" h)      "System.Int32")
    (setf (gethash "LONG" h)     "System.Int64")
    (setf (gethash "SINGLE" h)   "System.Single")
    (setf (gethash "FLOAT" h)    "System.Single")
    (setf (gethash "DOUBLE" h)   "System.Double")
    (setf (gethash "DECIMAL" h)  "System.Decimal")
    ;; Commonly referenced BCL types
    (setf (gethash "EXCEPTION" h)  "System.Exception")
    (setf (gethash "EVENTARGS" h)  "System.EventArgs")
    (setf (gethash "TYPE" h)       "System.Type")
    ;; Commonly implemented BCL interfaces
    (setf (gethash "IDISPOSABLE" h) "System.IDisposable")
    (setf (gethash "ICLONEABLE" h)  "System.ICloneable")
    (setf (gethash "IFORMATTABLE" h) "System.IFormattable")
    (setf (gethash "INOTIFYPROPERTYCHANGED" h)
          "System.ComponentModel.INotifyPropertyChanged")
    (setf (gethash "ICOMMAND" h) "System.Windows.Input.ICommand")
    ;; Commonly used delegate / event arg types
    (setf (gethash "EVENTHANDLER" h) "System.EventHandler")
    (setf (gethash "PROPERTYCHANGEDEVENTHANDLER" h)
          "System.ComponentModel.PropertyChangedEventHandler")
    (setf (gethash "PROPERTYCHANGEDEVENTARGS" h)
          "System.ComponentModel.PropertyChangedEventArgs")
    h)
  "Hash-table mapping symbol-name strings (upper-case) to fully qualified
   .NET type names. Used by DOTNET:DEFINE-CLASS to resolve symbol
   short-names. Users may extend with their own entries (e.g. MAUI types).")

(defun dotnet::%resolve-type (spec)
  "Resolve a type reference: string passes through unchanged; symbol is
   looked up in *TYPE-ALIASES* (keyed by symbol-name). Unknown symbol is
   a (hopefully compile-time) error."
  (cond
    ((stringp spec) spec)
    ((symbolp spec)
     (or (gethash (symbol-name spec) dotnet::*type-aliases*)
         (error "dotnet:define-class: unknown type short-name ~S.~%  ~
                 Register via (setf (gethash ~S dotnet::*type-aliases*) \"Namespace.Full.Name\") ~
                 or supply a full-name string."
                spec (symbol-name spec))))
    ((null spec) nil)
    (t (error "dotnet:define-class: type spec must be a symbol or string: ~S" spec))))

(defmacro dotnet:define-class (full-name supers &body options)
  (let* ((base-type-spec (first supers))
         (base-type (dotnet::%resolve-type base-type-spec))
         (fields-opt (cdr (assoc :fields options)))
         (attrs-opt  (cdr (assoc :attributes options)))
         (methods-opt (cdr (assoc :methods options)))
         (ctor-spec (cdr (assoc :ctor options)))
         (properties-opt (cdr (assoc :properties options)))
         (implements-opt (cdr (assoc :implements options)))
         (events-opt (cdr (assoc :events options))))
    ;; ctor-spec is of shape (() body...) — first element is currently
    ;; required to be the empty list (no user params). Parameterized ctors
    ;; are a future extension.
    (when (and ctor-spec (not (null (first ctor-spec))))
      (error "dotnet:define-class: (:ctor ...) currently only supports a zero-arg form; got ~S"
             (first ctor-spec)))
    `(dotnet:%define-class
      ,full-name
      ,base-type
      ,(if fields-opt
           `(list ,@(mapcar (lambda (f)
                              `(list ,(first f)
                                     ,(dotnet::%resolve-type (second f))))
                            fields-opt))
           'nil)
      ,(if attrs-opt
           `(list ,@(mapcar (lambda (a)
                              `(list ,@a))
                            attrs-opt))
           'nil)
      ,(if methods-opt
           `(list
             ,@(mapcar
                (lambda (m)
                  (destructuring-bind (name params &rest tail) m
                    (unless (eq (first tail) :returns)
                      (error "dotnet:define-class: method spec ~S must start with :returns after params" name))
                    (let ((return-type (second tail))
                          (override nil)
                          (method-attrs nil)
                          (body (cddr tail)))
                      ;; Optional keyword options between :returns and body.
                      (loop while (and body (keywordp (first body)))
                            do (case (first body)
                                 (:override (setf override (second body)))
                                 (:attributes (setf method-attrs (second body)))
                                 (otherwise
                                  (error "dotnet:define-class: unknown method option ~S in ~S"
                                         (first body) name)))
                               (setf body (cddr body)))
                      (let ((param-names (mapcar #'first params))
                            (param-types (mapcar (lambda (p) (dotnet::%resolve-type (second p)))
                                                 params)))
                        `(list ,name ,(dotnet::%resolve-type return-type)
                               (list ,@param-types)
                               (lambda (self ,@param-names)
                                 (declare (ignorable self))
                                 ,@body)
                               ,override
                               ,(if method-attrs
                                    `(list ,@(mapcar (lambda (a) `(list ,@a))
                                                     method-attrs))
                                    'nil))))))
                methods-opt))
           'nil)
      ,(if ctor-spec
           `(lambda (self)
              (declare (ignorable self))
              ,@(rest ctor-spec))
           'nil)
      ,(if properties-opt
           `(list ,@(mapcar
                     (lambda (p)
                       (destructuring-bind (pname ptype &rest tail) p
                         (let ((notify nil))
                           (loop while tail
                                 do (case (first tail)
                                      (:notify (setf notify (second tail)))
                                      (otherwise
                                       (error "dotnet:define-class: unknown property option ~S in ~S"
                                              (first tail) pname)))
                                    (setf tail (cddr tail)))
                           `(list ,pname
                                  ,(dotnet::%resolve-type ptype)
                                  ,notify))))
                     properties-opt))
           'nil)
      ,(if implements-opt
           `(list ,@(mapcar (lambda (i) (dotnet::%resolve-type i))
                            implements-opt))
           'nil)
      ,(if events-opt
           `(list ,@(mapcar (lambda (e)
                              `(list ,(first e)
                                     ,(dotnet::%resolve-type (second e))))
                            events-opt))
           'nil))))

;;; ---------------------------------------------------------------------------
;;; D892 — dotnet:ref: indexer sugar (get_Item / set_Item)
;;;
;;; (dotnet:ref obj key)           → (dotnet:invoke obj "get_Item" key)
;;; (setf (dotnet:ref obj key) val)→ (dotnet:invoke obj "set_Item" key val)
;;;
;;; Works with any .NET type that exposes an indexer (List<T>, Dictionary<K,V>,
;;; arrays via reflection, etc.).

(export 'dotnet::ref (find-package :dotnet))

(defun dotnet:ref (obj &rest keys)
  (apply #'dotnet:invoke obj "get_Item" keys))

(defsetf dotnet:ref (obj &rest keys) (val)
  `(dotnet:invoke ,obj "set_Item" ,@keys ,val))

;;; ---------------------------------------------------------------------------
;;; D893 — dotnet:using: IDisposable resource cleanup macro
;;;
;;; (dotnet:using ((var init-expr) ...) body...)
;;;
;;; Binds each VAR to INIT-EXPR in sequence and guarantees (dotnet:invoke var
;;; "Dispose") is called on exit — even if BODY signals an error.  Resources
;;; are disposed in innermost-first order, matching C# `using` semantics.

(export 'dotnet::using (find-package :dotnet))

(defmacro dotnet:using (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (let* ((binding (car bindings))
             (var (car binding))
             (expr (cadr binding)))
        `(let ((,var ,expr))
           (unwind-protect
             (dotnet:using ,(cdr bindings) ,@body)
             (dotnet:invoke ,var "Dispose"))))))

(provide :dotnet-class)
