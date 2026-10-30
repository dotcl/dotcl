;;; dotnet:define-class: user-facing macro wrapping DOTNET:%DEFINE-CLASS.
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
;;;       ("System.ObsoleteAttribute" "message" :diagnostic-id "X1"))
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
;;; An attribute spec is (type-name constructor-arg... :member value...).
;;; Arguments before the first keyword are the attribute constructor's; from the
;;; first keyword on, each pair sets a settable property or a public field, which
;;; is how most .NET attributes are configured. A member name matches
;;; case-insensitively with dashes removed, so :main-launcher, :mainlauncher and
;;; "MainLauncher" all reach MainLauncher. The same shape works in a method
;;; spec's :attributes option.
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
;;; DOTNET::*TYPE-ALIASES*: a hash-table keyed by symbol-name, containing
;;; common BCL short-names). Unknown symbols signal a compile-time error;
;;; users extend the table to add MAUI / ASP.NET / user types.
;;;
;;; Inside a :methods spec, param names are symbols (lexical vars in body),
;;; method name is a string, and the body is an implicit progn whose final
;;; value is converted to the declared return type (void discards it).

(export 'dotnet::define-class (find-package :dotnet))
(export 'dotnet::library (find-package :dotnet))
(export 'dotnet::register-class-handlers (find-package :dotnet))
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
    (setf (gethash "INT128" h)   "System.Int128")
    (setf (gethash "UINT128" h)  "System.UInt128")
    (setf (gethash "HALF" h)     "System.Half")
    (setf (gethash "SINGLE" h)   "System.Single")
    (setf (gethash "FLOAT" h)    "System.Single")
    (setf (gethash "DOUBLE" h)   "System.Double")
    (setf (gethash "DECIMAL" h)  "System.Decimal")
    (setf (gethash "BIGINTEGER" h) "System.Numerics.BigInteger")
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
   looked up in *TYPE-ALIASES* (keyed by symbol-name). Symbols that are
   registered CLOS classes (e.g. from a previous dotnet:define-class) also
   pass through to let C# resolve them in the dynamic assembly. Truly unknown
   symbols remain compile-time errors."
  (cond
    ((stringp spec) spec)
    ((symbolp spec)
     (or (gethash (symbol-name spec) dotnet::*type-aliases*)
         ;; Dynamically-defined classes registered in CLOS by a previous
         ;; dotnet:define-class: pass the symbol-name through for C# resolution.
         (and (find-class spec nil) (symbol-name spec))
         (error "dotnet:define-class: unknown type short-name ~S.~%  ~
                 Register via (setf (gethash ~S dotnet::*type-aliases*) \"Namespace.Full.Name\") ~
                 or supply a full-name string."
                spec (symbol-name spec))))
    ((null spec) nil)
    (t (error "dotnet:define-class: type spec must be a symbol or string: ~S" spec))))

(defun dotnet::%process-ctor-form (ctor-form &optional bake-index)
  "Process one (:ctor params body...) form and return a list-generating form
   suitable for the ctor-specs-list arg of dotnet:%define-class.
   Each result is (list lambda-or-nil param-types base-arg-indices).
   When the ctor has NO body forms (only an optional (:base ...)), the lambda
   slot is NIL: %define-class then emits a ctor that merely forwards to base and
   returns, with no Lisp dispatch. Such a ctor needs no runtime, so a type whose
   only members are base-forwarding ctors (e.g. a CL condition mapped to a .NET
   exception: (:class \"MyError\" (\"System.Exception\") (:ctor ((m String)) (:base m))))
   is STANDALONE; a C# consumer can throw/catch it with no DotCL.Runtime."
  (let* ((ctor-params (first ctor-form))
         (ctor-body-raw (rest ctor-form))
         ;; Extract optional (:base ...) leading form
         (base-form (when (and ctor-body-raw
                               (consp (first ctor-body-raw))
                               (eq (car (first ctor-body-raw)) :base))
                      (first ctor-body-raw)))
         (ctor-body (if base-form (rest ctor-body-raw) ctor-body-raw))
         (base-arg-names (if base-form (cdr base-form) nil))
         (ctor-param-names (mapcar #'first ctor-params))
         (ctor-param-types (mapcar (lambda (p) (dotnet::%resolve-type (second p)))
                                   ctor-params))
         (base-arg-indices (mapcar (lambda (n)
                                     (or (position n ctor-param-names :test #'eq)
                                         (error "dotnet:define-class: (:base ~S); ~S is not a ctor param" n n)))
                                   base-arg-names))
         ;; SELF must be interned in the CALLER's package -- the package the
         ;; method body's bare `self' is read in -- not this file's compile-time
         ;; package. *package* here is the caller's, since this runs at the
         ;; caller's macroexpansion time (robust even from a prebuilt fasl).
         (self-sym (intern "SELF" *package*)))
    (when (and bake-index ctor-body)
      (let ((fn (dotnet::%baked-name (format nil "NEW~D" bake-index))))
        (push `(defun ,fn (,self-sym ,@ctor-param-names)
                 (declare (ignorable ,self-sym))
                 ,@ctor-body)
              dotnet::*library-defuns*)
        (setf ctor-body `(quote ,fn))))
    `(list ,(cond
              ((and bake-index ctor-body) ctor-body)
              (ctor-body
               `(lambda (,self-sym ,@ctor-param-names)
                  (declare (ignorable ,self-sym))
                  ,@ctor-body))
              (t 'nil))  ; no body => base-forwarding-only ctor, no Lisp dispatch
           (list ,@ctor-param-types)
           (list ,@base-arg-indices))))

(defun dotnet::%param-name-string (sym)
  "Emitted .NET name for a parameter written as SYM.

   A symbol read the ordinary way arrives upcased, and .NET parameter names are
   conventionally lowercase, so N becomes \"n\". A symbol whose name already
   holds a lowercase letter was written with its case preserved, as
   |__originalMethod| is, and is taken verbatim: a caller that binds by name
   needs the exact spelling, and the case cannot be recovered once folded."
  (let ((s (symbol-name sym)))
    (if (some #'lower-case-p s) s (string-downcase s))))

(defvar dotnet::*library-core-functions* nil
  "The core functions the last dotnet:library build copied into its .dll.")

(defvar dotnet::*library-defuns* :none
  "Inside a dotnet:library expansion, the (defun ...) forms to compile into the
   library, most recent first. :NONE elsewhere.")

(defvar dotnet::*class-full-name* nil
  "Full name of the class whose options are being expanded.")

(defun dotnet::%baked-name (&rest parts)
  "A symbol, in the caller's package, for a function compiled into a library
   for a member of the current class."
  (let* ((full (or dotnet::*class-full-name* "Anon"))
         (dot (position #\. full :from-end t))
         (short (if dot (subseq full (1+ dot)) full)))
    (intern (format nil "~A~{.~A~}" (string-upcase short) parts) *package*)))

(defun dotnet::%pascal-case-name (sym)
  "FAST-FIB -> \"FastFib\"."
  (let ((out (make-string-output-stream)) (up t))
    (loop for ch across (symbol-name sym)
          do (cond ((char= ch #\-) (setf up t))
                   (up (write-char (char-upcase ch) out) (setf up nil))
                   (t (write-char (char-downcase ch) out))))
    (get-output-stream-string out)))

(defun dotnet::%method-spec-form (m &optional force-static)
  "Return a form producing one method-spec list for dotnet:%define-class /
   dotnet:%save-library. M is (name params &rest tail); tail begins with
   :returns TYPE then optional :override / :attributes / :static options, then
   the body forms. A static method (via :static t or FORCE-STATIC; the latter
   used by a library :module's :functions) is emitted with NO self parameter and
   sets the 7th static-flag element, so it becomes a `public static' member."
  (destructuring-bind (name params &rest tail) m
    (unless (eq (first tail) :returns)
      (error "dotnet: method spec ~S must start with :returns after params" name))
    (let ((return-type (second tail))
          (override nil)
          (method-attrs nil)
          (cs-name nil)
          (static force-static)
          (body (cddr tail))
          ;; SELF interned in the CALLER's package (see %process-ctor-form).
          (self-sym (intern "SELF" *package*)))
      ;; Optional keyword options between :returns and body.
      (loop while (and body (keywordp (first body)))
            do (case (first body)
                 (:override (setf override (second body)))
                 (:attributes (setf method-attrs (second body)))
                 (:static (setf static (second body)))
                 (:name (setf cs-name (second body)))
                 (otherwise
                  (error "dotnet: unknown method option ~S in ~S"
                         (first body) name)))
               (setf body (cddr body)))
      (let* ((param-names (mapcar #'first params))
             (param-types (mapcar (lambda (p) (dotnet::%resolve-type (second p)))
                                  params))
             ;; A symbol name is a function compiled into the library: the
             ;; method calls (NAME ...) directly, no Lisp needed at run time.
             (baked (symbolp name)))
        (when baked
          (when (eq dotnet::*library-defuns* :none)
            (error "dotnet: ~S: a function named by a symbol is compiled into a library, so it belongs in dotnet:library" name))
          (if static
              (push `(defun ,name ,param-names ,@body) dotnet::*library-defuns*)
              (let ((fn (dotnet::%baked-name (symbol-name name))))
                (push `(defun ,fn (,self-sym ,@param-names) (declare (ignorable ,self-sym)) ,@body)
                      dotnet::*library-defuns*)
                (setf cs-name (or cs-name (dotnet::%pascal-case-name name))
                      name fn))))
        `(list ,(if baked (or cs-name (dotnet::%pascal-case-name name)) name)
               ,(dotnet::%resolve-type return-type)
               (list ,@param-types)
               ,(cond
                  (baked `(quote ,name))
                  (static
                    ;; static -> no self (DispatchLispStatic funcalls with just
                    ;; the declared params).
                   `(lambda (,@param-names) ,@body))
                  (t
                   `(lambda (,self-sym ,@param-names)
                      (declare (ignorable ,self-sym))
                      ,@body)))
               ,override
               ,(if method-attrs
                    `(list ,@(mapcar (lambda (a) `(list ,@a)) method-attrs))
                    'nil)
               ,static
               (list ,@(mapcar (function dotnet::%param-name-string)
                               param-names)))))))

(defun dotnet::%baked-class-p (options)
  "True when a class in a dotnet:library has its bodies compiled into the
   library: it has a method named by a symbol, or says (:bake t)."
  (and (not (eq dotnet::*library-defuns* :none))
       (or (second (assoc :bake options))
           (some (lambda (m) (and (consp m) (symbolp (first m))))
                 (cdr (assoc :methods options))))))

(defun dotnet::%class-spec-args (full-name supers options)
  "Return a LIST of 12 forms = the positional dotnet:%define-class args 0-11
   (full-name .. ctor-specs) for a class/module described by SUPERS + OPTIONS.
   Shared by dotnet:define-class (single type) and dotnet:library (one entry
   per type). OPTIONS accepts the define-class option forms plus :functions
   (methods emitted as `public static', i.e. defun exports)."
  (let* ((dotnet::*class-full-name* full-name)
         (bake (dotnet::%baked-class-p options))
         (base-type (when supers (dotnet::%resolve-type (first supers))))
         (fields-opt (cdr (assoc :fields options)))
         (attrs-opt  (cdr (assoc :attributes options)))
         (methods-opt (cdr (assoc :methods options)))
         (functions-opt (cdr (assoc :functions options)))
         (ctor-forms (mapcar #'cdr
                             (remove-if-not (lambda (opt) (eq (car opt) :ctor))
                                            options)))
         (properties-opt (cdr (assoc :properties options)))
         (implements-opt (cdr (assoc :implements options)))
         (events-opt (cdr (assoc :events options))))
    (list
     full-name
     base-type
     (if fields-opt
         `(list ,@(mapcar (lambda (f)
                            `(list ,(first f)
                                   ,(dotnet::%resolve-type (second f))))
                          fields-opt))
         'nil)
     (if attrs-opt
         `(list ,@(mapcar (lambda (a) `(list ,@a)) attrs-opt))
         'nil)
     (if (or methods-opt functions-opt)
         `(list ,@(append
                   (mapcar (lambda (m) (dotnet::%method-spec-form m nil)) methods-opt)
                   (mapcar (lambda (m) (dotnet::%method-spec-form m t)) functions-opt)))
         'nil)
     'nil   ; arg 5: single ctor-body (unused; ctors go via arg 11)
     (if properties-opt
         `(list ,@(mapcar
                   (lambda (p)
                     (destructuring-bind (pname ptype &rest tail) p
                       (let ((notify nil))
                         (loop while tail
                               do (case (first tail)
                                    (:notify (setf notify (second tail)))
                                    (otherwise
                                     (error "dotnet: unknown property option ~S in ~S"
                                            (first tail) pname)))
                                  (setf tail (cddr tail)))
                         `(list ,pname
                                ,(dotnet::%resolve-type ptype)
                                ,notify))))
                   properties-opt))
         'nil)
     (if implements-opt
         `(list ,@(mapcar (lambda (i) (dotnet::%resolve-type i)) implements-opt))
         'nil)
     (if events-opt
         `(list ,@(mapcar (lambda (e)
                            `(list ,(first e)
                                   ,(dotnet::%resolve-type (second e))))
                          events-opt))
         'nil)
     'nil   ; arg 9: ctor-param-types (unused; ctors go via arg 11)
     'nil   ; arg 10: base-ctor-arg-indices (unused; ctors go via arg 11)
     ;; arg 11: ctor-specs-list: one entry per :ctor form
     (if ctor-forms
         (let ((i 0))
           `(list ,@(mapcar (lambda (c)
                              (dotnet::%process-ctor-form c (and bake (incf i))))
                            ctor-forms)))
         'nil))))

(defmacro dotnet:define-class (full-name supers &body options)
  `(dotnet:%define-class ,@(dotnet::%class-spec-args full-name supers options)))

;;; dotnet:register-class-handlers: the same syntax as define-class, but no type
;;; is defined. The method and ctor bodies are installed under the keys the
;;; type's IL dispatches with, which is what a facade saved by dotnet:library
;;; (emitted in another process, loaded here) needs to reach its Lisp bodies.
;;; Nothing is emitted, so this also works on the emit-free runtime, where
;;; define-class cannot run.
;;;
;;;   (dotnet:register-class-handlers "MyPack.Calculator" ()
;;;     (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b))))
;;;
;;; For a (:module ...) of a library, write its :functions option the same way.
(defmacro dotnet:register-class-handlers (full-name supers &body options)
  `(dotnet:%register-class-handlers
    ,@(dotnet::%class-spec-args full-name supers options)))

;;; dotnet:library: aggregate several types into ONE C#-referenceable .dll.
;;;
;;;   (dotnet:library ("MyPack" :version "1.2.3.0" :path "out/MyPack.dll")
;;;     (:class "MyPack.Calculator" ()
;;;       (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b))))
;;;     (:class "MyPack.Greeter" ()
;;;       (:methods ("Hello" ((who String)) :returns String
;;;         (concatenate 'string "Hi " who))))
;;;     (:module "MyPack.MathOps"       ; static-function holder
;;;       (:functions ("Square" ((x Int32)) :returns Int32 (* x x)))))
;;;
;;; Library spec = (assembly-name &key version path). ASSEMBLY-NAME is the
;;; simple name a C# consumer references; PATH defaults to "<assembly-name>.dll".
;;; Each member-form is one of the following. It expands to a tagged member-spec
;;; (KIND . rest) in dotnet:%save-library's single member-spec-list:
;;;   (:class full-name (supers) options...): same option syntax as
;;;      define-class; a facade type (methods dispatch to Lisp).
;;;   (:module full-name options...): base-less class whose :functions are
;;;      `public static' members. A :methods entry may also carry :static t.
;;;   (:enum full-name [:underlying Type] member...): a public enum. Each
;;;      member is "Name" (auto-incremented from 0, or one past the previous
;;;      explicit value) or ("Name" value). Enums are pure metadata: the
;;;      emitted type is STANDALONE (no DotCL.Runtime, no Lisp at runtime).
;;;   (:constants full-name ("Name" Type value)...): a static holder of public
;;;      const fields. Also standalone (const literals inline into the consumer).
;;;      Type must be a literal-capable primitive/string/enum.
;;;   (:struct full-name ("Field" Type)...): a public value type (struct) with
;;;      public fields. Standalone (a fields-only struct is pure data).
;;;   (:interface full-name ("Method" ((param Type)...) :returns Type)...); a
;;;      public interface of abstract method signatures. Standalone (pure
;;;      signature metadata a C# consumer references and implements).
;;;   (:delegate full-name ((param Type)...) :returns Type): a public delegate
;;;      (callback) type. Standalone (runtime-provided delegate machinery).
;;;
;;; Any member may carry a type-level doc summary: a (:doc "...") option for
;;; :class/:module, or a leading :doc "..." for the other kinds. It is written to
;;; a sidecar <assembly-name>.xml next to the .dll so a C# consumer's IntelliSense
;;; shows the summary.
;;;
;;; The :class/:module types are facades (bodies dispatch to their Lisp
;;; lambdas), so running THEIR methods needs DotCL.Runtime + the Lisp loaded.
;;; The exception is a :functions entry (or a :static method) named by a
;;; symbol, as in (fib ((n Int64)) :returns Int64 ...): its body is compiled
;;; into the .dll as (defun fib ...), and the C# method (Fib, or :name "...")
;;; calls it with only DotCL.Runtime present. (:defun name lambda-list body...)
;;; adds such a function without exporting it.
(defun dotnet::%enum-member-name (x)
  (if (stringp x) x (symbol-name x)))

;;; Every tagged member-spec is (KIND DOC . rest): DOC is a type-level <summary>
;;; string (or nil), written into the sidecar .xml so a C# consumer's IntelliSense
;;; shows it. A member-form carries it as a leading (:doc "...") option.
(defun dotnet::%peel-doc (rest)
  "If REST starts with :doc STRING, strip it: (values doc remaining)."
  (if (eq (first rest) :doc)
      (values (second rest) (cddr rest))
      (values nil rest)))

(defun dotnet::%enum-spec-form (eform)
  "Build a tagged enum member-spec (:enum doc full-name underlying (name value)...)."
  (destructuring-bind (kw full-name &rest rest) eform
    (declare (ignore kw))
    (let ((doc nil) (underlying "System.Int32"))
      (loop while (keywordp (first rest))
            do (case (first rest)
                 (:doc (setf doc (second rest)))
                 (:underlying (setf underlying (dotnet::%resolve-type (second rest))))
                 (otherwise (error "dotnet:library :enum ~S: unknown option ~S"
                                   full-name (first rest))))
               (setf rest (cddr rest)))
      (let ((auto 0))
        `(list :enum ,doc ,full-name ,underlying
               ,@(mapcar
                  (lambda (m)
                    (if (consp m)
                        (destructuring-bind (name val) m
                          (when (integerp val) (setf auto (1+ val)))
                          `(list ,(dotnet::%enum-member-name name) ,val))
                        (prog1 `(list ,(dotnet::%enum-member-name m) ,auto)
                          (incf auto))))
                  rest))))))

(defun dotnet::%constants-spec-form (cform)
  "Build a tagged constants member-spec (:constants doc full-name (name type value)...)."
  (destructuring-bind (kw full-name &rest rest) cform
    (declare (ignore kw))
    (multiple-value-bind (doc members) (dotnet::%peel-doc rest)
      `(list :constants ,doc ,full-name
             ,@(mapcar
                (lambda (m)
                  (destructuring-bind (name type value) m
                    `(list ,(dotnet::%enum-member-name name)
                           ,(dotnet::%resolve-type type)
                           ,value)))
                members)))))

(defun dotnet::%struct-spec-form (sform)
  "Build a tagged struct member-spec (:struct doc full-name (field type)...)."
  (destructuring-bind (kw full-name &rest rest) sform
    (declare (ignore kw))
    (multiple-value-bind (doc fields) (dotnet::%peel-doc rest)
      `(list :struct ,doc ,full-name
             ,@(mapcar
                (lambda (f)
                  (destructuring-bind (name type) f
                    `(list ,(dotnet::%enum-member-name name)
                           ,(dotnet::%resolve-type type))))
                fields)))))

(defun dotnet::%interface-spec-form (iform)
  "Build a tagged interface member-spec
   (:interface doc full-name (method-name return-type (param-types))...)."
  (destructuring-bind (kw full-name &rest rest) iform
    (declare (ignore kw))
    (multiple-value-bind (doc methods) (dotnet::%peel-doc rest)
      `(list :interface ,doc ,full-name
             ,@(mapcar
                (lambda (m)
                  (destructuring-bind (name params &rest tail) m
                    (unless (eq (first tail) :returns)
                      (error "dotnet:library :interface method ~S must carry :returns after its params" name))
                    (let ((return-type (second tail))
                          (param-types (mapcar (lambda (p) (dotnet::%resolve-type (second p)))
                                               params)))
                      `(list ,name ,(dotnet::%resolve-type return-type)
                             (list ,@param-types)))))
                methods)))))

(defun dotnet::%delegate-spec-form (dform)
  "Build a tagged delegate member-spec (:delegate doc full-name return-type (param-types))."
  (destructuring-bind (kw full-name &rest rest) dform
    (declare (ignore kw))
    (multiple-value-bind (doc rest) (dotnet::%peel-doc rest)
      (destructuring-bind (params &rest tail) rest
        (unless (eq (first tail) :returns)
          (error "dotnet:library :delegate ~S must carry :returns after its params" full-name))
        (let ((return-type (second tail))
              (param-types (mapcar (lambda (p) (dotnet::%resolve-type (second p))) params)))
          `(list :delegate ,doc ,full-name ,(dotnet::%resolve-type return-type)
                 (list ,@param-types)))))))

(defun dotnet::%member-spec-form (m)
  "Expand one dotnet:library member-form into a tagged (KIND DOC . rest) spec form."
  (ecase (first m)
    (:class
     (destructuring-bind (kw full-name supers &rest opts) m
       (declare (ignore kw))
       `(list :class ,(cadr (assoc :doc opts)) ,@(dotnet::%class-spec-args full-name supers opts))))
    (:module
     (destructuring-bind (kw full-name &rest opts) m
       (declare (ignore kw))
       `(list :class ,(cadr (assoc :doc opts)) ,@(dotnet::%class-spec-args full-name '() opts))))
    (:defun
     (when (eq dotnet::*library-defuns* :none)
       (error "dotnet: (:defun ~S ...) belongs in dotnet:library" (second m)))
     (push `(defun ,@(rest m)) dotnet::*library-defuns*)
     nil)
    (:enum       (dotnet::%enum-spec-form m))
    (:constants  (dotnet::%constants-spec-form m))
    (:struct     (dotnet::%struct-spec-form m))
    (:interface  (dotnet::%interface-spec-form m))
    (:delegate   (dotnet::%delegate-spec-form m))))

(export 'dotnet::pack-library (find-package :dotnet))

(defun dotnet::pack-library (dll &key id version authors description output)
  "Write a NuGet package for the library DLL: the DLL, its XML doc, and a
   dependency on the DotCL.Runtime of this process. Returns the .nupkg path."
  (dotnet:%pack-library (namestring dll) id version authors description
                        (and output (namestring output))))

(defvar dotnet::*library-output* nil
  "When non-NIL, a dotnet:library form is being built by dotcl:build-library:
   the path every dotnet:library form writes its DLL to instead of its :path,
   or :OWN to keep each form's :path.")

(defvar dotnet::*library-written* nil
  "Inside dotcl:build-library, (assembly-name . path) of the library written.")

(defvar dotnet::*library-source-files* nil
  "Inside dotcl:build-library, the source files (truenames as strings) whose
   functions a library body may reach: each one it reaches, and each one those
   reach, is compiled into the library from its source.")

(defvar dotnet::*library-source-functions* nil
  "The functions of the source files the last dotnet:library build compiled
   into its .dll.")

(defun dotnet::%library-path (path asm-name)
  (let ((out dotnet::*library-output*))
    (cond ((null out) path)
          (dotnet::*library-written*
           (error "dotnet:library ~S: ~A already defines the library written to ~A; one build writes one library"
                  asm-name (car dotnet::*library-written*) (cdr dotnet::*library-written*)))
          ((and (not (eq out :own)) (string/= asm-name (pathname-name out)))
           (error "dotnet:library ~S is being written to ~A: the assembly name has to match the file name (rename the library, or set the project's AssemblyName to ~S)"
                  asm-name (file-namestring out) asm-name))
          (t (let ((written (if (eq out :own) path out)))
               (setf dotnet::*library-written* (cons asm-name written))
               written)))))

(defun dotnet::%source-truename (f)
  (namestring (truename f)))

(defun dotnet::%build-library (files output &optional source-files)
  "Load FILES in order with every dotnet:library form writing to OUTPUT (to its
   own :path when OUTPUT is NIL). A function defined in FILES or SOURCE-FILES
   that a library body reaches is compiled into the library. Returns the path
   written."
  (let ((dotnet::*library-output* (or output :own))
        (dotnet::*library-written* nil)
        (dotnet::*library-source-files*
          (remove-duplicates (mapcar #'dotnet::%source-truename (append files source-files))
                             :test #'string= :from-end t)))
    (dolist (f files)
      (load f))
    (unless dotnet::*library-written*
      (error "No dotnet:library form was evaluated while loading ~{~A~^, ~}; nothing was written~@[ to ~A~]"
             files output))
    (cdr dotnet::*library-written*)))

(defmacro dotnet:library (spec &body members)
  (destructuring-bind (asm-name &key version
                                (path (concatenate 'string asm-name ".dll"))
                                package package-id package-version authors description)
      spec
    (let* ((dotnet::*library-defuns* '())
           (specs (remove nil (mapcar #'dotnet::%member-spec-form members)))
           (out (gensym "PATH"))
           (save `(dotnet:%save-library
                   ,out ,asm-name ,version
                   (list ,@specs
                         ,@(when dotnet::*library-defuns*
                             `((list :bake nil ,@(mapcar (lambda (d) `(quote ,d))
                                                         (reverse dotnet::*library-defuns*)))))))))
      `(let ((,out (dotnet::%library-path ,path ,asm-name)))
         ,(if package
              `(progn ,save
                      (dotnet::pack-library ,out
                                            :id ,(or package-id asm-name)
                                            :version ,package-version
                                            :authors ,authors :description ,description
                                            :output ,(and (stringp package) package)))
              save)))))

;;; ---------------------------------------------------------------------------
;;; dotnet:ref: indexer sugar (get_Item / set_Item)
;;;
;;; (dotnet:ref obj key)           -> (dotnet:invoke obj "get_Item" key)
;;; (setf (dotnet:ref obj key) val)-> (dotnet:invoke obj "set_Item" key val)
;;;
;;; Works with any .NET type that exposes an indexer (List<T>, Dictionary<K,V>,
;;; arrays via reflection, etc.).

(export 'dotnet::ref (find-package :dotnet))

(defun dotnet::ref (obj &rest keys)
  (apply #'dotnet:invoke obj "get_Item" keys))

(defsetf dotnet::ref (obj &rest keys) (val)
  `(progn (dotnet:invoke ,obj "set_Item" ,@keys ,val)
          ,val))

;;; ---------------------------------------------------------------------------
;;; dotnet:using: IDisposable resource cleanup macro
;;;
;;; (dotnet:using ((var init-expr) ...) body...)
;;;
;;; Binds each VAR to INIT-EXPR in sequence and guarantees (dotnet:invoke var
;;; "Dispose") is called on exit: even if BODY signals an error.  Resources
;;; are disposed in innermost-first order, matching C# `using` semantics.

(export 'dotnet::using (find-package :dotnet))

(defmacro dotnet::using (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (let* ((binding (car bindings))
             (var (car binding))
             (expr (cadr binding)))
        `(let ((,var ,expr))
           (unwind-protect
             (dotnet::using ,(cdr bindings) ,@body)
             (dotnet:invoke ,var "Dispose"))))))

(provide :dotnet-class)

;;; ---------------------------------------------------------------------------
;;; dotnet:deref: the cell a `ref' / `out' parameter arrives as
;;;
;;; (dotnet:deref cell)            -> the caller's current value
;;; (setf (dotnet:deref cell) val) -> what the caller reads after the call
;;;
;;; A Lisp function is called with values, so a parameter the caller expects to
;;; be written back cannot be an ordinary argument. A method declared with a
;;; byref parameter type ("System.Object&") receives a DotCL.Emitter.ByRef cell
;;; instead, and whatever the body leaves in it is copied back on return.
;;; Leaving it alone leaves the caller's value alone.

(export 'dotnet::deref (find-package :dotnet))

(defun dotnet::deref (cell)
  (dotnet:invoke cell "Value"))

(defsetf dotnet::deref (cell) (val)
  `(progn (dotnet:%set-invoke ,cell "Value" ,val) ,val))
