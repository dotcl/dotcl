;;; dotnet-library.lisp -- build an ASDF system into a .NET library DLL.
;;;
;;;   (defsystem "greeter"
;;;     :defsystem-depends-on ("dotnet-library")
;;;     :build-operation "dotnet-library-op"
;;;     :build-pathname "bin/GreeterLib"          ; optional: writes bin/GreeterLib.dll
;;;     :depends-on ("text-util")
;;;     :serial t
;;;     :components ((:file "package")
;;;                  (:file "greet")
;;;                  (:dotnet-library "library")))
;;;
;;;   (asdf:make "greeter")
;;;
;;; library.lisp holds the dotnet:library form, the face C# sees. Loading the
;;; system does not load it, so (asdf:load-system "greeter") does not write a
;;; DLL; (asdf:make "greeter") loads the system and then that file, and every
;;; function of the system or of a system it depends on that the library's
;;; bodies reach is compiled into the DLL from its source.
;;;
;;; The same build is (dotcl:build-library :system "greeter"), `dotcl
;;; build-library --system greeter.asd` and the SDK's <LispSystem> item.

(require "asdf")
(require "dotnet-class")

(defpackage :dotnet-library
  (:use :cl)
  (:export #:library-op #:library-file
           #:system-library-files #:system-source-files #:build-system))

(in-package :dotnet-library)

(defclass library-file (asdf:source-file)
  ((type :initform "lisp"))
  (:documentation
   "A file holding a system's dotnet:library form. LIBRARY-OP loads it after the
system; loading or compiling the system leaves it alone."))

;;; ASDF resolves a component type (:dotnet-library) and a :build-operation
;;; string by name in its own package.
(setf (find-class 'asdf::dotnet-library) (find-class 'library-file))

(defmethod asdf:perform ((o asdf:compile-op) (c library-file)) nil)
(defmethod asdf:perform ((o asdf:load-op) (c library-file)) nil)
(defmethod asdf:output-files ((o asdf:compile-op) (c library-file)) nil)

(defclass library-op (asdf:non-propagating-operation) ()
  (:documentation
   "Write the .NET library a system's (:dotnet-library ...) component defines.
The system's :build-pathname, if any, names the DLL (its type is set to dll);
otherwise the dotnet:library form's own :path does."))

(setf (find-class 'asdf::dotnet-library-op) (find-class 'library-op))

(defvar *output* nil
  "When non-NIL, where LIBRARY-OP writes the DLL, over the system's own choice.")

(defvar *written* nil
  "The path the last LIBRARY-OP wrote.")

(defmethod asdf:component-depends-on ((o library-op) (s asdf:system))
  `((asdf:load-op ,s) ,@(call-next-method)))

(defmethod asdf:operation-done-p ((o library-op) (c asdf:component))
  nil)

(defmethod asdf:output-files ((o library-op) (s asdf:system))
  (let ((bp (asdf/system:component-build-pathname s)))
    (values (when bp
              (list (make-pathname :type "dll"
                                   :defaults (merge-pathnames bp (asdf:component-pathname s)))))
            t)))

(defmethod asdf:perform ((o library-op) (c asdf:component)) nil)

(defmethod asdf:perform ((o library-op) (s asdf:system))
  (let ((files (system-library-files s))
        (out (or *output* (first (asdf:output-files o s)))))
    (unless files
      (error "System ~A has no (:dotnet-library \"file\") component: that file holds the ~
dotnet:library form LIBRARY-OP builds" (asdf:component-name s)))
    (when out (ensure-directories-exist out))
    (setf *written*
          (dotnet::%build-library (mapcar #'namestring files)
                                  (and out (namestring out))
                                  (system-source-files s)))))

(defun system-library-files (system)
  "The files of SYSTEM's (:dotnet-library ...) components, in order."
  (let ((out '()))
    (labels ((walk (c)
               (typecase c
                 (library-file (push (asdf:component-pathname c) out))
                 (asdf:parent-component (mapc #'walk (asdf:component-children c))))))
      (walk (asdf:find-system system)))
    (nreverse out)))

(defun system-source-files (system)
  "The Lisp source files of SYSTEM and of every system it depends on, as
strings: the files whose functions a library built from SYSTEM may compile in."
  (loop for c in (asdf:required-components (asdf:find-system system) :other-systems t)
        when (typep c 'asdf:cl-source-file)
          collect (namestring (asdf:component-pathname c))))

(defun build-system (system &key output)
  "Write the .NET library SYSTEM defines (LIBRARY-OP), to OUTPUT when given.
Returns the path written."
  (let ((*output* output) (*written* nil))
    (asdf:operate 'library-op system)
    *written*))

(provide "dotnet-library")
