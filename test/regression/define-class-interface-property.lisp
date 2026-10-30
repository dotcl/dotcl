;;; A property whose name and type match an interface property is emitted as that
;;; interface's accessor pair, so the type loads and the interface can be used.

(load "contrib/dotnet-class/dotnet-class.lisp")

(dotnet:define-class "DcIfaceProp.Box" ()
  (:implements "System.Runtime.CompilerServices.IStrongBox")
  (:properties ("Value" Object)))

(deftest define-class-interface-property-implements
  (let* ((o (dotnet:new "DcIfaceProp.Box"))
         (ifs (dotnet:invoke (dotnet:invoke o "GetType") "GetInterfaces")))
    (setf (dotnet:invoke o "Value") 41)
    (list (dotnet:invoke ifs "Length") (dotnet:invoke o "Value")))
  (1 41))
