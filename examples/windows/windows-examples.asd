(defsystem "windows-examples"
  :description "dotcl Windows interop demos — registry, WinForms, WMI, COM"
  :components ((:file "package")
               (:file "dotnet-interop-examples" :depends-on ("package"))
               (:file "winforms-demo"           :depends-on ("package"))))
