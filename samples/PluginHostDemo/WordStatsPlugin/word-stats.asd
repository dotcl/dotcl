(defsystem "word-stats"
  :defsystem-depends-on ("dotnet-library")
  :build-operation "dotnet-library-op"
  :depends-on ("text-util")
  :serial t
  :components ((:file "package")
               (:file "stats")
               (:dotnet-library "plugin")))
