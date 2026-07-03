(defsystem "nuget"
  :description "Resolve a NuGet package and its transitive dependencies (managed + RID-specific native) and register them with the dotcl assembly/native resolver."
  :version "1.0"
  :components ((:file "nuget")))
