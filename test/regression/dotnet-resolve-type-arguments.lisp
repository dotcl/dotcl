;;; Type arguments of a generic type name are resolved by the same search as the
;;; outer name, so a type argument outside corelib is found without spelling out
;;; its assembly.

(defun drta-name (name)
  (dotnet:invoke (dotnet:resolve-type name) "ToString"))

(deftest drta-generic-argument-outside-corelib
  (drta-name "System.Action`1[System.Reflection.Metadata.MetadataReader]")
  "System.Action`1[System.Reflection.Metadata.MetadataReader]")

(deftest drta-nested-generic-with-spaces
  (drta-name "System.Collections.Generic.Dictionary`2[System.String, System.Collections.Generic.List`1[System.Reflection.Metadata.MetadataReader]]")
  "System.Collections.Generic.Dictionary`2[System.String,System.Collections.Generic.List`1[System.Reflection.Metadata.MetadataReader]]")

(deftest drta-array-argument-and-array-of-generic
  (list (drta-name "System.Action`1[System.Reflection.Metadata.MetadataReader[]]")
        (drta-name "System.Collections.Generic.List`1[System.Reflection.Metadata.MetadataReader][]")
        (drta-name "System.Collections.Generic.List`1[System.Reflection.Metadata.MetadataReader][,]"))
  ("System.Action`1[System.Reflection.Metadata.MetadataReader[]]"
   "System.Collections.Generic.List`1[System.Reflection.Metadata.MetadataReader][]"
   "System.Collections.Generic.List`1[System.Reflection.Metadata.MetadataReader][,]"))

(deftest drta-nested-type-argument
  (drta-name "System.Collections.Generic.List`1[System.Environment+SpecialFolder]")
  "System.Collections.Generic.List`1[System.Environment+SpecialFolder]")

(deftest drta-assembly-qualified-argument
  (drta-name "System.Collections.Generic.List`1[[System.Reflection.Metadata.MetadataReader, System.Reflection.Metadata]]")
  "System.Collections.Generic.List`1[System.Reflection.Metadata.MetadataReader]")

(deftest drta-unknown-argument-still-errors
  (handler-case (progn (dotnet:resolve-type "System.Action`1[No.Such.Type]") :resolved)
    (error () :error))
  :error)

(deftest drta-runtime-type-argument
  (drta-name "System.Action`1[DotCL.LispObject]")
  "System.Action`1[DotCL.LispObject]")
