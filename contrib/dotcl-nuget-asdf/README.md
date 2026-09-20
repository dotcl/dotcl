# dotcl-nuget-asdf

An ASDF component class that lets a system declare the NuGet packages it needs,
instead of calling `nuget:require` from somewhere in its own code. Loading the
system resolves the package and registers its assemblies, and `dotcl pack`
carries the resolved layout into the packages it builds.

    (defsystem "my-app"
      :defsystem-depends-on ("dotcl-nuget-asdf")
      :serial t
      :components ((:nuget "Newtonsoft.Json" :nuget-version "13.0.3")
                   (:file "app")))

Named in `:defsystem-depends-on` rather than required directly. The component
name is the package id, and the options are the ones `nuget:require` takes
(`:nuget-version`, `:source`, `:prerelease`, `:rid`, `:tfm`) -- note
`:nuget-version`, since ASDF takes `:version` for a component version of its
own. Depends on `asdf` and the `nuget` contrib. See
[docs/libraries.md](../../docs/libraries.md).
