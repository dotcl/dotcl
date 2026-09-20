# advice

CLOS-style advice on any .NET method in the running process, attached and
removed without a restart. `advice:watch` runs a Lisp closure after each call to
the target and hands it the instance (NIL for a static method), the argument
list and the return value; `advice:patch` may rewrite the result, `advice:trace`
times calls, and `unwatch` / `unpatch` / `untrace` take them off again.

    (require "dotcl-advice")

Depends on the `nuget` and `dotnet-class` contribs. The patching backend is the
HarmonyX NuGet package, resolved on first use -- so that first call wants the
.NET SDK unless the package is already laid out beside the program.
`advice:*harmony-package*` and `*harmony-version*` choose it (HarmonyX rather
than Lib.Harmony because Lib.Harmony's bundled MonoMod does not detour on
arm64/.NET 10).
