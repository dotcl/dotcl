# nuget

Resolve a NuGet package and its transitive dependencies and register the result
with dotcl's assembly and native-library resolver, after which the types are
visible to `dotnet:`. `nuget:require` is the entry point and resolves a given
package once per session; `nuget:resolve` always does the work and returns the
counts and the directory it laid out.

    (require "dotcl-nuget")
    (nuget:require "Newtonsoft.Json" :version "13.0.3")

Resolution runs `dotnet build` on a throwaway project, so it wants the .NET SDK
and, the first time, the network. An exact version is kept under
`nuget:cache-root` and reused by later processes; a floating version is resolved
afresh every time, since what it means can change. A packaged application looks
first in `nuget:bundled-root`, the layout shipped beside the executable, and so
needs neither the SDK nor the network.
