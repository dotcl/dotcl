# nuget

Resolve a NuGet package and its transitive dependencies and register the result
with dotcl's assembly and native-library resolver, after which the types are
visible to `dotnet:`. `nuget:require` is the entry point and resolves a given
package once per session; `nuget:resolve` always does the work and returns the
counts and the directory it laid out.

    (require "dotcl-nuget")
    (nuget:require "Newtonsoft.Json" :version "13.0.3")

Resolution runs `dotnet` on throwaway projects, so it wants the .NET SDK and,
the first time, the network: a restore decides the versions and a build lays
them out for the platform. Laid-out versions are kept under `nuget:cache-root`
and reused by later processes. Everything one image asks for is resolved as one
set, and a version once registered stays. A packaged application looks first in
`nuget:bundled-root`, the layout shipped beside the executable, and so needs
neither the SDK nor the network.

Packages a system declares (see `dotcl-nuget-asdf`) follow the project's lock
file, `dotcl-nuget.lock.json` in `nuget:*project-directory*`; `nuget:restore`
writes it. `DOTCL_NUGET_OFFLINE=1` forbids the network.
