#!/usr/bin/env sh
# Generate the three managed plugins Unity IL2CPP links into the WebGL build:
#   Assets/Plugins/DotCL.Runtime.dll  — emit-free, JSON-free netstandard2.0 runtime
#   Assets/Plugins/dotclcore.dll      — runtime core (stable-named, netstandard-retargeted fasl)
#   Assets/Plugins/appfasl.dll        — this app's Lisp (stable-named, netstandard-retargeted fasl)
# Cross-platform (only needs the dotnet SDK); build.sh runs this before Unity.
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"

# 1) Build the emit-free, JSON-free netstandard2.0 runtime and stage it as a plugin.
#    DotclNoJson drops dotnet:require + its System.Text.Json dependency, which is
#    absent from Unity's IL2CPP/WebGL BCL.
dotnet build "$HERE/../../runtime/DotCL.Runtime.csproj" -c Release -f netstandard2.0 -p:DotclNoJson=true
cp "$HERE/../../runtime/bin/Release/netstandard2.0/DotCL.Runtime.dll" "$HERE/Assets/Plugins/DotCL.Runtime.dll"

# 2) Precompile app.lisp + the dev SIL core into stable-named, netstandard-retargeted
#    fasls under Assets/Plugins (paths in precompile.lisp are relative to this dir).
cd "$HERE"
dotnet run --project "$HERE/../../runtime/runtime.csproj" -- \
  --asm "$HERE/../../compiler/cil-out.sil" precompile.lisp
