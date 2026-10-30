#!/bin/sh
# Pack this checkout's DotCL.Sdk onto a local feed (out/dev-feed) and point
# each DIR at it with a global.json and a nuget.config, so a project there that
# says <Project Sdk="DotCL.Sdk"> builds with the SDK as it is in the tree.
#
# The version carries a hash of the SDK's files: NuGet never re-reads a version
# it has cached, so an edited SDK has to get a new one.
#
# Usage: dev-feed.sh <repo-root> DIR...
set -eu
ROOT="$(cd "$1" && pwd)"; shift
base="$(sed -n 's:.*<Version>\(.*\)</Version>.*:\1:p' "$ROOT/runtime/Directory.Build.props" | head -1)"
if command -v sha1sum >/dev/null 2>&1; then sha=sha1sum; else sha="shasum"; fi
hash="$(cat "$ROOT/runtime/sdk/Sdk.props" "$ROOT/runtime/sdk/Sdk.targets" "$ROOT/runtime/sdk/DotCL.Sdk.csproj" | $sha | cut -c1-12)"
version="$base-dev.h$hash"
feed="$ROOT/out/dev-feed"
mkdir -p "$feed"
if [ ! -f "$feed/DotCL.Sdk.$version.nupkg" ]; then
  dotnet pack "$ROOT/runtime/sdk/DotCL.Sdk.csproj" -c Release -o "$feed" -p:Version="$version" \
    --nologo -v q >/dev/null
fi
for dir in "$@"; do
  cat > "$dir/global.json" <<JSON
{
  "msbuild-sdks": {
    "DotCL.Sdk": "$version"
  }
}
JSON
  cat > "$dir/nuget.config" <<XML
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <add key="dotcl-dev-feed" value="$feed" />
  </packageSources>
</configuration>
XML
done
echo "DotCL.Sdk $version"
