#!/bin/sh
# ci/prune-nuget.cs on a fake package cache: of each microsoft.* package it keeps the newest version
# of each major line (a prerelease sorts before its release), removes the rest, and leaves other
# packages alone. --dry lists what would go and removes nothing.
set -eu
ROOT="$(cd "${1%/}" && pwd)"
T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
for p in microsoft.netcore.app.runtime.osx-arm64/10.0.1 microsoft.netcore.app.runtime.osx-arm64/10.0.7 \
         microsoft.netcore.app.runtime.osx-arm64/10.0.12 microsoft.netcore.app.runtime.osx-arm64/9.0.3 \
         microsoft.netcore.app.runtime.osx-arm64/9.0.12 microsoft.netcore.app.runtime.osx-arm64/9.0.12-rc.1 \
         microsoft.netcore.app.runtime.osx-arm64/8.0.2 microsoft.aspnetcore.app.runtime.linux-x64/10.0.9 \
         microsoft.aspnetcore.app.runtime.linux-x64/10.0.10 newtonsoft.json/13.0.1 newtonsoft.json/12.0.3; do
    mkdir -p "$T/$p"; echo x > "$T/$p/data.bin"
done
expected_removed='microsoft.aspnetcore.app.runtime.linux-x64/10.0.9
microsoft.netcore.app.runtime.osx-arm64/10.0.1
microsoft.netcore.app.runtime.osx-arm64/10.0.7
microsoft.netcore.app.runtime.osx-arm64/9.0.12-rc.1
microsoft.netcore.app.runtime.osx-arm64/9.0.3'

listed=$(NUGET_PACKAGES="$T" dotnet run "$ROOT/ci/prune-nuget.cs" -- --dry | sed -n 's/^would remove \([^ ]*\) .*/\1/p' | sort)
[ "$listed" = "$expected_removed" ] || { echo "prune-nuget: --dry lists:"; echo "$listed"; exit 1; }
[ -d "$T/microsoft.netcore.app.runtime.osx-arm64/10.0.1" ] || { echo "prune-nuget: --dry removed something"; exit 1; }

NUGET_PACKAGES="$T" dotnet run "$ROOT/ci/prune-nuget.cs" > /dev/null
for gone in $expected_removed; do
    [ ! -d "$T/$gone" ] || { echo "prune-nuget: $gone is still there"; exit 1; }
done
for kept in microsoft.netcore.app.runtime.osx-arm64/10.0.12 microsoft.netcore.app.runtime.osx-arm64/9.0.12 \
            microsoft.netcore.app.runtime.osx-arm64/8.0.2 microsoft.aspnetcore.app.runtime.linux-x64/10.0.10 \
            newtonsoft.json/13.0.1 newtonsoft.json/12.0.3; do
    [ -d "$T/$kept" ] || { echo "prune-nuget: $kept was removed"; exit 1; }
done
echo "prune-nuget: ok"
