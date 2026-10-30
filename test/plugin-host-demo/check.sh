#!/bin/sh
# Build samples/PluginHostDemo with `dotnet build` (a C# host that loads plugin
# DLLs from a folder, one plugin in C#, two in Common Lisp built by DotCL.Sdk:
# one from a <LispLibrary> file, one from a <LispSystem> ASDF system that
# depends on another system) and check that the host runs all three, that the
# Lisp plugins work with no core loaded, and that an unchanged rebuild does not
# rebuild a Lisp library. Then the same libraries through `dotcl build-library`.
#
# Usage: check.sh <repo-root>
set -eu
ROOT="$(cd "${1%/}" && pwd)"
SAMPLE="$ROOT/samples/PluginHostDemo"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

[ -f "$ROOT/compiler/dotcl.core" ] || { echo "no compiler/dotcl.core (run 'make compile-core-fasl' first)"; exit 1; }

sh "$ROOT/runtime/sdk/dev-feed.sh" "$ROOT" "$SAMPLE" >"$WORK/feed.log" 2>&1 \
  || { cat "$WORK/feed.log"; echo "plugin-host-demo: FAIL (dev feed)"; exit 1; }
dotnet build "$SAMPLE/PluginHost/PluginHost.csproj" -c Release --nologo >"$WORK/build.log" 2>&1 </dev/null \
  || { tail -20 "$WORK/build.log"; echo "plugin-host-demo: FAIL (build)"; exit 1; }
HOST="$SAMPLE/PluginHost/bin/Release/net10.0"

# Run from an empty directory so nothing can be found by relative path.
mkdir "$WORK/run"
(cd "$WORK/run" && dotnet "$HOST/PluginHost.dll") >"$WORK/out.txt" 2>&1 </dev/null
cat "$WORK/out.txt"

fail=0
expect() { grep -qF "$1" "$WORK/out.txt" || { echo "  FAIL: missing: $1"; fail=1; }; }
expect '[CSharpPlugin.dll] upper (C#): HELLO PLUGIN WORLD'
expect '[LispPlugin.dll] reverse words (Common Lisp): olleh nigulp dlrow [3 words]'
expect '  progress: upper-casing'
expect '  progress: plugin'
expect '  completed: olleh nigulp dlrow [3 words]'
expect '[WordStatsPlugin.dll] word stats (Common Lisp, ASDF system): 3 words, longest "plugin", Hello Plugin World'
expect '  progress: 3 words'
expect '  completed: 3 words, longest "plugin", Hello Plugin World'
expect 'DotclHost.CoreLoaded = False'
[ -f "$HOST/plugins/LispPlugin/DotCL.Runtime.dll" ] || { echo "  FAIL: DotCL.Runtime.dll is not beside the Lisp plugin"; fail=1; }
[ -e "$SAMPLE/LispPlugin/bin/Release/net10.0/dotcl.core" ] && { echo "  FAIL: a core was shipped"; fail=1; }
[ -e "$SAMPLE/LispPlugin/bin/Release/net10.0/contrib" ] && { echo "  FAIL: the contrib tree was shipped"; fail=1; }

lib="$SAMPLE/LispPlugin/obj/Release/net10.0/LispPlugin.dll"
stamp="$WORK/stamp"; touch "$stamp"; sleep 1
dotnet build "$SAMPLE/PluginHost/PluginHost.csproj" -c Release --nologo >"$WORK/build2.log" 2>&1 </dev/null \
  || { tail -20 "$WORK/build2.log"; echo "  FAIL: the second build failed"; fail=1; }
[ -n "$(find "$lib" -newer "$stamp")" ] && { echo "  FAIL: the unchanged Lisp library was rebuilt"; fail=1; }
slib="$SAMPLE/WordStatsPlugin/obj/Release/net10.0/WordStatsPlugin.dll"
[ -n "$(find "$slib" -newer "$stamp")" ] && { echo "  FAIL: the unchanged Lisp system library was rebuilt"; fail=1; }

# The CLI entry: the same library from the same file.
dotnet run --project "$ROOT/runtime/runtime.csproj" -c Release --no-launch-profile -- \
  --core "$ROOT/compiler/dotcl.core" build-library "$SAMPLE/LispPlugin/plugin.lisp" \
  --reference "$SAMPLE/PluginHost.Abstractions/bin/Release/netstandard2.0/PluginHost.Abstractions.dll" \
  --output "$WORK/cli/LispPlugin.dll" >"$WORK/cli.log" 2>&1 </dev/null \
  || { tail -20 "$WORK/cli.log"; echo "  FAIL: dotcl build-library"; fail=1; }
[ -s "$WORK/cli/LispPlugin.dll" ] || { echo "  FAIL: dotcl build-library wrote nothing"; fail=1; }
dotnet run --project "$ROOT/runtime/runtime.csproj" -c Release --no-launch-profile -- \
  --core "$ROOT/compiler/dotcl.core" build-library --system "$SAMPLE/WordStatsPlugin/word-stats.asd" \
  --search-path "$SAMPLE/text-util" \
  --reference "$SAMPLE/PluginHost.Abstractions/bin/Release/netstandard2.0/PluginHost.Abstractions.dll" \
  --output "$WORK/cli/WordStatsPlugin.dll" >"$WORK/cli2.log" 2>&1 </dev/null \
  || { tail -20 "$WORK/cli2.log"; echo "  FAIL: dotcl build-library --system"; fail=1; }
[ -s "$WORK/cli/WordStatsPlugin.dll" ] || { echo "  FAIL: dotcl build-library --system wrote nothing"; fail=1; }

if [ "$fail" = 0 ]; then echo "plugin-host-demo: OK"; else echo "plugin-host-demo: FAIL"; exit 1; fi
