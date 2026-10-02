#!/bin/sh
# tool-deps check: a dotnet tool package must carry no <dependency>.
#
# Usage: sh test/pack-nuspec/tool-deps.sh DIR [VERSION]
#
# Looks at every *.nupkg in DIR (only *.VERSION.nupkg when VERSION is given)
# and picks the tool packages by their nuspec packageType (DotnetTool for the
# base pointer, DotnetToolRidPackage for dotcl.<rid> and dotcl.any). Libraries
# such as DotCL.Runtime are skipped: they are meant to have dependencies.
#
# Installing a tool never resolves dependencies, so a listed one does nothing
# at install time, but it is still wrong: it means the tool was built against
# some other project's restore output. The way this happens here is the tool
# (runtime.csproj) and the library (DotCL.Runtime.csproj) sharing
# runtime/obj/project.assets.json; see runtime/Directory.Build.props.
#
# Exits 1 if any tool package lists a dependency, and also if DIR holds no
# tool package at all, so a pack that produced nothing cannot pass this check.
set -u

dir=${1:?usage: tool-deps.sh DIR [VERSION]}
ver=${2:-}

if [ -n "$ver" ]; then
  pattern="*.$ver.nupkg"
else
  pattern="*.nupkg"
fi

checked=0
bad=0
for nupkg in "$dir"/$pattern; do
  [ -f "$nupkg" ] || continue
  nuspec=$(unzip -p "$nupkg" '*.nuspec' 2>/dev/null) || {
    echo "tool-deps: FAIL: cannot read the nuspec in $nupkg"
    bad=$((bad + 1))
    continue
  }
  echo "$nuspec" | grep -q '<packageType name="DotnetTool' || continue
  checked=$((checked + 1))
  if echo "$nuspec" | grep -q '<dependency '; then
    echo "tool-deps: FAIL: $(basename "$nupkg") lists dependencies:"
    echo "$nuspec" | sed -n '/<dependencies>/,/<\/dependencies>/p'
    bad=$((bad + 1))
  fi
done

if [ "$checked" -eq 0 ]; then
  echo "tool-deps: FAIL: no tool package matching $pattern in $dir"
  exit 1
fi
if [ "$bad" -ne 0 ]; then
  echo "tool-deps: FAIL ($bad of $checked tool packages)"
  exit 1
fi
echo "tool-deps: OK ($checked tool packages, none lists a dependency)"
