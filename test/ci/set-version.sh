#!/bin/sh
# scripts/set-version.lisp: the version lands in the three kinds of file, nothing else in them
# changes, and a bad version is refused. Needs a built runtime and compiler/dotcl.core.
set -u
ROOT="$(cd "${1%/}" && pwd)"
RT="${DOTCL_RUNTIME:-$ROOT/runtime/bin/Debug/net10.0/runtime.dll}"
W="$(mktemp -d)"; trap 'rm -rf "$W"' EXIT
mkdir -p "$W/runtime/sdk"
cp "$ROOT/runtime/Directory.Build.props" "$W/runtime/"
cp "$ROOT/runtime/sdk/Sdk.props" "$W/runtime/sdk/"
for t in "$ROOT"/templates/*/.template.config/template.json; do
  d="$W/${t#$ROOT/}"; mkdir -p "$(dirname "$d")"; cp "$t" "$d"
done
run() { (cd "$W" && dotnet "$RT" --core "$ROOT/compiler/dotcl.core" "$ROOT/scripts/set-version.lisp" "$@" < /dev/null); }
fail=0; note() { echo "set-version: FAIL: $1"; fail=1; }

out=$(run 9.8.7-test 2>&1) || note "a valid version was refused: $out"
echo "$out" | grep -q '^== runtime version set to 9.8.7-test ==$' || note "no confirmation line: $out"
grep -q '<Version>9.8.7-test</Version>' "$W/runtime/Directory.Build.props" || note "Directory.Build.props not stamped"
grep -q '>9.8.7-test</DotCLRuntimeVersion>' "$W/runtime/sdk/Sdk.props" || note "Sdk.props not stamped"
for t in "$W"/templates/*/.template.config/template.json; do
  grep -q '"defaultValue": "9.8.7-test"' "$t" || note "${t#$W/} not stamped"
done
# nothing else changed: put the old version back and the files are the originals
run "$(sed -n 's|.*<Version>\([^<]*\)</Version>.*|\1|p' "$ROOT/runtime/Directory.Build.props" | head -1)" > /dev/null 2>&1
cmp -s "$W/runtime/Directory.Build.props" "$ROOT/runtime/Directory.Build.props" || note "Directory.Build.props differs from the original after a round trip"
cmp -s "$W/runtime/sdk/Sdk.props" "$ROOT/runtime/sdk/Sdk.props" || note "Sdk.props differs from the original after a round trip"
for t in "$W"/templates/*/.template.config/template.json; do
  cmp -s "$t" "$ROOT/${t#$W/}" || note "${t#$W/} differs from the original after a round trip"
done
for bad in x.y 1.2; do
  run "$bad" > /dev/null 2>&1 && note "version $bad accepted"
done
run > /dev/null 2>&1 && note "no version accepted"
[ "$fail" -eq 0 ] && echo "set-version: ok"
exit $fail
