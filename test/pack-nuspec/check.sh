#!/bin/sh
# pack nuspec check: `dotcl pack` restamps published dotcl packages into an
# app's own tool packages. Assert that the produced nuspec describes the APP,
# not dotcl:
#
#   1. .asd metadata (:description / :homepage / :source-control / :author /
#      :license) lands in the nuspec, and a README next to the .asd is packaged.
#   2. Fields the app supplies neither in its .asd nor on the command line are
#      DROPPED rather than inherited — a donor projectUrl / repository / tags /
#      copyright under a different package id is wrong attribution, not stale.
#   3. NuGet's required fields (description, authors) are refused rather than
#      inherited: packing without them fails, with a message naming both ways
#      to supply them.
#
# Requires a directory of published dotcl packages (`make pack`). Skips — does
# not fail — when they are absent, so the suite still runs on a fresh clone.
#
# Usage: check.sh <repo-root> [from-dir]
set -eu
ROOT="${1%/}"
FROM="${2:-$ROOT/out}"
RT="$ROOT/runtime/runtime.csproj"
CORE="$ROOT/compiler/dotcl.core"

ver=""
for p in "$FROM"/dotcl.*.nupkg; do
  [ -e "$p" ] || continue
  # Only the pointer package is dotcl.<version>.nupkg; a RID package starts the
  # middle segment with the rid (dotcl.win-arm64.<version>.nupkg). Discriminate
  # on "starts with a digit", the same rule PackRestamp.InferDotclVersion uses —
  # the version itself contains dots, so counting them does not work.
  b="${p##*/}"; b="${b#dotcl.}"; b="${b%.nupkg}"
  case "$b" in [0-9]*) ;; *) continue ;; esac
  ver="$b"
done
if [ -z "$ver" ]; then
  echo "SKIP: no dotcl.<version>.nupkg in $FROM (run 'make pack' first)"
  exit 0
fi
if [ ! -f "$CORE" ]; then
  echo "SKIP: $CORE missing (run 'make pack' or 'make compile-core-fasl' first)"
  exit 0
fi
echo "=== donor: dotcl $ver from $FROM ==="

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Each fixture gets its own directory: the sibling-README default keys off the
# .asd's directory, so a README next to one system must not be seen by another.

# --- fixture: a system that declares full metadata, plus its own README -------
mkdir -p "$WORK/meta"
cat > "$WORK/meta/packmeta.asd" <<'EOF'
(defsystem "packmeta"
  :description "Fixture system for the pack nuspec check"
  :homepage "https://example.invalid/packmeta"
  :source-control (:git "https://github.com/example/packmeta.git")
  :author "Fixture Author"
  :license "MIT"
  :components ((:file "packmeta")))
EOF
cat > "$WORK/meta/packmeta.lisp" <<'EOF'
(defpackage :packmeta (:use :cl) (:export #:main))
(in-package :packmeta)
(defun main () (format t "packmeta~%"))
EOF
printf '# packmeta\n\nThe fixture app README, not dotcl'"'"'s.\n' > "$WORK/meta/README.md"

# --- fixture: a system that declares nothing, with no sibling README --------
mkdir -p "$WORK/bare"
cat > "$WORK/bare/packbare.asd" <<'EOF'
(defsystem "packbare" :components ((:file "packbare")))
EOF
cat > "$WORK/bare/packbare.lisp" <<'EOF'
(defpackage :packbare (:use :cl))
EOF

pack() { # $1 = search-subdir, $2 = system/id, rest = extra args
  sub="$1"; sys="$2"; shift 2
  dotnet run --project "$RT" -- --core "$CORE" --asd-search-path "$WORK/$sub" pack \
    --system "$sys" --id "$sys" --command "$sys" --version 0.0.1 \
    --dotcl-version "$ver" --from "$FROM" --rids any -o "$WORK/out" "$@"
}

fail=0
note() { echo "  FAIL: $1"; fail=1; }

echo "=== [1] .asd metadata reaches the nuspec ==="
pack meta packmeta >/dev/null 2>&1
rm -rf "$WORK/x"; mkdir -p "$WORK/x"
(cd "$WORK/x" && unzip -oq "$WORK/out/packmeta.0.0.1.nupkg")
NUSPEC="$WORK/x/packmeta.nuspec"
for pair in \
  'Fixture system for the pack nuspec check|description' \
  'https://example.invalid/packmeta|projectUrl' \
  'https://github.com/example/packmeta.git|repository' \
  'Fixture Author|authors' \
  'MIT|license'
do
  want="${pair%%|*}"; field="${pair##*|}"
  grep -q "$want" "$NUSPEC" || note "<$field> did not pick up the .asd value ($want)"
done
# The packaged README must be the app's, not dotcl's.
if [ -f "$WORK/x/README.md" ]; then
  grep -q 'fixture app README' "$WORK/x/README.md" \
    || note "packaged README.md is not the app's own"
else
  note "app README.md was not packaged"
fi

echo "=== [2] unsupplied donor fields are dropped ==="
pack bare packbare --description 'A bare fixture' --authors 'Someone' >/dev/null 2>&1
rm -rf "$WORK/y"; mkdir -p "$WORK/y"
(cd "$WORK/y" && unzip -oq "$WORK/out/packbare.0.0.1.nupkg")
BARE="$WORK/y/packbare.nuspec"
# Look only at <metadata>: contentFiles legitimately names dotcl payload paths.
sed -n '/<metadata>/,/<contentFiles>/p' "$BARE" > "$WORK/bare-md.xml"
for field in projectUrl repository tags copyright license readme; do
  grep -q "<$field" "$WORK/bare-md.xml" \
    && note "<$field> survived from the donor although the app supplied none"
done
grep -q 'github.com/dotcl' "$WORK/bare-md.xml" \
  && note "donor repository url survived in metadata"
[ -f "$WORK/y/README.md" ] && note "donor README.md shipped in a rebranded package"
# Supplied values are still there.
grep -q 'A bare fixture' "$BARE" || note "--description was not applied"
grep -q 'Someone' "$BARE" || note "--authors was not applied"
# Debug symbols must not ride along in a distributed tool.
if find "$WORK/y" -name '*.pdb' | grep -q .; then
  note "a .pdb shipped in the restamped package"
fi
# content/ and contentFiles/ are a portable payload copy nothing reads in a
# DotnetTool package (the SDK payload lives in the DotCL.Runtime package). A
# clean donor (runtime csproj Content Pack=false) carries none, so neither
# should the restamp. Guards against that csproj change being reverted.
if [ -d "$WORK/y/content" ] || [ -d "$WORK/y/contentFiles" ]; then
  note "restamped package carries a content/ or contentFiles/ payload copy"
fi

echo "=== [3] required fields are refused, not inherited ==="
if out=$(pack bare packbare 2>&1); then
  note "pack succeeded without a description/author (should have failed)"
else
  echo "$out" | grep -q 'needs a description' \
    || note "error message does not name the missing description: $out"
  echo "$out" | grep -q ':description in the .asd' \
    || note "error message does not mention the .asd as a source"
fi

if [ "$fail" -ne 0 ]; then
  echo "pack-nuspec: FAIL"
  exit 1
fi
echo "pack-nuspec: OK"
