#!/bin/sh
# pack nuspec check: `dotcl pack` restamps published dotcl packages into an
# app's own tool packages. Assert that the produced nuspec describes the APP,
# not dotcl:
#
#   1. .asd metadata (:description / :homepage / :source-control / :author /
#      :license) lands in the nuspec, and a README next to the .asd is packaged.
#      :version does too, so a project with a version in its .asd can pack
#      without --version, while an explicit --version still wins.
#   2. Fields the app supplies neither in its .asd nor on the command line are
#      DROPPED rather than inherited: a donor projectUrl / repository / tags /
#      copyright under a different package id is wrong attribution, not stale.
#   3. NuGet's required fields (description, authors) are refused rather than
#      inherited: packing without them fails, with a message naming both ways
#      to supply them.
#   4. Nothing dotcl wrote at runtime rides along in the payload. A restamp
#      copies the donor's files, so anything a dotcl run dropped into a publish
#      directory ends up inside someone else's application package.
#
# Requires a directory of published dotcl packages (`make pack`). Skips -- does
# not fail -- when they are absent, so the suite still runs on a fresh clone,
# unless DOTCL_CI=1 says this is CI, where a skip would be read as a pass.
#
# Usage: check.sh <repo-root> [from-dir]
set -eu

# A missing prerequisite is a convenience skip when this is run by hand, but in
# CI a skip is indistinguishable from a pass: the gate quietly stops gating and
# nothing in the log says so. DOTCL_CI=1 (set at the job level in
# .github/workflows/ci.yml) makes it a failure instead.
skip_or_fail() {
  echo "$1"
  if [ "${DOTCL_CI:-}" = "1" ]; then
    echo "  DOTCL_CI=1: a skipped check counts as a failure here" >&2
    exit 1
  fi
}
ROOT="${1%/}"
FROM="${2:-$ROOT/out}"
RT="$ROOT/runtime/runtime.csproj"
CORE="$ROOT/compiler/dotcl.core"

ver=""
for p in "$FROM"/dotcl.*.nupkg; do
  [ -e "$p" ] || continue
  # Only the pointer package is dotcl.<version>.nupkg; a RID package starts the
  # middle segment with the rid (dotcl.win-arm64.<version>.nupkg). Discriminate
  # on "starts with a digit", the same rule PackRestamp.InferDotclVersion uses;
  # the version itself contains dots, so counting them does not work.
  b="${p##*/}"; b="${b#dotcl.}"; b="${b%.nupkg}"
  case "$b" in [0-9]*) ;; *) continue ;; esac
  ver="$b"
done
if [ -z "$ver" ]; then
  skip_or_fail "SKIP: no dotcl.<version>.nupkg in $FROM (run 'make pack' first)"
  exit 0
fi
if [ ! -f "$CORE" ]; then
  skip_or_fail "SKIP: $CORE missing (run 'make pack' or 'make compile-core-fasl' first)"
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
  :version "0.9.2"
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

echo "=== [4] --r2r puts the ahead-of-time sibling beside the fasl ==="
# A packed tool loads its own dotcl.user.fasl by path and prefers
# dotcl.user.fasl.r2r-<rid> next to it. The producing half is checked here: the
# name and the directory have to be exactly what the loader probes, and every
# way this has gone wrong before -- a sibling under the wrong spelling of the
# RID, or in the wrong directory -- looked like success from the outside.
#
# Needs a real RID: the any-RID package carries no runtime, so there is nothing
# to compile against and --r2r has nothing to do there.
#
# Any RID the donor carries will do, because pack cross-compiles; it does not
# have to be this machine's. Asking the host for its RID is wrong by
# construction, not merely fragile: the process RID can be the distro-specific
# spelling (ubuntu.24.04-x64), while everything that produces these files uses
# the portable os-arch form, so the name would match no donor package and the
# gate would go red for an environmental reason. Read the RID off the donor set
# instead. "any" is skipped for the reason above.
rid=""
for p in "$FROM"/dotcl.*."$ver".nupkg; do
  [ -e "$p" ] || continue
  b="${p##*/}"; b="${b#dotcl.}"; b="${b%".$ver.nupkg"}"
  case "$b" in [0-9]*|any|"") ;; *) rid="$b"; break ;; esac
done
if [ -z "$rid" ]; then
  skip_or_fail "SKIP: no dotcl.<rid>.$ver.nupkg in $FROM (run 'make pack' first)"
else
  r2rout=$(dotnet run --project "$RT" -- --core "$CORE" --asd-search-path "$WORK/meta" pack \
             --system packmeta --id packr2r --command packr2r --version 0.0.1 \
             --dotcl-version "$ver" --from "$FROM" --rids "$rid" --r2r \
             -o "$WORK/out" 2>&1) || note "pack --r2r failed: $r2rout"
  pkg="$WORK/out/packr2r.$rid.0.0.1.nupkg"
  if [ ! -f "$pkg" ]; then
    note "pack --r2r produced no $pkg"
  elif echo "$r2rout" | grep -q 'no ReadyToRun image'; then
    # The package is still correct without it, only slower to start, so say what
    # is untested rather than passing quietly.
    skip_or_fail "SKIP: crossgen2 produced nothing here: $(echo "$r2rout" | grep 'no ReadyToRun image')"
    unzip -Z1 "$pkg" | grep -q "/dotcl.user.fasl$" \
      || note "a pack without a ReadyToRun image dropped the fasl too"
  else
    unzip -Z1 "$pkg" | grep -q "^tools/net10.0/$rid/dotcl.user.fasl.r2r-$rid$" \
      || note "no tools/net10.0/$rid/dotcl.user.fasl.r2r-$rid in the package"
    unzip -Z1 "$pkg" | grep -q "^tools/net10.0/$rid/dotcl.user.fasl$" \
      || note "the fasl itself is missing beside its ReadyToRun sibling"
    # One RID's images per package: the loader picks by name, and the other
    # five are dead weight a restamp has shipped before.
    if unzip -Z1 "$pkg" | grep 'r2r-' | grep -v "r2r-$rid" | grep -q .; then
      note "the package carries ReadyToRun images for a RID other than $rid"
    fi
  fi
fi

echo "=== [5] --version defaults to the .asd's :version ==="
# A project that states :version in its .asd should not have to repeat it on
# the command line -- that repetition is the last line of shell a packed
# project needed around `dotcl pack`. Three things decide this: the default
# applies, an explicit --version still beats it (release paths pass versions of
# their own and must keep them), and a system with no version anywhere is still
# a usage error rather than a package stamped with a version nobody wrote.
#
# packmeta declares :version "0.9.2" and packbare declares none.

# Default: no --version on the command line.
verout=$(dotnet run --project "$RT" -- --core "$CORE" --asd-search-path "$WORK/meta" pack \
           --system packmeta --id packver --command packver \
           --dotcl-version "$ver" --from "$FROM" --rids any -o "$WORK/out" 2>&1) \
  || note "pack without --version failed: $verout"
if [ ! -f "$WORK/out/packver.0.9.2.nupkg" ]; then
  note "no packver.0.9.2.nupkg: --version did not default to the .asd's :version"
  ls "$WORK/out" | grep '^packver' || true
else
  rm -rf "$WORK/v"; mkdir -p "$WORK/v"
  (cd "$WORK/v" && unzip -oq "$WORK/out/packver.0.9.2.nupkg")
  grep -q '<version>0.9.2</version>' "$WORK/v/packver.nuspec" \
    || note "<version> in the nuspec is not the .asd's 0.9.2"
fi

# Explicit: check [1] packed the same system with --version 0.0.1. The flag has
# to win, or a nightly build would start publishing the .asd's version.
grep -q '<version>0.0.1</version>' "$NUSPEC" \
  || note "an explicit --version did not override the .asd's :version"

# Neither: the usage error stays, and says where a version can come from.
if out=$(dotnet run --project "$RT" -- --core "$CORE" --asd-search-path "$WORK/bare" pack \
           --system packbare --id packbare --command packbare \
           --dotcl-version "$ver" --from "$FROM" --rids any -o "$WORK/out" \
           --description 'A bare fixture' --authors 'Someone' 2>&1); then
  note "pack succeeded with no version in the .asd and no --version"
else
  echo "$out" | grep -q 'missing required option(s): --version' \
    || note "the missing-version error changed wording: $out"
  echo "$out" | grep -q ':version in the .asd' \
    || note "the missing-version error does not name the .asd as a source: $out"
fi

# A .asd version NuGet cannot serve (5 components): pack warns, names --version
# as the way out, and still writes the package. An explicit --version of the
# same shape is the user's own statement and is not second-guessed.
mkdir -p "$WORK/odd"
cat > "$WORK/odd/packodd.asd" <<'EOF'
(defsystem "packodd" :version "1.2.3.4.5" :description "Odd version fixture"
  :author "Someone" :components ((:file "packodd")))
EOF
cat > "$WORK/odd/packodd.lisp" <<'EOF'
(defpackage :packodd (:use :cl))
EOF
oddout=$(dotnet run --project "$RT" -- --core "$CORE" --asd-search-path "$WORK/odd" pack \
           --system packodd --id packodd --command packodd \
           --dotcl-version "$ver" --from "$FROM" --rids any -o "$WORK/out" 2>&1) \
  || note "pack refused a .asd version NuGet cannot serve: $oddout"
echo "$oddout" | grep -q 'cannot be served by NuGet' \
  || note "no warning for the .asd's 5-component :version: $oddout"
echo "$oddout" | grep -q 'pass --version to override' \
  || note "the version warning does not name --version: $oddout"
[ -f "$WORK/out/packodd.1.2.3.4.5.nupkg" ] \
  || note "the package was not written after the version warning"
oddexp=$(dotnet run --project "$RT" -- --core "$CORE" --asd-search-path "$WORK/odd" pack \
           --system packodd --id packodd2 --command packodd2 --version 1.2.3.4.5 \
           --dotcl-version "$ver" --from "$FROM" --rids any -o "$WORK/out" 2>&1) || true
echo "$oddexp" | grep -q 'cannot be served by NuGet' \
  && note "an explicit --version was warned about: $oddexp"

echo "=== [6] no JIT profile rides along in the payload ==="
# The multi-core JIT profile is written by every dotcl run. It used to land
# beside the executing assembly, so a run against a publish directory left one
# there, `dotnet pack` swept it into the dotcl package, and a restamp copied it
# on into the application package: a file named after dotcl, carrying one
# machine's method traces, shipped to that application's users.
#
# It now goes to the user's cache instead, which stops new ones appearing. This
# is the other half: a developer tree that already has a stray, or anything
# else that writes one, must not get it past here. Match the extension rather
# than the old fixed name -- the file is named after the executable now, so
# "dotcl.profile" alone would miss every future spelling. The fixtures ship no
# .profile of their own, so any hit is contamination.
for pkg in "$WORK"/out/*.nupkg; do
  [ -e "$pkg" ] || continue
  strays=$(unzip -Z1 "$pkg" | grep '\.profile$' || true)
  [ -z "$strays" ] || note "${pkg##*/} carries a JIT profile: $strays"
done

if [ "$fail" -ne 0 ]; then
  echo "pack-nuspec: FAIL"
  exit 1
fi
echo "pack-nuspec: OK"
