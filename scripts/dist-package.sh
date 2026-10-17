#!/bin/sh
# Build the prebuilt package of one dist release for this dotcl and platform,
# and the line for the dist's dotcl-packages.txt that offers it.
#
# Usage: scripts/dist-package.sh [--dist <distinfo-url>]... [--version <v>] <system> <out-dir>
#
#   --dist     A versioned distinfo URL (the dist version to build from). Give
#              them in rising preference: the last one wins a system that more
#              than one dist has, as the dotcl overlay does over the stock dist.
#              Default: the stock dist and the dotcl overlay at the versions
#              pinned below.
#   --version  The package version. Default: <dotcl version without +build>-
#              <the release's directory name without the project, dots for
#              other characters>, e.g. 0.1.33-20261001.aff135e.
#
# Writes <out-dir>/<id>.<version>.nupkg (id dotcl-dist.<project>.<rid>) and
# appends its line to <out-dir>/dotcl-packages.txt. The fasls and the R2R
# images are for the platform this runs on.
#
# DOTCL is the command that runs dotcl (default: this tree's build). A package
# for users is built with the released tool: the line records
# (lisp-implementation-version) of the dotcl that built it, and only that dotcl
# uses it.
#
# The sources come from a fresh Quicklisp home in a temporary directory; nothing
# outside it is written but <out-dir>.
set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DISTS=""
VERSION=""
while [ $# -gt 0 ]; do
  case "$1" in
    --dist) DISTS="$DISTS $2"; shift 2 ;;
    --version) VERSION="$2"; shift 2 ;;
    -*) echo "unknown option $1" >&2; exit 2 ;;
    *) break ;;
  esac
done
[ $# -eq 2 ] || { sed -n 5,5p "$0" >&2; exit 2; }
SYSTEM="$1"
mkdir -p "$2"
OUT="$(cd "$2" && pwd)"
[ -n "$DISTS" ] || DISTS="http://beta.quicklisp.org/dist/quicklisp/2026-01-01/distinfo.txt https://dotcl.github.io/dist/dotcl/2026-10-12/distinfo.txt"

if [ -z "${DOTCL:-}" ]; then
  DOTCL="dotnet run --no-build --project $ROOT/runtime/runtime.csproj -- --core $ROOT/compiler/dotcl.core"
fi

WORK="$(mktemp -d)"
[ -n "${KEEP_WORK:-}" ] && echo "work: $WORK" || trap 'rm -rf "$WORK"' EXIT

unset CL_SOURCE_REGISTRY
mkdir -p "$WORK/xdg/data" "$WORK/xdg/config" "$WORK/xdg/config-dirs" "$WORK/xdg/data-dirs" "$WORK/cache"
export XDG_DATA_HOME="$WORK/xdg/data" XDG_CONFIG_HOME="$WORK/xdg/config"
export XDG_CONFIG_DIRS="$WORK/xdg/config-dirs" XDG_DATA_DIRS="$WORK/xdg/data-dirs"
export XDG_CACHE_HOME="$WORK/cache"

dists_lisp=""
for d in $DISTS; do dists_lisp="$dists_lisp \"$d\""; done

cat > "$WORK/fetch.lisp" <<EOF
(require "quicklisp")
(setf ql-setup:*offer-dotcl-dist* nil)
(loop for url in '($dists_lisp)
      for preference from 100
      do (setf (ql-dist:preference (ql-dist:install-dist url :prompt nil :replace t))
               preference))
(let ((seen '()))
  (labels ((walk (name)
             (let ((s (ql-dist:find-system name)))
               (when (and s (not (member name seen :test #'string=)))
                 (push name seen)
                 (ql-dist:ensure-installed s)
                 (mapc #'walk (ql-dist:required-systems s))))))
    (walk "$SYSTEM")))
(let* ((system (or (ql-dist:find-system "$SYSTEM") (error "no system $SYSTEM in the dists")))
       (release (ql-dist:release system)))
  (with-open-file (out "$WORK/release.txt" :direction :output)
    (format out "~a ~a ~a ~a~%" (ql-dist:name release) (ql-dist:prefix release)
            (lisp-implementation-version)
            (or (dotcl:package-rid) (error "no package RID for this platform"))))
  (asdf:load-asd (ql-dist:find-asdf-system-file "$SYSTEM"))
  (with-open-file (out "$WORK/author.txt" :direction :output)
    (write-line (or (asdf:system-author (asdf:find-system "$SYSTEM")) (ql-dist:name release)) out))
  (with-open-file (out "$WORK/registry.txt" :direction :output)
    (format out "(:source-registry~{ (:tree ~s)~} :ignore-inherited-configuration)~%"
            (mapcar (lambda (d) (namestring (ql-dist::relative-to d "software/")))
                    (ql-dist:enabled-dists)))))
EOF

echo "=== fetching the sources of $SYSTEM ==="
$DOTCL "$WORK/fetch.lisp" < /dev/null
read -r PROJECT PREFIX DOTCL_VERSION RID < "$WORK/release.txt"

if [ -z "$VERSION" ]; then
  suffix="$(printf '%s' "${PREFIX#"$PROJECT"-}" | tr -c 'A-Za-z0-9' '.' | tr -s '.' | sed 's/^\.//; s/\.$//')"
  VERSION="${DOTCL_VERSION%%+*}-$suffix"
fi
ID="dotcl-dist.$PROJECT.$RID"

echo "=== pack $ID $VERSION ($PREFIX, dotcl $DOTCL_VERSION, $RID) ==="
CL_SOURCE_REGISTRY="$(cat "$WORK/registry.txt")" \
  $DOTCL pack --library --system "$SYSTEM" --id "$ID" --version "$VERSION" --rids "$RID" \
         --authors "$(cat "$WORK/author.txt")" \
         --description "$SYSTEM from the Quicklisp release $PREFIX, compiled by dotcl $DOTCL_VERSION for $RID" \
         -o "$OUT/" < /dev/null
[ -f "$OUT/$ID.$VERSION.nupkg" ] || { echo "no $OUT/$ID.$VERSION.nupkg" >&2; exit 1; }

echo "$PROJECT $DOTCL_VERSION $RID $ID $VERSION $SYSTEM" >> "$OUT/dotcl-packages.txt"
echo "=== $OUT/$ID.$VERSION.nupkg"
echo "    line: $PROJECT $DOTCL_VERSION $RID $ID $VERSION $SYSTEM"
