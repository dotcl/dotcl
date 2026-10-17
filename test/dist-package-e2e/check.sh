#!/bin/sh
# A dist release offered as a prebuilt package, end to end, with ironclad:
#
#   1. scripts/dist-package.sh builds the package of ironclad from the pinned
#      dist versions onto a local feed (a directory) and writes its index line.
#   2. A fresh Quicklisp home installs the same dist versions; the dist that
#      offers ironclad gets the index and the feed, as a dist would publish
#      them (dotcl-packages-url and dotcl-package-source in its distinfo).
#   3. (ql:quickload "ironclad") with empty fasl and NuGet caches loads the
#      package: nothing is compiled, and ironclad works.
#   4. The same with the index line made for another dotcl version: ironclad
#      is compiled from its sources, as without a package.
#
# Needs the network for the dists and their archives (Quicklisp, the dotcl
# dist); restore sees the local feed only.
#
# Usage: check.sh <repo-root>     (DOTCL: the dotcl command, default this tree's build)
set -eu

ROOT="$(cd "${1:-.}" && pwd)"
if [ -z "${DOTCL:-}" ]; then
  DOTCL="dotnet run --no-build --project $ROOT/runtime/runtime.csproj -- --core $ROOT/compiler/dotcl.core"
fi
export DOTCL
WORK="$(mktemp -d)"
[ -n "${KEEP_WORK:-}" ] && echo "work: $WORK" || trap 'rm -rf "$WORK"' EXIT
fail=0

start=$(date +%s)
echo "=== build the package ==="
sh "$ROOT/scripts/dist-package.sh" ironclad "$WORK/feed" > "$WORK/produce.log" 2>&1 ||
  { echo "FAIL: scripts/dist-package.sh"; cat "$WORK/produce.log"; exit 1; }
tail -2 "$WORK/produce.log"
echo "  built in $(( $(date +%s) - start )) s"

unset CL_SOURCE_REGISTRY
mkdir -p "$WORK/xdg/data" "$WORK/xdg/config" "$WORK/xdg/config-dirs" "$WORK/xdg/data-dirs"
export XDG_DATA_HOME="$WORK/xdg/data" XDG_CONFIG_HOME="$WORK/xdg/config"
export XDG_CONFIG_DIRS="$WORK/xdg/config-dirs" XDG_DATA_DIRS="$WORK/xdg/data-dirs"
export RestoreSources="$WORK/feed/"

cat > "$WORK/home.lisp" <<EOF
(require "quicklisp")
(setf ql-setup:*offer-dotcl-dist* nil)
(loop for url in '("http://beta.quicklisp.org/dist/quicklisp/2026-01-01/distinfo.txt"
                   "https://dotcl.github.io/dist/dotcl/2026-10-12/distinfo.txt")
      for preference from 100
      do (setf (ql-dist:preference (ql-dist:install-dist url :prompt nil :replace t))
               preference))
(let ((dist (ql-dist:dist (ql-dist:find-system "ironclad"))))
  (with-open-file (out (ql-dist::relative-to dist "distinfo.txt")
                       :direction :output :if-exists :append)
    (format out "dotcl-packages-url: http://example.invalid/dotcl-packages.txt~%~
                 dotcl-package-source: $WORK/feed/~%"))
  (with-open-file (out "$WORK/index-path.txt" :direction :output)
    (write-string (namestring (ql-dist::relative-to dist "dotcl-packages.txt")) out)))
EOF
$DOTCL "$WORK/home.lisp" < /dev/null > "$WORK/home.log" 2>&1 ||
  { echo "FAIL: setting up the Quicklisp home"; cat "$WORK/home.log"; exit 1; }
INDEX="$(cat "$WORK/index-path.txt")"

cat > "$WORK/load.lisp" <<'EOF'
(require "quicklisp")
(defvar *compiled* 0)
(defmethod asdf:perform :before ((o asdf:compile-op) (c asdf:cl-source-file))
  (incf *compiled*))
(let ((start (get-internal-real-time)))
  (ql:quickload "ironclad" :silent t)
  (format t "e2e: quickload ~,2f s~%"
          (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
(format t "e2e: compiled ~d~%" *compiled*)
(format t "e2e: sha256 ~a~%"
        (uiop:symbol-call :ironclad :byte-array-to-hex-string
                          (uiop:symbol-call :ironclad :digest-sequence :sha256
                                            (uiop:symbol-call :ironclad :ascii-string-to-byte-array "abc"))))
EOF

# run <tag>: quickload ironclad with empty fasl and NuGet caches
run() {
  XDG_CACHE_HOME="$WORK/cache-$1" NUGET_PACKAGES="$WORK/pkgs-$1" \
    $DOTCL "$WORK/load.lisp" < /dev/null > "$WORK/$1.log" 2>&1 ||
    { echo "FAIL: quickload ($1)"; cat "$WORK/$1.log"; exit 1; }
  grep '^e2e:' "$WORK/$1.log" | sed 's/^/  /'
}
check() {
  if grep -qE "$3" "$WORK/$1.log"; then echo "  ok: $2"; else echo "  FAIL: $2"; fail=1; fi
}
check_not() {
  if grep -qE "$3" "$WORK/$1.log"; then echo "  FAIL: $2"; fail=1; else echo "  ok: $2"; fi
}
sha='e2e: sha256 ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'

cp "$WORK/feed/dotcl-packages.txt" "$INDEX"
echo "=== quickload from the package ==="
run package
check package "the package is used" '; quicklisp: ironclad from the package dotcl-dist\.ironclad\.'
check package "nothing is compiled" '^e2e: compiled 0$'
check package "ironclad works" "$sha"

awk '{ $2 = "0.0.0-another"; print }' "$WORK/feed/dotcl-packages.txt" > "$INDEX"
echo "=== quickload with the package made for another dotcl ==="
run source
check_not source "the package is not used" 'from the package'
check source "ironclad is compiled" '^e2e: compiled [1-9]'
check source "ironclad works" "$sha"

if [ "$fail" -ne 0 ]; then
  [ -n "${KEEP_WORK:-}" ] || cat "$WORK/package.log" "$WORK/source.log"
  echo "dist-package-e2e: FAIL"; exit 1
fi
echo "dist-package-e2e: OK"
