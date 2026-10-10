#!/bin/sh
# Library package end to end: the path a library author and a library user
# take, with nothing between them but a NuGet feed.
#
#   1. `dotcl pack --library` packs an ASDF system (e2e-greet) that depends on
#      another system in a directory of its own (e2e-helper) into a .nupkg on
#      a local feed (a directory).
#   2. A separate project in another directory (e2e-app) declares
#      (:nuget "e2e-greet" :nuget-version "1.0.0" :source <feed>) and nothing
#      else; loading it resolves the package through `dotnet restore`, puts
#      both systems where ASDF finds them, and the project calls into them.
#
# What is asserted:
#   - the package is written, under the name NuGet expects;
#   - the consumer prints the library's answer, which needs both systems;
#   - the consumer compiled only its own file: the library's fasls came from
#     the package (same dotcl), not from compiling the shipped sources;
#   - the consumer wrote its lock file, recording the package version.
#
# No network: restore sees the local feed only (RestoreSources, read by MSBuild
# from the environment, replaces the configured sources), and NuGet's package
# folder, dotcl's caches and ASDF's source registry start empty, so nothing
# already on this machine can answer instead of the package.
#
# Which dotcl runs it: when <from-dir> (default <repo-root>/out) holds the dotcl
# tool packages `make pack` writes, they are installed with `dotnet tool install`
# from that directory alone and the installed command is used, as a user would.
# Otherwise this tree's build is used (`dotnet run`), which is what a developer
# runs by hand; DOTCL_CI=1 makes the missing packages a failure instead, since in
# CI the point is the installed tool.
#
# With PRODUCER_VERSION set (e.g. 0.1.32), the library is packed by that
# released dotcl instead, installed with `dotnet tool install` from the
# configured NuGet sources (the one step here that uses the network), and
# loaded by the dotcl above. The consumer then has to use the old fasls as they
# are, after a warning naming the old version. A package made before the
# platform was recorded in dotcl-build.sexp gets this machine's (it was made
# here), since without one the consumer compiles the sources and the old fasls
# would never be loaded.
#
# Usage: [PRODUCER_VERSION=<v>] check.sh <repo-root> [from-dir]
set -eu

ROOT="${1%/}"
FROM="${2:-$ROOT/out}"
RT="$ROOT/runtime/runtime.csproj"
CORE="$ROOT/compiler/dotcl.core"

WORK="$(mktemp -d)"
[ -n "${KEEP_WORK:-}" ] && echo "work: $WORK" || trap 'rm -rf "$WORK"' EXIT
fail=0

# ASDF's source registry comes from CL_SOURCE_REGISTRY and the XDG directories.
# Point them all at empty directories under $WORK: a configuration on this
# machine that exposes other systems (a CI runner's (:tree <workspace>) sees the
# asdf/ checkout, whose asdf.asd OPERATE then loads and compiles in place of the
# preloaded ASDF) would otherwise take part in the check.
unset CL_SOURCE_REGISTRY
mkdir -p "$WORK/xdg/config" "$WORK/xdg/config-dirs" "$WORK/xdg/data" "$WORK/xdg/data-dirs"
export XDG_CONFIG_HOME="$WORK/xdg/config" XDG_CONFIG_DIRS="$WORK/xdg/config-dirs"
export XDG_DATA_HOME="$WORK/xdg/data" XDG_DATA_DIRS="$WORK/xdg/data-dirs"

# --- which dotcl -------------------------------------------------------------
ver=""
for p in "$FROM"/dotcl.*.nupkg; do
  [ -e "$p" ] || continue
  # dotcl.<version>.nupkg is the pointer; dotcl.<rid>.<version>.nupkg starts the
  # middle segment with the rid (the same rule PackRestamp.InferDotclVersion uses).
  b="${p##*/}"; b="${b#dotcl.}"; b="${b%.nupkg}"
  case "$b" in [0-9]*) ;; *) continue ;; esac
  ver="$b"
done

if [ -n "$ver" ]; then
  echo "=== dotcl: installing the tool $ver from $FROM ==="
  cat > "$WORK/tool.config" <<EOF
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="local" value="$FROM" />
  </packageSources>
</configuration>
EOF
  NUGET_PACKAGES="$WORK/tool-pkgs" dotnet tool install dotcl --version "$ver" \
    --configfile "$WORK/tool.config" --tool-path "$WORK/tool" > "$WORK/install.log" 2>&1 ||
    { echo "FAIL: dotnet tool install dotcl $ver from $FROM"; cat "$WORK/install.log"; exit 1; }
  exe="$WORK/tool/dotcl"
  [ -f "$exe" ] || exe="$WORK/tool/dotcl.cmd"
  dotcl() { "$exe" "$@"; }
else
  if [ "${DOTCL_CI:-}" = "1" ]; then
    echo "FAIL: no dotcl.<version>.nupkg in $FROM (run 'make pack' first)"
    echo "  DOTCL_CI=1: CI checks the installed tool, not the build tree" >&2
    exit 1
  fi
  if [ ! -f "$CORE" ]; then
    echo "FAIL: neither tool packages in $FROM nor $CORE (run 'make compile-core-fasl')"
    exit 1
  fi
  echo "=== dotcl: this tree's build (no tool packages in $FROM) ==="
  dotcl() { dotnet run --no-build --project "$RT" -- --core "$CORE" "$@"; }
fi

# --- which dotcl packs --------------------------------------------------------
producer() { dotcl "$@"; }
if [ -n "${PRODUCER_VERSION:-}" ]; then
  echo "=== producer: installing the released dotcl $PRODUCER_VERSION ==="
  (cd "$WORK" && NUGET_PACKAGES="$WORK/producer-pkgs" dotnet tool install dotcl \
     --version "$PRODUCER_VERSION" --tool-path "$WORK/producer") > "$WORK/producer.log" 2>&1 ||
    { echo "FAIL: dotnet tool install dotcl $PRODUCER_VERSION"; cat "$WORK/producer.log"; exit 1; }
  pexe="$WORK/producer/dotcl"
  [ -f "$pexe" ] || pexe="$WORK/producer/dotcl.cmd"
  producer() { "$pexe" "$@"; }
fi

# --- the library: e2e-greet, depending on e2e-helper in another directory ------
mkdir -p "$WORK/lib/helper" "$WORK/lib/greet" "$WORK/feed" "$WORK/app"
cat > "$WORK/lib/helper/e2e-helper.asd" <<'EOF'
(defsystem "e2e-helper"
  :version "0.3.0"
  :components ((:file "helper")))
EOF
cat > "$WORK/lib/helper/helper.lisp" <<'EOF'
(defpackage :e2e-helper (:use :cl) (:export #:shout))
(in-package :e2e-helper)
(defun shout (s) (string-upcase s))
EOF
cat > "$WORK/lib/greet/e2e-greet.asd" <<'EOF'
(defsystem "e2e-greet"
  :version "1.0.0"
  :author "Example Author"
  :description "Fixture library for the library package end-to-end check"
  :license "MIT"
  :depends-on ("e2e-helper")
  :components ((:file "greet")))
EOF
cat > "$WORK/lib/greet/greet.lisp" <<'EOF'
(defpackage :e2e-greet (:use :cl) (:export #:greet))
(in-package :e2e-greet)
(defun greet (name) (format nil "hello, ~a" (e2e-helper:shout name)))
EOF

echo "=== pack --library ==="
if ! (cd "$WORK/lib" &&
      XDG_CACHE_HOME="$WORK/cache-pack" \
      producer --asd-search-path "$WORK/lib/greet" --asd-search-path "$WORK/lib/helper" \
            pack --library --system e2e-greet -o "$WORK/feed/" < /dev/null) \
     > "$WORK/pack.log" 2>&1; then
  echo "FAIL: dotcl pack --library"; cat "$WORK/pack.log"; exit 1
fi
cat "$WORK/pack.log"
if [ ! -f "$WORK/feed/e2e-greet.1.0.0.nupkg" ]; then
  echo "FAIL: no e2e-greet.1.0.0.nupkg on the feed"; ls -l "$WORK/feed"; exit 1
fi

if [ -n "${PRODUCER_VERSION:-}" ]; then
  cat > "$WORK/stamp.lisp" <<EOF
(require "dotnet-nuget")
(let* ((zip (dotnet:static "System.IO.Compression.ZipFile" "Open"
                           "$WORK/feed/e2e-greet.1.0.0.nupkg"
                           (dotnet:static "System.IO.Compression.ZipArchiveMode" "Update")))
       (entries (dotnet:invoke zip "get_Entries"))
       (names (loop for i below (dotnet:invoke entries "get_Count")
                    for n = (dotnet:invoke (dotnet:invoke entries "get_Item" i) "get_FullName")
                    when (search "/dotcl-build.sexp" n) collect n))
       (stamped 0))
  (dolist (name names)
    (let* ((e (dotnet:invoke zip "GetEntry" name))
           (r (dotnet:new "System.IO.StreamReader" (dotnet:invoke e "Open")))
           (record (prog1 (read-from-string (dotnet:invoke r "ReadToEnd"))
                     (dotnet:invoke r "Dispose"))))
      (unless (getf record :platform)
        (dotnet:invoke e "Delete")
        (let ((w (dotnet:new "System.IO.StreamWriter"
                             (dotnet:invoke (dotnet:invoke zip "CreateEntry" name) "Open"))))
          (dotnet:invoke w "Write"
                         (with-standard-io-syntax
                           (prin1-to-string
                            (append record (list :platform (nuget::%platform-features))))))
          (dotnet:invoke w "Dispose"))
        (incf stamped))))
  (dotnet:invoke zip "Dispose")
  (format t "e2e: build records given this machine's platform: ~d of ~d~%"
          stamped (length names)))
EOF
  dotcl "$WORK/stamp.lisp" < /dev/null ||
    { echo "FAIL: stamping the platform into the package"; exit 1; }
fi

# --- the consumer: another project, another directory ------------------------
cat > "$WORK/app/e2e-app.asd" <<EOF
(defsystem "e2e-app"
  :defsystem-depends-on ("asdf-nuget")
  :serial t
  :components ((:nuget "e2e-greet" :nuget-version "1.0.0" :source "$WORK/feed/")
               (:file "app")))
EOF
cat > "$WORK/app/app.lisp" <<'EOF'
(defpackage :e2e-app (:use :cl) (:export #:main))
(in-package :e2e-app)
(defun main () (e2e-greet:greet "nuget"))
EOF
# A file rather than --eval: the quoting survives every shell this runs from.
cat > "$WORK/app/run.lisp" <<'EOF'
(require "asdf")
(push (make-pathname :name nil :type nil :defaults *load-truename*)
      asdf:*central-registry*)
(defvar *compiled* '())
(defmethod asdf:perform :before ((o asdf:compile-op) (c asdf:cl-source-file))
  (push (asdf:component-name (asdf:component-system c)) *compiled*))
(asdf:load-system "e2e-app")
(format t "e2e: answer ~s~%" (uiop:symbol-call :e2e-app :main))
(format t "e2e: compiled ~s~%" (sort (remove-duplicates *compiled* :test #'string=) #'string<))
EOF

echo "=== consume (:nuget \"e2e-greet\") from $WORK/app ==="
if ! (cd "$WORK/app" &&
      XDG_CACHE_HOME="$WORK/cache-app" NUGET_PACKAGES="$WORK/pkgs" \
      RestoreSources="$WORK/feed/" \
      dotcl "$WORK/app/run.lisp" < /dev/null) > "$WORK/consume.log" 2>&1; then
  echo "FAIL: loading e2e-app"; cat "$WORK/consume.log"; exit 1
fi
cat "$WORK/consume.log"

check() {
  if grep -qF "$2" "$WORK/consume.log"; then
    echo "  ok: $1"
  else
    echo "  FAIL: $1 (expected a line with: $2)"; fail=1
  fi
}
check "the library answers through its dependency" 'e2e: answer "hello, NUGET"'
check "only the consumer's own file was compiled" 'e2e: compiled ("e2e-app")'
if [ -n "${PRODUCER_VERSION:-}" ]; then
  check "the consumer warned that another dotcl made the fasls" \
        "was compiled by dotcl $PRODUCER_VERSION"
  if grep -q "compiled by a different dotcl core" "$WORK/consume.log"; then
    echo "  FAIL: the loader warned per fasl as well (one line per package is the point)"
    fail=1
  else
    echo "  ok: no per-fasl warning from the loader"
  fi
fi
if grep -q '"e2e-greet"' "$WORK/app/dotnet-nuget.lock.json" 2>/dev/null &&
   grep -q '"1.0.0"' "$WORK/app/dotnet-nuget.lock.json"; then
  echo "  ok: the lock file records e2e-greet 1.0.0"
else
  echo "  FAIL: the lock file does not record e2e-greet 1.0.0"
  cat "$WORK/app/dotnet-nuget.lock.json" 2>/dev/null || echo "  (no lock file)"
  fail=1
fi

if [ "$fail" -ne 0 ]; then
  echo "pack-library-e2e: FAIL"
  exit 1
fi
echo "pack-library-e2e: OK"
