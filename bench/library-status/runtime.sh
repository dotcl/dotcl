# Sourced by run-quickload.sh, run-tests.sh and run-forks.sh.
#
# Which build of the runtime measures the libraries. A user runs the Release
# build (the NuGet tool and the packs are Release), and a Debug build of the
# runtime is several times slower on the same work: the JIT treats a Debug
# assembly as debuggable and skips optimisations that depend on the C#
# compiler's output. A library that loads in a minute in Release can hit the
# per-system bound in Debug, so the table measures Release by default.
#
# The core is compiler/dotcl.core, the one the packs ship (and start about 1.5
# seconds sooner than from compiler/cil-out.sil, once per system measured). It
# and the fasls (contrib, the ASDF cache) are IL that dotcl's compiler writes;
# the configuration the C# runtime was built with does not enter them, so they
# are shared by both builds.
#
# This sets $exe, $core and $runtime_desc ("Release"), which each script writes
# into every entry it produces as "runtime", and stops when the build or the
# core is missing or older than its sources: an old build measures a dotcl that
# is not the one the table names. `make bench-build` builds Release, `make
# compile-core-fasl` the core.
#
# LIBRARY_STATUS_LISP=sbcl measures the same rows on SBCL instead, for the
# time comparison in the table. The drivers are plain Common Lisp apart from
# loading Quicklisp and quitting; with sbcl, Quicklisp is loaded from the
# setup.lisp of LIBRARY_STATUS_QL_HOME, which should hold the same dists.
#
# Environment:
#   LIBRARY_STATUS_CONFIG   Release (default) or Debug
#   LIBRARY_STATUS_LISP     dotcl (default) or sbcl
#   LIBRARY_STATUS_SBCL     the sbcl executable (default: sbcl on PATH)

# select_runtime CMD: sets lisp, exe, core and runtime_desc; exits 1 on a
# missing or stale build or core.
select_runtime() {
  lisp="${LIBRARY_STATUS_LISP:-dotcl}"
  case "$lisp" in
    dotcl) ;;
    sbcl)
      exe=$(command -v "${LIBRARY_STATUS_SBCL:-sbcl}") \
        || { echo "$1: no sbcl (${LIBRARY_STATUS_SBCL:-sbcl})" >&2; exit 1; }
      core=""
      runtime_desc=$("$exe" --version < /dev/null | tr -d '\r')
      echo "$1: runtime $runtime_desc ($exe)"
      return 0 ;;
    *) echo "$1: LIBRARY_STATUS_LISP must be dotcl or sbcl, not $lisp" >&2; exit 1 ;;
  esac
  config="${LIBRARY_STATUS_CONFIG:-Release}"
  case "$config" in
    Release|Debug) ;;
    *) echo "$1: LIBRARY_STATUS_CONFIG must be Release or Debug, not $config" >&2; exit 1 ;;
  esac
  bindir="$root/runtime/bin/$config/net10.0"
  exe="$bindir/runtime.exe"
  [ -x "$exe" ] || exe="$bindir/runtime"
  if [ ! -x "$exe" ]; then
    if [ "$config" = Release ]; then
      echo "$1: no Release runtime in $bindir; run make bench-build first" >&2
    else
      echo "$1: no Debug runtime in $bindir; run make build first" >&2
    fi
    exit 1
  fi
  # Any C# source or project file newer than the built runtime assembly means
  # the build is not the source tree.
  newer=$(find "$root/runtime" \( -name bin -o -name obj \) -prune -o \
               \( -name '*.cs' -o -name '*.csproj' \) -newer "$bindir/DotCL.Runtime.dll" -print \
          | head -1)
  if [ -n "$newer" ]; then
    echo "$1: the $config runtime in $bindir is older than $newer;" >&2
    echo "$1: rebuild it first (make bench-build for Release, make build for Debug)" >&2
    exit 1
  fi
  core="$root/compiler/dotcl.core"
  if [ ! -f "$core" ] || [ ! "$core" -nt "$root/compiler/cil-out.sil" ]; then
    echo "$1: $core is missing or older than compiler/cil-out.sil; run make compile-core-fasl first" >&2
    exit 1
  fi
  runtime_desc="$config"
  echo "$1: runtime $runtime_desc ($exe, core $core)"
}

# write_launcher DIR: writes DIR/lisp, which runs one driver file on the
# selected Lisp. It execs, so `timeout` bounds the Lisp process itself.
write_launcher() {
  if [ "$lisp" = sbcl ]; then
    printf '#!/bin/sh\nexec "%s" --dynamic-space-size 8192 --non-interactive --no-sysinit --no-userinit --load "$1"\n' "$exe" > "$1/lisp"
  else
    printf '#!/bin/sh\nexec "%s" --core "%s" "$1"\n' "$exe" "$core" > "$1/lisp"
  fi
  chmod +x "$1/lisp"
}

# driver_prelude: the first forms of a driver. Loads Quicklisp and defines
# LIBSTATUS-QUIT and LIBSTATUS-WITH-DEBUGGER-HOOK (see README, Stage 2b).
driver_prelude() {
  if [ "$lisp" = sbcl ]; then
    printf '(load "%s/setup.lisp")\n' "$qlhome"
    echo '(defun libstatus-quit (code) (finish-output) (sb-ext:exit :code code :abort t))'
    echo '(defmacro libstatus-with-debugger-hook (hook &body body) `(let ((*debugger-hook* ,hook) (sb-ext:*invoke-debugger-hook* nil)) ,@body))'
  else
    echo '(require "quicklisp")'
    echo '(defun libstatus-quit (code) (finish-output) (dotcl:quit code))'
    echo '(defmacro libstatus-with-debugger-hook (hook &body body) `(let ((*debugger-hook* ,hook)) ,@body))'
  fi
}

# now_seconds: wall-clock seconds with a fraction, for the "seconds" fields.
now_seconds() {
  perl -MTime::HiRes=time -e 'printf "%.1f\n", time'
}
