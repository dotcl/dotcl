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
# Environment:
#   LIBRARY_STATUS_CONFIG   Release (default) or Debug

# select_runtime CMD: sets exe, core and runtime_desc; exits 1 on a missing or
# stale build or core.
select_runtime() {
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
