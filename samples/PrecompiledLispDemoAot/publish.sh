#!/bin/sh
# Publish this sample as a NativeAOT native binary. Cross-platform:
# Linux / macOS / Windows (git-bash). The RID is picked from the host OS+arch;
# override with e.g.  RID=linux-arm64 ./publish.sh
#
# NativeAOT needs a C toolchain for the *target*:
#   Linux   - clang + zlib headers           (e.g. apt install clang zlib1g-dev)
#   macOS   - Xcode command line tools        (xcode-select --install)
#   Windows - Visual Studio with the "Desktop development with C++" workload
#             (ILC locates MSVC via vswhere; no manual vcvars needed when the
#              workload is installed)
#
# The csproj's BuildNs2Runtime + PrecompileLisp targets run automatically during
# publish, regenerating the emit-free runtime DLL and the stable-named Lisp
# images (dotclcore.dll / appfasl.dll) before ILC links them in.
set -e
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

if [ -z "$RID" ]; then
  os=$(uname -s)
  case "$os" in
    MINGW*|MSYS*|CYGWIN*)
      # git-bash often runs as an emulated x64 process on ARM64 Windows, so
      # neither `uname -m` nor the PROCESSOR_ARCHITECTURE* env vars reveal the
      # true OS arch (both report x64). The machine-level registry value does.
      raw=$(reg query "HKLM\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment" //v PROCESSOR_ARCHITECTURE 2>/dev/null \
            | tr -d '\r' | sed -n 's/.*REG_SZ[[:space:]]*//p')
      [ -n "$raw" ] || raw=${PROCESSOR_ARCHITEW6432:-$PROCESSOR_ARCHITECTURE} ;;
    *) raw=$(uname -m) ;;
  esac
  case "$raw" in
    x86_64|amd64|AMD64) arch=x64 ;;
    arm64|aarch64|ARM64) arch=arm64 ;;
    *) arch=$raw ;;
  esac
  case "$os" in
    Linux)                 RID=linux-$arch ;;
    Darwin)                RID=osx-$arch ;;
    MINGW*|MSYS*|CYGWIN*)  RID=win-$arch ;;
    *) echo "Unknown OS '$os'; set RID=... explicitly." >&2; exit 1 ;;
  esac
fi

echo "Publishing NativeAOT for $RID ..."
dotnet publish "$HERE/PrecompiledLispDemoAot.csproj" -r "$RID" -c Release -p:PublishAot=true
echo "-> bin/$arch/Release/net10.0/$RID/publish/  (native binary)"
