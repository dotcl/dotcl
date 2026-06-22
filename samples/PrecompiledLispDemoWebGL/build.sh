#!/usr/bin/env sh
# Cross-platform (Linux / macOS / Windows-git-bash) one-shot: stage the three
# plugins, then run a headless Unity WebGL build. Output lands in Build/
# (index.html + Build/*.unityweb,*.js). Serve Build/ over HTTP and open it
# (file:// won't load wasm); any static server works (decompressionFallback).
#
# The only per-OS difference is the Unity editor path, resolved below from the
# project's pinned version (override with UNITY=...). Everything else
# (dotnet, the Unity -batchmode flags) is identical across platforms.
#
# Override the editor location with:  UNITY=/path/to/Unity ./build.sh
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"

# Pinned editor version (so the path doesn't hardcode it).
VER="$(sed -n 's/^m_EditorVersion: //p' "$HERE/ProjectSettings/ProjectVersion.txt" | tr -d '\r')"

if [ -n "${UNITY:-}" ]; then
  UNITY_BIN="$UNITY"
else
  case "$(uname -s)" in
    Darwin)               UNITY_BIN="/Applications/Unity/Hub/Editor/$VER/Unity.app/Contents/MacOS/Unity" ;;
    Linux)                UNITY_BIN="$HOME/Unity/Hub/Editor/$VER/Editor/Unity" ;;
    MINGW*|MSYS*|CYGWIN*) UNITY_BIN="/c/Program Files/Unity/Hub/Editor/$VER/Editor/Unity.exe" ;;
    *) echo "Unknown OS '$(uname -s)'. Set UNITY=/path/to/Unity and retry." >&2; exit 1 ;;
  esac
fi

if [ ! -x "$UNITY_BIN" ] && [ ! -f "$UNITY_BIN" ]; then
  echo "Unity editor $VER not found at:" >&2
  echo "  $UNITY_BIN" >&2
  echo "Install it via Unity Hub (with WebGL Build Support), or set UNITY=/path/to/Unity." >&2
  exit 1
fi

sh "$HERE/prepare-plugins.sh"

"$UNITY_BIN" -batchmode -nographics -quit \
  -projectPath "$HERE" \
  -buildTarget WebGL \
  -executeMethod BuildWebGL.Build \
  -logFile "$HERE/build.log"
RC=$?
echo "Unity exit code: $RC  (see build.log)"
exit $RC
