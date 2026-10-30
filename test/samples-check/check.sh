#!/bin/sh
# Usage: check.sh <repo-root>; DOTCL_SAMPLES_AOT=0 skips the native AOT sample.
set -eu

ROOT="${1%/}"
ROOT="$(cd "$ROOT" && pwd)"

for f in compiler/cil-out.sil compiler/dotcl.core; do
  if [ ! -f "$ROOT/$f" ]; then
    echo "missing $ROOT/$f (run 'make cross-compile compile-core-fasl' first)" >&2
    exit 1
  fi
done
for cmd in curl dotnet mkfifo timeout; do
  command -v "$cmd" >/dev/null 2>&1 || { echo "missing required command: $cmd" >&2; exit 1; }
done

export CL_SOURCE_REGISTRY='(:source-registry :ignore-inherited-configuration)'

WORK="$(cd "$(mktemp -d)" && pwd -P)"
PIDS=""
TOTAL=0
FAIL=0
SAMPLE_FAIL=0

note() { echo "  FAIL: $1"; SAMPLE_FAIL=1; }
dump_log() { [ -f "$1" ] && { echo "  --- $1 (tail) ---"; tail -10 "$1" | sed 's/^/  /'; }; }

stop_pid() {
  p="$1"
  [ -n "$p" ] || return 0
  if kill -0 "$p" 2>/dev/null; then
    kill "$p" 2>/dev/null || true
    i=0
    while kill -0 "$p" 2>/dev/null; do
      i=$((i + 1))
      if [ "$i" -ge 50 ]; then
        kill -9 "$p" 2>/dev/null || true
        break
      fi
      sleep 0.1
    done
  fi
  wait "$p" 2>/dev/null || true
  rest=""
  for x in $PIDS; do [ "$x" = "$p" ] || rest="$rest $x"; done
  PIDS="$rest"
}

cleanup() {
  exec 3>&- 2>/dev/null || true
  for p in $PIDS; do stop_pid "$p"; done
  rm -rf "$WORK"
}
trap cleanup EXIT INT TERM HUP

wait_for_pattern() { # file fixed-string seconds
  f="$1"; pat="$2"; n=$(( ${3} * 5 )); i=0
  while [ "$i" -lt "$n" ]; do
    if [ -f "$f" ] && grep -qF -- "$pat" "$f"; then return 0; fi
    sleep 0.2
    i=$((i + 1))
  done
  return 1
}

wait_for_http() { # url output-file seconds
  url="$1"; out="$2"; n=$(( ${3} * 2 )); i=0
  while [ "$i" -lt "$n" ]; do
    if curl -sf --max-time 2 "$url" -o "$out" 2>/dev/null; then return 0; fi
    sleep 0.5
    i=$((i + 1))
  done
  return 1
}

tree_version() {
  sed -n 's:.*<Version>\([^<]*\)</Version>.*:\1:p' "$ROOT/runtime/Directory.Build.props" | head -1
}

require_line() { # file fixed-string description
  grep -qF -- "$2" "$1" || note "$3"
}

run_aspnet() {
  src="$WORK/aspnet"
  feed="$WORK/aspnet-feed"
  pkg="$WORK/aspnet-pkg"
  mkdir -p "$feed" "$pkg"

  cp -R "$ROOT/runtime" "$pkg/runtime"
  cp -R "$ROOT/runtime.docgen" "$pkg/runtime.docgen"
  cp "$ROOT/README.md" "$pkg/README.md"
  mkdir -p "$pkg/scripts"
  cp "$ROOT/scripts/UnicodeData.props" "$pkg/scripts/UnicodeData.props"
  rm -rf "$pkg/runtime/bin" "$pkg/runtime/obj" "$pkg/runtime/contrib" \
         "$pkg/runtime.docgen/bin" "$pkg/runtime.docgen/obj" \
         "$pkg/runtime/build-tasks/bin" "$pkg/runtime/build-tasks/obj"
  cp "$ROOT/compiler/dotcl.core" "$pkg/runtime/dotcl.core"
  mkdir -p "$pkg/runtime/contrib"
  for d in "$ROOT"/contrib/*/; do cp -R "${d%/}" "$pkg/runtime/contrib/"; done
  rm -f "$pkg/runtime/contrib/asdf/asdf.lisp" "$pkg/runtime/contrib/asdf/asdf.sil" \
        "$pkg/runtime/contrib/quicklisp/quicklisp.lisp"

  if ! timeout 600 dotnet pack "$pkg/runtime/DotCL.Runtime.csproj" -c Release -o "$feed" \
       --nologo >"$WORK/aspnet-pack.log" 2>&1; then
    note "packing this tree's DotCL.Runtime failed"
    dump_log "$WORK/aspnet-pack.log"
    return 1
  fi
  ver="$(tree_version)"
  if [ -z "$ver" ]; then note "cannot read the tree version"; return 1; fi

  cp -R "$ROOT/samples/AspNetLispDemo" "$src"
  rm -rf "$src/bin" "$src/obj"
  sed "s:Include=\"DotCL.Runtime\" Version=\"[^\"]*\":Include=\"DotCL.Runtime\" Version=\"$ver\":" \
    "$src/AspNetLispDemo.csproj" >"$WORK/aspnet.csproj"
  mv "$WORK/aspnet.csproj" "$src/AspNetLispDemo.csproj"
  grep -qF "Include=\"DotCL.Runtime\" Version=\"$ver\"" "$src/AspNetLispDemo.csproj" \
    || { note "could not point the sample at DotCL.Runtime $ver"; return 1; }

  cat >"$src/nuget.config" <<EOF
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="local" value="$feed" />
  </packageSources>
</configuration>
EOF

  if ! (cd "$src" && NUGET_PACKAGES="$WORK/aspnet-nuget" timeout 600 \
          dotnet build --nologo >"$WORK/aspnet-build.log" 2>&1); then
    note "building AspNetLispDemo against this tree failed"
    dump_log "$WORK/aspnet-build.log"
    return 1
  fi

  dll="$src/bin/Debug/net10.0/AspNetLispDemo.dll"
  if [ ! -f "$dll" ]; then note "no AspNetLispDemo.dll"; return 1; fi

  dotnet "$dll" < /dev/null >"$WORK/aspnet-run.log" 2>&1 &
  pid=$!
  PIDS="$PIDS $pid"
  if ! wait_for_pattern "$WORK/aspnet-run.log" "[aspnet] running on http://localhost:5180" 60; then
    note "AspNetLispDemo did not start"
    stop_pid "$pid"
    dump_log "$WORK/aspnet-run.log"
    return 1
  fi

  hello="$WORK/hello.txt"
  got_hello=0
  if wait_for_http "http://localhost:5180/api/hello" "$hello" 30; then
    value="$(tr -d '\r' <"$hello")"
    if [ "$value" != "hello from lisp" ]; then
      note "/api/hello returned $value, not 'hello from lisp'"
    else
      got_hello=1
    fi
  else
    note "/api/hello did not answer"
  fi

  async="$WORK/async.txt"
  got_async=0
  if curl -sf --max-time 10 "http://localhost:5180/api/async-hello" -o "$async" 2>/dev/null; then
    value="$(tr -d '\r' <"$async")"
    if [ "$value" != "hello from async lisp" ]; then
      note "/api/async-hello returned $value, not 'hello from async lisp'"
    else
      got_async=1
    fi
  else
    note "/api/async-hello did not answer"
  fi

  stop_pid "$pid"
  [ "$got_hello" = 1 ] && [ "$got_async" = 1 ] || dump_log "$WORK/aspnet-run.log"
  [ "$SAMPLE_FAIL" -eq 0 ]
}

run_hot() {
  src="$ROOT/samples/HotReloadDemo"
  rm -rf "$src/bin" "$src/obj"
  if ! timeout 600 dotnet build "$src/HotReloadDemo.csproj" -c Debug --nologo \
       >"$WORK/hot-build.log" 2>&1; then
    note "building HotReloadDemo failed"
    dump_log "$WORK/hot-build.log"
    return 1
  fi
  out="$src/bin/Debug/net10.0"
  cp "$ROOT/compiler/cil-out.sil" "$out/dotcl.core"

  run="$WORK/hot"
  mkdir -p "$run"
  cp "$src/handlers.lisp" "$run/handlers.lisp"

  oldpwd="$(pwd)"
  cd "$run"
  dotnet "$out/HotReloadDemo.dll" < /dev/null >"$WORK/hot-run.log" 2>&1 &
  pid=$!
  cd "$oldpwd"
  PIDS="$PIDS $pid"

  if ! wait_for_pattern "$WORK/hot-run.log" "request 2 -> hello from Lisp, request #2" 30; then
    note "HotReloadDemo did not get to request 2"
    stop_pid "$pid"
    dump_log "$WORK/hot-run.log"
    return 1
  fi

  sed 's/hello from Lisp/HELLO AGAIN/' "$run/handlers.lisp" >"$run/handlers.new"
  cat "$run/handlers.new" >"$run/handlers.lisp"
  rm -f "$run/handlers.new"

  if ! wait_for_pattern "$WORK/hot-run.log" "request 3 -> HELLO AGAIN, request #3" 30; then
    note "request 3 did not show the rewritten handler"
    stop_pid "$pid"
    dump_log "$WORK/hot-run.log"
    return 1
  fi

  stop_pid "$pid"
  [ "$SAMPLE_FAIL" -eq 0 ]
}

run_mcp() {
  src="$ROOT/samples/McpServerDemo"
  rm -rf "$src/bin" "$src/obj"
  if ! timeout 600 dotnet build "$src/McpServerDemo.csproj" -c Release --nologo \
       >"$WORK/mcp-build.log" 2>&1; then
    note "building McpServerDemo failed"
    dump_log "$WORK/mcp-build.log"
    return 1
  fi
  dll="$src/bin/Release/net10.0/McpServerDemo.dll"
  [ -f "$dll" ] || { note "no McpServerDemo.dll"; return 1; }

  fifo="$WORK/mcp.in"
  out="$WORK/mcp.out"
  err="$WORK/mcp.err"
  rm -f "$fifo"
  mkfifo "$fifo"
  exec 3<>"$fifo"
  dotnet "$dll" <"$fifo" >"$out" 2>"$err" &
  pid=$!
  PIDS="$PIDS $pid"

  printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"check","version":"1"}}}' >&3
  if ! wait_for_pattern "$out" '"id":1' 60; then
    note "MCP initialize got no response"
    exec 3>&- 2>/dev/null || true
    stop_pid "$pid"
    dump_log "$err"
    return 1
  fi

  printf '%s\n' '{"jsonrpc":"2.0","method":"notifications/initialized"}' >&3
  printf '%s\n' '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"lisp_eval","arguments":{"code":"(+ 1 2 3 4 5)"}}}' >&3
  if ! wait_for_pattern "$out" '"id":2' 60; then
    note "MCP tools/call got no response"
    exec 3>&- 2>/dev/null || true
    stop_pid "$pid"
    dump_log "$err"
    return 1
  fi

  exec 3>&- 2>/dev/null || true
  stop_pid "$pid"

  if grep -F '"id":2' "$out" | grep -qF '"text":"15"'; then
    :
  else
    note "lisp_eval (+ 1 2 3 4 5) did not return \"15\""
    dump_log "$out"
  fi
  [ "$SAMPLE_FAIL" -eq 0 ]
}

run_precompiled() {
  src="$ROOT/samples/PrecompiledLispDemo"
  rm -rf "$src/bin" "$src/obj"
  if ! timeout 600 dotnet build "$src/PrecompiledLispDemo.csproj" -c Debug --nologo \
       >"$WORK/precompiled-build.log" 2>&1; then
    note "building PrecompiledLispDemo failed"
    dump_log "$WORK/precompiled-build.log"
    return 1
  fi
  dll="$src/bin/Debug/net10.0/PrecompiledLispDemo.dll"
  [ -f "$dll" ] || { note "no PrecompiledLispDemo.dll"; return 1; }

  if ! timeout 120 dotnet "$dll" < /dev/null >"$WORK/precompiled-run.log" 2>&1; then
    note "running PrecompiledLispDemo failed or timed out"
    dump_log "$WORK/precompiled-run.log"
    return 1
  fi

  require_line "$WORK/precompiled-run.log" "fib(20)          = 6765" "fib(20) is not 6765"
  require_line "$WORK/precompiled-run.log" "sum-squares(100) = 328350" "sum-squares(100) is not 328350"
  require_line "$WORK/precompiled-run.log" "eval blocked as expected" "eval was not blocked"
  [ "$SAMPLE_FAIL" -eq 0 ]
}

run_aot() {
  src="$ROOT/samples/PrecompiledLispDemoAot"
  rm -rf "$src/bin" "$src/obj"
  if ! (cd "$src" && timeout 900 sh publish.sh >"$WORK/aot-publish.log" 2>&1); then
    note "native AOT publish failed or timed out"
    dump_log "$WORK/aot-publish.log"
    return 1
  fi

  exe="$(find "$src/bin/Release" -type f \( -name PrecompiledLispDemoAot -o -name PrecompiledLispDemoAot.exe \) -path '*/publish/*' 2>/dev/null | head -1)"
  if [ -z "$exe" ]; then note "no native AOT binary was produced"; return 1; fi

  if ! timeout 120 "$exe" < /dev/null >"$WORK/aot-run.log" 2>&1; then
    note "running the native AOT binary failed or timed out"
    dump_log "$WORK/aot-run.log"
    return 1
  fi

  require_line "$WORK/aot-run.log" "IsDynamicCodeSupported = False" "the AOT binary reports dynamic code support"
  require_line "$WORK/aot-run.log" "fib(20)          = 6765" "fib(20) is not 6765"
  require_line "$WORK/aot-run.log" "sum-squares(100) = 328350" "sum-squares(100) is not 328350"
  require_line "$WORK/aot-run.log" "greet len        = 5" "greet length is not 5"
  require_line "$WORK/aot-run.log" "defun cube; (cube 5)          = 125" "emit-free eval did not run CUBE"
  require_line "$WORK/aot-run.log" "Done; precompiled Lisp + emit-free eval" "the AOT binary did not finish"
  [ "$SAMPLE_FAIL" -eq 0 ]
}

run_sample() { # name function
  name="$1"; fn="$2"
  echo "=== $name ==="
  SAMPLE_FAIL=0
  start="$(date +%s)"
  if "$fn"; then :; else SAMPLE_FAIL=1; fi
  secs=$(( $(date +%s) - start ))
  TOTAL=$(( TOTAL + secs ))
  if [ "$SAMPLE_FAIL" -eq 0 ]; then
    echo "PASS $name (${secs}s)"
  else
    echo "FAIL $name (${secs}s)"
    FAIL=1
  fi
}

run_sample AspNetLispDemo run_aspnet
run_sample HotReloadDemo run_hot
run_sample McpServerDemo run_mcp
run_sample PrecompiledLispDemo run_precompiled

aot="${DOTCL_SAMPLES_AOT:-1}"
case "$aot" in
  0|false|no|off) aot=0 ;;
  *) aot=1 ;;
esac
if [ "$aot" = 1 ]; then
  run_sample PrecompiledLispDemoAot run_aot
else
  echo "=== PrecompiledLispDemoAot ==="
  echo "SKIP PrecompiledLispDemoAot (DOTCL_SAMPLES_AOT=0)"
fi

echo "TOTAL ${TOTAL}s"
if [ "$FAIL" -ne 0 ]; then
  echo "samples: FAIL"
  exit 1
fi
echo "samples: OK"
