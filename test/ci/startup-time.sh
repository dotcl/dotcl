#!/bin/sh
# bench/ledger/startup-time.lisp: the one STARTUP line, and the failures it reports.
# Needs a built runtime and compiler/dotcl.core (make build compile-core-fasl).
set -u
ROOT="$(cd "${1%/}" && pwd)"
RT="${DOTCL_RUNTIME:-$ROOT/runtime/bin/Debug/net10.0/runtime.dll}"
run() { dotnet "$RT" --core "$ROOT/compiler/dotcl.core" "$ROOT/bench/ledger/startup-time.lisp" "$@" < /dev/null; }

out=$(run 3 sleep 0.2 2>&1) || { echo "startup-time: sleep failed: $out"; exit 1; }
# the runtime may print warnings of its own (stale contrib fasls and the like) before the result
out=$(echo "$out" | grep '^STARTUP ' | tail -1)
echo "$out" | grep -Eq '^STARTUP [0-9]+\.[0-9] [0-9]+\.[0-9] [0-9]+\.[0-9] 3$' \
    || { echo "startup-time: unexpected output: $out"; exit 1; }
median=$(echo "$out" | awk '{ print int($2) }')
[ "$median" -ge 190 ] && [ "$median" -le 400 ] || { echo "startup-time: median $median ms for sleep 0.2"; exit 1; }

STARTUP_EXPECT=hello run 2 sh -c 'echo hello' > /dev/null 2>&1 || { echo "startup-time: expected output not accepted"; exit 1; }
STARTUP_EXPECT=zzz run 2 sh -c 'echo hello' > /dev/null 2>&1 && { echo "startup-time: missing expected output accepted"; exit 1; }
run 2 sh -c 'exit 3' > /dev/null 2>&1 && { echo "startup-time: failing command accepted"; exit 1; }
run 0 sleep 0 > /dev/null 2>&1 && { echo "startup-time: count 0 accepted"; exit 1; }
echo "startup-time: ok"
