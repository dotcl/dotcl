#!/bin/sh
# Two versions of swank in one dotcl image, each on its own port, each driven
# by its own version of SLIME at the same time. Run setup.sh first.
#   test/package-units-swank/run.sh [units|global]
# Needs emacs. Exits 0 when both clients see their own version everywhere.
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=${ROOT:-$PWD/out/package-units-swank}
ORDER=${1:-units}
DOTCL=${DOTCL:-runtime/bin/Debug/net10.0/runtime --asm compiler/cil-out.sil}
rm -f "$ROOT/server.ready"
ROOT=$ROOT ORDER=$ORDER $DOTCL "$HERE/server.lisp" < /dev/null > "$ROOT/server-$ORDER.log" 2>&1 &
PID=$!
n=0
until [ -f "$ROOT/server.ready" ] || [ $n -ge 600 ] || ! kill -0 $PID 2>/dev/null; do sleep 2; n=$((n+2)); done
[ -f "$ROOT/server.ready" ] || { echo "server did not start"; tail -20 "$ROOT/server-$ORDER.log"; kill $PID 2>/dev/null; exit 1; }
for v in 231 new; do
  port=47471; [ $v = new ] && port=47472
  SLIME_DIR=$ROOT/slime-$v PORT=$port PROBE=$HERE/probe.lisp \
    timeout 300 emacs --batch -Q -l "$HERE/client.el" < /dev/null > "$ROOT/client-$ORDER-$v.log" 2>&1 &
  eval "C_$v=\$!"
done
wait $C_231; wait $C_new
kill $PID 2>/dev/null
ok=0
check() { # log version find-cycle casify-char
  log=$ROOT/client-$ORDER-$1.log
  grep '=>' "$log"
  grep -q "^version => \"$2\"" "$log" || { echo "FAIL $1: version"; ok=1; }
  grep -q '^SLDB =>\|ERROR' "$log" && { echo "FAIL $1: error"; ok=1; }
  grep -q '^repl => "3"' "$log" || { echo "FAIL $1: repl"; ok=1; }
  for w in worker repl thread; do
    grep "^probe $w => " "$log" | grep -qF "swank=SWANK version=$2 find-cycle=$3 casify-char=$4 swanks=1 read-back=T" \
      || { echo "FAIL $1: probe $w"; ok=1; }
  done
}
check 231 2.31 NIL T
check new 2.32.git T NIL
[ $ok = 0 ] && echo "PASS ($ORDER)"
exit $ok
