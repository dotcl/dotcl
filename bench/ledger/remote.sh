#!/bin/bash
# Run the ledger on another machine from this tree's current commit.
#
#   bench/ledger/remote.sh MACHINE [run.sh subcommand]      (default: all)
#
# MACHINE is a row of machines.tsv (name, ssh host, work dir, lock file). The
# script sends HEAD as a git bundle, checks it out in <work dir>/tree, builds the
# Release runtime and dotcl.core there (unless LEDGER_NOBUILD=1), runs run.sh with
# the baseline kept in <work dir>/baseline/<name>.tsv, and copies the output back
# to out/ledger-<name>/ here (LEDGER_DEST overrides). A baseline saved on the
# machine (LEDGER_SAVE=1) is copied to bench/ledger/baseline/<name>.tsv so it can
# be committed. LEDGER_BASELINE names another local file to use instead, both as
# the previous run sent to the machine and as the place the new one is saved.
#
# LEDGER_ROUNDS, LEDGER_STARTUP_COUNT, LEDGER_ONLY, LEDGER_SAVE, LEDGER_REG_PCT, LEDGER_APPEND and the
# LEDGER_* threshold and steady-detection variables are passed on.
# LEDGER_PUBLISH=1 replaces the ledger issue body from here, after the copy back
# (gh is run on this machine, not on the measuring one).
set -eu
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
name=${1:?usage: remote.sh MACHINE [subcommand]}
sub=${2:-all}
row=$(awk -F'\t' -v n="$name" '$1 == n' "$here/machines.tsv")
[ -n "$row" ] || { echo "no machine $name in machines.tsv" >&2; exit 1; }
host=$(echo "$row" | cut -f2); wd=$(echo "$row" | cut -f3); lock=$(echo "$row" | cut -f4)
base=${LEDGER_BASELINE:-$here/baseline/$name.tsv}
rev=$(git -C "$root" rev-parse HEAD)
bundle=$(mktemp "${TMPDIR:-/tmp}/ledger-XXXXXX")
trap 'rm -f "$bundle"' EXIT
git -C "$root" bundle create "$bundle" HEAD 2> /dev/null

ssh "$host" "mkdir -p $wd/baseline"
# The committed baseline is the previous run to compare with; send it so the machine's copy is not stale or missing.
[ -f "$base" ] && scp -q "$base" "$host:$wd/baseline/$name.tsv"
scp -q "$bundle" "$host:$wd/tree.bundle"
ssh "$host" "set -e; cd $wd
  [ -d tree ] || git init -q tree
  cd tree && git fetch -q ../tree.bundle HEAD && git checkout -q -f --detach FETCH_HEAD && git clean -q -f -d -e out -e contrib/asdf"
if [ "${LEDGER_NOBUILD:-0}" != 1 ]; then
  ssh "$host" "cd $wd/tree && export UseSharedCompilation=false MSBUILDDISABLENODEREUSE=1 DOTNET_CLI_USE_MSBUILD_SERVER=0
    ${lock:+flock $lock} bash -c 'make setup-asdf cross-compile compile-asdf-fasl compile-quicklisp-fasl < /dev/null > $wd/build.log 2>&1 && make bench-build compile-core-fasl < /dev/null >> $wd/build.log 2>&1'"
fi
pass=""
for v in LEDGER_ROUNDS LEDGER_STARTUP_COUNT LEDGER_ONLY LEDGER_SAVE LEDGER_REG_PCT LEDGER_APPEND \
         LEDGER_COLD_MAX LEDGER_STEADY_MAX LEDGER_STEADY_STRONG \
         LEDGER_STEADY_TOLERANCE LEDGER_STEADY_WINDOW \
         LEDGER_STEADY_MIN_RUNS LEDGER_STEADY_MIN_SECONDS \
         LEDGER_STEADY_MAX_RUNS LEDGER_STEADY_MAX_SECONDS; do
  eval "val=\${$v:-}"; [ -n "$val" ] && pass="$pass $v='$val'"
done
ssh "$host" "cd $wd/tree && LEDGER_MACHINE=$name LEDGER_LOCK=$lock LEDGER_WORK=$wd/work LEDGER_PREV=$wd/baseline/$name.tsv$pass bench/ledger/run.sh $sub"

dest=${LEDGER_DEST:-$root/out/ledger-$name}
mkdir -p "$dest"
scp -q -r "$host:$wd/work/out/." "$dest/"
if [ "${LEDGER_SAVE:-0}" = 1 ]; then
  mkdir -p "$(dirname "$base")"
  scp -q "$host:$wd/baseline/$name.tsv" "$base"
fi
echo "results: $dest/results.md (commit $rev)"
if [ "${LEDGER_PUBLISH:-0}" = 1 ]; then
  LEDGER_OUT=$dest LEDGER_WORK=$dest "$here/run.sh" publish
fi
