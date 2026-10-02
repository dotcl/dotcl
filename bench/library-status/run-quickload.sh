#!/bin/sh
# Stage 2 of the library-status pipeline: try to QUICKLOAD every system in
# targets.txt with dotcl, one at a time, and write results.json.
#
# The rules this follows are the ones in README.md, and they are there because
# each has bitten:
#
#   - One system per process, in sequence. Two dotcl processes measuring at once
#     stop being comparable and make the machine unusable.
#   - Each system is bounded (`timeout`), not the run. A system that exceeds the
#     bound is recorded as a failure that timed out -- a real answer about that
#     system rather than a run that never ends.
#   - Only the process this script started is ever stopped. Never by name: other
#     work on the machine runs the same executable.
#   - The built runtime is invoked directly rather than through `dotnet run`, so
#     no build check runs between systems and nothing can rebuild mid-run.
#
# results.json is rewritten after every system, so it is valid JSON at every
# moment and an interrupted run leaves usable results. LIBRARY_STATUS_RESUME=1
# then continues where it stopped: the run takes tens of minutes and a session
# that ends in the middle of it should not cost the systems already measured.
#
# Usage:  run-quickload.sh [repo-root]
#   LIMIT=N                  stop after N systems (a smoke test: LIMIT=1)
#   LIBRARY_STATUS_RESUME=1  skip systems already recorded in the output
#   LIBRARY_STATUS_TIMEOUT   seconds per system (default 300)
#   LIBRARY_STATUS_RETRY     seconds for the one retry a timeout gets (default 900)
#   LIBRARY_STATUS_TARGETS   targets.txt
#   LIBRARY_STATUS_JSON      output results.json
#   LIBRARY_STATUS_LOGDIR    per-system output, kept for diagnosis
set -eu

root="${1:-.}"
# Note filters (scrub_note), shared with the other stage.
. "$(dirname "$0")/scrub.sh"
targets="${LIBRARY_STATUS_TARGETS:-$root/bench/library-status/targets.txt}"
out="${LIBRARY_STATUS_JSON:-$root/bench/library-status/results.json}"
logdir="${LIBRARY_STATUS_LOGDIR:-/tmp/library-status-logs}"
per="${LIBRARY_STATUS_TIMEOUT:-300}"
retry="${LIBRARY_STATUS_RETRY:-900}"
limit="${LIMIT:-0}"
resume="${LIBRARY_STATUS_RESUME:-0}"

exe="$root/runtime/bin/Debug/net10.0/runtime.exe"
[ -x "$exe" ] || exe="$root/runtime/bin/Debug/net10.0/runtime"
[ -x "$exe" ] || { echo "run-quickload: no built runtime; run make build first" >&2; exit 1; }
core="$root/compiler/cil-out.sil"
# GNU timeout bounds each run. macOS has none by default; Homebrew coreutils
# installs it as gtimeout. Without either, every run would fail with 127 and
# be recorded as a library failure, so stop here instead.
if command -v timeout >/dev/null 2>&1; then timeout=timeout
elif command -v gtimeout >/dev/null 2>&1; then timeout=gtimeout
else echo "run-quickload: needs GNU timeout (coreutils; gtimeout on macOS)" >&2; exit 1
fi
[ -f "$core" ] || { echo "run-quickload: no $core; run make cross-compile first" >&2; exit 1; }
[ -f "$targets" ] || { echo "run-quickload: no targets at $targets" >&2; exit 1; }

checked=$(date +%Y-%m-%d)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$logdir"

# One JSON string: the few characters that would break it are removed rather
# than escaped, because these fields are one short line of prose, not a
# transcript. Also drops the driver's "<path>:<line>: " prefix and the loader's
# progress dots, neither of which says anything about the library.
jstr() {
  printf '%s' "$1" \
    | tr -d '"\\' | tr '\n\r\t' '   ' \
    | sed 's|^[. ]*||; s|^[^ ]*drv\.lisp:[0-9]*: ||; s|  *| |g; s| *$||' \
    | scrub_note \
    | cut -c1-160
}

: > "$work/entries"
if [ "$resume" = "1" ] && [ -f "$out" ]; then
  # Entries are one per line, so resuming is a matter of keeping the lines that
  # are already there and skipping those system names below. The stored lines
  # carry the comma that separates them; strip it, or every resume appends
  # another one and the file stops being JSON.
  grep '^  {"system"' "$out" | sed 's/,*$//' > "$work/entries" || true
  sed -n 's/^  {"system": "\([^"]*\)".*/\1/p' "$work/entries" | sort -u > "$work/done"
else
  : > "$work/done"
fi

write_json() {
  {
    echo '['
    sed '$!s/$/,/' "$work/entries"
    echo ']'
  } > "$out"
}

n=0
ran=0
skipped=0
started=$(date +%s)
write_json

for sys in $(grep -v '^#' "$targets" | grep -v '^[[:space:]]*$'); do
  if grep -qx "$sys" "$work/done" 2>/dev/null; then
    skipped=$((skipped + 1))
    continue
  fi
  n=$((n + 1))
  if [ "$limit" -gt 0 ] && [ "$n" -gt "$limit" ]; then n=$((n - 1)); break; fi

  # The driver is written per system so the name is a literal: quoting a name
  # through --eval is one more thing to get wrong per system.
  # QL packages do not exist until the client is required, and LOAD reads one
  # form at a time, so the QL: forms below are read after that has happened.
  #
  # QUICKLOAD is wrapped because an unhandled error inside it enters the
  # debugger, and with stdin closed the debugger returns rather than stopping:
  # the forms after it still run and print LIBSTATUS-OK, so a library that did
  # not load is recorded as one that did. Handling the error here is also the
  # definition wanted -- "quickload signaled and nothing handled it" is what a
  # batch load failure is.
  cat > "$work/drv.lisp" <<DRVEOF
(require "quicklisp")
;; Which dists this home has, and in what order. A row measured in a home
;; without the dotcl overlay looks like a library failure (swank did).
(format t "~&LIBSTATUS-DISTS ~{~a~^ ~}~%"
        (mapcar (lambda (d) (format nil "~a=~a" (ql-dist:name d) (ql-dist:version d)))
                (ql-dist:enabled-dists)))
(handler-case (ql:quickload "$sys")
  (error (e)
    (format t "~&LIBSTATUS-FAIL ~a~%" e)
    (finish-output)
    (dotcl:quit 1)))
;; Which dist answered decides load-only vs patched: the dotcl overlay carries
;; patched releases for the libraries that need a change to run here. PREFIX is
;; the versioned release name ("alexandria-20241012-git"); NAME is the project.
(let* ((s (ql-dist:find-system "$sys"))
       (r (and s (ql-dist:release s))))
  (format t "~&LIBSTATUS-RELEASE ~a~%"
          (if r (or (ignore-errors (ql-dist:prefix r)) (ql-dist:name r)) "?"))
  (format t "~&LIBSTATUS-DIST ~a~%"
          (if r (ql-dist:name (ql-dist:dist r)) "?")))
(format t "~&LIBSTATUS-OK~%")
(finish-output)
(dotcl:quit 0)
DRVEOF

  # A slash in a system name ("foo/test") is not a directory here.
  log="$logdir/$(printf '%s' "$sys" | tr / _).txt"
  t0=$(date +%s)
  set +e
  "$timeout" "$per" "$exe" --asm "$core" "$work/drv.lisp" > "$log" 2>&1 < /dev/null
  code=$?
  set -e
  retried=""
  # A timeout is the one outcome that is not an answer about the library: it is
  # the bound speaking. With a cold ASDF cache every dependency is compiled from
  # source, and systems that finish in seconds afterwards take minutes the first
  # time (postmodern 86s, sdl2 336s, measured). Give exactly those one longer
  # run, so "fail: timeout" means the library really does not finish.
  if [ "$code" -eq 124 ]; then
    retried=" (retried)"
    set +e
    "$timeout" "$retry" "$exe" --asm "$core" "$work/drv.lisp" > "$log" 2>&1 < /dev/null
    code=$?
    set -e
  fi
  t1=$(date +%s)
  elapsed=$((t1 - t0))

  release=$(sed -n 's/^LIBSTATUS-RELEASE //p' "$log" | tail -1)
  dist=$(sed -n 's/^LIBSTATUS-DIST //p' "$log" | tail -1)
  note=""

  # The debugger banner is checked as well as the exit code: a nested debugger
  # that returns on end-of-input leaves the run looking successful.
  if [ "$code" -eq 0 ] && grep -q '^LIBSTATUS-OK' "$log" \
       && ! grep -q '^; Debugger entered on' "$log"; then
    if [ "$dist" = "dotcl" ]; then
      status="patched"
      note="loaded from the dotcl dist overlay"
    else
      status="load-only"
    fi
  elif [ "$code" -eq 124 ]; then
    status="fail"
    note="timeout after ${retry}s${retried}"
  else
    status="fail"
    # What the driver caught, when it got that far. The report can wrap over
    # several lines, so the whole block is taken and joined.
    note=$(sed -n '/^LIBSTATUS-FAIL /,$p' "$log" | sed '1s/^LIBSTATUS-FAIL //' | tr '\n' ' ')
    # ASDF wraps the real cause inside "Error while trying to load definition
    # for system X from pathname <path>: <cause>". The cause is the part worth
    # recording, and the path is what pushes it past the width of a note.
    case "$note" in
      *.asd:*) note=$(printf '%s' "$note" | sed 's|.*/\([^/]*\.asd\): *|\1: |') ;;
    esac
    # Then the debugger banner, which names the condition on one line and its
    # report on the next.
    [ -n "$note" ] || note=$(sed -n 's/^; Debugger entered on //p; s/^;   //p' "$log" \
                               | head -2 | tr '\n' ' ')
    # The FIRST line naming a condition: what went wrong, rather than the last
    # frame of the stack that followed it. Stack frames themselves are dropped.
    [ -n "$note" ] || note=$(grep -a -E '(Unbound variable|Undefined function|Undeclared local|not found|Unhandled exception|[A-Za-z.]+(Error|Exception|Condition)[: ])' "$log" \
             | grep -a -v '^[[:space:]]*at ' | head -1)
    [ -n "$note" ] || note=$(grep -a -v '^[[:space:]]*$' "$log" | tail -1)
  fi

  release_json=null
  [ -n "$release" ] && [ "$release" != "?" ] && release_json="\"$(jstr "$release")\""

  printf '  {"system": "%s", "release": %s, "status": "%s", "issue": null, "checked": "%s", "note": "%s"}\n' \
    "$sys" "$release_json" "$status" "$checked" "$(jstr "${note:-}")" >> "$work/entries"
  write_json
  ran=$((ran + 1))

  printf '%3d %-34s %-10s %3ds %s\n' "$ran" "$sys" "$status" "$elapsed" "$(jstr "${note:-}")"
done

finished=$(date +%s)

echo ""
echo "run-quickload: $ran systems in $((finished - started))s ($skipped already recorded) -> $out"
for s in load-only patched fail; do
  printf '  %-10s %s\n' "$s" "$(grep -c "\"status\": \"$s\"" "$work/entries" || true)"
done
echo "  logs       $logdir"
