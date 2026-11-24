#!/bin/sh
# Compare two versions of results.json or tests.json: which systems got worse,
# better, were added or removed.
#
# Usage:  delta.sh [FILE] [OLD-REV [NEW-REV]]
#         delta.sh OLD-FILE NEW-FILE
#         delta.sh FILE OLD-FILE NEW-FILE
#
# With no FILE both results.json and tests.json are compared. OLD-REV defaults
# to the last commit that changed the file and NEW-REV to the working tree.
# Instead of revisions, paths to two files may be given. A system whose only
# change is its date, seconds, release or note is not listed.
#
# Only POSIX sh and awk are used. The measuring scripts write one JSON object
# per line, and run-forks.sh already reads these files with sed and awk, so a
# separate JSON tool would be another dependency for no extra safety here.
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
me=delta.sh

usage() {
  echo "usage: $me [FILE] [OLD-REV [NEW-REV]]" >&2
  echo "       $me OLD-FILE NEW-FILE" >&2
  echo "       $me FILE OLD-FILE NEW-FILE" >&2
  exit 2
}

case $# in
  0) mode=both ;;
  1) mode=single; file=$1 ;;
  2)
    if [ -f "$1" ] && [ -f "$2" ]; then
      mode=files; old_file=$1; new_file=$2
    else
      mode=single; file=$1; old_spec=$2
    fi
    ;;
  3) mode=single; file=$1; old_spec=$2; new_spec=$3 ;;
  *) usage ;;
esac

tmp=$(mktemp -d "${TMPDIR:-/tmp}/library-status-delta.XXXXXX") || exit 2
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

resolve() {
  case $1 in
    /*) printf '%s\n' "$1" ;;
    *)
      if [ -f "$1" ]; then
        printf '%s\n' "$(CDPATH= cd -- "$(dirname -- "$1")" && pwd -P)/$(basename -- "$1")"
      elif [ -f "$here/$1" ]; then
        printf '%s\n' "$here/$1"
      else
        printf '%s\n' "$1"
      fi
      ;;
  esac
}

file_rev_paths() {
  dir=$(dirname -- "$1")
  root=$(git -C "$dir" rev-parse --show-toplevel 2>/dev/null) || {
    echo "delta: $1 is not in a git repository" >&2; exit 2; }
  root=$(CDPATH= cd -- "$root" && pwd -P)
  abs=$(CDPATH= cd -- "$dir" && pwd -P)/$(basename -- "$1")
  case $abs in
    "$root"/*) rel=${abs#"$root"/} ;;
    *) echo "delta: $1 is not inside $root" >&2; exit 2 ;;
  esac
}

old_default_rev() {
  file_rev_paths "$1"
  rev=$(git -C "$root" log -1 --format=%H -- "$rel" 2>/dev/null) || rev=
  [ -n "$rev" ] || { echo "delta: no commit changed $1" >&2; exit 2; }
  printf '%s\n' "$rev"
}

add_rev() {
  file_rev_paths "$1"
  dest=$tmp/content-$((${ncontent:-0} + 1))
  ncontent=$((${ncontent:-0} + 1))
  if ! git -C "$root" show "$2:$rel" > "$dest" 2>/dev/null; then
    echo "delta: cannot read $1 at $2" >&2
    exit 2
  fi
  printf '%s\n' "$dest" >> "$3"
}

add_file() {
  printf '%s\n' "$(resolve "$1")" >> "$2"
}

add_source() {
  # add_source FILE SPEC SIDE-LIST
  if [ -f "$2" ]; then
    add_file "$2" "$3"
  else
    add_rev "$1" "$2" "$3"
  fi
}

: > "$tmp/old.list"
: > "$tmp/new.list"

if [ "$mode" = files ]; then
  add_file "$old_file" "$tmp/old.list"
  add_file "$new_file" "$tmp/new.list"
else
  if [ "$mode" = both ]; then
    results_file=$here/results.json
    tests_file=$here/tests.json
  else
    results_file=$(resolve "$file")
    tests_file=
  fi
  for f in "$results_file" "$tests_file"; do
    [ -n "$f" ] || continue
    if [ -n "${old_spec:-}" ]; then add_source "$f" "$old_spec" "$tmp/old.list"
    else add_rev "$f" "$(old_default_rev "$f")" "$tmp/old.list"; fi
    if [ -n "${new_spec:-}" ]; then add_source "$f" "$new_spec" "$tmp/new.list"
    else add_file "$f" "$tmp/new.list"; fi
  done
fi

normalize() {
  awk '
    function keypos(line, key,    pat, pos) {
      pat = "\"" key "\""
      pos = index(line, pat)
      if (pos == 0) return 0
      return pos + length(pat)
    }
    function jstr(line, key,    p, rest, i) {
      p = keypos(line, key)
      if (p == 0) return ""
      rest = substr(line, p)
      if (rest !~ /^[ \t]*:[ \t]*"/) return ""
      sub(/^[ \t]*:[ \t]*"/, "", rest)
      i = index(rest, "\"")
      if (i == 0) return ""
      return substr(rest, 1, i - 1)
    }
    function jnum(line, key,    p, rest) {
      p = keypos(line, key)
      if (p == 0) return ""
      rest = substr(line, p)
      if (rest !~ /^[ \t]*:[ \t]*-?[0-9]/) return ""
      sub(/^[ \t]*:[ \t]*/, "", rest)
      match(rest, /^-?[0-9]+/)
      return substr(rest, 1, RLENGTH)
    }
    function add_object(obj,    sys, st, vd, ps, fl) {
      sys = jstr(obj, "system")
      if (sys == "") return
      st = jstr(obj, "status")
      vd = jstr(obj, "verdict")
      ps = jnum(obj, "passed")
      fl = jnum(obj, "failed")
      if (!(sys in seen)) {
        seen[sys] = 1
        order[++count] = sys
      }
      if (st != "") status[sys] = st
      if (vd != "") verdict[sys] = vd
      if (ps != "") passed[sys] = ps
      if (fl != "") failed[sys] = fl
    }
    { text = text $0 "\n" }
    END {
      n = length(text)
      i = 1
      while (i <= n) {
        if (substr(text, i, 1) != "{") { i++; continue }
        depth = 0; instr = 0; esc = 0; j = i
        while (j <= n) {
          ch = substr(text, j, 1)
          if (instr) {
            if (esc) esc = 0
            else if (ch == "\\") esc = 1
            else if (ch == "\"") instr = 0
          } else if (ch == "\"") {
            instr = 1
          } else if (ch == "{") {
            depth++
          } else if (ch == "}") {
            depth--
            if (depth == 0) break
          }
          j++
        }
        if (depth == 0) {
          add_object(substr(text, i, j - i + 1))
          i = j + 1
        } else {
          i++
        }
      }
      for (i = 1; i <= count; i++) {
        s = order[i]
        printf "%s\t%s\t%s\t%s\t%s\n", s, status[s], verdict[s], passed[s], failed[s]
      }
    }
  ' "$@"
}

set --
while IFS= read -r p; do set -- "$@" "$p"; done < "$tmp/old.list"
normalize "$@" > "$tmp/old.tsv"
set --
while IFS= read -r p; do set -- "$@" "$p"; done < "$tmp/new.list"
normalize "$@" > "$tmp/new.tsv"

awk -F '\t' '
  FILENAME == ARGV[1] {
    s = $1
    os[s] = $2; ov[s] = $3; op[s] = $4; of[s] = $5
    if (!(s in inold)) { inold[s] = 1; oldorder[++nold] = s }
    next
  }
  {
    s = $1
    ns[s] = $2; nv[s] = $3; np[s] = $4; nf[s] = $5
    if (!(s in innew)) { innew[s] = 1; neworder[++nnew] = s }
    next
  }
  function srank(v) {
    if (v == "ok") return 0
    if (v == "load-only") return 1
    if (v == "patched") return 2
    if (v == "native") return 3
    if (v == "fail") return 4
    if (v == "not checked") return 5
    return 6
  }
  function trank(v) {
    if (v == "pass") return 0
    if (v == "fail") return 1
    if (v == "no-result" || v == "no-tests") return 2
    if (v == "error") return 3
    if (v == "timeout" || v == "load-fail") return 4
    return 5
  }
  function describe(s,    d, sep, a, b) {
    d = ""; sep = ""
    a = os[s]; b = ns[s]
    if (a != "" && b != "" && a != b) { d = d sep "load " a " -> " b; sep = "; " }
    a = ov[s]; b = nv[s]
    if (a != "" && b != "" && a != b) { d = d sep "tests " a " -> " b; sep = "; " }
    a = op[s]; b = np[s]
    if (a != "" && b != "" && (a + 0) != (b + 0)) { d = d sep "passed " a " -> " b; sep = "; " }
    a = of[s]; b = nf[s]
    if (a != "" && b != "" && (a + 0) != (b + 0)) { d = d sep "failed " a " -> " b; sep = "; " }
    return d
  }
  function judge(s,    a, b) {
    cw = 0; cb = 0
    a = os[s]; b = ns[s]
    if (a != "" && b != "" && srank(a) != srank(b)) {
      if (srank(b) > srank(a)) cw = 1; else cb = 1
    }
    a = ov[s]; b = nv[s]
    if (a != "" && b != "" && trank(a) != trank(b)) {
      if (trank(b) > trank(a)) cw = 1; else cb = 1
    }
    a = op[s]; b = np[s]
    if (a != "" && b != "" && (a + 0) != (b + 0)) {
      if ((b + 0) < (a + 0)) cw = 1; else cb = 1
    }
    a = of[s]; b = nf[s]
    if (a != "" && b != "" && (a + 0) != (b + 0)) {
      if ((b + 0) > (a + 0)) cw = 1; else cb = 1
    }
  }
  function state(s,    d, sep) {
    d = ""; sep = ""
    if (os[s] != "") { d = d sep "load " os[s]; sep = "; " }
    if (ov[s] != "") { d = d sep "tests " ov[s]; sep = "; " }
    if (op[s] != "") { d = d sep "passed " op[s]; sep = "; " }
    if (of[s] != "") { d = d sep "failed " of[s]; sep = "; " }
    return d
  }
  function nstate(s,    d, sep) {
    d = ""; sep = ""
    if (ns[s] != "") { d = d sep "load " ns[s]; sep = "; " }
    if (nv[s] != "") { d = d sep "tests " nv[s]; sep = "; " }
    if (np[s] != "") { d = d sep "passed " np[s]; sep = "; " }
    if (nf[s] != "") { d = d sep "failed " nf[s]; sep = "; " }
    return d
  }
  END {
    for (i = 1; i <= nold; i++) {
      s = oldorder[i]
      if (s in innew) {
        d = describe(s)
        if (d != "") {
          judge(s)
          if (cw) { wlines[++wn] = "  " s "  " d }
          else if (cb) { blines[++bn] = "  " s "  " d }
        }
      } else {
        rlines[++rn] = "  " s "  " state(s)
      }
    }
    for (i = 1; i <= nnew; i++) {
      s = neworder[i]
      if (!(s in inold)) alines[++an] = "  " s "  " nstate(s)
    }
    if (wn) { print "WORSE"; for (i = 1; i <= wn; i++) print wlines[i] }
    if (bn) { print "BETTER"; for (i = 1; i <= bn; i++) print blines[i] }
    if (an) { print "ADDED"; for (i = 1; i <= an; i++) print alines[i] }
    if (rn) { print "REMOVED"; for (i = 1; i <= rn; i++) print rlines[i] }
    printf "worse %d, better %d, added %d, removed %d\n", wn, bn, an, rn
    exit (wn > 0)
  }
' "$tmp/old.tsv" "$tmp/new.tsv"
