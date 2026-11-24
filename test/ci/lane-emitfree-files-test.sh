#!/bin/sh
# ci/lane-emitfree-files.sh: the regression files a candidate adds or changes (not run.lisp, not a deletion,
# not a non-lisp file) go to `make test-regression-files ... MODE=emitfree`; a failure of that make is the
# script's failure; no such file means exit 0 and no make.
set -u
ROOT="$(cd "${1%/}" && pwd)"
W="$(mktemp -d)"; trap 'rm -rf "$W"' EXIT
fail=0; note() { echo "lane-emitfree-files-test: FAIL: $1"; fail=1; }

R="$W/repo"; mkdir -p "$R/test/regression" "$R/bin"
cat > "$R/bin/make" <<'EOT'
#!/bin/sh
echo "$@" >> "$MAKELOG"
exit "${FAKE_RC:-0}"
EOT
chmod +x "$R/bin/make"
cd "$R" && git init -q . && git config user.email t@example.invalid && git config user.name t || { echo "lane-emitfree-files-test: git is not usable"; exit 1; }
echo '(a)' > test/regression/a.lisp; echo '(old)' > test/regression/old.lisp; echo '(run)' > test/regression/run.lisp
git add . && git commit -q -m base && git branch base

run() { MAKELOG="$W/make.log" PATH="$R/bin:$PATH" bash "$ROOT/ci/lane-emitfree-files.sh" base 2>&1; }

# 1. nothing changed: exit 0, make is not run
: > "$W/make.log"
out=$(run); rc=$?
[ "$rc" = 0 ] || note "no change: exit $rc, expected 0"
echo "$out" | grep -q "no regression file added or changed" || note "no change: the message is missing: $out"
[ ! -s "$W/make.log" ] || note "no change: make was run: $(cat "$W/make.log")"

# 2. a file added, one changed, run.lisp changed, a non-lisp file added, a file deleted
echo '(b)' > test/regression/b.lisp; echo '(a2)' >> test/regression/a.lisp; echo '(run2)' >> test/regression/run.lisp
echo x > test/regression/notes.txt; git rm -q test/regression/old.lisp
git add . && git commit -q -m cand
: > "$W/make.log"
out=$(run); rc=$?
[ "$rc" = 0 ] || note "files changed: exit $rc, expected 0: $out"
log=$(cat "$W/make.log")
echo "$log" | grep -q "test-regression-files" || note "make was not asked for test-regression-files: $log"
echo "$log" | grep -q "MODE=emitfree" || note "MODE=emitfree is missing: $log"
echo "$log" | grep -q "test/regression/b.lisp" || note "the added file is missing: $log"
echo "$log" | grep -q "test/regression/a.lisp" || note "the changed file is missing: $log"
echo "$log" | grep -q "run.lisp" && note "run.lisp was passed: $log"
echo "$log" | grep -q "notes.txt\|old.lisp" && note "a non-lisp or deleted file was passed: $log"

# 3. the make fails: the script fails
: > "$W/make.log"
out=$(FAKE_RC=2 run); rc=$?
[ "$rc" = 1 ] || note "a failing make (exit 2): exit $rc, expected 1"

[ "$fail" = 0 ] && echo "lane-emitfree-files-test: OK"
exit "$fail"
