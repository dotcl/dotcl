#!/bin/sh
# ci/check-script-refs.sh: a script that a workflow, the Makefile or a ci script runs and that is not a
# tracked file (untracked, or missing) fails the check; a tracked one, a URL and a comment do not.
set -u
ROOT="$(cd "${1%/}" && pwd)"
W="$(mktemp -d)"; trap 'rm -rf "$W"' EXIT
fail=0; note() { echo "check-script-refs-test: FAIL: $1"; fail=1; }

R="$W/repo"; mkdir -p "$R/.github/workflows" "$R/ci" "$R/test/ci" "$R/scripts"
cd "$R" && git init -q . && git config user.email t@example.invalid && git config user.name t || { echo "check-script-refs-test: git is not usable"; exit 1; }
cat > .github/workflows/w.yml <<'EOT'
jobs:
  x:
    steps:
      - run: bash ci/tracked.sh
      - run: sh test/ci/also-tracked.sh arg
      # - run: bash ci/commented-out-missing.sh
      - run: curl -fL https://example.invalid/scripts/remote-only.sh | sh
EOT
printf 'all:\n\tsh $(DOTCL_ROOT)scripts/untracked.sh\n\tpython3 scripts/missing.py\n' > Makefile
echo 'echo hi' > ci/tracked.sh; echo 'echo hi' > test/ci/also-tracked.sh
git add .github Makefile ci/tracked.sh test/ci/also-tracked.sh
git commit -q -m init
echo 'echo hi' > scripts/untracked.sh    # on disk, never added

out=$(sh "$ROOT/ci/check-script-refs.sh" "$R" 2>&1); rc=$?
[ "$rc" = 1 ] || note "exit $rc, expected 1 with an untracked and a missing script"
echo "$out" | grep -q "scripts/untracked.sh.*not tracked" || note "the untracked script is not named: $out"
echo "$out" | grep -q "scripts/missing.py.*does not exist" || note "the missing script is not named"
echo "$out" | grep -q "tracked.sh" && echo "$out" | grep "ci/tracked.sh\|also-tracked.sh" | grep -q . && note "a tracked script was reported"
echo "$out" | grep -q "remote-only\|commented-out" && note "a URL or a comment was read as a reference"

git add scripts/untracked.sh; printf 'all:\n\tsh $(DOTCL_ROOT)scripts/untracked.sh\n' > Makefile; git add Makefile; git commit -q -m fix
sh "$ROOT/ci/check-script-refs.sh" "$R" > "$W/ok.txt" 2>&1 || note "a repo whose references are all tracked was refused: $(cat "$W/ok.txt")"

[ "$fail" = 0 ] && echo "check-script-refs-test: OK"
exit "$fail"
