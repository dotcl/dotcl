#!/bin/sh
# fetch-ql-projects.sh DIR [COMMIT]
#
# Make DIR a checkout of quicklisp-projects at COMMIT, detached, for rank.lisp
# (DOTCL_QL_PROJECTS=DIR). The repository is where Quicklisp records where
# each project is fetched from, and rank.lisp reads the owner of a project off
# that location.
#
# The default COMMIT is the last one before the Quicklisp dist the table is
# ranked against (quicklisp 2026-01-01), so that the owners match the dist.
# Pass another commit when moving to another dist; rank.lisp writes the commit
# it read into targets.tsv.
set -eu
dir=${1:?usage: fetch-ql-projects.sh DIR [COMMIT]}
commit=${2:-2e314d2b48f6e65a5afc1fb399a1aec2466b8531}
url=https://github.com/quicklisp/quicklisp-projects.git
if [ ! -d "$dir/.git" ]; then
  git clone -q "$url" "$dir"
fi
git -C "$dir" fetch -q origin
git -C "$dir" -c advice.detachedHead=false checkout -q --detach "$commit"
echo "quicklisp-projects at $(git -C "$dir" rev-parse HEAD) in $dir"
