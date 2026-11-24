#!/bin/sh
# The SIL a cross-compile writes does not depend on how many gensyms the compiling process had
# consumed before it started (the names of uninterned symbols carry the counters; the writer numbers
# them again in order of appearance). Linux consumes more at startup than a Mac, so two copies of
# one tree compiled there differed in every such name.
#
#   test/ci/cross-compile-deterministic.sh ROOT   (the host is $DOTCL_LISP, as for `make cross-compile`)
set -u
ROOT=$(cd "${1:-.}" && pwd)
HOST=${DOTCL_LISP:-ros -L sbcl-bin run}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
cat > "$T/in.lisp" <<'EOF'
(defun cc-det-f (a b) (let ((x (+ a b))) (flet ((g (y) (* x y))) (mapcar #'g (list a b)))))
(defun cc-det-h (l) (loop for i in l collect (let ((k (* i i))) (lambda (z) (+ k z)))))
(defun cc-det-k (list2 &key x1) (list list2 x1))
(defun cc-det-m (&rest r) (multiple-value-bind (p q) (values 1 2) (list p q r)))
EOF
fail=0
for n in 0 85 1000; do
  DOTCL_INPUTS="$T/in.lisp" DOTCL_OUTPUT="$T/out-$n.sil" \
    $HOST --eval "(dotimes (i $n) (gensym))" --load "$ROOT/compiler/cil-compile.lisp" > "$T/log-$n.txt" 2>&1
  [ -s "$T/out-$n.sil" ] || { echo "  FAIL cross-compile-deterministic: no output for $n gensyms first"; tail -3 "$T/log-$n.txt"; fail=1; }
done
if [ "$fail" = 0 ]; then
  for n in 85 1000; do
    if cmp -s "$T/out-0.sil" "$T/out-$n.sil"; then echo "  ok    cross-compile output is the same after $n gensyms"
    else echo "  FAIL  cross-compile output differs after $n gensyms"; diff "$T/out-0.sil" "$T/out-$n.sil" | head -4 | cut -c1-150; fail=1; fi
  done
fi
# A name written by hand, with a digit in it, is not a generated one: it stays as written.
if [ "$fail" = 0 ]; then
  if grep -q ':LAMBDA-LIST (#:LIST2 &KEY #:X1)' "$T/out-0.sil"; then
    echo "  ok    a hand-written name with a digit (LIST2) is not renamed"
  else echo "  FAIL  LIST2 was renamed"; grep -o ':LAMBDA-LIST.\{0,60\}' "$T/out-0.sil" | head -3; fail=1; fi
fi
exit $fail
