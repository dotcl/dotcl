#!/usr/bin/env bash
# Per-function SIL counts, one record per DEFMETHOD-DIRECT:
#
#   NAME   TOTAL   INSTRS
#
# TOTAL counts every (: form in the body. INSTRS excludes the three that are
# not code -- :DECLARE-LOCAL, :LABEL and :LINE -- which the assembler consumes
# rather than emits.
#
# Read INSTRS. TOTAL is kept because earlier records were taken with it, and a
# number in a decision record has to stay reproducible; but it overstates any
# change that introduces temporaries, since each one brings a :DECLARE-LOCAL
# along with it. That is not hypothetical: a change measured here as "+5
# instructions" was +3 real instructions and 2 declarations, while removing two
# runtime calls.
#
# Neither column sees a specialisation -- one call swapped for another leaves
# both untouched. Diff the operand names too.
#
# Three ways an instruction count has misled us, all of them real:
#   - the count did not move and the code changed   (a call specialised)
#   - the count rose and the work fell              (a call replaced by its body)
#   - the count included things that are not code   (this tool, before INSTRS)
# Say which column you are quoting, and check it against what the operands say.
# See README.md.
tr -d '\r' < "$1" | tr '\n' ' ' | sed 's/(:DEFMETHOD-DIRECT /\n&/g' \
| awk 'NR>1 {
    name = $2; gsub(/"/, "", name);
    total = gsub(/\(:/, "");
    noncode = gsub(/DECLARE-LOCAL|:LABEL|:LINE/, "");
    printf "%-16s %5d %5d\n", name, total, total - noncode
  }'
