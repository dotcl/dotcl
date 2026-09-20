#!/bin/bash
# Merge the two halves of the C# parity benchmark into bench-state.json.
#
# Usage: make-parity-state.sh <csharp-out> <dotcl-out> [existing-bench-state.json]
#
# Both input files are the benchmark's own stdout: one "name<TAB>milliseconds"
# line per kernel. Each pair becomes one entry named "parity/<name>":
#
#     "parity/tak": {"csharp_ms": 4.498, "dotcl_ms": 11.186, "ratio": 2.49}
#
# Entries already in the existing file are carried through verbatim, in their
# original order, with any previous "parity/*" row for a kernel measured now
# replaced rather than duplicated. Verbatim matters: the cl-bench rows written
# by make-state.sh have a different shape (dotcl/sbcl/ratio), and re-deriving
# fields would mean knowing every shape the file can hold.

set -e

csharp_file="$1"
dotcl_file="$2"
existing_file="$3"

if [ -z "$csharp_file" ] || [ -z "$dotcl_file" ]; then
    echo "Usage: $0 <csharp-out> <dotcl-out> [existing-bench-state.json]" >&2
    exit 1
fi

for f in "$csharp_file" "$dotcl_file"; do
    if [ ! -s "$f" ]; then
        echo "$0: $f is missing or empty; the benchmark did not produce results." >&2
        exit 1
    fi
done

# name<TAB>value, ignoring blank lines and anything without a numeric value.
# A kernel that crashed mid-run leaves no line at all, and is then reported
# below as a missing pair rather than silently dropped.
#
# A name may appear more than once: the caller runs each half of the benchmark
# in several processes and concatenates their output. The minimum across them
# is kept, because what is being estimated is a floor. Five samples inside one
# process did not settle it -- the remaining spread was between processes, at
# 1.3x on a kernel whose in-process samples agreed to a few percent -- and a
# minimum over processes is the same one-sided filter applied one level up. It
# can only remove noise, never hide a regression: making the code slower raises
# the floor in every process.
read_pairs() {
    awk -F'\t' 'NF >= 2 && $2 + 0 == $2 {
                    if (!($1 in m) || $2 < m[$1]) { if (!($1 in m)) o[++n] = $1; m[$1] = $2 }
                }
                END { for (i = 1; i <= n; i++) print o[i] "\t" m[o[i]] }' "$1"
}

csharp_pairs=$(read_pairs "$csharp_file")
dotcl_pairs=$(read_pairs "$dotcl_file")

names=$(echo "$dotcl_pairs" | cut -f1)

lookup() {
    echo "$1" | awk -F'\t' -v n="$2" '$1 == n { print $2; exit }'
}

# Build the new rows first, so a missing counterpart is reported before
# anything is written.
new_names=()
new_rows=()
missing=0
while IFS= read -r name; do
    [ -z "$name" ] && continue
    c=$(lookup "$csharp_pairs" "$name")
    d=$(lookup "$dotcl_pairs" "$name")
    if [ -z "$c" ]; then
        echo "$0: no C# result for kernel '$name'" >&2
        missing=1
        continue
    fi
    ratio="null"
    if awk "BEGIN { exit !($c > 0) }"; then
        ratio=$(awk "BEGIN { printf \"%.2f\", $d / $c }")
    fi
    new_names+=("parity/$name")
    new_rows+=("{\"csharp_ms\": $c, \"dotcl_ms\": $d, \"ratio\": $ratio}")
done <<< "$names"

if [ "$missing" -ne 0 ]; then
    echo "$0: the two halves did not measure the same set of kernels." >&2
    exit 1
fi

if [ ${#new_names[@]} -eq 0 ]; then
    echo "$0: no kernels measured." >&2
    exit 1
fi

# Existing entries, verbatim, minus any parity row we are about to rewrite.
kept_names=()
kept_rows=()
if [ -n "$existing_file" ] && [ -f "$existing_file" ]; then
    while IFS= read -r line; do
        # The closing brace is required so the `"benchmarks": {` header, which
        # opens an object and never closes it on its own line, is not read as
        # an entry named "benchmarks".
        name=$(echo "$line" | sed -n 's/^ *"\([^"]*\)": {.*},\{0,1\}$/\1/p')
        [ -z "$name" ] && continue
        skip=0
        for n in "${new_names[@]}"; do
            if [ "$n" = "$name" ]; then skip=1; break; fi
        done
        [ "$skip" -eq 1 ] && continue
        row=$(echo "$line" | sed 's/^ *"[^"]*": //; s/,$//')
        kept_names+=("$name")
        kept_rows+=("$row")
    done < "$existing_file"
fi

all_names=("${kept_names[@]}" "${new_names[@]}")
all_rows=("${kept_rows[@]}" "${new_rows[@]}")
total=${#all_names[@]}

echo "{"
echo "  \"updated\": \"$(date +%Y-%m-%d)\","
echo "  \"benchmarks\": {"
for i in "${!all_names[@]}"; do
    comma=","
    if [ "$i" -eq $((total - 1)) ]; then comma=""; fi
    printf '    "%s": %s%s\n' "${all_names[$i]}" "${all_rows[$i]}" "$comma"
done
echo "  }"
echo "}"
