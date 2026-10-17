#!/bin/bash
# Merge dotcl and SBCL benchmark JSON outputs into bench-state.json
# Usage: make-state.sh <dotcl-stderr> <sbcl-stderr> [existing-bench-state.json]
# When existing file is provided, new results are merged into it (preserving entries not in new results).

set -e

dotcl_file="$1"
sbcl_file="$2"
existing_file="$3"

if [ -z "$dotcl_file" ] || [ -z "$sbcl_file" ]; then
    echo "Usage: $0 <dotcl-stderr> <sbcl-stderr> [existing-bench-state.json]" >&2
    exit 1
fi

# Extract "name": value pairs from the JSON block in stderr output
# Returns lines like: name<TAB>value
# Handles incomplete JSON (timeout: no closing brace).
#
# The harness writes this block to stderr, and so does the host's compiler: an
# SBCL note can land on the same line right after a value, e.g.
#     "search-sequence/consed": 2000016; in: LAMBDA ()
# Keeping only the leading number (or "null") drops that trailing text instead
# of carrying it into bench-state.json, where it makes the file unparseable and
# the entry ratio-less.
extract_results() {
    sed -n '/{/,$ p' "$1" | grep ':' | sed 's/["{},]//g; s/:/ /' \
        | awk '{ v = $2
                 if (v ~ /^null/) v = "null"
                 else sub(/[^0-9.eE+-].*$/, "", v)
                 print $1 "\t" v }'
}

dotcl_results=$(extract_results "$dotcl_file")
sbcl_results=$(extract_results "$sbcl_file")

# Collect new benchmark names
declare -A new_names
new_names_list=()
while IFS=$'\t' read -r name _; do
    if [ -n "$name" ]; then
        new_names["$name"]=1
        new_names_list+=("$name")
    fi
done <<< "$dotcl_results"

# Collect existing entries not in new results
existing_order=()
declare -A ex_raw
if [ -n "$existing_file" ] && [ -f "$existing_file" ]; then
    while IFS= read -r line; do
        # Match any entry line:    "name": {...}
        #
        # Not only the {"dotcl": ...} shape this script writes. bench-state.json
        # also carries rows from the C# parity benchmark, whose fields are
        # different (csharp_ms / dotcl_ms / ratio), and a name pattern tied to
        # one shape silently deleted them on every cl-bench run. Keeping the
        # payload verbatim preserves any shape without this script having to
        # know them all.
        # The closing brace is required so the `"benchmarks": {` header, which
        # opens an object and never closes it on its own line, is not read as
        # an entry named "benchmarks".
        name=$(echo "$line" | sed -n 's/^ *"\([^"]*\)": {.*},\{0,1\}$/\1/p')
        [ -z "$name" ] && continue
        [ -n "${new_names[$name]+_}" ] && continue
        existing_order+=("$name")
        ex_raw["$name"]=$(echo "$line" | sed 's/^ *"[^"]*": //; s/,$//')
    done < "$existing_file"
fi

# Build combined list: new results first, then preserved existing entries
all_names=("${new_names_list[@]}" "${existing_order[@]}")
total=${#all_names[@]}

echo "{"
echo "  \"updated\": \"$(date +%Y-%m-%d)\","
echo "  \"benchmarks\": {"

for i in "${!all_names[@]}"; do
    name="${all_names[$i]}"

    if [ -n "${new_names[$name]+_}" ]; then
        # New result
        dotcl_val=$(echo "$dotcl_results" | awk -F'\t' -v n="$name" '$1 == n {print $2}')
        sbcl_val=$(echo "$sbcl_results" | awk -F'\t' -v n="$name" '$1 == n {print $2}')

        dotcl_out="${dotcl_val:-null}"
        sbcl_out="${sbcl_val:-null}"

        ratio="null"
        status=""
        if [ "$dotcl_out" = "null" ]; then
            status=', "status": "dotcl-error"'
        elif [ "$sbcl_out" = "null" ]; then
            status=', "status": "sbcl-error"'
        else
            if awk "BEGIN { exit ($sbcl_out == 0) }" 2>/dev/null; then
                ratio=$(awk "BEGIN { printf \"%.1f\", $dotcl_out / $sbcl_out }" 2>/dev/null || echo "null")
            else
                status=', "status": "sbcl-zero"'
            fi
        fi
        comma=","
        if [ $i -eq $((total - 1)) ]; then comma=""; fi

        printf '    "%s": {"dotcl": %s, "sbcl": %s, "ratio": %s%s}%s\n' \
            "$name" "$dotcl_out" "$sbcl_out" "$ratio" "$status" "$comma"
    else
        # Existing entry, carried through verbatim.
        comma=","
        if [ $i -eq $((total - 1)) ]; then comma=""; fi

        printf '    "%s": %s%s\n' "$name" "${ex_raw[$name]}" "$comma"
    fi
done

echo "  }"
echo "}"
