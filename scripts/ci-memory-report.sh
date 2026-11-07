#!/bin/sh
# What a CI job actually cost in memory, appended to the step summary.
#
#   sh scripts/ci-memory-report.sh
#
# Informational only, and deliberately incapable of failing the job: nothing
# here is checked and the script always exits 0. Reporting must never decide the
# colour of a run, and a sizing measurement least of all.
#
# memory.peak is the cgroup high-water mark, reset by the job's "Runner info"
# step where the kernel allows it (>= 6.8). Ancestors are printed unreset so the
# figure is still readable if the reset was refused, and /proc/self/cgroup is
# printed so it is clear which cgroup the number belongs to (the job's processes
# are children of a long-lived service, not of a container).

show_mem() {
	for f in memory.peak memory.current memory.max \
		memory.max_usage_in_bytes memory.usage_in_bytes memory.limit_in_bytes; do
		[ -r "$1/$f" ] && echo "$1/$f = $(cat "$1/$f")"
	done
}

{
	echo "## Job memory"
	echo ""
	echo '```'
	echo "nproc: $(nproc)"
	echo ""
	echo "--- /proc/self/cgroup ---"
	cat /proc/self/cgroup
	echo ""
	echo "--- cgroup memory (this cgroup first, then its ancestors) ---"
	cg2=$(awk -F: '$1 == "0" { print $3 }' /proc/self/cgroup)
	cg1=$(awk -F: '$2 ~ /(^|,)memory(,|$)/ { print $3; exit }' /proc/self/cgroup)
	for base in "/sys/fs/cgroup${cg2}" ${cg1:+"/sys/fs/cgroup/memory${cg1}"}; do
		d="$base"
		while : ; do
			case "$d" in /sys/fs/cgroup*) ;; *) break ;; esac
			show_mem "$d"
			nd=$(dirname "$d")
			[ "$nd" = "$d" ] && break
			d="$nd"
		done
	done
	echo ""
	echo "--- free -m ---"
	free -m
	echo ""
	echo "--- /usr/bin/time ---"
	if [ -x /usr/bin/time ]; then
		echo "present (a heavy step can be wrapped in /usr/bin/time -v)"
	else
		echo "absent (apt-get install time) -- the cgroup figures above are the only route"
	fi
	echo '```'
} >> "${GITHUB_STEP_SUMMARY:-/dev/stdout}" 2>&1
exit 0
