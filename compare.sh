#!/bin/sh

set -eu

RUNS=${RUNS:-10}
OUTFILE=${OUTFILE:-results.csv}
TMPFILE=$(mktemp)

trap 'rm -f "$TMPFILE"' EXIT

detect_time()
{
	if /usr/bin/time -l true >/dev/null 2>&1; then
		TIME_IMPL=bsd
	elif command -v gtime >/dev/null 2>&1; then
		TIME_IMPL=gtime
	elif /usr/bin/time -f '%e,%M' true >/dev/null 2>&1; then
		TIME_IMPL=gnu
	else
		TIME_IMPL=posix
	fi
}

run_once_bsd()
{
	tmp=$(mktemp)

	/usr/bin/time -l "$1" >/dev/null 2>"$tmp"

	elapsed=$(
		awk '
		$2 == "real" {
			print $1
			exit
		}' "$tmp"
	)

	rss=$(
		awk '
		/maximum resident set size/ {
			print $1
			exit
		}' "$tmp"
	)

	rm -f "$tmp"

	printf '%s,%s\n' \
		"${elapsed:-0}" \
		"${rss:-0}"
}

run_once_gtime()
{
	tmp=$(mktemp)

	gtime -f '%e,%M' \
		"$1" >/dev/null 2>"$tmp"

	cat "$tmp"

	rm -f "$tmp"
}

run_once_gnu()
{
	tmp=$(mktemp)

	/usr/bin/time -f '%e,%M' \
		"$1" >/dev/null 2>"$tmp"

	cat "$tmp"

	rm -f "$tmp"
}

run_once_posix()
{
	tmp=$(mktemp)

	/usr/bin/time \
		"$1" >/dev/null 2>"$tmp"

	elapsed=$(
		awk '
			/real/ {
				gsub(/[^0-9.]/, "", $2)
				print $2
				exit
			}
		' "$tmp"
	)

	rm -f "$tmp"

	printf '%s,%s\n' \
		"${elapsed:-0}" \
		"0"
}

run_once()
{
	case "$TIME_IMPL" in
		bsd) run_once_bsd "$1" ;;
		gtime) run_once_gtime "$1" ;;
		gnu) run_once_gnu "$1" ;;
		*) run_once_posix "$1" ;;
	esac
}

average_program()
{
	prog=$1

	i=1

	while [ "$i" -le "$RUNS" ]
	do
		run_once "$prog"
		i=$((i + 1))
	done |
	awk -F, '
	{
		time_sum += $1
		rss_sum += $2
		count++
	}
	END {
		printf "%.6f,%.0f\n",
			time_sum / count,
			rss_sum / count
	}'
}

detect_time

printf '%s\n' \
	'Language,Time,Mem usage,LOC' \
	> "$OUTFILE"

exec 3< benchmark.list

while IFS='|' read -r name prog source <&3
do
	[ -x "$prog" ] || continue

	printf 'Benchmarking %s...\n' "$name" >&2

	stats=$(average_program "$prog")

	time_sec=$(printf '%s\n' "$stats" | cut -d, -f1)
	rss=$(printf '%s\n' "$stats" | cut -d, -f2)
	loc=$(awk 'END { print NR }' "$source")

	printf '%s,%s,%s,%s\n' \
		"$name" \
		"$time_sec" \
		"$rss" \
		"$loc" \
		>> "$TMPFILE"

done

sort -t, -k2,2n "$TMPFILE" |
while IFS=, read -r lang secs rss loc
do
	time_fmt=$(
		awk -v s="$secs" '
		BEGIN {
			m=int(s/60)
			printf "%02d:%05.2f", m, s-(m*60)
		}'
	)

	printf '%s,%s,%sK,%s\n' \
		"$lang" \
		"$time_fmt" \
		"$rss" \
		"$loc"
done >> "$OUTFILE"

printf 'Results written to %s\n' "$OUTFILE" >&2
