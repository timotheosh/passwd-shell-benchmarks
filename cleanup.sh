#!/bin/sh

set -eu

printf '%s\n' 'Cleaning build artifacts...'

rm -rf \
	bin \
	logs \
	benchmark.list \
	results.csv

find . -type d \( \
	-name target \
	-o -name '.stack-work' \
\) -prune -exec rm -rf {} +

find . -type f \( \
	-name '*.o' \
	-o -name '*.hi' \
	-o -name '*.dyn_o' \
	-o -name '*.dyn_hi' \
	-o -name '*.prof' \
	-o -name '*.aux' \
	-o -name '*.hp' \
	-o -name '*.eventlog' \
\) -delete

printf '%s\n' 'Done.'
