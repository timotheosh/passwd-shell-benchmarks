#!/bin/sh
# run this after the binaries have been built.
# verifies that the programs produce valid output
set -eu

# Dynamically count the number of bash entries in the passwd file
EXPECTED_COUNT=$(grep -c '/bash$' /etc/passwd)

exec 3< benchmark.list

while IFS='|' read -r name prog source <&3
do
    # skip if not executable
    [ -x "$prog" ] || continue
    
    base=$(basename "$prog")
    printf 'validating %s...\n' "$name"
    
    # run program and check if output matches the dynamic count
    if "$prog" | grep -q "$EXPECTED_COUNT"; then
	printf '%-20s OK\n' "$base"
    else
        printf '%-20s FAILED\n' "$base"
    fi
done

