#!/bin/sh

# run this after the binaries have been built.
# verifies that the programs produce valid output

set -eu
VAL=validation

# Create directory if it does not exist
mkdir -p "$VAL"

exec 3< benchmark.list
while IFS='|' read -r name prog source <&3
do 
    [ -x "$prog" ] || continue
    base=$(basename "$prog")
    printf 'validating %s...\n' "$name"
    # run program once to generate validation data
    "$prog" > "$VAL/${base}"
done

# loop over files using a direct glob
for i in "$VAL"/*
do
    # ensure file exists (handles empty directories safely)
    [ -e "$i" ] || continue

    base=`echo $i | awk -F\- '{ print $2 }'`

    # check silently using exit codes
    if grep -q '44804' "$i"; then
        echo "$base:  OK"
    else
        echo "$base:  failed"
    fi
done

