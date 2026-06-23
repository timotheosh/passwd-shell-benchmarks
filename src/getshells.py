#!/usr/bin/env python3
import sys
from collections import Counter

with open('passwd') as f:
    data = f.read()

shells = Counter(line.rpartition(':')[2] for line in data.splitlines() if line)

sys.stdout.write(''.join('%-18s %5d\n' % (s, shells[s]) for s in sorted(shells)))
