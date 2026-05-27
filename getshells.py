#!/usr/bin/env python3

from collections import defaultdict
shellcnt = defaultdict(int)

with open('passwd', "r") as pw:
    for line in pw.readlines():
        shell = line[line.rfind(':')+1:].strip()
        shellcnt[shell] += 1

shells = sorted(shellcnt.keys())

for i in shells:
    print("{0:18} {1:5}".format(i, shellcnt[i]))
