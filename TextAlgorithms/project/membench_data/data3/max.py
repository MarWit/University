#!/usr/bin/env python

import re

KB = [1597, 2583, 4181, 10946, 17711, 46368, 75025];
NAMES = ['kasai', 'lcp9', 'lcp6']

REG = re.compile(r'[0-9]+')

for kb in KB:
    for name in NAMES:
        with open('{}_{}'.format(kb, name), 'rt') as f:
            maximum = 0
            for line in f:
                line = line.strip()
                maximum = max(maximum, int(REG.search(line).group(0)))
            print('{}_{}={}'.format(4 * kb // 1024, name, maximum))
