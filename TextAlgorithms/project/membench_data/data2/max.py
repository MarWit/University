#!/usr/bin/env python

import re

KB = [1, 2, 4, 8, 16, 32, 64, 128]
NAMES = ['kasai', '9n', '6n']

REG = re.compile(r'[0-9]+')

for kb in KB:
    for name in NAMES:
        with open('{}KB_{}'.format(kb, name), 'rt') as f:
            maximum = 0
            for line in f:
                line = line.strip()
                maximum = max(maximum, int(REG.search(line).group(0)))
            print('{}KB_{}={}'.format(kb, name, maximum))
