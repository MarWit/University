#!/usr/bin/env python

from collections import defaultdict

def transform(c):
    out = defaultdict(lambda: None, {
        '$': 0,
        ' ': 1,
        '.': 2,
        ',': 3,
        '-': 5,
    })[c]

    offset = 6

    c = ord(c)
    if c >= ord('A') and c <= ord('Z'):
        out = (c - ord('A')) * 2 + offset
    if c >= ord('a') and c <= ord('z'):
        out = (c - ord('a')) * 2 + 1 + offset

    return out

KB = 1024
N = 64
LEN = KB * N

with open('./lipsum.txt', 'rt') as f:
    data = f.read()
    transformed = [transform(c) for c in data]
    transformed = [str(c) for c in transformed if c is not None][:LEN]

    print('pub const LIPSUM: [u32; {}] =[{}, 0];'.format(LEN + 1, ', '.join(transformed)))

