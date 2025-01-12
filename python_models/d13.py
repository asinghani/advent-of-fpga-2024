import sys
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=13, use_sample=USE_SAMPLE, raw=True)
dat = ints(dat)

h_accum1 = 0
h_accum2 = 0

INT_MAX = 0xFFFFFFFF

max_poss = 0

for i in range(0, len(dat), 6):
    xa, ya, xb, yb, xp, yp = dat[i:i+6]
    max_poss = max([max_poss, xp // xa, xp // xb, yp // ya, yp // yb])

    h_tmp = INT_MAX

    # Megapipeline
    for a in range(100):
        for b in range(100):
            if mul18(a, xa) + mul18(b, xb) == xp:
                if mul18(a, ya) + mul18(b, yb) == yp:
                    h_tmp = min(h_tmp, 3*a + b)

    if h_tmp != INT_MAX:
        h_accum1 += h_tmp

print(max_poss)
print()
print(f"Part 1 = {h_accum1}")
print(f"Part 2 = {h_accum2}")
