import sys, math
from util import *
from parsing_util import *

USE_SAMPLE = "b" if len(sys.argv) < 2 else None
dat = readinputs(day=10, use_sample=USE_SAMPLE)
dat = [[int(a) for a in x] for x in dat]

dim = len(dat)
assert len(dat) == len(dat[0])

def valid(a):
    return a >= 0 and a < dim

def search(x, y, n, do_mask=True):
    accum = 0

    # mask out
    if n == 9:
        if do_mask: dat[x][y] = 10
        return 1

    for xo, yo in [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]:
        if valid(xo) and valid(yo):
            if dat[xo][yo] == (n+1):
                accum += search(xo, yo, n+1, do_mask=do_mask)

    return accum

accum = 0

for x in range(dim):
    for y in range(dim):
        if dat[x][y] == 0:
            a = search(x, y, 0)
            accum += a

            # cleanup
            for i in range(dim):
                for j in range(dim):
                    if dat[i][j] == 10:
                        dat[i][j] = 9


print(accum)

accum = 0

for x in range(dim):
    for y in range(dim):
        if dat[x][y] == 0:
            a = search(x, y, 0, do_mask=False)
            accum += a

            # cleanup
            for i in range(dim):
                for j in range(dim):
                    if dat[i][j] == 10:
                        dat[i][j] = 9


print(accum)
