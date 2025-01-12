import sys
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=4, use_sample=USE_SAMPLE)
assert len(dat) == len(dat[0])

hc_max_width = 256

h_width = len(dat[0])

assert h_width < hc_max_width

h_accum1 = 0
h_accum2 = 0

h_row = 0
h_col = 0

h_shreg = [0 for _ in range((hc_max_width * 3) + 4 + 1)]

# Stream of bytes again
dat = "".join(dat)
dat = [ord(x) for x in dat]


def do_sliding_window(width, height, fn):
    global h_col, h_row, h_shreg

    mtx = [[h_shreg[i*h_width+j] for j in range(width)] for i in range(height)]
    mtx_valid = h_col >= (width-1) and h_row >= (height-1)

    if not mtx_valid:
        return None
    else:
        return fn(mtx)

def diag(mtx):
    return [mtx[i][i] for i in range(len(mtx))]

def revdiag(mtx):
    return [mtx[len(mtx)-i-1][i] for i in range(len(mtx))]

for x in dat:
    # Simplify X/M/A/S to 2 bits
    X, M, A, S = 0, 1, 2, 3
    x = list(b"XMAS").index(x)

    h_shreg = [x] + h_shreg[:-1]

    # Part 1
    if do_sliding_window(4, 1, lambda x: (x == [[X, M, A, S]] or \
                                          x == [[S, A, M, X]])):
        h_accum1 += 1

    if do_sliding_window(1, 4, lambda x: (x == [[X], [M], [A], [S]] or \
                                          x == [[S], [A], [M], [X]])):
        h_accum1 += 1

    if do_sliding_window(4, 4, lambda x: (diag(x) == [X, M, A, S] or \
                                          diag(x) == [S, A, M, X])):
        h_accum1 += 1

    if do_sliding_window(4, 4, lambda x: (revdiag(x) == [X, M, A, S] or \
                                          revdiag(x) == [S, A, M, X])):
        h_accum1 += 1


    # Part 2
    def x_mas_sliding_window_check(mtx):
        pair1 = [mtx[0][0], mtx[2][2]]
        pair2 = [mtx[2][0], mtx[0][2]]

        return (pair1 == [M, S] or pair1 == [S, M]) \
            and (pair2 == [M, S] or pair2 == [S, M]) \
            and mtx[1][1] == A

    if do_sliding_window(3, 3, x_mas_sliding_window_check):
        h_accum2 += 1

    h_col += 1
    if h_col == h_width:
        h_col = 0
        h_row += 1

print(f"Part 1 = {h_accum1}")
print(f"Part 2 = {h_accum2}")
