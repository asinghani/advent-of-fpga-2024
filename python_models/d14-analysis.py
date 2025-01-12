import sys
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=14, use_sample=USE_SAMPLE)
dat = negintsall(dat)

h_w = 11 if USE_SAMPLE else 101
h_h = 7 if USE_SAMPLE else 103

h_quads = [0 for _ in range(4)]

def accum_quad(x, y):
    mid_x = (h_w >> 1)
    mid_y = (h_h >> 1)

    if x > mid_x and y > mid_y: h_quads[0] += 1
    if x > mid_x and y < mid_y: h_quads[1] += 1
    if x < mid_x and y > mid_y: h_quads[2] += 1
    if x < mid_x and y < mid_y: h_quads[3] += 1

h_accum1 = 1
h_accum2 = 0

locs = [(x, y) for (x, y, _, _) in dat]

for _ in range(100):
    for i in range(len(dat)):
        x, y = locs[i]
        _, _, vx, vy = dat[i]

        # Easier than doing modulo in HW
        x += vx
        if x < 0: x += h_w
        if x >= h_w: x -= h_w

        y += vy
        if y < 0: y += h_h
        if y >= h_h: y -= h_h

        locs[i] = (x, y)

for x, y in locs:
    accum_quad(x, y)

print(h_quads)

h_accum1 = h_quads[0] * h_quads[1] * h_quads[2] * h_quads[3]

print(f"Part 1 = {h_accum1}")

## part 2

locs = [(x, y) for (x, y, _, _) in dat]

for count in range(1, 8051):
    for i in range(len(dat)):
        x, y = locs[i]
        _, _, vx, vy = dat[i]

        # Easier than doing modulo in HW
        x += vx
        if x < 0: x += h_w
        if x >= h_w: x -= h_w

        y += vy
        if y < 0: y += h_h
        if y >= h_h: y -= h_h

        locs[i] = (x, y)

    if len(set(locs)) == len(locs):
        break

for x in range(h_w):
    for y in range(h_h):
        if (x, y) in locs:
            print("O", end="")
        else:
            print(" ", end="")
    print()

print(count)
