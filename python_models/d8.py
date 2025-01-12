import sys, math
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=8, use_sample=USE_SAMPLE, raw=True)
dat = dat.strip() + "\n"

hc_max_grid_dim = 256

h_grid_x = 0
h_grid_y = 0

h_accum1 = 0
h_accum2 = 0

h_iter = 0

dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

part1_ram = BlockRAM(bits=1, depth=(hc_max_grid_dim**2))
part2_ram = BlockRAM(bits=1, depth=(hc_max_grid_dim**2))

# one BRAM each
coord_rams = [BlockRAM(bits=18, depth=(1024)) for _ in range(96)]
coord_indices = [0 for _ in range(96)]

def addr(x, y):
    return (x << 8) | (y << 0)

def unaddr(a):
    x = (a >> 8) & 0xFF
    y = (a >> 0) & 0xFF
    return (x, y)

for c in dat:
    c = ord(c)
    if c == ord("\n"):
        h_grid_x = 0
        h_grid_y += 1
    else:
        if c != ord("."):
            idx = c - 32
            coord_rams[idx].write(data=addr(h_grid_x, h_grid_y), addr=coord_indices[idx])
            coord_indices[idx] += 1

        h_grid_x += 1

h_grid_dim = h_grid_y
print(h_grid_dim)

for i in range(hc_max_grid_dim**2):
    part1_ram.write(addr=i, data=0)
    part2_ram.write(addr=i, data=0)

def gcd(x, y):
    a, b = x, y
    if b > a: a, b = b, a
    while b != 0:
        a = a - b
        if b > a: a, b = b, a

    # For testing
    assert math.gcd(x, y) == a
    return a

def fracreduce(x, y):
    if x == 0: return (0, 1)
    if y == 0: return (1, 0)
    g = gcd(x, y)
    # TODO: HW divider
    return (x//g, y//g)

def sgn(a):
    return 1 if a >= 0 else -1

for i in range(96):
    num_coords = coord_indices[i]

    for a in range(num_coords):
        for b in range(num_coords):
            if a != b:
                x1, y1 = unaddr(coord_rams[i].read(addr=a))
                x2, y2 = unaddr(coord_rams[i].read(addr=b))

                dx = x1 - x2
                dy = y1 - y2

                xn = x1 + dx
                yn = y1 + dy

                # In HW the overflow will handle the first half
                if xn >= 0 and xn < h_grid_dim:
                    if yn >= 0 and yn < h_grid_dim:
                        part1_ram.write(addr=addr(xn, yn), data=1)

                xa, ya = fracreduce(abs(dx), abs(dy))
                xa = abs(xa) * sgn(dx)
                ya = abs(ya) * sgn(dy)
                assert xa != 0 or ya != 0

                x, y = x1, y1
                while x >= 0 and x < h_grid_dim and \
                        y >= 0 and y < h_grid_dim:
                    part2_ram.write(addr=addr(x, y), data=1)
                    x += xa
                    y += ya

                x, y = x1, y1
                while x >= 0 and x < h_grid_dim and \
                        y >= 0 and y < h_grid_dim:
                    part2_ram.write(addr=addr(x, y), data=1)
                    x -= xa
                    y -= ya

for i in range(hc_max_grid_dim**2):
    h_accum1 += part1_ram.read(addr=i)
    h_accum2 += part2_ram.read(addr=i)

print(f"Part 1 = {h_accum1}")
print(f"Part 2 = {h_accum2}")
