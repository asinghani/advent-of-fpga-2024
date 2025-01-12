import sys
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=6, use_sample=USE_SAMPLE)

# Entire grid would take ~65kbits
hc_max_grid_dim = 256

# Hardware resources
h_grid_dim = len(dat)

h_origin_x = 0
h_origin_y = 0

h_guard_x = 0
h_guard_y = 0
h_guard_dir = 0

h_guard2_x = 0
h_guard2_y = 0
h_guard2_dir = 0

h_accum1 = 0
h_accum2 = 0

h_extra_obs_x = 0
h_extra_obs_y = 0

h_iter = 0

dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

grid_ram = BlockRAM(bits=1, depth=(hc_max_grid_dim**2))
visited_ram = BlockRAM(bits=1, depth=(hc_max_grid_dim**2))

def addr(x, y):
    return (x << 8) | y

for i in range(hc_max_grid_dim**2):
    grid_ram.write(addr=i, data=0)
    visited_ram.write(addr=i, data=0)

# Loading grid
for i in range(h_grid_dim):
    for j in range(h_grid_dim):
        x = dat[i][j]
        obs = (x == "#")
        if x == "^":
            h_origin_x = i
            h_origin_y = j

        grid_ram.write(addr=addr(i, j), data=obs)

h_guard_x = h_origin_x
h_guard_y = h_origin_y

count = 0
while True:
    count += 1
    visited_ram.write(addr=addr(h_guard_x, h_guard_y), data=1)

    dx, dy = dirs[h_guard_dir]
    next_x = h_guard_x + dx
    next_y = h_guard_y + dy

    # In HW the negative would overflow and just be greater
    if next_x < 0 or next_x >= h_grid_dim: break
    if next_y < 0 or next_y >= h_grid_dim: break

    if grid_ram.read(addr=addr(next_x, next_y)):
        h_guard_dir += 1
        h_guard_dir = h_guard_dir % 4
    else:
        h_guard_x = next_x
        h_guard_y = next_y

print("Num iters", count)

count2 = 0
for i in range(hc_max_grid_dim):
    for j in range(hc_max_grid_dim):
        visited = visited_ram.read(addr=addr(i, j))
        h_accum1 += visited

        if visited and (i, j) != (h_origin_x, h_origin_y):
            h_guard_x = h_origin_x
            h_guard_y = h_origin_y
            h_guard_dir = 0

            h_guard2_x = h_origin_x
            h_guard2_y = h_origin_y
            h_guard2_dir = 0

            h_extra_obs_x = i
            h_extra_obs_y = j

            it = 0
            while True:
                count2 += 1

                if it % 2 == 1:
                    dx, dy = dirs[h_guard_dir]
                    next_x = h_guard_x + dx
                    next_y = h_guard_y + dy

                    # In HW the negative would overflow and just be greater
                    if next_x < 0 or next_x >= h_grid_dim: break
                    if next_y < 0 or next_y >= h_grid_dim: break

                    if grid_ram.read(addr=addr(next_x, next_y)) or \
                        (next_x, next_y) == (h_extra_obs_x, h_extra_obs_y):
                        h_guard_dir += 1
                        h_guard_dir = h_guard_dir % 4
                    else:
                        h_guard_x = next_x
                        h_guard_y = next_y

                dx, dy = dirs[h_guard2_dir]
                next_x = h_guard2_x + dx
                next_y = h_guard2_y + dy

                # In HW the negative would overflow and just be greater
                if next_x < 0 or next_x >= h_grid_dim: break
                if next_y < 0 or next_y >= h_grid_dim: break

                if grid_ram.read(addr=addr(next_x, next_y)) or \
                        (next_x, next_y) == (h_extra_obs_x, h_extra_obs_y):
                    h_guard2_dir += 1
                    h_guard2_dir = h_guard2_dir % 4
                else:
                    h_guard2_x = next_x
                    h_guard2_y = next_y

                if h_guard_dir == h_guard2_dir and \
                        h_guard_x == h_guard2_x and \
                        h_guard_y == h_guard2_y:

                    h_accum2 += 1
                    break

                it += 1

print("Num iters", count2)

print(f"Part 1 = {h_accum1}")
print(f"Part 2 = {h_accum2}")
