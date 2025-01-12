import sys
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=2, use_sample=USE_SAMPLE)

# Data parsing into a list of lists
dat = intsall(dat)

print(len(dat))
print(max(len(x) for x in dat))

# Flatten the data by inserting gaps
flat = [0]

for l in dat:
    flat += [(x << 1) | 1 for x in l]
    flat += [0]

# Hardware resources
valid = 0
h_count1 = 0
h_count2 = 0
h_fails_a = 0
h_fails_d = 0

h_last = 0
h_last_valid = 0

h_base = 0
h_idx = 0

h_skip = 0

hc_max_report_length = 16
hc_ram_depth = 16384
h_ram = BlockRAM(bits=9, depth=hc_ram_depth)

# Loading data
for i in range(hc_ram_depth):
    h_ram.write(addr=i, data=(flat[i] if i < len(flat) else 0))

# Iterate over the list

while True:
    if h_idx == hc_ram_depth - 1:
        break

    x = h_ram.read(addr=h_idx)

    if x == 0:
        if valid:
            safe = (h_fails_a == 0) or (h_fails_d == 0)
            h_count1 += safe and (h_skip == 0)
            h_count2 += safe

            if safe or h_skip == (hc_max_report_length - 1):
                h_base = h_idx
                h_idx += 1
                h_skip = 0
            else:
                h_idx = h_base+1
                h_skip += 1
        else:
            h_idx += 1
            h_skip = 0

        valid = 0
        h_fails_a = 0
        h_fails_d = 0
        h_last_valid = 0
    else:
        value = x >> 1
        valid = 1

        skip = (h_idx - h_base) == h_skip

        if h_last_valid and not skip:
            safe_a = (value > h_last) and \
                     (value - h_last) == 1 or \
                     (value - h_last) == 2 or \
                     (value - h_last) == 3

            safe_d = (value < h_last) and \
                     (h_last - value) == 1 or \
                     (h_last - value) == 2 or \
                     (h_last - value) == 3

            if not safe_a:
                h_fails_a = 1

            if not safe_d:
                h_fails_d = 1

        if not skip:
            h_last = value
            h_last_valid = 1

        h_idx += 1

print(f"Part 1 = {h_count1}")
print(f"Part 2 = {h_count2}")
