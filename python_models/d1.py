import sys
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=1, use_sample=USE_SAMPLE)

# Data parsing into two lists
dat = intsall(dat)
list1, list2 = list(zip(*dat))
assert len(list1) == len(list2)
print(len(list1), len(list2))

# Hardware resources
h_length = 0
h_accumulator = 0
w_accumulator = 32
h_tmp = 0
w_tmp = 16
h_ram1 = BlockRAM(bits=18, depth=2048)
h_ram2 = BlockRAM(bits=18, depth=2048)

# Loading data
for i in range(len(list1)):
    h_ram1.write(addr=i, data=list1[i])

for i in range(len(list2)):
    h_ram2.write(addr=i, data=list2[i])

h_length = len(list1)

# Stage 1: Bubble sort
for i in range(h_length):
    for j in range(h_length-1):
        # Sort both in parallel
        for ram in [h_ram1, h_ram2]:
            # 2-4 cycles for this process depending on
            # single or dual port RAM
            a = ram.read(addr=j)
            b = ram.read(addr=j+1)

            gt = a > b
            a0 = b if gt else a
            b0 = a if gt else b

            ram.write(addr=j, data=a0)
            ram.write(addr=j+1, data=b0)

# Stage 2: Iterate + sum for part1
h_accumulator = 0

for i in range(h_length):
    a = h_ram1.read(addr=i)
    b = h_ram2.read(addr=i)

    gt = a > b
    diff = (a - b) if gt else (b - a)

    h_accumulator += diff
    checkwidth(h_accumulator, w_accumulator)

print(f"Part 1 = {h_accumulator}")

# Stage 3: Iterate + search for part2
h_accumulator = 0

for i in range(h_length):
    value = h_ram1.read(addr=i)
    h_tmp = 0
    for j in range(h_length):
        y = h_ram2.read(addr=j)

        h_tmp += (value == y)

    h_accumulator += mul18(value, h_tmp)
    checkwidth(h_accumulator, w_accumulator)

print(f"Part 2 = {h_accumulator}")
