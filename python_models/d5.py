import sys
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=5, use_sample=USE_SAMPLE)
rules = []
seqs = []

while True:
    x = dat.pop(0)
    if x == "": break
    rules.append(ints(x))

while len(dat):
    x = dat.pop(0)
    seqs.append(ints(x))

# Only store one sequence at a time
# Bubblesort takes on the order of ~10 x 32 x 32 clocks
# which should mean we can roughly keep up in real-time
# with < one sequence worth of backpressure at a 115200 baud
hc_max_seq_len = 32

# Hardware resources
h_seq_len = 0
h_did_swap = 0

h_accum1 = 0
h_accum2 = 0

# the values are 2-digit numbers, i.e. <128, so a sparse "adjacency matrix"
# type thing works here
rules_ram = BlockRAM(bits=1, depth=(128*128))
seq_ram = BlockRAM(bits=9, depth=hc_max_seq_len)

for i in range(128*128):
    rules_ram.write(addr=i, data=0)

# Loading rules
for i in range(len(rules)):
    a, b = rules[i]
    rules_ram.write(addr=((a << 7) | b), data=1)

for seq in seqs:
    h_seq_len = len(seq)
    h_did_swap = 0

    for i in range(h_seq_len):
        seq_ram.write(addr=i, data=seq[i])

    # Basically just a contrived bubble sort again
    for i in range(h_seq_len):
        for j in range(h_seq_len-1):
            a = seq_ram.read(addr=j)
            b = seq_ram.read(addr=j+1)

            gt = rules_ram.read(addr=(b << 7 | a))
            a0 = b if gt else a
            b0 = a if gt else b

            if gt: h_did_swap = 1

            seq_ram.write(addr=j, data=a0)
            seq_ram.write(addr=j+1, data=b0)

    mid = seq_ram.read(addr=(h_seq_len >> 1))

    if h_did_swap:
        h_accum2 += mid
    else:
        h_accum1 += mid

print(f"Part 1 = {h_accum1}")
print(f"Part 2 = {h_accum2}")
