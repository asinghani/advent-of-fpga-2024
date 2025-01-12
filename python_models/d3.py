import sys
from util import *
from parsing_util import *

USE_SAMPLE = "b" if len(sys.argv) < 2 else None
dat = readinputs(day=3, use_sample=USE_SAMPLE, raw=True)
dat = [ord(x) for x in dat]

h_accum1 = 0
w_accum1 = 64
h_accum2 = 0
w_accum2 = 64

h_enabled = 1

hc_lookback_len = len("mul(AAA,BBB)")
h_shreg = [0 for _ in range(hc_lookback_len)]

# Input is just a single null-terminated string of bytes

def is_numeric(x):
    return x >= ord("0") and x <= ord("9")

def to_numeral(x):
    return x - ord("0")

# Flush out the window at the end of the data
for x in dat + ([0] * hc_lookback_len):
    h_shreg = h_shreg[1:] + [x]

    if h_shreg[:4] == list(b"do()"):
        h_enabled = 1

    if h_shreg[:7] == list(b"don't()"):
        h_enabled = 0

    if h_shreg[:4] == list(b"mul("):
        ra = 0
        rb = 0
        r_valid = 0
        # Generate the following 9 permutations and mux them
        for alen in [1, 2, 3]:
            for blen in [1, 2, 3]:
                a = (3 - alen) * [ord("0")] + (h_shreg[4:4+alen])
                b = (3 - blen) * [ord("0")] + (h_shreg[4+1+alen:4+1+alen+blen])
                comma = h_shreg[4+alen]
                paren = h_shreg[4+1+alen+blen]

                if all(is_numeric(x) for x in a) \
                    and all(is_numeric(x) for x in b) \
                    and comma == ord(",") and paren == ord(")"):

                    a = to_numeral(a[0]) * 100 + to_numeral(a[1]) * 10 + to_numeral(a[2])
                    b = to_numeral(b[0]) * 100 + to_numeral(b[1]) * 10 + to_numeral(b[2])
                    print(a, b)

                    h_accum1 += mul18(a, b)
                    if h_enabled:
                        h_accum2 += mul18(a, b)

print(f"Part 1 = {h_accum1}")
print(f"Part 2 = {h_accum2}")
