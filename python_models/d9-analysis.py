import sys, math
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=9, use_sample=USE_SAMPLE, raw=True)
dat = [int(x) for x in dat.strip()]

arr = []

num_nones = 0
ptr = 0

for i, x in enumerate(dat):
    is_gap = (i % 2)
    if is_gap:
        for _ in range(x):
            num_nones += 1
            arr.append(None)
    else:
        for _ in range(x):
            arr.append(i//2)

print("len", len(arr))

while num_nones != 0:
    x = arr.pop()
    while arr[ptr] is not None:
        ptr += 1

    arr[ptr] = x
    num_nones -= 1

print(sum([a*b for a, b in enumerate(arr)]))

# pt 2

arr_data = []
arr_gaps = []


accum = 0

i = 0
for c, x in enumerate(dat):
    is_gap = (c % 2)
    if is_gap:
        arr_gaps.append((i, x))
        i += x
    else:
        arr_data.append((i, x, c//2))
        i += x

for idx, length, val in arr_data[::-1]:
    done = False
    for i in range(len(arr_gaps)):
        idx1, length1 = arr_gaps[i]
        if idx1 < idx and length1 >= length:
            arr_gaps[i] = (idx1+length, length1 - length)
            for j in range(length):
                accum += (idx1 + j) * val
            done = True
            break

    if not done:
        for j in range(length):
            accum += (idx+j) * val

print(accum)

# pt 1 but simpler

arr_data = []
arr_gaps = []

accum = 0

i = 0
for c, x in enumerate(dat):
    is_gap = (c % 2)
    if is_gap:
        arr_gaps.append((i, x))
        i += x
    else:
        arr_data.append((i, x, c//2))
        i += x

for idx, length, val in arr_data[::-1]:
    done = False
    for i in range(len(arr_gaps)):
        idx1, length1 = arr_gaps[i]
        if idx1 < idx:
            ll = min(length, length1)
            arr_gaps[i] = (idx1+ll, length1-ll)
            for j in range(ll):
                accum += (idx1 + j) * val

            length -= ll
            if length == 0:
                done = True
                break

    if not done:
        for j in range(length):
            accum += (idx+j) * val

print(accum)
