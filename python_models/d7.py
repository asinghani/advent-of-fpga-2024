import sys
from tqdm import tqdm
from util import *
from parsing_util import *

USE_SAMPLE = "a" if len(sys.argv) < 2 else None
dat = readinputs(day=7, use_sample=USE_SAMPLE)

dat = intsall(dat)

# Some metrics about the data
print(max(len(x)-1 for x in dat))
print(max(x[0] for x in dat))
print(max(max(x[1:]) for x in dat))
print(sum(2**(len(x)-2) for x in dat))
print(sum(3**(len(x)-2) for x in dat))
print()

# Only store one sequence at a time, just in registers
# Will need a FIFO or backpressure to keep up
# Elements are all 1-3 digits long so 10 bits
# Accumulator needs to be 64 bits with an invalid bit for overflows
hc_max_seq_len = 16
hc_width_bits = 64
hc_mask_width = (1 << hc_width_bits) - 1
h_seq_len = 0

h_accum1 = 0
h_accum2 = 0

# Hardware resources
# We pad the end with zeros s.t. they only ever get added to the
# final value (a no-op) and avoid counting past the actual seq len
h_stream = [0 for _ in range(hc_max_seq_len)]

OP_ADD = 0
OP_MUL = 1
OP_CAT = 2
OP_NOP = 3

# pipeline stage type:
# - accum: bits64
# - goal: bits64
# - remain: (bits10, bits2) list
# - ops: bits2 list
# - valid: bool

def add_stage(accum, val):
    accum_new = accum + val
    # In HW: check if any high bits set
    overflow = (accum_new & hc_mask_width) != accum_new
    return accum_new, overflow

def mul_stage(accum, val):
    # TODO
    accum_new = accum * val
    # In HW: check if any high bits set
    overflow = (accum_new & hc_mask_width) != accum_new
    return accum_new, overflow

def cat_stage(accum, val, val_digits):
    if val_digits == 1:
        accum = accum * 10
    if val_digits == 2:
        accum = accum * 100
    if val_digits == 3:
        accum = accum * 1000

    accum_new = accum + val
    # In HW: check if any high bits set
    overflow = (accum_new & hc_mask_width) != accum_new
    return accum_new, overflow

def pipeline_stage(arg):
    accum = arg["accum"]
    val, val_digits = arg["remain"][0]
    op = arg["ops"][0]

    if val_digits == 0:
        op = OP_NOP

    if op == OP_ADD:
        accum_new, overflow = add_stage(accum, val)
    if op == OP_MUL:
        accum_new, overflow = mul_stage(accum, val)
    if op == OP_CAT:
        accum_new, overflow = cat_stage(accum, val, val_digits)
    if op == OP_NOP:
        accum_new = accum
        overflow = 0

    return {
        "accum": accum_new,
        "goal": arg["goal"],
        "remain": arg["remain"][1:] + [(0, 0)],
        "ops": arg["ops"][1:] + [OP_NOP],
        "valid": arg["valid"] and not overflow
    }


def binary_counter(prev):
    if prev[0] == 0:
        return [1] + prev[1:]
    else:
        return [0] + binary_counter(prev[1:])

def ternary_counter(prev):
    if prev[0] == 0:
        return [1] + prev[1:]
    elif prev[0] == 1:
        return [2] + prev[1:]
    else:
        return [0] + ternary_counter(prev[1:])

def reached_max(count, seq_len):
    return count[seq_len] != 0

total_iters = 0

for seq in tqdm(dat):
    goal = seq[0]
    accum_init = seq[1]
    seq = seq[2:]
    h_seq_len = len(seq)

    for i in range(hc_max_seq_len):
        h_stream[i] = (0, 0)

    for i in range(len(seq)):
        x = seq[i]
        digits = 1 if seq[i] < 10 else (2 if seq[i] < 100 else 3)
        h_stream[i] = (x, digits)

    def main_datapath(count_fn):
        seq = [0 for _ in range(hc_max_seq_len)]
        while not reached_max(seq, h_seq_len):
            arg = {
                "accum": accum_init,
                "goal": goal,
                "remain": h_stream,
                "ops": seq,
                "valid": 1
            }

            for _ in range(hc_max_seq_len):
                arg = pipeline_stage(arg)

            if arg["valid"] and arg["accum"] == arg["goal"]:
                return goal
                break
            seq = count_fn(seq)

            global total_iters
            total_iters += 1

        return 0

    # Dispatch both parts at once, wait for both to complete

    # Pt1
    h_accum1 += main_datapath(binary_counter)

    # Pt2
    h_accum2 += main_datapath(ternary_counter)


    # HW would flush the pipeline after this process ends

print(total_iters)


print(f"Part 1 = {h_accum1}")
print(f"Part 2 = {h_accum2}")
