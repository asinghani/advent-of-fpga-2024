import re

def readinputs(day, use_sample=None, raw=False):
    if use_sample:
        fn = f"../inputs/sample{day}{use_sample}.txt"
    else:
        fn = f"../inputs/input{day}.txt"

    with open(fn, "r") as f:
        if raw:
            return f.read()
        else:
            return [x.strip() for x in f.readlines()]

def ints(s):
    return [int(x) for x in re.findall(r'\d+', s)]

def intsall(a):
    return [ints(x) for x in a]

def negints(s):
    return [int(x) for x in re.findall(r'-?\d+', s)]

def negintsall(a):
    return [negints(x) for x in a]
