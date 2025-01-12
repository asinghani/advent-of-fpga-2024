import functools
import math
import pprint

cache_bits = [(10 if n < 12 else 4) for n in range(76)]
caches = [{x: (-1, 0) for x in range(1 << (cache_bits[i]))} for i in range(76)]

# analysis
all_nums = []
iters = [0 for _ in range(76)]
hits = [0 for _ in range(76)]

def solve(x, n, mode=0):
    global iters, hits

    cache = caches[n]
    cache_idx = x & ((1 << cache_bits[n]) - 1)
    cache_idx = cache_idx
    included = (cache[cache_idx][0] == x)

    iters[n] += 1
    if included: hits[n] += 1

    if not included:
        all_nums.append(x)
        if n == 0:
            sol = 1
        elif x == 0:
            sol = solve(1, n-1, mode=0)
        elif len(str(x)) % 2 == 0:
            sol = solve(int(str(x)[len(str(x))//2:]),n-1, mode=1) + \
                solve(int(str(x)[:len(str(x))//2]),n-1, mode=1)
        else:
            sol = solve(x*2024,n-1, mode=2)

        cache[cache_idx] = (x, sol)

    return cache[cache_idx][1]

result = sum(solve(x, 75) for x in [572556,22,0,528,4679021,1,10725,2790])

print("iters", sum(iters))
print("hits", sum(hits))
print("misses", sum(iters)-sum(hits))

pprint.pprint([(a, b, a-b) for a, b in zip(iters, hits)])


print(len(all_nums))
print(max(all_nums))

print("result", result)
