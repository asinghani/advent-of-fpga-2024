import random

class BlockRAM:
    def __init__(self, depth, bits, zero=False):
        def init():
            if zero: return 0
            else: return random.randint(0, (1 << bits) - 1)

        self.ram = [init() for _ in range(depth)]
        self.bits = bits

    def write(self, addr, data):
        assert addr >= 0 and addr < len(self.ram)
        checkwidth(data, self.bits)
        self.ram[addr] = data

    def read(self, addr):
        assert addr >= 0 and addr < len(self.ram)
        return self.ram[addr]

def checkwidth(x, bits):
    assert x >= 0 and x < (1 << bits)

def mul18(x, y):
    checkwidth(x, 18)
    checkwidth(y, 18)
    return x * y

def mul36(x, y):
    checkwidth(x, 36)
    checkwidth(y, 36)
    return x * y
