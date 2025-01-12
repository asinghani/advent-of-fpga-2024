# Wrapper script for UART interfacing, since OCaml doesn't have a great serial
# port library readily available
import serial
import sys
import time
import string

printable = [ord(x) for x in string.printable]

with serial.Serial(sys.argv[1], int(sys.argv[2]), timeout=0.1) as ser:
    ser.rts = 0
    ser.dtr = 1
    time.sleep(0.2)
    ser.dtr = 0
    time.sleep(0.2)

    wait_time = int(sys.argv[3])

    def flush():
        ser.flush()
        time.sleep(0.1)

    for cmd in sys.argv[4:]:
        if cmd == "rts1":
            flush()
            ser.rts = True
        elif cmd == "rts0":
            flush()
            ser.rts = False
        elif cmd == "dtr1":
            flush()
            ser.dtr = True
        elif cmd == "dtr0":
            flush()
            ser.dtr = False
        else:
            ser.write(bytes([int(cmd, 16)]))

    flush()
    print(f"Finished writing data (len = {len(sys.argv[4:])})")

    start = time.time()
    while True:
        if (time.time() - start) > wait_time:
            break
        value = ser.read(100)
        for a in value:
            if a in printable:
                sys.stdout.write(chr(a))
            else:
                sys.stdout.write(f"\\x{a:02X}")

            sys.stdout.flush()
