# Advent of FPGA 2024

My attempt at completing 2024's Advent of Code problems in the form of synthesizable FPGA designs.

I started this fairly late into December (and had to set up board infrastructure for the ULX3S), so I've only completed 8 days by the end of the month (more to come soon).

## Repo Structure

```
├── fpga
│   ├── bin - Binary build targets
│   ├── input_parser - Code for parsing inputs and preparing them for the FPGA
│   ├── src - FPGA designs and supporting utilities for parsing and board support
│   ├── test - Tests to simulate each day on the sample inputs
│   └── uart_wrapper.py - Script for interfacing with the designs on the FPGA
├── inputs - Directory to store each day's inputs (not checked into the repo)
└── python_models - Python models of the implemented algorithms
```

## Implementation

I chose to use Hardcaml for the design, since its strong type system and ability to generate complex designs (especially the access to recursion and functional operations over lists) make many days' implementations much cleaner.

I targetted the ULX3S FPGA platform  with the ECP5-85K (mostly for the quick compile times and relatively limited resources, making some designs more interesting).

Most of the inputs inputs are preprocessed by using a regex to extract all integers and converting them to 32-bit or 64-bit little-endian values, to avoid needing unique string parsing logic in the hardeare. Aside from the preprocessing, all computations (including printing the output in base-10) are done in the FPGA.

### Board Wrapper

In order to simplify the shared logic, several components are shared between the different days' designs:

- Setting up the board and its clocks and reset
- UART transmit and receive with a FIFO, including the ability to reset the board
- Encoding the results into base 10 and outputting to the UART

## Days Implemented

### Day 1

**Part 1:** Load each list into a Block RAM. Sort the lists using a bubble sort (2 counters and a small state machine). Iterate over the lists and accumulate the differences.

**Part 2:** Iterate over the first list, for each value, iterate over the second list, accumulating the occurrences, then multiplying and accumulating the result.

### Day 2

**Part 1:** For each reading, place all of the values into a register bank (of length 32 and width 8 bits). In parallel, check the validity of each pair of adjacent readings, and use a pipelined tree to reduce them down to check if each pair is valid, and accumulate if so.

**Part 2:** Do the same as part 1, but check validity for each set of 2-apart elements as well, and create 32 unique tree reductions (with most of the logic being shared), one for each possible removed reading. Then use a further pipelined tree reduction to check if any of these match, and accumulate if so.

### Day 3

**Part 1:** Shift the string into a shift register (of length equal to the longest string we care about, i.e. "mul(XXX,XXX")"). At each character, check if the string matches the format "mul(A,B)" (for all 9 combinations of lengths of A and B), and if so, parse the string into a pair of 10-bit integers and multiply them, then accumulate.

**Part 2:** Same as part 1, but also checking for "do()" and "don't()" strings, and setting a register to disable the accumulator when "don't()" is seen.

### Day 4

**Part 1:** We create a sliding window convolution (using a shift register to store the most recent N-1 rows and the first N elements of the last row, sufficient to construct the sliding window). A fairly wide (heavily pipelined) multiplexer is used to allow this convolution to adapt to arbitrary widths. Three convolutions are used: a 1x4 (for vertical), 4x1 (for horizontal), and 4x4 (for diagonal). For each convolution, the relevant values are checked (in both directions), and accumulated if there is a match.

**Part 2:** Similar to part 1, but with a 3x3 window and slightly more complex logic for checking for a match (due to there being 4 different possible matches).

### Day 5

**Parts 1 and 2:** We start by loading the ordering rules. Since page numbers are limited to 2 digits (i.e. 7 bits), we can build a bitmap of all of the ordering rules (of size 128*128 = 16384 bits, which fits into a single block RAM). This allows a single-cycle lookup of an ordering rule. Then, load each list of numbers into a Block RAM, and sort them using bubble sort, but indexing into the rules RAM instead of a regular numeric comparison. Find the middle element by halving the list length, and accumulate it to the part 1 register (if no two elements were swapped during the bubble sort, i.e. the list was already ordered correctly), or the part 2 register otherwise.

### Day 7

**Part 1:** We can build a very deep pipeline to compute the entire sequence of operations (at a throughput of one computation per cycle), by maintaining the list of operands, operators, and the expected result as a state vector that is passed between each pipeline stage. At the end of the pipeline, we check the validity of the result and accumulate if so. To feed this pipeline, we use a binary counter of length equal to the number of operands minus one, to try every permutation of operators.

**Part 2:** We do the same as part 1, but adding a third operator and using a trinary counter instead, to iterate over the new set of all possible operators.

### Day 13

**Part 1 and 2:** The problem can be reduced down to two equations in two variables. We can use a 2x2 matrix inversion to find a closed-form solution for the two button counts. This closed form solution can be evaluated in hardware using an iterative divider, and then can simply check whether the solution exists and has all-integer coefficients in order to check if it is valid.

```
a = (yb*xp - xb*yp) / (xa*yb - xb*ya)
b = (xa*yp - ya*xp) / (xa*yb - xb*ya)
```

### Day 22

**Part 1:** We can implement the given PRNG functon in the FPGA logic and compute it for each of the given inputs, then accumulate the results.
