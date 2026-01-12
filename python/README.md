# Advent of Code 2022 - Python Solutions

Python port of the Clojure solutions for Advent of Code 2022.

## Requirements

- Python 3.10+ (uses modern type hints)
- No external dependencies (uses only standard library)

## Structure

```
python/
├── 2022/
│   ├── day01.py
│   ├── day02.py
│   ├── ...
│   └── day25.py
├── run_all.py      # Test runner to verify all solutions
└── README.md
```

## Running Solutions

### Run a single day

```bash
# From repository root
python3 -m python.2022.day01
python3 -m python.2022.day15
```

### Run all days with verification

```bash
cd /path/to/advent-of-code
python3 -m python.run_all
```

## Solution Patterns

Each solution follows a consistent structure:

```python
"""Day N: Title - https://adventofcode.com/2022/day/N"""

INPUT_FILE = "inputs/2022/dayNN.txt"

def parse_input(filename: str) -> ...:
    """Parse the input file."""
    ...

def part1(...) -> ...:
    """Solve part 1."""
    ...

def part2(...) -> ...:
    """Solve part 2."""
    ...

def main():
    data = parse_input(INPUT_FILE)
    answer1 = part1(data)
    answer2 = part2(data)
    print(f"Part 1: {answer1}")
    print(f"Part 2: {answer2}")
    return answer1, answer2

if __name__ == "__main__":
    main()
```

## Answers

All solutions produce the same answers as the original Clojure implementations.

| Day | Part 1 | Part 2 |
|-----|--------|--------|
| 01 | 68802 | 205370 |
| 02 | 17189 | 13490 |
| 03 | 8088 | 2522 |
| 04 | 605 | 914 |
| 05 | VGBBJCRMN | LBBVJBRMH |
| 06 | 1850 | 2823 |
| 07 | 1908462 | 3979145 |
| 08 | 1662 | 537600 |
| 09 | 6037 | 2485 |
| 10 | 11720 | ERCREPCJ |
| 11 | 57838 | 15050382231 |
| 12 | 468 | 459 |
| 13 | 6656 | 19716 |
| 14 | 885 | 28691 |
| 15 | 5688618 | 12625383204261 |
| 16 | 1641 | 2261 |
| 17 | 3067 | 1514369501484 |
| 18 | 4504 | 2556 |
| 19 | 1127 | 21546 |
| 20 | 8721 | 831878881825 |
| 21 | 155708040358220 | 3342154812537 |
| 22 | 123046 | 195032 |
| 23 | 4336 | 1005 |
| 24 | 314 | 896 |
| 25 | 2-0-020-1==1021=--01 | ⭐ |
