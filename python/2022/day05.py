"""Day 5: Supply Stacks - https://adventofcode.com/2022/day/5"""

import re
from copy import deepcopy

INPUT_FILE = "inputs/2022/day05.txt"


def parse_input(filename: str) -> tuple[list[list[str]], list[tuple[int, int, int]]]:
    """Parse input into stacks and moves."""
    with open(filename) as f:
        content = f.read()

    parts = content.split("\n\n")
    stack_lines = parts[0].split("\n")
    move_lines = parts[1].strip().split("\n")

    # Parse stacks - each crate position is at index 1, 5, 9, ... (every 4 chars)
    # Last line contains stack numbers, skip it
    stack_lines = stack_lines[:-1]

    # Determine number of stacks from line length
    num_stacks = (len(stack_lines[0]) + 1) // 4

    # Initialize stacks (0-indexed internally)
    stacks = [[] for _ in range(num_stacks)]

    # Process from bottom to top (reverse order of lines)
    for line in reversed(stack_lines):
        for i in range(num_stacks):
            pos = 1 + i * 4
            if pos < len(line) and line[pos] != ' ':
                stacks[i].append(line[pos])

    # Parse moves
    moves = []
    for line in move_lines:
        match = re.match(r"move (\d+) from (\d+) to (\d+)", line)
        if match:
            n, src, dst = map(int, match.groups())
            # Convert to 0-indexed
            moves.append((n, src - 1, dst - 1))

    return stacks, moves


def move_one_at_a_time(stacks: list[list[str]], n: int, src: int, dst: int) -> None:
    """Move n crates one at a time from src to dst (part 1)."""
    for _ in range(n):
        if stacks[src]:
            crate = stacks[src].pop()
            stacks[dst].append(crate)


def move_n_at_a_time(stacks: list[list[str]], n: int, src: int, dst: int) -> None:
    """Move n crates at once from src to dst (part 2)."""
    # Take n crates from top of src
    crates = stacks[src][-n:]
    stacks[src] = stacks[src][:-n]
    stacks[dst].extend(crates)


def top_of_stacks(stacks: list[list[str]]) -> str:
    """Get the top crate from each stack as a string."""
    return "".join(stack[-1] if stack else "" for stack in stacks)


def part1(stacks: list[list[str]], moves: list[tuple[int, int, int]]) -> str:
    """Execute moves one crate at a time."""
    stacks = deepcopy(stacks)
    for n, src, dst in moves:
        move_one_at_a_time(stacks, n, src, dst)
    return top_of_stacks(stacks)


def part2(stacks: list[list[str]], moves: list[tuple[int, int, int]]) -> str:
    """Execute moves n crates at a time."""
    stacks = deepcopy(stacks)
    for n, src, dst in moves:
        move_n_at_a_time(stacks, n, src, dst)
    return top_of_stacks(stacks)


def main():
    stacks, moves = parse_input(INPUT_FILE)

    answer1 = part1(stacks, moves)
    answer2 = part2(stacks, moves)

    print(f"Part 1: {answer1}")  # VGBBJCRMN
    print(f"Part 2: {answer2}")  # LBBVJBRMH

    return answer1, answer2


if __name__ == "__main__":
    main()
