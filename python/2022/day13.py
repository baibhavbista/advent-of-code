"""Day 13: Distress Signal - https://adventofcode.com/2022/day/13"""

import ast
from functools import cmp_to_key

INPUT_FILE = "inputs/2022/day13.txt"


def parse_input(filename: str) -> list[tuple]:
    """Parse input into pairs of packets."""
    with open(filename) as f:
        content = f.read().strip()

    pairs = []
    for pair_str in content.split("\n\n"):
        lines = pair_str.split("\n")
        left = ast.literal_eval(lines[0])
        right = ast.literal_eval(lines[1])
        pairs.append((left, right))

    return pairs


def compare(left, right) -> int:
    """
    Compare two packets.
    Returns: -1 if correct order, 1 if wrong order, 0 if continue
    """
    # Both integers
    if isinstance(left, int) and isinstance(right, int):
        if left < right:
            return -1
        elif left > right:
            return 1
        return 0

    # Both lists
    if isinstance(left, list) and isinstance(right, list):
        for i in range(min(len(left), len(right))):
            result = compare(left[i], right[i])
            if result != 0:
                return result
        # Compare lengths
        if len(left) < len(right):
            return -1
        elif len(left) > len(right):
            return 1
        return 0

    # Mixed types - convert integer to list
    if isinstance(left, int):
        return compare([left], right)
    else:
        return compare(left, [right])


def part1(pairs: list[tuple]) -> int:
    """Sum of 1-indexed indices of pairs in correct order."""
    total = 0
    for i, (left, right) in enumerate(pairs):
        if compare(left, right) == -1:
            total += i + 1
    return total


def part2(pairs: list[tuple]) -> int:
    """Find decoder key after sorting with divider packets."""
    # Get all packets
    packets = []
    for left, right in pairs:
        packets.append(left)
        packets.append(right)

    # Add divider packets
    divider1 = [[2]]
    divider2 = [[6]]
    packets.append(divider1)
    packets.append(divider2)

    # Sort packets
    packets.sort(key=cmp_to_key(compare))

    # Find divider packet positions (1-indexed)
    idx1 = packets.index(divider1) + 1
    idx2 = packets.index(divider2) + 1

    return idx1 * idx2


def main():
    pairs = parse_input(INPUT_FILE)

    answer1 = part1(pairs)
    answer2 = part2(pairs)

    print(f"Part 1: {answer1}")  # 6656
    print(f"Part 2: {answer2}")  # 19716

    return answer1, answer2


if __name__ == "__main__":
    main()
