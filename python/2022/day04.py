"""Day 4: Camp Cleanup - https://adventofcode.com/2022/day/4"""

INPUT_FILE = "inputs/2022/day04.txt"


def parse_input(filename: str) -> list[tuple[int, int, int, int]]:
    """Parse input into list of (a, b, c, d) tuples representing ranges."""
    pairs = []
    with open(filename) as f:
        for line in f.read().strip().split("\n"):
            # Format: "a-b,c-d"
            left, right = line.split(",")
            a, b = map(int, left.split("-"))
            c, d = map(int, right.split("-"))
            pairs.append((a, b, c, d))
    return pairs


def one_fully_contains_other(a: int, b: int, c: int, d: int) -> bool:
    """Check if one range fully contains the other."""
    # Either [a,b] contains [c,d] or [c,d] contains [a,b]
    return (a == c or b == d or
            (a < c and d < b) or
            (c < a and b < d))


def ranges_overlap(a: int, b: int, c: int, d: int) -> bool:
    """Check if ranges overlap at all."""
    # Swap to ensure a <= c
    if c < a:
        a, b, c, d = c, d, a, b
    # They overlap if second range starts before first ends
    return c <= b


def part1(pairs: list[tuple[int, int, int, int]]) -> int:
    """Count pairs where one range fully contains the other."""
    return sum(1 for a, b, c, d in pairs if one_fully_contains_other(a, b, c, d))


def part2(pairs: list[tuple[int, int, int, int]]) -> int:
    """Count pairs where ranges overlap."""
    return sum(1 for a, b, c, d in pairs if ranges_overlap(a, b, c, d))


def main():
    pairs = parse_input(INPUT_FILE)

    answer1 = part1(pairs)
    answer2 = part2(pairs)

    print(f"Part 1: {answer1}")  # 605
    print(f"Part 2: {answer2}")  # 914

    return answer1, answer2


if __name__ == "__main__":
    main()
