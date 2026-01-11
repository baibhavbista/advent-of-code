"""Day 1: Calorie Counting - https://adventofcode.com/2022/day/1"""

INPUT_FILE = "inputs/2022/day01.txt"


def parse_input(filename: str) -> list[list[int]]:
    """Parse input into groups of calories per elf."""
    with open(filename) as f:
        content = f.read().strip()

    groups = content.split("\n\n")
    return [[int(cal) for cal in group.split("\n")] for group in groups]


def total_calories_per_elf(elf_groups: list[list[int]]) -> list[int]:
    """Calculate total calories carried by each elf."""
    return [sum(group) for group in elf_groups]


def part1(totals: list[int]) -> int:
    """Maximum number of calories carried by any elf."""
    return max(totals)


def part2(totals: list[int]) -> int:
    """Sum of calories carried by the 3 elves with most calories."""
    return sum(sorted(totals, reverse=True)[:3])


def main():
    elf_groups = parse_input(INPUT_FILE)
    totals = total_calories_per_elf(elf_groups)

    answer1 = part1(totals)
    answer2 = part2(totals)

    print(f"Part 1: {answer1}")
    print(f"Part 2: {answer2}")

    return answer1, answer2


if __name__ == "__main__":
    main()
