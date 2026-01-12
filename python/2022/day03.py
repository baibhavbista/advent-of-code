"""Day 3: Rucksack Reorganization - https://adventofcode.com/2022/day/3"""

INPUT_FILE = "inputs/2022/day03.txt"


def parse_input(filename: str) -> list[str]:
    """Parse input into list of rucksack strings."""
    with open(filename) as f:
        return f.read().strip().split("\n")


def priority(char: str) -> int:
    """Calculate priority of an item type."""
    if char.islower():
        return ord(char) - ord('a') + 1
    else:
        return ord(char) - ord('A') + 27


def find_common_item_in_rucksack(rucksack: str) -> str:
    """Find the item type that appears in both compartments."""
    mid = len(rucksack) // 2
    first_half = set(rucksack[:mid])
    second_half = set(rucksack[mid:])
    common = first_half & second_half
    return common.pop()


def find_badge(group: list[str]) -> str:
    """Find the common item type among all rucksacks in a group."""
    sets = [set(rucksack) for rucksack in group]
    common = sets[0]
    for s in sets[1:]:
        common = common & s
    return common.pop()


def part1(rucksacks: list[str]) -> int:
    """Sum of priorities of items appearing in both compartments."""
    return sum(priority(find_common_item_in_rucksack(r)) for r in rucksacks)


def part2(rucksacks: list[str]) -> int:
    """Sum of priorities of badge items for each 3-elf group."""
    total = 0
    for i in range(0, len(rucksacks), 3):
        group = rucksacks[i:i+3]
        badge = find_badge(group)
        total += priority(badge)
    return total


def main():
    rucksacks = parse_input(INPUT_FILE)

    answer1 = part1(rucksacks)
    answer2 = part2(rucksacks)

    print(f"Part 1: {answer1}")
    print(f"Part 2: {answer2}")

    return answer1, answer2


if __name__ == "__main__":
    main()
