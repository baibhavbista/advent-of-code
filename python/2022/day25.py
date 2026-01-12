"""Day 25: Full of Hot Air - https://adventofcode.com/2022/day/25"""

INPUT_FILE = "inputs/2022/day25.txt"

SNAFU_DIGITS = {'0': 0, '1': 1, '2': 2, '=': -2, '-': -1}
DIGIT_TO_SNAFU = {0: '0', 1: '1', 2: '2', 3: '=', 4: '-'}


def snafu_to_decimal(snafu: str) -> int:
    """Convert SNAFU number to decimal."""
    result = 0
    for char in snafu:
        result = result * 5 + SNAFU_DIGITS[char]
    return result


def decimal_to_snafu(decimal: int) -> str:
    """Convert decimal number to SNAFU."""
    if decimal == 0:
        return '0'

    digits = []

    while decimal > 0:
        remainder = decimal % 5
        digits.append(DIGIT_TO_SNAFU[remainder])

        if remainder >= 3:
            decimal += 5  # Carry

        decimal //= 5

    return ''.join(reversed(digits))


def parse_input(filename: str) -> list[str]:
    """Parse SNAFU numbers from input."""
    with open(filename) as f:
        return [line.strip() for line in f if line.strip()]


def part1(snafu_numbers: list[str]) -> str:
    """Sum all SNAFU numbers and return result in SNAFU."""
    total = sum(snafu_to_decimal(s) for s in snafu_numbers)
    return decimal_to_snafu(total)


def part2() -> str:
    """Part 2 is just getting all 50 stars."""
    return "Merry Christmas!"


def main():
    snafu_numbers = parse_input(INPUT_FILE)

    answer1 = part1(snafu_numbers)
    answer2 = part2()

    print(f"Part 1: {answer1}")  # 2-0-020-1==1021=--01
    print(f"Part 2: {answer2}")

    return answer1, answer2


if __name__ == "__main__":
    main()
