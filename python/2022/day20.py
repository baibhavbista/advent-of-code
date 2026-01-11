"""Day 20: Grove Positioning System - https://adventofcode.com/2022/day/20"""

INPUT_FILE = "inputs/2022/day20.txt"


def parse_input(filename: str) -> list[int]:
    """Parse list of numbers."""
    with open(filename) as f:
        return [int(line.strip()) for line in f]


def mix(numbers: list[int], decryption_key: int = 1, num_mixes: int = 1) -> int:
    """Mix the list and return sum of grove coordinates."""
    n = len(numbers)
    # Apply decryption key
    actual_numbers = [x * decryption_key for x in numbers]

    # Track original indices
    indices = list(range(n))

    for _ in range(num_mixes):
        for original_idx in range(n):
            # Find current position of this element
            current_pos = indices.index(original_idx)
            value = actual_numbers[original_idx]

            # Remove from current position
            indices.pop(current_pos)

            # Calculate new position
            new_pos = (current_pos + value) % (n - 1)

            # Insert at new position
            indices.insert(new_pos, original_idx)

    # Find position of 0 in mixed list
    zero_original_idx = actual_numbers.index(0)
    zero_pos = indices.index(zero_original_idx)

    # Get values at positions 1000, 2000, 3000 after 0
    result = 0
    for offset in [1000, 2000, 3000]:
        pos = (zero_pos + offset) % n
        original_idx = indices[pos]
        result += actual_numbers[original_idx]

    return result


def part1(numbers: list[int]) -> int:
    """Mix once and find grove coordinates."""
    return mix(numbers)


def part2(numbers: list[int]) -> int:
    """Apply decryption key, mix 10 times, find grove coordinates."""
    return mix(numbers, decryption_key=811589153, num_mixes=10)


def main():
    numbers = parse_input(INPUT_FILE)

    answer1 = part1(numbers)
    answer2 = part2(numbers)

    print(f"Part 1: {answer1}")  # 8721
    print(f"Part 2: {answer2}")  # 831878881825

    return answer1, answer2


if __name__ == "__main__":
    main()
