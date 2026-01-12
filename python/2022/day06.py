"""Day 6: Tuning Trouble - https://adventofcode.com/2022/day/6"""

INPUT_FILE = "inputs/2022/day06.txt"


def parse_input(filename: str) -> str:
    """Parse input into datastream buffer string."""
    with open(filename) as f:
        return f.read().strip()


def index_after_n_unique_chars(buffer: str, n: int) -> int:
    """Find the index after first occurrence of n unique consecutive characters."""
    for i in range(n, len(buffer) + 1):
        if len(set(buffer[i-n:i])) == n:
            return i
    return -1


def part1(buffer: str) -> int:
    """Find start-of-packet marker (4 unique chars)."""
    return index_after_n_unique_chars(buffer, 4)


def part2(buffer: str) -> int:
    """Find start-of-message marker (14 unique chars)."""
    return index_after_n_unique_chars(buffer, 14)


def main():
    buffer = parse_input(INPUT_FILE)

    # Test cases from problem
    assert index_after_n_unique_chars("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4) == 7
    assert index_after_n_unique_chars("bvwbjplbgvbhsrlpgdmjqwftvncz", 4) == 5
    assert index_after_n_unique_chars("nppdvjthqldpwncqszvftbrmjlhg", 4) == 6
    assert index_after_n_unique_chars("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4) == 10
    assert index_after_n_unique_chars("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4) == 11

    assert index_after_n_unique_chars("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14) == 19
    assert index_after_n_unique_chars("bvwbjplbgvbhsrlpgdmjqwftvncz", 14) == 23
    assert index_after_n_unique_chars("nppdvjthqldpwncqszvftbrmjlhg", 14) == 23
    assert index_after_n_unique_chars("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14) == 29
    assert index_after_n_unique_chars("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14) == 26

    answer1 = part1(buffer)
    answer2 = part2(buffer)

    print(f"Part 1: {answer1}")  # 1850
    print(f"Part 2: {answer2}")  # 2823

    return answer1, answer2


if __name__ == "__main__":
    main()
