"""Day 2: Rock Paper Scissors - https://adventofcode.com/2022/day/2"""

INPUT_FILE = "inputs/2022/day02.txt"


def parse_input(filename: str) -> list[tuple[str, str]]:
    """Parse input into list of (their_move, my_move/outcome) tuples."""
    with open(filename) as f:
        return [tuple(line.split()) for line in f.read().strip().split("\n")]


def char_offset(char: str, base: str) -> int:
    """Get offset of character from base character."""
    return ord(char) - ord(base)


def shape_score(my_move_int: int) -> int:
    """Score for the shape chosen (1 for Rock, 2 for Paper, 3 for Scissors)."""
    return my_move_int + 1


def outcome_int(their_move_int: int, my_move_int: int) -> int:
    """Calculate outcome: 0=loss, 1=draw, 2=win."""
    return (my_move_int - their_move_int + 1) % 3


def outcome_score(outcome: int) -> int:
    """Score for outcome (0, 3, or 6)."""
    return outcome * 3


def my_move_int_from_outcome(their_move_int: int, outcome: int) -> int:
    """Calculate my move given their move and desired outcome."""
    return (their_move_int + outcome - 1) % 3


def round_score_part1(their_move: str, my_move: str) -> int:
    """Calculate score for a round in part 1."""
    their_move_int = char_offset(their_move, "A")
    my_move_int = char_offset(my_move, "X")

    return shape_score(my_move_int) + outcome_score(outcome_int(their_move_int, my_move_int))


def round_score_part2(their_move: str, outcome_char: str) -> int:
    """Calculate score for a round in part 2."""
    their_move_int = char_offset(their_move, "A")
    outcome = char_offset(outcome_char, "X")
    my_move_int = my_move_int_from_outcome(their_move_int, outcome)

    return shape_score(my_move_int) + outcome_score(outcome)


def part1(rounds: list[tuple[str, str]]) -> int:
    """Total score if following part 1 interpretation."""
    return sum(round_score_part1(their, mine) for their, mine in rounds)


def part2(rounds: list[tuple[str, str]]) -> int:
    """Total score if following part 2 interpretation."""
    return sum(round_score_part2(their, outcome) for their, outcome in rounds)


def main():
    rounds = parse_input(INPUT_FILE)

    answer1 = part1(rounds)
    answer2 = part2(rounds)

    print(f"Part 1: {answer1}")  # 17189
    print(f"Part 2: {answer2}")  # 13490

    return answer1, answer2


if __name__ == "__main__":
    main()
