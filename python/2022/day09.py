"""Day 9: Rope Bridge - https://adventofcode.com/2022/day/9"""

INPUT_FILE = "inputs/2022/day09.txt"


def parse_input(filename: str) -> list[str]:
    """Parse input into list of individual moves."""
    moves = []
    with open(filename) as f:
        for line in f:
            direction, count = line.strip().split()
            moves.extend([direction] * int(count))
    return moves


def is_adjacent(pos1: tuple[int, int], pos2: tuple[int, int]) -> bool:
    """Check if two positions are adjacent (including diagonally)."""
    return abs(pos1[0] - pos2[0]) <= 1 and abs(pos1[1] - pos2[1]) <= 1


def move_head(pos: tuple[int, int], direction: str) -> tuple[int, int]:
    """Move head position in given direction."""
    x, y = pos
    if direction == 'L':
        return (x - 1, y)
    elif direction == 'R':
        return (x + 1, y)
    elif direction == 'U':
        return (x, y + 1)
    elif direction == 'D':
        return (x, y - 1)
    return pos


def tail_catchup(tail_pos: tuple[int, int], head_pos: tuple[int, int]) -> tuple[int, int]:
    """Move tail to catch up with head if needed."""
    if is_adjacent(tail_pos, head_pos):
        return tail_pos

    tx, ty = tail_pos
    hx, hy = head_pos

    # If in same row or column, move towards head
    if tx == hx or ty == hy:
        return ((tx + hx) // 2, (ty + hy) // 2)

    # Move diagonally
    dx = 1 if hx > tx else -1
    dy = 1 if hy > ty else -1
    return (tx + dx, ty + dy)


def simulate_rope(moves: list[str], num_knots: int) -> int:
    """Simulate rope with given number of knots and count tail positions."""
    # All knots start at origin
    knots = [(0, 0)] * num_knots
    tail_positions = {(0, 0)}

    for direction in moves:
        # Move head
        knots[0] = move_head(knots[0], direction)

        # Each subsequent knot follows the one before it
        for i in range(1, num_knots):
            knots[i] = tail_catchup(knots[i], knots[i - 1])

        # Track tail position
        tail_positions.add(knots[-1])

    return len(tail_positions)


def part1(moves: list[str]) -> int:
    """Count positions visited by tail with 2 knots."""
    return simulate_rope(moves, 2)


def part2(moves: list[str]) -> int:
    """Count positions visited by tail with 10 knots."""
    return simulate_rope(moves, 10)


def main():
    moves = parse_input(INPUT_FILE)

    answer1 = part1(moves)
    answer2 = part2(moves)

    print(f"Part 1: {answer1}")  # 6037
    print(f"Part 2: {answer2}")  # 2485

    return answer1, answer2


if __name__ == "__main__":
    main()
