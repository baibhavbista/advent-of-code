"""Day 24: Blizzard Basin - https://adventofcode.com/2022/day/24"""

from collections import deque

INPUT_FILE = "inputs/2022/day24.txt"

DIRECTION_OFFSETS = {
    '^': (-1, 0),
    'v': (1, 0),
    '<': (0, -1),
    '>': (0, 1),
}


def parse_input(filename: str) -> tuple[set, tuple, tuple, tuple]:
    """Parse input and return blizzards, start, end, dimensions."""
    with open(filename) as f:
        lines = [line.rstrip() for line in f]

    rows = len(lines)
    cols = len(lines[0])

    # Find start and end (gaps in walls)
    start = (0, lines[0].index('.'))
    end = (rows - 1, lines[rows - 1].index('.'))

    # Parse blizzards
    blizzards = set()
    for row, line in enumerate(lines):
        for col, char in enumerate(line):
            if char in '^v<>':
                blizzards.add((row, col, char))

    return blizzards, start, end, (rows, cols)


def compute_blizzard_positions(blizzards: set, dims: tuple, num_states: int) -> list[set]:
    """Precompute blizzard positions for each time step."""
    rows, cols = dims
    inner_rows = rows - 2
    inner_cols = cols - 2

    positions = []

    for t in range(num_states):
        pos_at_t = set()
        for row, col, direction in blizzards:
            dr, dc = DIRECTION_OFFSETS[direction]

            # Calculate new position (wrap within inner area)
            new_row = 1 + ((row - 1 + dr * t) % inner_rows)
            new_col = 1 + ((col - 1 + dc * t) % inner_cols)

            pos_at_t.add((new_row, new_col))

        positions.append(pos_at_t)

    return positions


def lcm(a: int, b: int) -> int:
    """Compute least common multiple."""
    def gcd(x, y):
        while y:
            x, y = y, x % y
        return x
    return a * b // gcd(a, b)


def bfs(start: tuple, end: tuple, dims: tuple, blizzard_positions: list,
        start_time: int = 0) -> int:
    """BFS to find shortest path avoiding blizzards."""
    rows, cols = dims
    cycle_len = len(blizzard_positions)

    # State: (row, col, time % cycle_len)
    queue = deque([(start[0], start[1], start_time)])
    visited = {(start[0], start[1], start_time % cycle_len)}

    while queue:
        row, col, time = queue.popleft()

        if (row, col) == end:
            return time

        next_time = time + 1
        next_blizzards = blizzard_positions[next_time % cycle_len]

        # Try all moves: wait, up, down, left, right
        for dr, dc in [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]:
            new_row, new_col = row + dr, col + dc

            # Check bounds
            if new_row < 0 or new_row >= rows or new_col < 0 or new_col >= cols:
                continue

            # Check walls (except start and end)
            if (new_row, new_col) not in [start, end]:
                if new_row == 0 or new_row == rows - 1 or new_col == 0 or new_col == cols - 1:
                    continue

            # Check blizzards
            if (new_row, new_col) in next_blizzards:
                continue

            state = (new_row, new_col, next_time % cycle_len)
            if state not in visited:
                visited.add(state)
                queue.append((new_row, new_col, next_time))

    return -1


def part1(blizzards: set, start: tuple, end: tuple, dims: tuple) -> int:
    """Find shortest path from start to end."""
    rows, cols = dims
    cycle_len = lcm(rows - 2, cols - 2)
    blizzard_positions = compute_blizzard_positions(blizzards, dims, cycle_len)

    return bfs(start, end, dims, blizzard_positions)


def part2(blizzards: set, start: tuple, end: tuple, dims: tuple) -> int:
    """Find shortest path: start -> end -> start -> end."""
    rows, cols = dims
    cycle_len = lcm(rows - 2, cols - 2)
    blizzard_positions = compute_blizzard_positions(blizzards, dims, cycle_len)

    time1 = bfs(start, end, dims, blizzard_positions, 0)
    time2 = bfs(end, start, dims, blizzard_positions, time1)
    time3 = bfs(start, end, dims, blizzard_positions, time2)

    return time3


def main():
    blizzards, start, end, dims = parse_input(INPUT_FILE)

    answer1 = part1(blizzards, start, end, dims)
    answer2 = part2(blizzards, start, end, dims)

    print(f"Part 1: {answer1}")
    print(f"Part 2: {answer2}")

    return answer1, answer2


if __name__ == "__main__":
    main()
