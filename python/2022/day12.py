"""Day 12: Hill Climbing Algorithm - https://adventofcode.com/2022/day/12"""

from collections import deque

INPUT_FILE = "inputs/2022/day12.txt"


def parse_input(filename: str) -> tuple[list[list[int]], tuple[int, int], tuple[int, int]]:
    """Parse input into height grid, start position, and end position."""
    with open(filename) as f:
        lines = [line.strip() for line in f]

    start = None
    end = None
    grid = []

    for r, line in enumerate(lines):
        row = []
        for c, char in enumerate(line):
            if char == 'S':
                start = (r, c)
                row.append(0)  # height 'a'
            elif char == 'E':
                end = (r, c)
                row.append(25)  # height 'z'
            else:
                row.append(ord(char) - ord('a'))
        grid.append(row)

    return grid, start, end


def bfs(grid: list[list[int]], start: tuple[int, int],
        can_move, is_target) -> int:
    """BFS to find shortest path."""
    rows = len(grid)
    cols = len(grid[0])

    queue = deque([(start, 0)])
    visited = {start}

    while queue:
        (r, c), dist = queue.popleft()

        if is_target(r, c):
            return dist

        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc

            if 0 <= nr < rows and 0 <= nc < cols:
                if (nr, nc) not in visited and can_move(r, c, nr, nc):
                    visited.add((nr, nc))
                    queue.append(((nr, nc), dist + 1))

    return float('inf')


def part1(grid: list[list[int]], start: tuple[int, int], end: tuple[int, int]) -> int:
    """Find shortest path from S to E."""
    def can_move(r, c, nr, nc):
        return grid[nr][nc] <= grid[r][c] + 1

    def is_target(r, c):
        return (r, c) == end

    return bfs(grid, start, can_move, is_target)


def part2(grid: list[list[int]], end: tuple[int, int]) -> int:
    """Find shortest path from any 'a' elevation to E."""
    # Search backwards from E
    def can_move(r, c, nr, nc):
        # Reversed: can move if current height is at most 1 higher than next
        return grid[r][c] <= grid[nr][nc] + 1

    def is_target(r, c):
        return grid[r][c] == 0  # elevation 'a'

    return bfs(grid, end, can_move, is_target)


def main():
    grid, start, end = parse_input(INPUT_FILE)

    answer1 = part1(grid, start, end)
    answer2 = part2(grid, end)

    print(f"Part 1: {answer1}")  # 468
    print(f"Part 2: {answer2}")  # 459

    return answer1, answer2


if __name__ == "__main__":
    main()
