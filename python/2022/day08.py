"""Day 8: Treetop Tree House - https://adventofcode.com/2022/day/8"""

INPUT_FILE = "inputs/2022/day08.txt"


def parse_input(filename: str) -> list[list[int]]:
    """Parse input into 2D grid of tree heights."""
    with open(filename) as f:
        return [[int(c) for c in line.strip()] for line in f]


def transpose(grid: list[list[int]]) -> list[list[int]]:
    """Transpose a 2D grid (swap rows and columns)."""
    return [list(row) for row in zip(*grid)]


def reverse_rows(grid: list[list[int]]) -> list[list[int]]:
    """Reverse each row in the grid."""
    return [row[::-1] for row in grid]


def visible_from_left(row: list[int]) -> list[bool]:
    """For each tree, check if it's visible from the left."""
    result = []
    max_height = -1
    for height in row:
        if height > max_height:
            result.append(True)
            max_height = height
        else:
            result.append(False)
    return result


def viewing_distance_left(row: list[int], idx: int) -> int:
    """Count trees visible to the left from position idx."""
    height = row[idx]
    count = 0
    for i in range(idx - 1, -1, -1):
        count += 1
        if row[i] >= height:
            break
    return count


def compute_visibility_matrix(grid: list[list[int]]) -> list[list[bool]]:
    """Compute visibility from all 4 directions."""
    rows = len(grid)
    cols = len(grid[0])

    # Initialize result matrix
    visible = [[False] * cols for _ in range(rows)]

    # From left
    for r, row in enumerate(grid):
        vis = visible_from_left(row)
        for c in range(cols):
            visible[r][c] = visible[r][c] or vis[c]

    # From right
    reversed_grid = reverse_rows(grid)
    for r, row in enumerate(reversed_grid):
        vis = visible_from_left(row)
        for c in range(cols):
            visible[r][cols - 1 - c] = visible[r][cols - 1 - c] or vis[c]

    # From top
    transposed = transpose(grid)
    for c, col in enumerate(transposed):
        vis = visible_from_left(col)
        for r in range(rows):
            visible[r][c] = visible[r][c] or vis[r]

    # From bottom
    transposed_reversed = reverse_rows(transposed)
    for c, col in enumerate(transposed_reversed):
        vis = visible_from_left(col)
        for r in range(rows):
            visible[rows - 1 - r][c] = visible[rows - 1 - r][c] or vis[r]

    return visible


def compute_scenic_scores(grid: list[list[int]]) -> list[list[int]]:
    """Compute scenic score for each tree."""
    rows = len(grid)
    cols = len(grid[0])
    scores = [[1] * cols for _ in range(rows)]

    # Left viewing distance
    for r, row in enumerate(grid):
        for c in range(cols):
            scores[r][c] *= viewing_distance_left(row, c)

    # Right viewing distance - iterate on reversed row, map back to original
    reversed_grid = reverse_rows(grid)
    for r, row in enumerate(reversed_grid):
        for c in range(cols):
            # c is position in reversed row, (cols-1-c) is position in original
            scores[r][cols - 1 - c] *= viewing_distance_left(row, c)

    # Top viewing distance
    transposed = transpose(grid)
    for c, col in enumerate(transposed):
        for r in range(rows):
            scores[r][c] *= viewing_distance_left(col, r)

    # Bottom viewing distance - iterate on reversed column, map back to original
    transposed_reversed = reverse_rows(transposed)
    for c, col in enumerate(transposed_reversed):
        for r in range(rows):
            # r is position in reversed column, (rows-1-r) is position in original
            scores[rows - 1 - r][c] *= viewing_distance_left(col, r)

    return scores


def part1(grid: list[list[int]]) -> int:
    """Count trees visible from outside."""
    visibility = compute_visibility_matrix(grid)
    return sum(1 for row in visibility for visible in row if visible)


def part2(grid: list[list[int]]) -> int:
    """Find highest scenic score."""
    scores = compute_scenic_scores(grid)
    return max(max(row) for row in scores)


def main():
    grid = parse_input(INPUT_FILE)

    answer1 = part1(grid)
    answer2 = part2(grid)

    print(f"Part 1: {answer1}")  # 1662
    print(f"Part 2: {answer2}")  # 537600

    return answer1, answer2


if __name__ == "__main__":
    main()
