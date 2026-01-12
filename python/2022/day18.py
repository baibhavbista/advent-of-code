"""Day 18: Boiling Boulders - https://adventofcode.com/2022/day/18"""

from collections import deque

INPUT_FILE = "inputs/2022/day18.txt"


def parse_input(filename: str) -> set[tuple[int, int, int]]:
    """Parse cube coordinates."""
    cubes = set()
    with open(filename) as f:
        for line in f:
            x, y, z = map(int, line.strip().split(","))
            cubes.add((x, y, z))
    return cubes


def get_neighbors(coord: tuple[int, int, int]) -> list[tuple[int, int, int]]:
    """Get 6 face-adjacent neighbors."""
    x, y, z = coord
    return [
        (x + 1, y, z), (x - 1, y, z),
        (x, y + 1, z), (x, y - 1, z),
        (x, y, z + 1), (x, y, z - 1)
    ]


def count_exposed_faces(cubes: set[tuple[int, int, int]]) -> int:
    """Count faces not touching another cube."""
    total = 0
    for cube in cubes:
        for neighbor in get_neighbors(cube):
            if neighbor not in cubes:
                total += 1
    return total


def count_external_faces(cubes: set[tuple[int, int, int]]) -> int:
    """Count faces exposed to exterior (not trapped air pockets)."""
    # Find bounding box with padding
    if not cubes:
        return 0

    min_x = min(c[0] for c in cubes) - 1
    max_x = max(c[0] for c in cubes) + 1
    min_y = min(c[1] for c in cubes) - 1
    max_y = max(c[1] for c in cubes) + 1
    min_z = min(c[2] for c in cubes) - 1
    max_z = max(c[2] for c in cubes) + 1

    def in_bounds(coord):
        x, y, z = coord
        return (min_x <= x <= max_x and
                min_y <= y <= max_y and
                min_z <= z <= max_z)

    # BFS from corner to find all reachable external points
    start = (min_x, min_y, min_z)
    visited = {start}
    queue = deque([start])

    while queue:
        current = queue.popleft()
        for neighbor in get_neighbors(current):
            if neighbor not in visited and neighbor not in cubes and in_bounds(neighbor):
                visited.add(neighbor)
                queue.append(neighbor)

    # Count cube faces touching external air
    external_faces = 0
    for cube in cubes:
        for neighbor in get_neighbors(cube):
            if neighbor in visited:
                external_faces += 1

    return external_faces


def part1(cubes: set[tuple[int, int, int]]) -> int:
    """Total surface area."""
    return count_exposed_faces(cubes)


def part2(cubes: set[tuple[int, int, int]]) -> int:
    """External surface area only."""
    return count_external_faces(cubes)


def main():
    cubes = parse_input(INPUT_FILE)

    answer1 = part1(cubes)
    answer2 = part2(cubes)

    print(f"Part 1: {answer1}")  # 4504
    print(f"Part 2: {answer2}")  # 2556

    return answer1, answer2


if __name__ == "__main__":
    main()
