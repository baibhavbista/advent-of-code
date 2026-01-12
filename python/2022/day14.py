"""Day 14: Regolith Reservoir - https://adventofcode.com/2022/day/14"""

INPUT_FILE = "inputs/2022/day14.txt"

SAND_SOURCE = (500, 0)


def parse_input(filename: str) -> set[tuple[int, int]]:
    """Parse rock structures into set of coordinates."""
    rocks = set()

    with open(filename) as f:
        for line in f:
            points = []
            for coord_str in line.strip().split(" -> "):
                x, y = map(int, coord_str.split(","))
                points.append((x, y))

            # Draw lines between consecutive points
            for i in range(len(points) - 1):
                x1, y1 = points[i]
                x2, y2 = points[i + 1]

                if x1 == x2:  # Vertical line
                    for y in range(min(y1, y2), max(y1, y2) + 1):
                        rocks.add((x1, y))
                else:  # Horizontal line
                    for x in range(min(x1, x2), max(x1, x2) + 1):
                        rocks.add((x, y1))

    return rocks


def get_bounds(rocks: set[tuple[int, int]]) -> tuple[int, int, int, int]:
    """Get bounding box of rocks + sand source."""
    all_coords = rocks | {SAND_SOURCE}
    min_x = min(c[0] for c in all_coords)
    max_x = max(c[0] for c in all_coords)
    min_y = min(c[1] for c in all_coords)
    max_y = max(c[1] for c in all_coords)
    return min_x, max_x, min_y, max_y


def simulate_sand_part1(rocks: set[tuple[int, int]]) -> int:
    """Simulate sand falling until it goes into the void."""
    min_x, max_x, min_y, max_y = get_bounds(rocks)
    obstructions = rocks.copy()
    sand_count = 0

    while True:
        # Drop new sand from source
        x, y = SAND_SOURCE

        while True:
            # Check if we're falling into the void
            if y > max_y:
                return sand_count

            # Try to move: down, down-left, down-right
            if (x, y + 1) not in obstructions:
                y += 1
            elif (x - 1, y + 1) not in obstructions:
                x -= 1
                y += 1
            elif (x + 1, y + 1) not in obstructions:
                x += 1
                y += 1
            else:
                # Sand comes to rest
                obstructions.add((x, y))
                sand_count += 1
                break


def simulate_sand_part2(rocks: set[tuple[int, int]]) -> int:
    """Simulate sand with infinite floor until source is blocked."""
    _, _, _, max_y = get_bounds(rocks)
    floor_y = max_y + 2
    obstructions = rocks.copy()
    sand_count = 0

    while True:
        # Drop new sand from source
        x, y = SAND_SOURCE

        # Check if source is blocked
        if SAND_SOURCE in obstructions:
            return sand_count

        while True:
            # Check if we hit the floor
            if y + 1 == floor_y:
                obstructions.add((x, y))
                sand_count += 1
                break

            # Try to move: down, down-left, down-right
            if (x, y + 1) not in obstructions:
                y += 1
            elif (x - 1, y + 1) not in obstructions:
                x -= 1
                y += 1
            elif (x + 1, y + 1) not in obstructions:
                x += 1
                y += 1
            else:
                # Sand comes to rest
                obstructions.add((x, y))
                sand_count += 1
                break


def part1(rocks: set[tuple[int, int]]) -> int:
    """Count sand units before falling into void."""
    return simulate_sand_part1(rocks)


def part2(rocks: set[tuple[int, int]]) -> int:
    """Count sand units until source is blocked."""
    return simulate_sand_part2(rocks)


def main():
    rocks = parse_input(INPUT_FILE)

    answer1 = part1(rocks)
    answer2 = part2(rocks)

    print(f"Part 1: {answer1}")  # 885
    print(f"Part 2: {answer2}")  # 28691

    return answer1, answer2


if __name__ == "__main__":
    main()
