"""Day 17: Pyroclastic Flow - https://adventofcode.com/2022/day/17"""

INPUT_FILE = "inputs/2022/day17.txt"

# Rock shapes defined by offsets from bottom-left corner
ROCK_TYPES = [
    [(0, 0), (1, 0), (2, 0), (3, 0)],           # -
    [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],   # +
    [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],   # J
    [(0, 0), (0, 1), (0, 2), (0, 3)],           # |
    [(0, 0), (1, 0), (0, 1), (1, 1)],           # square
]


def parse_input(filename: str) -> list[int]:
    """Parse jet pattern into list of x-offsets (-1 for left, 1 for right)."""
    with open(filename) as f:
        content = f.read().strip()
    return [-1 if c == '<' else 1 for c in content]


def get_rock_coords(rock_type: int, bottom_left: tuple[int, int]) -> list[tuple[int, int]]:
    """Get absolute coordinates of rock pieces."""
    bx, by = bottom_left
    return [(bx + dx, by + dy) for dx, dy in ROCK_TYPES[rock_type]]


def simulate(jet_pattern: list[int], num_rocks: int) -> int:
    """Simulate falling rocks and return tower height."""
    occupied = set()
    tower_height = 0
    jet_index = 0
    height_gains = []

    for rock_num in range(num_rocks):
        rock_type = rock_num % len(ROCK_TYPES)

        # Starting position: left edge at x=2, bottom at y=tower_height+3
        rock_x = 2
        rock_y = tower_height + 3

        while True:
            # Apply jet
            jet_dx = jet_pattern[jet_index]
            jet_index = (jet_index + 1) % len(jet_pattern)

            # Try horizontal movement
            new_x = rock_x + jet_dx
            new_coords = get_rock_coords(rock_type, (new_x, rock_y))

            # Check bounds and collisions
            valid = True
            for x, y in new_coords:
                if x < 0 or x > 6 or (x, y) in occupied:
                    valid = False
                    break

            if valid:
                rock_x = new_x

            # Try falling
            new_y = rock_y - 1
            new_coords = get_rock_coords(rock_type, (rock_x, new_y))

            # Check floor and collisions
            can_fall = True
            for x, y in new_coords:
                if y < 0 or (x, y) in occupied:
                    can_fall = False
                    break

            if can_fall:
                rock_y = new_y
            else:
                # Rock comes to rest
                coords = get_rock_coords(rock_type, (rock_x, rock_y))
                for coord in coords:
                    occupied.add(coord)

                old_height = tower_height
                tower_height = max(tower_height, max(y + 1 for _, y in coords))
                height_gains.append(tower_height - old_height)
                break

    return tower_height


def simulate_with_cycle_detection(jet_pattern: list[int], target_rocks: int) -> int:
    """Simulate with cycle detection for large number of rocks."""
    if target_rocks <= 10000:
        return simulate(jet_pattern, target_rocks)

    # Simulate enough to find a cycle
    occupied = set()
    tower_height = 0
    jet_index = 0
    height_gains = []

    for rock_num in range(10000):
        rock_type = rock_num % len(ROCK_TYPES)
        rock_x = 2
        rock_y = tower_height + 3

        while True:
            jet_dx = jet_pattern[jet_index]
            jet_index = (jet_index + 1) % len(jet_pattern)

            new_x = rock_x + jet_dx
            new_coords = get_rock_coords(rock_type, (new_x, rock_y))

            valid = all(0 <= x <= 6 and (x, y) not in occupied for x, y in new_coords)
            if valid:
                rock_x = new_x

            new_y = rock_y - 1
            new_coords = get_rock_coords(rock_type, (rock_x, new_y))

            can_fall = all(y >= 0 and (x, y) not in occupied for x, y in new_coords)

            if can_fall:
                rock_y = new_y
            else:
                coords = get_rock_coords(rock_type, (rock_x, rock_y))
                for coord in coords:
                    occupied.add(coord)
                old_height = tower_height
                tower_height = max(tower_height, max(y + 1 for _, y in coords))
                height_gains.append(tower_height - old_height)
                break

    # Find cycle using Floyd's algorithm variant
    # Look for repeating pattern in height_gains
    for cycle_start in range(100, 5000):
        for cycle_len in range(100, (len(height_gains) - cycle_start) // 2):
            # Check if pattern repeats
            match = True
            for i in range(min(100, cycle_len)):
                if height_gains[cycle_start + i] != height_gains[cycle_start + cycle_len + i]:
                    match = False
                    break
            if match:
                # Found potential cycle
                prefix_sum = sum(height_gains[:cycle_start])
                cycle_sum = sum(height_gains[cycle_start:cycle_start + cycle_len])

                rocks_after_prefix = target_rocks - cycle_start
                full_cycles = rocks_after_prefix // cycle_len
                remaining = rocks_after_prefix % cycle_len
                remaining_sum = sum(height_gains[cycle_start:cycle_start + remaining])

                return prefix_sum + full_cycles * cycle_sum + remaining_sum

    # Fallback (shouldn't reach here with proper input)
    return simulate(jet_pattern, target_rocks)


def part1(jet_pattern: list[int]) -> int:
    """Height after 2022 rocks."""
    return simulate(jet_pattern, 2022)


def part2(jet_pattern: list[int]) -> int:
    """Height after 1000000000000 rocks."""
    return simulate_with_cycle_detection(jet_pattern, 1000000000000)


def main():
    jet_pattern = parse_input(INPUT_FILE)

    answer1 = part1(jet_pattern)
    answer2 = part2(jet_pattern)

    print(f"Part 1: {answer1}")  # 3067
    print(f"Part 2: {answer2}")  # 1514369501484

    return answer1, answer2


if __name__ == "__main__":
    main()
