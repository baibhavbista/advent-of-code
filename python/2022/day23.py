"""Day 23: Unstable Diffusion - https://adventofcode.com/2022/day/23"""

from collections import Counter

INPUT_FILE = "inputs/2022/day23.txt"

# Direction checks and proposed moves
DIRECTIONS = [
    # North: check N, NE, NW; move N
    ([(-1, 0), (-1, 1), (-1, -1)], (-1, 0)),
    # South: check S, SE, SW; move S
    ([(1, 0), (1, 1), (1, -1)], (1, 0)),
    # West: check W, NW, SW; move W
    ([(0, -1), (-1, -1), (1, -1)], (0, -1)),
    # East: check E, NE, SE; move E
    ([(0, 1), (-1, 1), (1, 1)], (0, 1)),
]

ALL_NEIGHBORS = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]


def parse_input(filename: str) -> set[tuple[int, int]]:
    """Parse elf positions."""
    elves = set()
    with open(filename) as f:
        for row, line in enumerate(f):
            for col, char in enumerate(line):
                if char == '#':
                    elves.add((row, col))
    return elves


def simulate_round(elves: set[tuple[int, int]], round_num: int) -> set[tuple[int, int]]:
    """Simulate one round of movement."""
    # First half: propose moves
    proposals = {}  # elf -> proposed position

    for elf in elves:
        row, col = elf

        # Check if any neighbors exist
        has_neighbor = any((row + dr, col + dc) in elves for dr, dc in ALL_NEIGHBORS)

        if not has_neighbor:
            proposals[elf] = elf
            continue

        # Try each direction in order (rotated based on round)
        proposed = None
        for i in range(4):
            dir_idx = (round_num + i) % 4
            checks, move = DIRECTIONS[dir_idx]

            # Check if all positions in this direction are empty
            if all((row + dr, col + dc) not in elves for dr, dc in checks):
                proposed = (row + move[0], col + move[1])
                break

        if proposed is None:
            proposals[elf] = elf
        else:
            proposals[elf] = proposed

    # Second half: execute moves (only if unique proposal)
    proposal_counts = Counter(proposals.values())

    new_elves = set()
    for elf, proposed in proposals.items():
        if proposal_counts[proposed] == 1:
            new_elves.add(proposed)
        else:
            new_elves.add(elf)

    return new_elves


def count_empty_tiles(elves: set[tuple[int, int]]) -> int:
    """Count empty tiles in smallest bounding rectangle."""
    if not elves:
        return 0

    min_row = min(e[0] for e in elves)
    max_row = max(e[0] for e in elves)
    min_col = min(e[1] for e in elves)
    max_col = max(e[1] for e in elves)

    total_tiles = (max_row - min_row + 1) * (max_col - min_col + 1)
    return total_tiles - len(elves)


def part1(elves: set[tuple[int, int]]) -> int:
    """Empty tiles after 10 rounds."""
    elves = elves.copy()
    for round_num in range(10):
        elves = simulate_round(elves, round_num)
    return count_empty_tiles(elves)


def part2(elves: set[tuple[int, int]]) -> int:
    """First round where no elf moves."""
    elves = elves.copy()
    round_num = 0

    while True:
        new_elves = simulate_round(elves, round_num)
        round_num += 1

        if new_elves == elves:
            return round_num

        elves = new_elves


def main():
    elves = parse_input(INPUT_FILE)

    answer1 = part1(elves)
    answer2 = part2(elves)

    print(f"Part 1: {answer1}")  # 4336
    print(f"Part 2: {answer2}")  # 1005

    return answer1, answer2


if __name__ == "__main__":
    main()
