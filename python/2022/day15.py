"""Day 15: Beacon Exclusion Zone - https://adventofcode.com/2022/day/15"""

import re
from collections import Counter

INPUT_FILE = "inputs/2022/day15.txt"


def manhattan_distance(p1: tuple[int, int], p2: tuple[int, int]) -> int:
    """Calculate Manhattan distance between two points."""
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


def parse_input(filename: str) -> list[dict]:
    """Parse sensor data from input."""
    sensors = []
    pattern = r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"

    with open(filename) as f:
        for line in f:
            match = re.match(pattern, line.strip())
            if match:
                sx, sy, bx, by = map(int, match.groups())
                sensor = (sx, sy)
                beacon = (bx, by)
                distance = manhattan_distance(sensor, beacon)
                sensors.append({
                    'sensor': sensor,
                    'beacon': beacon,
                    'distance': distance
                })

    return sensors


def count_excluded_positions(sensors: list[dict], y: int) -> int:
    """Count positions where beacon cannot be present at given y."""
    excluded = set()
    beacons_at_y = set()

    for data in sensors:
        sx, sy = data['sensor']
        bx, by = data['beacon']
        dist = data['distance']

        # Track beacons on this row
        if by == y:
            beacons_at_y.add((bx, by))

        # Calculate horizontal range at this y
        vertical_dist = abs(sy - y)
        if vertical_dist <= dist:
            horizontal_range = dist - vertical_dist
            for x in range(sx - horizontal_range, sx + horizontal_range + 1):
                excluded.add((x, y))

    # Remove positions where beacons actually are
    return len(excluded - beacons_at_y)


def find_distress_beacon(sensors: list[dict], coord_limit: int) -> int:
    """Find the tuning frequency of the distress beacon."""
    # The beacon must be just outside at least one sensor's range
    # Check points on the frontier (just outside each sensor's diamond)

    frontier_points = Counter()

    for data in sensors:
        sx, sy = data['sensor']
        dist = data['distance']

        # Points just outside the diamond (at distance dist + 1)
        for offset in range(dist + 2):
            # Four edges of the diamond
            points = [
                (sx + offset, sy - (dist + 1 - offset)),  # top-right edge
                (sx + offset, sy + (dist + 1 - offset)),  # bottom-right edge
                (sx - offset, sy - (dist + 1 - offset)),  # top-left edge
                (sx - offset, sy + (dist + 1 - offset)),  # bottom-left edge
            ]

            for x, y in points:
                if 0 <= x <= coord_limit and 0 <= y <= coord_limit:
                    frontier_points[(x, y)] += 1

    # Check candidates with high frequency first
    for (x, y), count in frontier_points.most_common():
        if count < 4:
            break

        # Verify this point is outside all sensor ranges
        valid = True
        for data in sensors:
            if manhattan_distance(data['sensor'], (x, y)) <= data['distance']:
                valid = False
                break
            if (x, y) == data['beacon']:
                valid = False
                break

        if valid:
            return x * 4000000 + y

    return -1


def part1(sensors: list[dict]) -> int:
    """Count positions where beacon cannot be at y=2000000."""
    return count_excluded_positions(sensors, 2000000)


def part2(sensors: list[dict]) -> int:
    """Find tuning frequency of distress beacon."""
    return find_distress_beacon(sensors, 4000000)


def main():
    sensors = parse_input(INPUT_FILE)

    answer1 = part1(sensors)
    answer2 = part2(sensors)

    print(f"Part 1: {answer1}")  # 5688618
    print(f"Part 2: {answer2}")  # 12625383204261

    return answer1, answer2


if __name__ == "__main__":
    main()
