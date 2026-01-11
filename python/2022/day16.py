"""Day 16: Proboscidea Volcanium - https://adventofcode.com/2022/day/16"""

import re
from collections import defaultdict

INPUT_FILE = "inputs/2022/day16.txt"


def parse_input(filename: str) -> dict:
    """Parse valve specifications."""
    valves = {}
    pattern = r"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)"

    with open(filename) as f:
        for line in f:
            match = re.match(pattern, line.strip())
            if match:
                name = match.group(1)
                flow_rate = int(match.group(2))
                connected = [v.strip() for v in match.group(3).split(",")]
                valves[name] = {
                    'flow_rate': flow_rate,
                    'connected': connected
                }

    return valves


def solve_part1(valves: dict, total_minutes: int = 30) -> int:
    """Find maximum pressure release in given time."""
    non_zero_valves = frozenset(
        name for name, data in valves.items() if data['flow_rate'] > 0
    )

    # State: (current_valve, opened_valves_frozenset) -> max_pressure
    # Use frontier-based approach
    frontier = [('AA', frozenset(), 0)]  # (at_valve, opened, pressure)

    for minute in range(total_minutes, 0, -1):
        next_frontier = {}

        for at_valve, opened, pressure in frontier:
            # If all valuable valves are open, just wait
            if opened == non_zero_valves:
                key = (at_valve, opened)
                if key not in next_frontier or next_frontier[key] < pressure:
                    next_frontier[key] = pressure
                continue

            # Option 1: Move to connected valves
            for next_valve in valves[at_valve]['connected']:
                key = (next_valve, opened)
                if key not in next_frontier or next_frontier[key] < pressure:
                    next_frontier[key] = pressure

            # Option 2: Open current valve (if not already open and has flow)
            if at_valve not in opened and valves[at_valve]['flow_rate'] > 0:
                new_opened = opened | {at_valve}
                new_pressure = pressure + valves[at_valve]['flow_rate'] * (minute - 1)
                key = (at_valve, new_opened)
                if key not in next_frontier or next_frontier[key] < new_pressure:
                    next_frontier[key] = new_pressure

        frontier = [(k[0], k[1], v) for k, v in next_frontier.items()]

    return max(pressure for _, _, pressure in frontier)


def solve_part2(valves: dict, total_minutes: int = 26) -> int:
    """Find maximum pressure release with elephant helping."""
    non_zero_valves = frozenset(
        name for name, data in valves.items() if data['flow_rate'] > 0
    )

    # Collect all reachable (opened_set -> max_pressure) mappings
    frontier = [('AA', frozenset(), 0)]
    all_states = defaultdict(int)

    for minute in range(total_minutes, 0, -1):
        next_frontier = {}

        for at_valve, opened, pressure in frontier:
            # Track this state
            if pressure > all_states[opened]:
                all_states[opened] = pressure

            if opened == non_zero_valves:
                key = (at_valve, opened)
                if key not in next_frontier or next_frontier[key] < pressure:
                    next_frontier[key] = pressure
                continue

            # Move to connected valves
            for next_valve in valves[at_valve]['connected']:
                key = (next_valve, opened)
                if key not in next_frontier or next_frontier[key] < pressure:
                    next_frontier[key] = pressure

            # Open current valve
            if at_valve not in opened and valves[at_valve]['flow_rate'] > 0:
                new_opened = opened | {at_valve}
                new_pressure = pressure + valves[at_valve]['flow_rate'] * (minute - 1)
                key = (at_valve, new_opened)
                if key not in next_frontier or next_frontier[key] < new_pressure:
                    next_frontier[key] = new_pressure

        frontier = [(k[0], k[1], v) for k, v in next_frontier.items()]

    # Add final states
    for at_valve, opened, pressure in frontier:
        if pressure > all_states[opened]:
            all_states[opened] = pressure

    # Find best combination where person and elephant open disjoint sets
    max_pressure = 0
    items = list(all_states.items())

    for i, (set1, pressure1) in enumerate(items):
        for set2, pressure2 in items[i:]:
            if not (set1 & set2):  # Disjoint sets
                total = pressure1 + pressure2
                if total > max_pressure:
                    max_pressure = total

    return max_pressure


def part1(valves: dict) -> int:
    """Maximum pressure in 30 minutes alone."""
    return solve_part1(valves, 30)


def part2(valves: dict) -> int:
    """Maximum pressure in 26 minutes with elephant."""
    return solve_part2(valves, 26)


def main():
    valves = parse_input(INPUT_FILE)

    answer1 = part1(valves)
    answer2 = part2(valves)

    print(f"Part 1: {answer1}")  # 1641
    print(f"Part 2: {answer2}")  # 2261

    return answer1, answer2


if __name__ == "__main__":
    main()
