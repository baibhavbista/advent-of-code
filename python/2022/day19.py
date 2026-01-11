"""Day 19: Not Enough Minerals - https://adventofcode.com/2022/day/19"""

import re

INPUT_FILE = "inputs/2022/day19.txt"


def parse_input(filename: str) -> list[list[list[int]]]:
    """Parse blueprints into cost matrices.

    Each blueprint is a list of 4 robot costs: [ore, clay, obsidian, geode]
    Each cost is [ore, clay, obsidian, geode] needed.
    """
    blueprints = []
    pattern = (r"Blueprint \d+: Each ore robot costs (\d+) ore\. "
               r"Each clay robot costs (\d+) ore\. "
               r"Each obsidian robot costs (\d+) ore and (\d+) clay\. "
               r"Each geode robot costs (\d+) ore and (\d+) obsidian\.")

    with open(filename) as f:
        for line in f:
            match = re.match(pattern, line.strip())
            if match:
                nums = list(map(int, match.groups()))
                ore_cost, clay_cost, obs_ore, obs_clay, geo_ore, geo_obs = nums

                blueprint = [
                    [ore_cost, 0, 0, 0],        # ore robot
                    [clay_cost, 0, 0, 0],       # clay robot
                    [obs_ore, obs_clay, 0, 0],  # obsidian robot
                    [geo_ore, 0, geo_obs, 0],   # geode robot
                ]
                blueprints.append(blueprint)

    return blueprints


def max_geodes(blueprint: list[list[int]], time_limit: int) -> int:
    """Find maximum geodes collectable with given blueprint and time."""
    # Maximum resources we could ever need per turn
    max_ore = max(blueprint[i][0] for i in range(4))
    max_clay = blueprint[2][1]
    max_obsidian = blueprint[3][2]

    best = 0

    # State: (robots, materials, time_left)
    # robots and materials are (ore, clay, obsidian, geode)
    initial = ((1, 0, 0, 0), (0, 0, 0, 0), time_limit)
    stack = [initial]

    visited = set()

    while stack:
        robots, materials, time_left = stack.pop()

        if time_left == 0:
            best = max(best, materials[3])
            continue

        # Pruning: max possible geodes if we build geode robot every turn
        max_possible = materials[3] + robots[3] * time_left + (time_left * (time_left - 1)) // 2
        if max_possible <= best:
            continue

        # Prune excessive robots (don't need more than max consumption)
        robots = (
            min(robots[0], max_ore),
            min(robots[1], max_clay),
            min(robots[2], max_obsidian),
            robots[3]
        )

        # Prune excessive materials
        materials = (
            min(materials[0], max_ore * time_left),
            min(materials[1], max_clay * time_left),
            min(materials[2], max_obsidian * time_left),
            materials[3]
        )

        state_key = (robots, materials, time_left)
        if state_key in visited:
            continue
        visited.add(state_key)

        # Try building each robot type
        built_something = False

        for robot_type in range(3, -1, -1):  # Prioritize geode robots
            cost = blueprint[robot_type]

            # Check if we can build this robot
            if all(materials[i] >= cost[i] for i in range(4)):
                # If we can build a geode robot, do it immediately
                if robot_type == 3:
                    new_materials = tuple(
                        materials[i] + robots[i] - cost[i] for i in range(4)
                    )
                    new_robots = tuple(
                        robots[i] + (1 if i == robot_type else 0) for i in range(4)
                    )
                    stack.append((new_robots, new_materials, time_left - 1))
                    built_something = True
                    break  # Always build geode robot if possible
                else:
                    new_materials = tuple(
                        materials[i] + robots[i] - cost[i] for i in range(4)
                    )
                    new_robots = tuple(
                        robots[i] + (1 if i == robot_type else 0) for i in range(4)
                    )
                    stack.append((new_robots, new_materials, time_left - 1))
                    built_something = True

        # Option to not build anything (wait)
        if not built_something or True:  # Always consider waiting if we didn't force geode
            new_materials = tuple(materials[i] + robots[i] for i in range(4))
            stack.append((robots, new_materials, time_left - 1))

    return best


def part1(blueprints: list[list[list[int]]]) -> int:
    """Sum of quality levels (blueprint_id * max_geodes) for all blueprints."""
    total = 0
    for i, blueprint in enumerate(blueprints):
        geodes = max_geodes(blueprint, 24)
        quality = (i + 1) * geodes
        total += quality
    return total


def part2(blueprints: list[list[list[int]]]) -> int:
    """Product of max geodes for first 3 blueprints with 32 minutes."""
    result = 1
    for blueprint in blueprints[:3]:
        geodes = max_geodes(blueprint, 32)
        result *= geodes
    return result


def main():
    blueprints = parse_input(INPUT_FILE)

    answer1 = part1(blueprints)
    answer2 = part2(blueprints)

    print(f"Part 1: {answer1}")  # 1127
    print(f"Part 2: {answer2}")  # 21546

    return answer1, answer2


if __name__ == "__main__":
    main()
