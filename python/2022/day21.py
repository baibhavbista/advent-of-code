"""Day 21: Monkey Math - https://adventofcode.com/2022/day/21"""

import re

INPUT_FILE = "inputs/2022/day21.txt"


def parse_input(filename: str) -> tuple[dict[str, int], dict[str, tuple[str, str, str]]]:
    """Parse monkey definitions into numbers and equations."""
    numbers = {}
    equations = {}

    number_pattern = r"(\w+): (\d+)"
    equation_pattern = r"(\w+): (\w+) ([+\-*/]) (\w+)"

    with open(filename) as f:
        for line in f:
            line = line.strip()
            num_match = re.match(number_pattern, line)
            if num_match:
                name = num_match.group(1)
                value = int(num_match.group(2))
                numbers[name] = value
            else:
                eq_match = re.match(equation_pattern, line)
                if eq_match:
                    name = eq_match.group(1)
                    left = eq_match.group(2)
                    op = eq_match.group(3)
                    right = eq_match.group(4)
                    equations[name] = (op, left, right)

    return numbers, equations


def evaluate(name: str, numbers: dict[str, int], equations: dict[str, tuple]) -> int:
    """Recursively evaluate a monkey's value."""
    if name in numbers:
        return numbers[name]

    op, left, right = equations[name]
    left_val = evaluate(left, numbers, equations)
    right_val = evaluate(right, numbers, equations)

    if op == '+':
        return left_val + right_val
    elif op == '-':
        return left_val - right_val
    elif op == '*':
        return left_val * right_val
    else:  # /
        return left_val // right_val


def find_path_to_humn(name: str, equations: dict[str, tuple], path: list[str]) -> list[str] | None:
    """Find path from root to humn."""
    if name == 'humn':
        return path + ['humn']

    if name not in equations:
        return None

    _, left, right = equations[name]

    left_path = find_path_to_humn(left, equations, path + [name])
    if left_path:
        return left_path

    right_path = find_path_to_humn(right, equations, path + [name])
    if right_path:
        return right_path

    return None


def solve_for_humn(numbers: dict[str, int], equations: dict[str, tuple]) -> int:
    """Solve for humn value to make root's children equal."""
    # Find path from root to humn
    path = find_path_to_humn('root', equations, [])
    path_set = set(path)

    # Evaluate all values not dependent on humn
    cached = dict(numbers)

    def eval_if_possible(name: str) -> int | None:
        if name in cached:
            return cached[name]
        if name in path_set:
            return None
        if name not in equations:
            return None

        op, left, right = equations[name]
        left_val = eval_if_possible(left)
        right_val = eval_if_possible(right)

        if left_val is None or right_val is None:
            return None

        if op == '+':
            result = left_val + right_val
        elif op == '-':
            result = left_val - right_val
        elif op == '*':
            result = left_val * right_val
        else:
            result = left_val // right_val

        cached[name] = result
        return result

    # Evaluate everything possible
    for name in equations:
        eval_if_possible(name)

    # For root, the two children should be equal
    _, left, right = equations['root']

    # One of them should be evaluable
    if left in path_set:
        target = cached[right]
        current = left
    else:
        target = cached[left]
        current = right

    # Work backwards through the path
    while current != 'humn':
        op, left, right = equations[current]

        if left in path_set or left == 'humn':
            # Unknown is on the left
            known = cached[right]
            next_current = left

            # Solve: target = unknown op known
            if op == '+':
                target = target - known
            elif op == '-':
                target = target + known
            elif op == '*':
                target = target // known
            else:
                target = target * known
        else:
            # Unknown is on the right
            known = cached[left]
            next_current = right

            # Solve: target = known op unknown
            if op == '+':
                target = target - known
            elif op == '-':
                target = known - target
            elif op == '*':
                target = target // known
            else:
                target = known // target

        current = next_current

    return target


def part1(numbers: dict[str, int], equations: dict[str, tuple]) -> int:
    """Evaluate root."""
    return evaluate('root', numbers, equations)


def part2(numbers: dict[str, int], equations: dict[str, tuple]) -> int:
    """Find humn value to make root's children equal."""
    return solve_for_humn(numbers, equations)


def main():
    numbers, equations = parse_input(INPUT_FILE)

    answer1 = part1(numbers, equations)
    answer2 = part2(numbers, equations)

    print(f"Part 1: {answer1}")  # 155708040358220
    print(f"Part 2: {answer2}")  # 3342154812537

    return answer1, answer2


if __name__ == "__main__":
    main()
