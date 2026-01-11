"""Day 11: Monkey in the Middle - https://adventofcode.com/2022/day/11"""

import re
from math import lcm
from copy import deepcopy

INPUT_FILE = "inputs/2022/day11.txt"


def parse_monkey(monkey_str: str) -> dict:
    """Parse a single monkey's specification."""
    lines = [line.strip() for line in monkey_str.split("\n")]

    # Monkey number
    monkey_num = int(re.match(r"Monkey (\d+):", lines[0]).group(1))

    # Starting items
    items_str = re.match(r"Starting items: (.+)", lines[1]).group(1)
    items = [int(x.strip()) for x in items_str.split(",")]

    # Operation
    op_match = re.match(r"Operation: new = (.+)", lines[2])
    op_str = op_match.group(1)

    def make_op(op_str):
        # Parse "old * 19" or "old + old" etc.
        parts = op_str.split()
        operand1, operator, operand2 = parts[0], parts[1], parts[2]

        def operation(old):
            a = old if operand1 == "old" else int(operand1)
            b = old if operand2 == "old" else int(operand2)
            if operator == "+":
                return a + b
            else:  # *
                return a * b
        return operation

    # Test divisibility
    div_by = int(re.match(r"Test: divisible by (\d+)", lines[3]).group(1))

    # True/false targets
    true_target = int(re.match(r"If true: throw to monkey (\d+)", lines[4]).group(1))
    false_target = int(re.match(r"If false: throw to monkey (\d+)", lines[5]).group(1))

    return {
        'num': monkey_num,
        'items': items,
        'operation': make_op(op_str),
        'div_by': div_by,
        'true_target': true_target,
        'false_target': false_target,
        'inspected': 0
    }


def parse_input(filename: str) -> list[dict]:
    """Parse all monkeys from input."""
    with open(filename) as f:
        content = f.read().strip()

    monkey_strs = content.split("\n\n")
    return [parse_monkey(s) for s in monkey_strs]


def simulate_round(monkeys: list[dict], worry_reducer) -> None:
    """Simulate one round of monkey business."""
    for monkey in monkeys:
        while monkey['items']:
            # Pop item from front
            item = monkey['items'].pop(0)

            # Inspect: apply operation
            item = monkey['operation'](item)
            monkey['inspected'] += 1

            # Apply worry reducer
            item = worry_reducer(item)

            # Test and throw
            if item % monkey['div_by'] == 0:
                target = monkey['true_target']
            else:
                target = monkey['false_target']

            monkeys[target]['items'].append(item)


def calculate_monkey_business(monkeys: list[dict]) -> int:
    """Calculate monkey business level."""
    inspected = sorted([m['inspected'] for m in monkeys], reverse=True)
    return inspected[0] * inspected[1]


def part1(monkeys: list[dict]) -> int:
    """Run 20 rounds with worry divided by 3."""
    monkeys = deepcopy(monkeys)

    def worry_reducer(item):
        return item // 3

    for _ in range(20):
        simulate_round(monkeys, worry_reducer)

    return calculate_monkey_business(monkeys)


def part2(monkeys: list[dict]) -> int:
    """Run 10000 rounds with modular arithmetic."""
    monkeys = deepcopy(monkeys)

    # Calculate LCM of all divisibility tests
    modulus = lcm(*[m['div_by'] for m in monkeys])

    def worry_reducer(item):
        return item % modulus

    for _ in range(10000):
        simulate_round(monkeys, worry_reducer)

    return calculate_monkey_business(monkeys)


def main():
    monkeys = parse_input(INPUT_FILE)

    answer1 = part1(monkeys)
    answer2 = part2(monkeys)

    print(f"Part 1: {answer1}")  # 57838
    print(f"Part 2: {answer2}")  # 15050382231

    return answer1, answer2


if __name__ == "__main__":
    main()
