"""Day 10: Cathode-Ray Tube - https://adventofcode.com/2022/day/10"""

INPUT_FILE = "inputs/2022/day10.txt"


def parse_input(filename: str) -> list[tuple[int, int]]:
    """Parse input into list of (cycle_duration, value_change) tuples."""
    instructions = []
    with open(filename) as f:
        for line in f:
            parts = line.strip().split()
            if parts[0] == "noop":
                instructions.append((1, 0))
            else:  # addx
                instructions.append((2, int(parts[1])))
    return instructions


def compute_register_values(instructions: list[tuple[int, int]]) -> list[int]:
    """Compute X register value at each cycle."""
    x = 1
    values = [x]  # Index 0 = value at start of cycle 1

    for duration, change in instructions:
        for _ in range(duration):
            values.append(x)
        x += change

    return values


def part1(values: list[int]) -> int:
    """Calculate signal strength at specific cycles."""
    interesting_cycles = [20, 60, 100, 140, 180, 220]
    return sum(cycle * values[cycle] for cycle in interesting_cycles)


def part2(values: list[int]) -> str:
    """Render the CRT display."""
    output = []

    for cycle in range(1, 241):
        pixel_pos = (cycle - 1) % 40
        sprite_center = values[cycle]

        if sprite_center - 1 <= pixel_pos <= sprite_center + 1:
            output.append('#')
        else:
            output.append('.')

    # Split into 6 rows of 40 characters
    lines = []
    for i in range(0, 240, 40):
        lines.append(''.join(output[i:i+40]))

    return '\n'.join(lines)


def main():
    instructions = parse_input(INPUT_FILE)
    values = compute_register_values(instructions)

    answer1 = part1(values)
    answer2 = part2(values)

    print(f"Part 1: {answer1}")  # 11720
    print(f"Part 2:\n{answer2}")  # ERCREPCJ

    return answer1, answer2


if __name__ == "__main__":
    main()
