"""Day 22: Monkey Map - https://adventofcode.com/2022/day/22"""

import re

INPUT_FILE = "inputs/2022/day22.txt"

# Direction values for answer
FACING_VALUES = {'right': 0, 'down': 1, 'left': 2, 'up': 3}

DIRECTION_OFFSETS = {
    'right': (0, 1),
    'down': (1, 0),
    'left': (0, -1),
    'up': (-1, 0)
}

CLOCKWISE = {'right': 'down', 'down': 'left', 'left': 'up', 'up': 'right'}
COUNTER_CLOCKWISE = {'right': 'up', 'up': 'left', 'left': 'down', 'down': 'right'}


def parse_input(filename: str) -> tuple[list[str], list]:
    """Parse board and movement instructions."""
    with open(filename) as f:
        content = f.read()

    parts = content.split("\n\n")
    board_lines = parts[0].split("\n")

    # Normalize board lines to same length
    max_len = max(len(line) for line in board_lines)
    board = [line.ljust(max_len) for line in board_lines]

    # Parse instructions
    instructions_str = parts[1].strip()
    instructions = []
    for match in re.findall(r'(\d+|[LR])', instructions_str):
        if match in 'LR':
            instructions.append(match)
        else:
            instructions.append(int(match))

    return board, instructions


def find_start(board: list[str]) -> tuple[int, int]:
    """Find starting position (first open tile in first row)."""
    for c, char in enumerate(board[0]):
        if char == '.':
            return (0, c)
    return (0, 0)


def get_row_bounds(board: list[str], row: int) -> tuple[int, int]:
    """Get start and end column for a row."""
    line = board[row]
    start = len(line) - len(line.lstrip())
    end = len(line.rstrip())
    return start, end


def get_col_bounds(board: list[str], col: int) -> tuple[int, int]:
    """Get start and end row for a column."""
    start = 0
    end = len(board)

    for r in range(len(board)):
        if col < len(board[r]) and board[r][col] != ' ':
            start = r
            break

    for r in range(len(board) - 1, -1, -1):
        if col < len(board[r]) and board[r][col] != ' ':
            end = r + 1
            break

    return start, end


def wrap_part1(board: list[str], row: int, col: int, facing: str) -> tuple[int, int, str]:
    """Wrap around for part 1 (flat map)."""
    dr, dc = DIRECTION_OFFSETS[facing]
    new_row, new_col = row + dr, col + dc

    if facing in ('left', 'right'):
        start, end = get_row_bounds(board, row)
        new_col = start + (new_col - start) % (end - start)
    else:
        start, end = get_col_bounds(board, col)
        new_row = start + (new_row - start) % (end - start)

    return new_row, new_col, facing


def move(board: list[str], pos: tuple[int, int], facing: str, steps: int,
         wrap_fn) -> tuple[tuple[int, int], str]:
    """Move forward, wrapping and stopping at walls."""
    row, col = pos

    for _ in range(steps):
        new_row, new_col, new_facing = wrap_fn(board, row, col, facing)

        # Check for wall
        if board[new_row][new_col] == '#':
            break

        row, col, facing = new_row, new_col, new_facing

    return (row, col), facing


def solve(board: list[str], instructions: list, wrap_fn) -> int:
    """Solve with given wrap function."""
    pos = find_start(board)
    facing = 'right'

    for instr in instructions:
        if isinstance(instr, int):
            pos, facing = move(board, pos, facing, instr, wrap_fn)
        elif instr == 'R':
            facing = CLOCKWISE[facing]
        else:  # 'L'
            facing = COUNTER_CLOCKWISE[facing]

    row, col = pos
    return 1000 * (row + 1) + 4 * (col + 1) + FACING_VALUES[facing]


# Part 2: Cube wrapping (hardcoded for the specific input layout)
# The input has this shape:
#   12
#   3
#  45
#  6

def get_face(row: int, col: int, size: int = 50) -> int:
    """Determine which face a position is on."""
    face_row = row // size
    face_col = col // size

    if face_row == 0 and face_col == 1:
        return 1
    elif face_row == 0 and face_col == 2:
        return 2
    elif face_row == 1 and face_col == 1:
        return 3
    elif face_row == 2 and face_col == 0:
        return 4
    elif face_row == 2 and face_col == 1:
        return 5
    elif face_row == 3 and face_col == 0:
        return 6
    return 0


def wrap_part2_cube(board: list[str], row: int, col: int, facing: str,
                    size: int = 50) -> tuple[int, int, str]:
    """Wrap around cube edges."""
    dr, dc = DIRECTION_OFFSETS[facing]
    new_row, new_col = row + dr, col + dc

    # Check if we're still on the board
    if (0 <= new_row < len(board) and 0 <= new_col < len(board[new_row]) and
            board[new_row][new_col] != ' '):
        return new_row, new_col, facing

    # Determine current face and local coordinates
    face = get_face(row, col, size)
    local_row = row % size
    local_col = col % size

    # Cube face transitions (hardcoded for the input layout)
    # Format: (new_face, new_facing, transform)
    transitions = {
        (1, 'up'): (6, 'right', lambda r, c: (c, 0)),
        (1, 'left'): (4, 'right', lambda r, c: (size - 1 - r, 0)),
        (2, 'up'): (6, 'up', lambda r, c: (size - 1, c)),
        (2, 'right'): (5, 'left', lambda r, c: (size - 1 - r, size - 1)),
        (2, 'down'): (3, 'left', lambda r, c: (c, size - 1)),
        (3, 'left'): (4, 'down', lambda r, c: (0, r)),
        (3, 'right'): (2, 'up', lambda r, c: (size - 1, r)),
        (4, 'up'): (3, 'right', lambda r, c: (c, 0)),
        (4, 'left'): (1, 'right', lambda r, c: (size - 1 - r, 0)),
        (5, 'right'): (2, 'left', lambda r, c: (size - 1 - r, size - 1)),
        (5, 'down'): (6, 'left', lambda r, c: (c, size - 1)),
        (6, 'left'): (1, 'down', lambda r, c: (0, r)),
        (6, 'right'): (5, 'up', lambda r, c: (size - 1, r)),
        (6, 'down'): (2, 'down', lambda r, c: (0, c)),
    }

    key = (face, facing)
    if key in transitions:
        new_face, new_facing, transform = transitions[key]
        new_local_row, new_local_col = transform(local_row, local_col)

        # Convert back to global coordinates
        face_origins = {
            1: (0, size),
            2: (0, 2 * size),
            3: (size, size),
            4: (2 * size, 0),
            5: (2 * size, size),
            6: (3 * size, 0),
        }

        origin_row, origin_col = face_origins[new_face]
        return origin_row + new_local_row, origin_col + new_local_col, new_facing

    return new_row, new_col, facing


def part1(board: list[str], instructions: list) -> int:
    """Solve with flat wrap."""
    return solve(board, instructions, wrap_part1)


def part2(board: list[str], instructions: list) -> int:
    """Solve with cube wrap."""
    return solve(board, instructions, wrap_part2_cube)


def main():
    board, instructions = parse_input(INPUT_FILE)

    answer1 = part1(board, instructions)
    answer2 = part2(board, instructions)

    print(f"Part 1: {answer1}")  # 123046
    print(f"Part 2: {answer2}")  # 195032

    return answer1, answer2


if __name__ == "__main__":
    main()
