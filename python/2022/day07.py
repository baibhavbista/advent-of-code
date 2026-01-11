"""Day 7: No Space Left On Device - https://adventofcode.com/2022/day/7"""

import re
from functools import lru_cache

INPUT_FILE = "inputs/2022/day07.txt"


def parse_input(filename: str) -> dict:
    """Parse terminal output into directory structure."""
    with open(filename) as f:
        lines = f.read().strip().split("\n")

    # dir_to_files maps directory path (tuple) to list of items
    # Each item is either {'type': 'file', 'name': str, 'size': int}
    # or {'type': 'dir', 'path': tuple}
    dir_to_files = {}
    current_path = ()

    for line in lines:
        if line.startswith("$"):
            # Command
            match = re.match(r"\$ (cd|ls)\s*(.+)?", line)
            if match:
                cmd = match.group(1)
                arg = match.group(2)

                if cmd == "cd":
                    if arg == "/":
                        current_path = ()
                    elif arg == "..":
                        current_path = current_path[:-1]
                    else:
                        current_path = current_path + (arg,)
        else:
            # Output from ls
            parts = line.split(" ", 1)
            if current_path not in dir_to_files:
                dir_to_files[current_path] = []

            if parts[0] == "dir":
                dir_to_files[current_path].append({
                    'type': 'dir',
                    'path': current_path + (parts[1],)
                })
            else:
                dir_to_files[current_path].append({
                    'type': 'file',
                    'name': parts[1],
                    'size': int(parts[0])
                })

    return dir_to_files


def calculate_sizes(dir_to_files: dict) -> dict:
    """Calculate size of each directory."""
    # Use memoization via dictionary
    sizes = {}

    def get_size(path: tuple) -> int:
        if path in sizes:
            return sizes[path]

        total = 0
        items = dir_to_files.get(path, [])
        for item in items:
            if item['type'] == 'file':
                total += item['size']
            else:
                total += get_size(item['path'])

        sizes[path] = total
        return total

    # Calculate size for all directories
    for path in dir_to_files:
        get_size(path)

    # Make sure root is included
    get_size(())

    return sizes


def part1(sizes: dict) -> int:
    """Sum of sizes of directories with size at most 100000."""
    return sum(size for size in sizes.values() if size <= 100000)


def part2(sizes: dict) -> int:
    """Find smallest directory to delete to free enough space."""
    total_disk_space = 70000000
    required_unused = 30000000

    used_space = sizes[()]
    current_unused = total_disk_space - used_space
    space_needed = required_unused - current_unused

    # Find smallest directory that is at least space_needed
    candidates = [size for size in sizes.values() if size >= space_needed]
    return min(candidates)


def main():
    dir_to_files = parse_input(INPUT_FILE)
    sizes = calculate_sizes(dir_to_files)

    answer1 = part1(sizes)
    answer2 = part2(sizes)

    print(f"Part 1: {answer1}")  # 1908462
    print(f"Part 2: {answer2}")  # 3979145

    return answer1, answer2


if __name__ == "__main__":
    main()
