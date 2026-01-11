#!/usr/bin/env python3
"""Run all Advent of Code 2022 solutions and verify answers."""

import sys
import time
import importlib
from pathlib import Path

# Expected answers (verified against Clojure solutions)
EXPECTED_ANSWERS = {
    1: (68802, 205370),
    2: (17189, 13490),
    3: (8088, 2522),
    4: (605, 914),
    5: ("VGBBJCRMN", "LBBVJBRMH"),
    6: (1850, 2823),
    7: (1908462, 3979145),
    8: (1662, 537600),
    9: (6037, 2485),
    10: (11720, None),  # Part 2 is visual output "ERCREPCJ"
    11: (57838, 15050382231),
    12: (468, 459),
    13: (6656, 19716),
    14: (885, 28691),
    15: (5688618, 12625383204261),
    16: (1641, 2261),
    17: (3067, 1514369501484),
    18: (4504, 2556),
    19: (1127, 21546),
    20: (8721, 831878881825),
    21: (155708040358220, 3342154812537),
    22: (123046, 195032),
    23: (4336, 1005),
    24: (314, 896),
    25: ("2-0-020-1==1021=--01", "Merry Christmas!"),
}


def run_day(day: int) -> tuple:
    """Run a specific day's solution and return results."""
    module_name = f"2022.day{day:02d}"
    try:
        module = importlib.import_module(module_name)
        return module.main()
    except Exception as e:
        return (f"Error: {e}", None)


def main():
    """Run all solutions and verify answers."""
    print("=" * 60)
    print("Advent of Code 2022 - Python Solutions")
    print("=" * 60)

    passed = 0
    failed = 0
    skipped = 0
    total_time = 0

    for day in range(1, 26):
        print(f"\nDay {day:02d}: ", end="", flush=True)

        start = time.time()
        try:
            result = run_day(day)
            elapsed = time.time() - start
            total_time += elapsed

            expected = EXPECTED_ANSWERS.get(day)

            if expected is None:
                print(f"? (no expected answer) - {result} [{elapsed:.2f}s]")
                skipped += 1
            elif expected[0] is None or expected[1] is None:
                # Partial verification
                part1_ok = expected[0] is None or result[0] == expected[0]
                part2_ok = expected[1] is None or result[1] == expected[1]
                if part1_ok and part2_ok:
                    print(f"OK {result} [{elapsed:.2f}s]")
                    passed += 1
                else:
                    print(f"FAIL - got {result}, expected {expected} [{elapsed:.2f}s]")
                    failed += 1
            elif result == expected:
                print(f"OK [{elapsed:.2f}s]")
                passed += 1
            else:
                print(f"FAIL - got {result}, expected {expected} [{elapsed:.2f}s]")
                failed += 1

        except Exception as e:
            print(f"ERROR: {e}")
            failed += 1

    print("\n" + "=" * 60)
    print(f"Results: {passed} passed, {failed} failed, {skipped} skipped")
    print(f"Total time: {total_time:.2f}s")
    print("=" * 60)

    return failed == 0


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
