#!/usr/bin/env python3
"""
Automated tests for z3_minimal.py with MiniZork

Runs a sequence of commands and verifies expected outputs.
"""

import subprocess
import sys

# Test scenarios: (name, commands, expected_outputs)
TESTS = [
    {
        "name": "Basic Navigation - Circle House",
        "commands": [
            "look",
            "north",
            "look",
            "south",       # blocked - stay at North
            "southwest",   # go to West of House
            "south",       # go to South of House
            "east",        # go to Behind House
            "look",
            "quit",
            "y"
        ],
        "expected": [
            ("West of House", "Initial room description"),
            ("North of House", "Moved north"),
            ("narrow path winds north", "North room details"),
            ("windows are all boarded", "South blocked from north"),
            ("West of House", "Back to west via southwest"),
            ("South of House", "Moved south"),
            ("Behind House", "At back of house"),
        ]
    },
    {
        "name": "Mailbox Interaction",
        "commands": [
            "examine mailbox",
            "open mailbox",
            "look in mailbox",
            "take leaflet",
            "read leaflet",
            "inventory",
            "quit",
            "y"
        ],
        "expected": [
            ("small mailbox", "Mailbox exists"),
            ("Opening the small mailbox", "Opened mailbox"),
            ("leaflet", "Leaflet visible"),
            ("Taken", "Took leaflet"),
            ("WELCOME TO ZORK", "Read leaflet content"),
            ("A leaflet", "Leaflet in inventory"),
        ]
    },
    {
        "name": "Forest and Tree",
        "commands": [
            "north",
            "north",
            "look",
            "climb tree",
            "take egg",
            "inventory",
            "down",
            "quit",
            "y"
        ],
        "expected": [
            ("North of House", "At north side"),
            ("Forest", "In forest"),
            ("large tree", "Tree visible"),
            ("nest", "Climbed tree, see nest"),
            ("egg", "Egg interaction"),
        ]
    },
    {
        "name": "Enter House - Kitchen and Living Room",
        "commands": [
            "south",
            "east",
            "open window",
            "west",
            "look",
            "west",
            "look",
            "take lantern",
            "take sword",
            "inventory",
            "quit",
            "y"
        ],
        "expected": [
            ("South of House", "At south"),
            ("Behind House", "At back of house"),
            ("open the window", "Opened window"),
            ("Kitchen", "Entered kitchen"),
            ("bottle", "Bottle in kitchen"),
            ("Living Room", "In living room"),
            ("brass lantern", "Lantern visible"),
            ("elvish sword", "Sword visible"),
            ("lantern", "Lantern in inventory"),
        ]
    },
]


def run_test(test, story_path):
    """Run a single test scenario"""
    commands = "\n".join(test["commands"])

    try:
        result = subprocess.run(
            ["python3", "z3_minimal.py", story_path],
            input=commands,
            capture_output=True,
            text=True,
            timeout=10
        )
        output = result.stdout + result.stderr
    except subprocess.TimeoutExpired:
        return False, "TIMEOUT", []
    except Exception as e:
        return False, f"ERROR: {e}", []

    # Check expected outputs
    results = []
    all_passed = True

    for expected_text, description in test["expected"]:
        found = expected_text.lower() in output.lower()
        results.append((description, expected_text, found))
        if not found:
            all_passed = False

    return all_passed, output, results


def main():
    story_path = "../test-games/minizork.z3"
    if len(sys.argv) > 1:
        story_path = sys.argv[1]

    print("=" * 60)
    print("Z-Machine V3 Interpreter Tests - MiniZork")
    print("=" * 60)
    print()

    total_tests = 0
    passed_tests = 0

    for test in TESTS:
        print(f"Test: {test['name']}")
        print("-" * 40)

        success, output, results = run_test(test, story_path)
        total_tests += 1

        if success:
            passed_tests += 1
            print(f"  PASSED")
        else:
            print(f"  FAILED")

        # Show individual checks
        for description, expected, found in results:
            status = "OK" if found else "MISSING"
            print(f"    [{status}] {description}: '{expected}'")

        print()

    # Summary
    print("=" * 60)
    print(f"Results: {passed_tests}/{total_tests} tests passed")
    print("=" * 60)

    if passed_tests == total_tests:
        print("\nAll tests PASSED!")
        return 0
    else:
        print(f"\n{total_tests - passed_tests} test(s) FAILED")
        return 1


if __name__ == "__main__":
    sys.exit(main())
