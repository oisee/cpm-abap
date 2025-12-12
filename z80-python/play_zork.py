#!/usr/bin/env python3
"""Interactive ZORK player using Python Z80 + CP/M emulator"""

import os
import sys
from cpm_z80 import CpmEmulator


def play_zork(zork_dir="../test-games"):
    """Run ZORK1.COM interactively"""
    cpm = CpmEmulator(zork_dir)
    cpm.reset()
    cpm.debug = False

    zork_path = os.path.join(zork_dir, "ZORK1.COM")
    if not os.path.exists(zork_path):
        print(f"Error: {zork_path} not found")
        return

    cpm.load_file(zork_path)
    cpm.setup_fcb(0x005C, "ZORK1.DAT")

    print("=" * 60)
    print("ZORK via Python Z80 + CP/M Emulator")
    print("Type 'quit' to exit game, Ctrl+C to force quit")
    print("=" * 60)

    try:
        while True:
            # Run until input needed or exit
            for _ in range(10000000):
                pc = cpm.cpu.get_pc()

                if pc == 0xFE00:
                    func = cpm.cpu.get_c()

                    # Check if waiting for console input
                    if func in [1, 6, 10] and not cpm.bus.input_buffer:
                        # Print accumulated output
                        output = cpm.bus.get_output()
                        if output:
                            print(output, end="", flush=True)
                            cpm.bus.clear_output()

                        # Get user input
                        try:
                            line = input()
                            cpm.provide_input(line + "\n")
                        except EOFError:
                            print("\n[EOF - exiting]")
                            return

                    if not cpm.bdos_call(func):
                        print(cpm.bus.get_output())
                        return

                    cpm.cpu.step()
                    continue

                if pc == 0xFF00:
                    print(cpm.bus.get_output())
                    return

                cpm.cpu.step()

                if cpm.cpu.mv_halted or not cpm.cpu.mv_running:
                    print(cpm.bus.get_output())
                    return

    except KeyboardInterrupt:
        print("\n[Interrupted]")


if __name__ == "__main__":
    zork_dir = sys.argv[1] if len(sys.argv) > 1 else "../test-games"
    play_zork(zork_dir)
