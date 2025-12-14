#!/usr/bin/env python3
"""
Interactive Microsoft BASIC on Z80 6502 Emulator

Usage:
    python3 run_msbasic.py           # Interactive mode
    python3 run_msbasic.py -i input  # With input file (for testing)
"""

import sys
import tty
import termios
import argparse
from run_6502 import Emu6502Runner

def main():
    parser = argparse.ArgumentParser(description='Run MS BASIC interactively')
    parser.add_argument('-i', '--input', help='Input file for non-interactive testing')
    parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output')
    args = parser.parse_args()

    # Load MS BASIC
    with open("msbasic/msbasic.bin", "rb") as f:
        code = f.read()

    print(f"Microsoft BASIC on Z80 6502 Emulator")
    print(f"Loaded {len(code)} bytes")
    print("=" * 50)
    print("Press Ctrl-C to exit")
    print("=" * 50)

    emu = Emu6502Runner(verbose=args.verbose)
    emu.load_program(code, org=0x0800)

    # Override entry point to COLD START at $2730
    entry_6502 = 0x2730
    emu.start_bank = entry_6502 >> 13
    emu.start_addr = 0xC000 + ((entry_6502 & 0x1FFF) * 2)

    if args.input:
        # Non-interactive with input file
        with open(args.input, 'rb') as f:
            emu.bus.input_buffer = f.read().decode('latin-1')
        emu.run(max_cycles=10_000_000)
    else:
        # Interactive mode with raw terminal
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)

        def read_key():
            try:
                ch = sys.stdin.read(1)
                if ch:
                    # Convert Enter to CR for BASIC
                    if ch == '\n':
                        return 0x0D
                    return ord(ch)
            except:
                pass
            return 0xFF  # No input available

        try:
            tty.setraw(fd)
            tty.setcbreak(fd)
            emu.bus.on_input = read_key
            emu.run(max_cycles=100_000_000)
        except KeyboardInterrupt:
            pass
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
            print()

    print("=" * 50)
    print("Done.")

if __name__ == '__main__':
    main()
