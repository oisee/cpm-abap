#!/usr/bin/env python3
"""
6502 Emulator - Main Entry Point

Runs 6502 threaded code on VZ80 virtual machine.
"""

import sys
import os

# Add parent directory for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from vz80 import VZ80
from emu6502.runtime import Emu6502, DATA_BASE, CODE_BASE
from emu6502.handlers import HANDLERS, get_handler


class Emulator6502:
    """
    Complete 6502 emulator using RET-threading on VZ80.
    """

    def __init__(self):
        self.vm = VZ80()
        self.emu = None
        self.debug = False

    def reset(self):
        """Reset emulator"""
        self.vm.reset()
        self.emu = Emu6502(self.vm)

    def load_threaded_code(self, code: bytes, org: int = 0xC000):
        """Load pre-translated threaded code"""
        self.vm.bus.load(org, code)
        # Set Z80 SP to start of code (this is our 6502 PC!)
        self.vm.cpu.sp = org

    def load_6502_data(self, data: bytes, addr: int = 0):
        """Load data into 6502 memory space"""
        for i, b in enumerate(data):
            self.vm.bus.write_mem(DATA_BASE + addr + i, b)

    def step(self) -> bool:
        """
        Execute one 6502 instruction.
        Returns False when program ends (BRK).
        """
        # Pop handler address from code stream
        handler_addr = self.emu.pop_handler()

        # Extract opcode from handler address
        opcode = handler_addr & 0xFF

        if self.debug:
            print(f"PC=${self.vm.cpu.sp:04X} Handler=${handler_addr:04X} Op=${opcode:02X}")

        # Get and execute handler
        handler = get_handler(opcode)
        if handler is None:
            if self.debug:
                print(f"  Unimplemented opcode ${opcode:02X}")
            return True  # Skip unimplemented opcodes

        return handler(self.emu)

    def run(self, max_instructions: int = 1_000_000) -> str:
        """
        Run until BRK or max instructions.
        Returns console output.
        """
        self.vm.bus.clear_output()
        count = 0

        while count < max_instructions:
            if not self.step():
                break
            count += 1

        if self.debug:
            print(f"\nExecuted {count} instructions")

        return self.vm.bus.get_output()

    def provide_input(self, text: str):
        """Provide input for the program"""
        self.vm.bus.provide_input(text)


def test_hello():
    """Test: Run Hello World 6502 program"""
    print("=" * 60)
    print("Test: Hello World (6502 on VZ80)")
    print("=" * 60)

    # 6502 program: Print "Hi!" using COUT trap
    # LDA #'H'; JSR $FDED; LDA #'i'; JSR $FDED; LDA #'!'; JSR $FDED; BRK
    code_6502 = bytes([
        0xA9, 0x48,       # LDA #'H'
        0x20, 0xED, 0xFD, # JSR $FDED
        0xA9, 0x69,       # LDA #'i'
        0x20, 0xED, 0xFD, # JSR $FDED
        0xA9, 0x21,       # LDA #'!'
        0x20, 0xED, 0xFD, # JSR $FDED
        0x00,             # BRK
    ])

    # Translate to threaded code (each byte → handler address)
    from converter.memory_map import opcode_to_handler
    threaded = bytearray()
    for b in code_6502:
        h = opcode_to_handler(b)
        threaded.append(h & 0xFF)
        threaded.append((h >> 8) & 0xFF)

    print(f"6502 code: {len(code_6502)} bytes")
    print(f"Threaded:  {len(threaded)} bytes")
    print()

    # Run emulator
    emu = Emulator6502()
    emu.reset()
    emu.debug = True
    emu.load_threaded_code(bytes(threaded))

    output = emu.run()

    print()
    print(f"Output: '{output}'")
    print("=" * 60)

    return output == "Hi!"


def test_loop():
    """Test: Loop with INX and BNE"""
    print("=" * 60)
    print("Test: Loop (count 0-9)")
    print("=" * 60)

    # 6502 program: Print '0'-'9'
    # LDX #0
    # loop: TXA; CLC; ADC #$30; JSR $FDED; INX; CPX #10; BNE loop; BRK
    code_6502 = bytes([
        0xA2, 0x00,       # LDX #0
        # loop:
        0x8A,             # TXA
        0x18,             # CLC
        0x69, 0x30,       # ADC #'0'
        0x20, 0xED, 0xFD, # JSR $FDED
        0xE8,             # INX
        0xE0, 0x0A,       # CPX #10
        0xD0, 0xF4,       # BNE loop (-12 bytes = -6 instructions, but in 6502 bytes)
        0x00,             # BRK
    ])

    # Calculate correct branch offset
    # BNE is at offset 12, target is offset 2
    # In 6502: 12 + 2 + offset = 2 → offset = -12 = 0xF4 ✓

    from converter.memory_map import opcode_to_handler
    threaded = bytearray()
    for b in code_6502:
        h = opcode_to_handler(b)
        threaded.append(h & 0xFF)
        threaded.append((h >> 8) & 0xFF)

    print(f"6502 code: {len(code_6502)} bytes")
    print(f"Threaded:  {len(threaded)} bytes")
    print()

    emu = Emulator6502()
    emu.reset()
    emu.load_threaded_code(bytes(threaded))

    output = emu.run()

    print(f"Output: '{output}'")
    expected = "0123456789"
    print(f"Expected: '{expected}'")
    print("=" * 60)

    return output == expected


if __name__ == "__main__":
    # Run tests
    results = []

    try:
        results.append(("Hello World", test_hello()))
    except Exception as e:
        print(f"Hello World FAILED: {e}")
        results.append(("Hello World", False))

    print()

    try:
        results.append(("Loop", test_loop()))
    except Exception as e:
        print(f"Loop FAILED: {e}")
        import traceback
        traceback.print_exc()
        results.append(("Loop", False))

    print()
    print("=" * 60)
    print("Results:")
    for name, passed in results:
        status = "PASS" if passed else "FAIL"
        print(f"  {name}: {status}")
