#!/usr/bin/env python3
"""
Interactive 6502 Emulator on Z80 Threaded Code
"""

from vz80 import VZ80Bus
from z80 import Z80
from runtime_6502_data import *

class Emu6502:
    def __init__(self):
        self.bus = VZ80Bus()
        self.cpu = Z80(self.bus)
        self.output = []
        self.bus.on_output = lambda ch: self.output.append(ch)
        self._load_runtime()

    def _load_runtime(self):
        # Load runtime components
        for i, b in enumerate(HLE_CODE):
            self.bus.fixed_memory[HLE_ORG + i] = b
        for i, b in enumerate(HANDLERS_CODE):
            self.bus.fixed_memory[HANDLERS_ORG + i] = b
        for i, b in enumerate(HANDLER_TABLE):
            self.bus.fixed_memory[TABLE_ORG + i] = b
        for i, b in enumerate(RUNTIME_CODE):
            self.bus.fixed_memory[RUNTIME_ORG + i] = b

        # Trap table - fill with UNIMPL first
        for i in range(1024):
            self.bus.fixed_memory[0x8C00 + i] = 0x50 if i % 2 == 0 else 0x60

        # Set up key traps
        traps = {
            0xFDED: LABELS['TRAP_COUT'],    # COUT - character output
            0xFD0C: LABELS['TRAP_RDKEY'],   # RDKEY - read key
            0xFD6A: LABELS['TRAP_GETLN'],   # GETLN - get line
            0xFC58: LABELS['TRAP_HOME'],    # HOME - clear screen
        }
        for rom_addr, handler in traps.items():
            offset = rom_addr & 0x3FF
            self.bus.fixed_memory[0x8C00 + offset] = handler & 0xFF
            self.bus.fixed_memory[0x8C00 + offset + 1] = handler >> 8

        # Init state
        self.bus.fixed_memory[0x8F00] = 0
        self.bus.fixed_memory[0x8F01] = 0
        self.bus.fixed_memory[0x8F02] = 0xFF
        self.bus.fixed_memory[0x8EFE] = 0xFE
        self.bus.fixed_memory[0x8EFF] = 0x8E
        self.bus.fixed_memory[0x8200] = 0xC9

    def run(self, code: bytes, org: int = 0x0800, max_cycles: int = 100000) -> str:
        """Run 6502 code and return output string"""
        self.output.clear()

        # Convert to threaded code
        threaded = bytearray()
        for b in code:
            threaded.append(b)
            threaded.append(0x70 | (b & 0x0F))

        # Load at correct offset
        code_offset = org * 2
        for i, b in enumerate(threaded):
            self.bus.banked_memory[0][code_offset + i] = b

        # Reset CPU
        self.cpu.sp = 0xC000 + code_offset
        self.cpu.pc = 0x8200
        self.cpu.a = self.cpu.b = self.cpu.c = 0
        self.bus.current_bank = 0

        # Run until HALT or max cycles
        cycles = 0
        while cycles < max_cycles:
            if self.bus.read_mem(self.cpu.pc) == 0x76:
                break
            self.cpu.step()
            cycles += 1

        return ''.join(self.output)

    def asm(self, *instructions) -> bytes:
        """Simple assembler helper"""
        code = bytearray()
        for instr in instructions:
            if isinstance(instr, (list, tuple)):
                code.extend(instr)
            elif isinstance(instr, int):
                code.append(instr)
            elif isinstance(instr, bytes):
                code.extend(instr)
        return bytes(code)

    def print_char(self, ch: str) -> bytes:
        """Generate code to print a character"""
        return bytes([0xA9, ord(ch), 0x20, 0xED, 0xFD])

    def print_string(self, s: str) -> bytes:
        """Generate code to print a string"""
        code = bytearray()
        for ch in s:
            code.extend([0xA9, ord(ch), 0x20, 0xED, 0xFD])
        return bytes(code)


# Quick helper functions
def lda_imm(val): return bytes([0xA9, val & 0xFF])
def ldx_imm(val): return bytes([0xA2, val & 0xFF])
def ldy_imm(val): return bytes([0xA0, val & 0xFF])
def sta_zp(addr): return bytes([0x85, addr & 0xFF])
def lda_zp(addr): return bytes([0xA5, addr & 0xFF])
def jsr(addr): return bytes([0x20, addr & 0xFF, (addr >> 8) & 0xFF])
def rts(): return bytes([0x60])
def brk(): return bytes([0x00])
def inx(): return bytes([0xE8])
def iny(): return bytes([0xC8])
def dex(): return bytes([0xCA])
def dey(): return bytes([0x88])

COUT = 0xFDED  # Apple II character output


if __name__ == '__main__':
    emu = Emu6502()

    print("6502 Emulator on Z80 Threaded Code")
    print("=" * 40)

    # Example 1: Hello World
    print("\n[Test 1] Hello World:")
    code = emu.print_string("Hello, 6502!\n") + brk()
    result = emu.run(code)
    print(f"Output: {result!r}")

    # Example 2: Loop with counter
    print("\n[Test 2] Count 0-9:")
    code = emu.asm(
        ldx_imm(0),          # X = 0
        # loop:
        [0x8A],              # TXA (A = X)
        [0x18],              # CLC
        [0x69, 0x30],        # ADC #'0'
        jsr(COUT),           # print digit
        inx(),               # X++
        [0xE0, 10],          # CPX #10
        [0xD0, 256-12],      # BNE loop (branch back 12 bytes)
        lda_imm(0x0A),       # newline
        jsr(COUT),
        brk()
    )
    result = emu.run(code)
    print(f"Output: {result!r}")

    # Example 3: Subroutine
    print("\n[Test 3] Subroutine call:")
    # Main at $0800, subroutine at $0820
    main_code = emu.asm(
        jsr(0x0820),         # call print_stars
        jsr(0x0820),         # call again
        lda_imm(0x0A),
        jsr(COUT),
        brk()
    )
    sub_code = emu.asm(      # print_stars at $0820
        lda_imm(ord('*')), jsr(COUT),
        lda_imm(ord('*')), jsr(COUT),
        lda_imm(ord('*')), jsr(COUT),
        rts()
    )
    # Pad main to reach $0820
    full_code = main_code + bytes(0x20 - len(main_code)) + sub_code
    result = emu.run(full_code)
    print(f"Output: {result!r}")

    print("\n" + "=" * 40)
    print("Interactive mode: use 'emu' object")
    print("Example: emu.run(emu.print_string('Test') + brk())")
