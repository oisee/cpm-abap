"""
6502 Emulator Runtime for Z80/VZ80

This module implements the RET-threaded 6502 emulator.
It can generate Z80 machine code for handlers OR run
directly in Python for testing.

Memory Map (Z80):
    $6000-$6FFF  Runtime: long handlers, traps, utilities
    $7000-$7FFF  Handler Table (16-way interleaved, 4 KB)
    $8000-$BFFF  6502 DATA Memory (16 KB)
    $C000-$FFFF  6502 CODE Stream (threaded, banked)

6502 Registers (stored in memory):
    REG_A   = $5F00  Accumulator
    REG_X   = $5F01  X index
    REG_Y   = $5F02  Y index
    REG_SP  = $5F03  Stack pointer (6502 stack at $8100)
    REG_P   = $5F04  Processor status (N V - B D I Z C)

Shadow return stack (for JSR/RTS):
    $5F10-$5F7F  Shadow stack (56 entries × 2 bytes)
    $5F08        Shadow stack pointer
"""

# Memory addresses
DATA_BASE = 0x8000      # 6502 memory starts here
CODE_BASE = 0xC000      # Threaded code starts here
HANDLER_BASE = 0x7000   # Handler table

# 6502 register addresses
REG_A = 0x5F00
REG_X = 0x5F01
REG_Y = 0x5F02
REG_SP = 0x5F03
REG_P = 0x5F04

# Shadow stack for JSR/RTS
SHADOW_SP = 0x5F08
SHADOW_STACK = 0x5F10

# 6502 Flag bits in REG_P
FLAG_C = 0x01  # Carry
FLAG_Z = 0x02  # Zero
FLAG_I = 0x04  # Interrupt disable
FLAG_D = 0x08  # Decimal mode (NOT IMPLEMENTED)
FLAG_B = 0x10  # Break
FLAG_V = 0x40  # Overflow
FLAG_N = 0x80  # Negative


class Emu6502:
    """
    6502 Emulator using RET-threading on VZ80.

    This is a Python reference implementation.
    The same logic will be implemented in Z80 assembly.
    """

    def __init__(self, vz80):
        """
        Args:
            vz80: VZ80 virtual machine instance
        """
        self.vm = vz80
        self.bus = vz80.bus
        self.running = True
        self.debug = False

        # Initialize registers in memory
        self._write_reg(REG_A, 0)
        self._write_reg(REG_X, 0)
        self._write_reg(REG_Y, 0)
        self._write_reg(REG_SP, 0xFF)  # Stack at $01FF going down
        self._write_reg(REG_P, 0x20)   # Unused bit always set

        # Initialize shadow stack pointer
        self._write16(SHADOW_SP, SHADOW_STACK)

    # Memory access helpers
    def _read(self, addr: int) -> int:
        return self.bus.read_mem(addr)

    def _write(self, addr: int, val: int):
        self.bus.write_mem(addr, val & 0xFF)

    def _read16(self, addr: int) -> int:
        lo = self._read(addr)
        hi = self._read(addr + 1)
        return (hi << 8) | lo

    def _write16(self, addr: int, val: int):
        self._write(addr, val & 0xFF)
        self._write(addr + 1, (val >> 8) & 0xFF)

    # Register access
    def _read_reg(self, addr: int) -> int:
        return self._read(addr)

    def _write_reg(self, addr: int, val: int):
        self._write(addr, val)

    @property
    def a(self) -> int:
        return self._read_reg(REG_A)

    @a.setter
    def a(self, val: int):
        self._write_reg(REG_A, val & 0xFF)

    @property
    def x(self) -> int:
        return self._read_reg(REG_X)

    @x.setter
    def x(self, val: int):
        self._write_reg(REG_X, val & 0xFF)

    @property
    def y(self) -> int:
        return self._read_reg(REG_Y)

    @y.setter
    def y(self, val: int):
        self._write_reg(REG_Y, val & 0xFF)

    @property
    def sp(self) -> int:
        return self._read_reg(REG_SP)

    @sp.setter
    def sp(self, val: int):
        self._write_reg(REG_SP, val & 0xFF)

    @property
    def p(self) -> int:
        return self._read_reg(REG_P)

    @p.setter
    def p(self, val: int):
        self._write_reg(REG_P, val | 0x20)  # Bit 5 always set

    # Flag helpers
    def get_flag(self, flag: int) -> bool:
        return (self.p & flag) != 0

    def set_flag(self, flag: int, val: bool):
        if val:
            self.p = self.p | flag
        else:
            self.p = self.p & ~flag

    def update_nz(self, val: int):
        """Update N and Z flags based on value"""
        self.set_flag(FLAG_Z, (val & 0xFF) == 0)
        self.set_flag(FLAG_N, (val & 0x80) != 0)

    # 6502 memory access (translated addresses)
    def read_6502(self, addr: int) -> int:
        """Read from 6502 address space"""
        return self._read(DATA_BASE + (addr & 0x3FFF))

    def write_6502(self, addr: int, val: int):
        """Write to 6502 address space"""
        self._write(DATA_BASE + (addr & 0x3FFF), val)

    # Stack operations (6502 stack at $8100-$81FF)
    def push(self, val: int):
        """Push byte to 6502 stack"""
        stack_addr = DATA_BASE + 0x100 + self.sp
        self._write(stack_addr, val)
        self.sp = (self.sp - 1) & 0xFF

    def pull(self) -> int:
        """Pull byte from 6502 stack"""
        self.sp = (self.sp + 1) & 0xFF
        stack_addr = DATA_BASE + 0x100 + self.sp
        return self._read(stack_addr)

    def push16(self, val: int):
        """Push 16-bit value (high byte first, like 6502)"""
        self.push((val >> 8) & 0xFF)
        self.push(val & 0xFF)

    def pull16(self) -> int:
        """Pull 16-bit value"""
        lo = self.pull()
        hi = self.pull()
        return (hi << 8) | lo

    # Shadow stack for threaded code (JSR/RTS)
    def shadow_push(self, val: int):
        """Push Z80 return address to shadow stack"""
        sp = self._read16(SHADOW_SP)
        self._write16(sp, val)
        self._write16(SHADOW_SP, sp + 2)

    def shadow_pull(self) -> int:
        """Pull Z80 return address from shadow stack"""
        sp = self._read16(SHADOW_SP) - 2
        self._write16(SHADOW_SP, sp)
        return self._read16(sp)

    # Threaded code execution
    def pop_handler(self) -> int:
        """
        Pop next handler address from code stream.
        In real Z80, this is just RET (pops from SP).
        Here we simulate it.
        """
        # Z80 SP points to threaded code
        z80_sp = self.vm.cpu.sp
        handler = self._read16(z80_sp)
        self.vm.cpu.sp = (z80_sp + 2) & 0xFFFF
        return handler

    def pop_operand_byte(self) -> int:
        """
        Pop next byte (low byte of handler) from code stream.
        Used for immediate/zeropage operands.
        """
        handler = self.pop_handler()
        return handler & 0xFF  # Low byte is the operand

    def pop_operand_word(self) -> int:
        """
        Pop 16-bit operand from code stream.
        Two handlers = two bytes (lo, hi).
        """
        lo = self.pop_operand_byte()
        hi = self.pop_operand_byte()
        return (hi << 8) | lo


# Trap handlers
class Traps:
    """HLE trap implementations"""

    @staticmethod
    def cout(emu: Emu6502):
        """$FDED - Output character from A"""
        ch = emu.a
        emu.bus.write_io(0x01, ch)  # VZ80 console output

    @staticmethod
    def getln(emu: Emu6502):
        """$FD6A - Read line into $0200"""
        buf_addr = DATA_BASE + 0x200
        count = 0
        while True:
            ch = emu.bus.read_io(0x01)
            if ch == 0xFF:  # No input
                continue
            if ch == 0x0D or ch == 0x0A:  # Enter
                break
            if count < 255:
                emu._write(buf_addr + count, ch)
                count += 1
        emu.x = count

    @staticmethod
    def home(emu: Emu6502):
        """$FC58 - Clear screen"""
        # Output form feed or ANSI clear
        emu.bus.write_io(0x01, 0x0C)  # Form feed


if __name__ == "__main__":
    print("6502 Emulator Runtime")
    print("=" * 40)
    print(f"DATA_BASE:    ${DATA_BASE:04X}")
    print(f"CODE_BASE:    ${CODE_BASE:04X}")
    print(f"HANDLER_BASE: ${HANDLER_BASE:04X}")
    print()
    print("6502 Registers:")
    print(f"  REG_A:  ${REG_A:04X}")
    print(f"  REG_X:  ${REG_X:04X}")
    print(f"  REG_Y:  ${REG_Y:04X}")
    print(f"  REG_SP: ${REG_SP:04X}")
    print(f"  REG_P:  ${REG_P:04X}")
