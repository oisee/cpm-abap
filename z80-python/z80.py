"""
Z80 CPU Emulator - Python port of ABAP implementation
Ported from zcl_cpu_z80.clas.abap for local testing
"""

from typing import List, Optional, Callable
from dataclasses import dataclass


# Flag bit positions in F register
FLAG_C = 1      # Carry
FLAG_N = 2      # Add/Subtract
FLAG_PV = 4     # Parity/Overflow
FLAG_F3 = 8     # Undocumented (bit 3)
FLAG_H = 16     # Half-carry
FLAG_F5 = 32    # Undocumented (bit 5)
FLAG_Z = 64     # Zero
FLAG_S = 128    # Sign


class Bus:
    """Bus interface for memory (64KB) and I/O (256 ports)"""

    def __init__(self):
        self.memory = bytearray(65536)
        self.io_ports = bytearray(256)
        self.input_buffer = ""
        self.output_buffer = ""

    def read_mem(self, addr: int) -> int:
        return self.memory[addr & 0xFFFF]

    def write_mem(self, addr: int, val: int):
        self.memory[addr & 0xFFFF] = val & 0xFF

    def read_io(self, port: int) -> int:
        return self.io_ports[port & 0xFF]

    def write_io(self, port: int, val: int):
        self.io_ports[port & 0xFF] = val & 0xFF

    def load(self, addr: int, data: bytes):
        for i, b in enumerate(data):
            self.memory[(addr + i) & 0xFFFF] = b

    def provide_input(self, text: str):
        self.input_buffer += text

    def get_output(self) -> str:
        return self.output_buffer

    def clear_output(self):
        self.output_buffer = ""


class Z80:
    """Z80 CPU Emulator"""

    def __init__(self, bus: Bus):
        self.bus = bus

        # Main registers (16-bit pairs)
        self.af = 0  # A (high) + F flags (low)
        self.bc = 0
        self.de = 0
        self.hl = 0

        # Alternate register set
        self.af_alt = 0
        self.bc_alt = 0
        self.de_alt = 0
        self.hl_alt = 0

        # Index registers
        self.ix = 0
        self.iy = 0

        # Other registers
        self.sp = 0xFFFF
        self.pc = 0
        self.i = 0   # Interrupt vector
        self.r = 0   # Memory refresh

        # Interrupt state
        self.iff1 = False
        self.iff2 = False
        self.im = 0  # Interrupt mode (0, 1, 2)

        # Execution state
        self.cycles = 0
        self.running = True
        self.halted = False

        # Pre-computed flag tables
        self._init_flag_tables()

    def _init_flag_tables(self):
        """Initialize pre-computed flag lookup tables"""
        self.sz_flags = []   # Sign + Zero
        self.szp_flags = []  # Sign + Zero + Parity

        for i in range(256):
            flags = 0
            if i >= 128:
                flags |= FLAG_S
            if i == 0:
                flags |= FLAG_Z
            self.sz_flags.append(flags)

            # Calculate parity
            parity = bin(i).count('1') % 2
            if parity == 0:
                flags |= FLAG_PV
            self.szp_flags.append(flags)

    def reset(self):
        """Reset CPU to initial state"""
        self.af = 0xFFFF
        self.bc = 0
        self.de = 0
        self.hl = 0
        self.af_alt = 0
        self.bc_alt = 0
        self.de_alt = 0
        self.hl_alt = 0
        self.ix = 0
        self.iy = 0
        self.sp = 0xFFFF
        self.pc = 0
        self.i = 0
        self.r = 0
        self.iff1 = False
        self.iff2 = False
        self.im = 0
        self.cycles = 0
        self.running = True
        self.halted = False

    # Register access helpers
    def get_high(self, pair: int) -> int:
        return (pair >> 8) & 0xFF

    def get_low(self, pair: int) -> int:
        return pair & 0xFF

    def set_high(self, pair: int, val: int) -> int:
        return (pair & 0xFF) | ((val & 0xFF) << 8)

    def set_low(self, pair: int, val: int) -> int:
        return (pair & 0xFF00) | (val & 0xFF)

    @property
    def a(self) -> int:
        return self.get_high(self.af)

    @a.setter
    def a(self, val: int):
        self.af = self.set_high(self.af, val)

    @property
    def f(self) -> int:
        return self.get_low(self.af)

    @f.setter
    def f(self, val: int):
        self.af = self.set_low(self.af, val)

    @property
    def b(self) -> int:
        return self.get_high(self.bc)

    @b.setter
    def b(self, val: int):
        self.bc = self.set_high(self.bc, val)

    @property
    def c(self) -> int:
        return self.get_low(self.bc)

    @c.setter
    def c(self, val: int):
        self.bc = self.set_low(self.bc, val)

    @property
    def d(self) -> int:
        return self.get_high(self.de)

    @d.setter
    def d(self, val: int):
        self.de = self.set_high(self.de, val)

    @property
    def e(self) -> int:
        return self.get_low(self.de)

    @e.setter
    def e(self, val: int):
        self.de = self.set_low(self.de, val)

    @property
    def h(self) -> int:
        return self.get_high(self.hl)

    @h.setter
    def h(self, val: int):
        self.hl = self.set_high(self.hl, val)

    @property
    def l(self) -> int:
        return self.get_low(self.hl)

    @l.setter
    def l(self, val: int):
        self.hl = self.set_low(self.hl, val)

    # Memory access
    def read8(self, addr: int) -> int:
        return self.bus.read_mem(addr & 0xFFFF)

    def write8(self, addr: int, val: int):
        self.bus.write_mem(addr & 0xFFFF, val & 0xFF)

    def read16(self, addr: int) -> int:
        lo = self.read8(addr)
        hi = self.read8(addr + 1)
        return (hi << 8) | lo

    def write16(self, addr: int, val: int):
        self.write8(addr, val & 0xFF)
        self.write8(addr + 1, (val >> 8) & 0xFF)

    # Fetch with PC increment
    def fetch8(self) -> int:
        val = self.read8(self.pc)
        self.pc = (self.pc + 1) & 0xFFFF
        return val

    def fetch16(self) -> int:
        lo = self.fetch8()
        hi = self.fetch8()
        return (hi << 8) | lo

    # Stack operations
    def push16(self, val: int):
        self.sp = (self.sp - 2) & 0xFFFF
        self.write16(self.sp, val)

    def pop16(self) -> int:
        val = self.read16(self.sp)
        self.sp = (self.sp + 2) & 0xFFFF
        return val

    # Flag helpers
    def get_flag(self, flag: int) -> bool:
        return (self.f & flag) != 0

    def set_flag(self, flag: int, val: bool):
        if val:
            self.f = self.f | flag
        else:
            self.f = self.f & ~flag

    # Register access by index (for opcode decoding)
    def get_reg8(self, idx: int) -> int:
        """Get 8-bit register by index: B=0, C=1, D=2, E=3, H=4, L=5, (HL)=6, A=7"""
        if idx == 0: return self.b
        elif idx == 1: return self.c
        elif idx == 2: return self.d
        elif idx == 3: return self.e
        elif idx == 4: return self.h
        elif idx == 5: return self.l
        elif idx == 6: return self.read8(self.hl)
        elif idx == 7: return self.a
        return 0

    def set_reg8(self, idx: int, val: int):
        """Set 8-bit register by index"""
        val = val & 0xFF
        if idx == 0: self.b = val
        elif idx == 1: self.c = val
        elif idx == 2: self.d = val
        elif idx == 3: self.e = val
        elif idx == 4: self.h = val
        elif idx == 5: self.l = val
        elif idx == 6: self.write8(self.hl, val)
        elif idx == 7: self.a = val

    def get_reg16(self, idx: int) -> int:
        """Get 16-bit register pair by index: BC=0, DE=1, HL=2, SP=3"""
        if idx == 0: return self.bc
        elif idx == 1: return self.de
        elif idx == 2: return self.hl
        elif idx == 3: return self.sp
        return 0

    def set_reg16(self, idx: int, val: int):
        """Set 16-bit register pair by index"""
        val = val & 0xFFFF
        if idx == 0: self.bc = val
        elif idx == 1: self.de = val
        elif idx == 2: self.hl = val
        elif idx == 3: self.sp = val

    # ALU operations
    def alu_add8(self, val: int, with_carry: bool = False) -> int:
        """ADD A,val or ADC A,val"""
        a = self.a
        carry_in = 1 if (with_carry and self.get_flag(FLAG_C)) else 0
        result = a + val + carry_in

        flags = 0
        if (result & 0xFF) == 0: flags |= FLAG_Z
        if result & 0x80: flags |= FLAG_S
        if result > 0xFF: flags |= FLAG_C
        if ((a ^ val ^ result) & 0x10): flags |= FLAG_H
        # Overflow: both operands same sign, result different sign
        if ((a ^ result) & (val ^ result) & 0x80): flags |= FLAG_PV

        self.a = result & 0xFF
        self.f = flags
        return self.a

    def alu_sub8(self, val: int, with_carry: bool = False) -> int:
        """SUB A,val or SBC A,val"""
        a = self.a
        carry_in = 1 if (with_carry and self.get_flag(FLAG_C)) else 0
        result = a - val - carry_in

        flags = FLAG_N
        if (result & 0xFF) == 0: flags |= FLAG_Z
        if result & 0x80: flags |= FLAG_S
        if result < 0: flags |= FLAG_C
        if ((a ^ val) & (a ^ result) & 0x80): flags |= FLAG_PV
        if ((a & 0xF) - (val & 0xF) - carry_in) < 0: flags |= FLAG_H

        self.a = result & 0xFF
        self.f = flags
        return self.a

    def alu_and8(self, val: int):
        """AND A,val"""
        result = self.a & val
        self.a = result
        self.f = self.szp_flags[result] | FLAG_H

    def alu_or8(self, val: int):
        """OR A,val"""
        result = self.a | val
        self.a = result
        self.f = self.szp_flags[result]

    def alu_xor8(self, val: int):
        """XOR A,val"""
        result = self.a ^ val
        self.a = result
        self.f = self.szp_flags[result]

    def alu_cp8(self, val: int):
        """CP A,val (compare without storing)"""
        a = self.a
        result = a - val

        flags = FLAG_N
        if (result & 0xFF) == 0: flags |= FLAG_Z
        if result & 0x80: flags |= FLAG_S
        if result < 0: flags |= FLAG_C
        if ((a ^ val) & (a ^ result) & 0x80): flags |= FLAG_PV
        if ((a & 0xF) - (val & 0xF)) < 0: flags |= FLAG_H

        self.f = flags

    def alu_inc8(self, val: int) -> int:
        """INC val"""
        result = (val + 1) & 0xFF
        flags = self.f & FLAG_C  # Preserve carry
        if result == 0: flags |= FLAG_Z
        if result & 0x80: flags |= FLAG_S
        if val == 0x7F: flags |= FLAG_PV  # Overflow
        if (val & 0xF) == 0xF: flags |= FLAG_H
        self.f = flags
        return result

    def alu_dec8(self, val: int) -> int:
        """DEC val"""
        result = (val - 1) & 0xFF
        flags = (self.f & FLAG_C) | FLAG_N  # Preserve carry, set N
        if result == 0: flags |= FLAG_Z
        if result & 0x80: flags |= FLAG_S
        if val == 0x80: flags |= FLAG_PV  # Overflow
        if (val & 0xF) == 0: flags |= FLAG_H
        self.f = flags
        return result

    # Rotate/shift operations
    def alu_rlc(self, val: int) -> int:
        """Rotate left circular"""
        bit7 = (val >> 7) & 1
        result = ((val << 1) | bit7) & 0xFF
        flags = self.szp_flags[result]
        if bit7: flags |= FLAG_C
        self.f = flags
        return result

    def alu_rrc(self, val: int) -> int:
        """Rotate right circular"""
        bit0 = val & 1
        result = ((val >> 1) | (bit0 << 7)) & 0xFF
        flags = self.szp_flags[result]
        if bit0: flags |= FLAG_C
        self.f = flags
        return result

    def alu_rl(self, val: int) -> int:
        """Rotate left through carry"""
        bit7 = (val >> 7) & 1
        carry = 1 if self.get_flag(FLAG_C) else 0
        result = ((val << 1) | carry) & 0xFF
        flags = self.szp_flags[result]
        if bit7: flags |= FLAG_C
        self.f = flags
        return result

    def alu_rr(self, val: int) -> int:
        """Rotate right through carry"""
        bit0 = val & 1
        carry = 0x80 if self.get_flag(FLAG_C) else 0
        result = ((val >> 1) | carry) & 0xFF
        flags = self.szp_flags[result]
        if bit0: flags |= FLAG_C
        self.f = flags
        return result

    def alu_sla(self, val: int) -> int:
        """Shift left arithmetic"""
        bit7 = (val >> 7) & 1
        result = (val << 1) & 0xFF
        flags = self.szp_flags[result]
        if bit7: flags |= FLAG_C
        self.f = flags
        return result

    def alu_sra(self, val: int) -> int:
        """Shift right arithmetic (preserve sign)"""
        bit0 = val & 1
        bit7 = val & 0x80
        result = ((val >> 1) | bit7) & 0xFF
        flags = self.szp_flags[result]
        if bit0: flags |= FLAG_C
        self.f = flags
        return result

    def alu_srl(self, val: int) -> int:
        """Shift right logical"""
        bit0 = val & 1
        result = (val >> 1) & 0xFF
        flags = self.szp_flags[result]
        if bit0: flags |= FLAG_C
        self.f = flags
        return result

    # Condition checking
    def check_condition(self, cond: int) -> bool:
        """Check condition code: NZ=0, Z=1, NC=2, C=3, PO=4, PE=5, P=6, M=7"""
        if cond == 0: return not self.get_flag(FLAG_Z)
        elif cond == 1: return self.get_flag(FLAG_Z)
        elif cond == 2: return not self.get_flag(FLAG_C)
        elif cond == 3: return self.get_flag(FLAG_C)
        elif cond == 4: return not self.get_flag(FLAG_PV)
        elif cond == 5: return self.get_flag(FLAG_PV)
        elif cond == 6: return not self.get_flag(FLAG_S)
        elif cond == 7: return self.get_flag(FLAG_S)
        return False

    def step(self) -> int:
        """Execute one instruction, return cycles consumed"""
        if self.halted:
            return 4

        opcode = self.fetch8()
        self.r = (self.r + 1) & 0x7F  # Refresh register

        return self.exec_main(opcode)

    def exec_cb(self) -> int:
        """Execute CB-prefixed instruction"""
        op = self.fetch8()
        reg = op & 7
        val = self.get_reg8(reg)

        group = op >> 6
        subop = (op >> 3) & 7

        if group == 0:  # Rotate/shift
            if subop == 0: result = self.alu_rlc(val)
            elif subop == 1: result = self.alu_rrc(val)
            elif subop == 2: result = self.alu_rl(val)
            elif subop == 3: result = self.alu_rr(val)
            elif subop == 4: result = self.alu_sla(val)
            elif subop == 5: result = self.alu_sra(val)
            elif subop == 6:  # SLL (undocumented)
                bit7 = (val >> 7) & 1
                result = ((val << 1) | 1) & 0xFF
                flags = self.szp_flags[result]
                if bit7: flags |= FLAG_C
                self.f = flags
            elif subop == 7: result = self.alu_srl(val)
            self.set_reg8(reg, result)

        elif group == 1:  # BIT
            bit = subop
            mask = 1 << bit
            flags = self.f & FLAG_C  # Preserve C
            flags |= FLAG_H
            if not (val & mask):
                flags |= FLAG_Z
            self.f = flags

        elif group == 2:  # RES
            bit = subop
            mask = 1 << bit
            result = val & ~mask
            self.set_reg8(reg, result)

        elif group == 3:  # SET
            bit = subop
            mask = 1 << bit
            result = val | mask
            self.set_reg8(reg, result)

        return 15 if reg == 6 else 8

    def exec_dd_fd(self, idx_reg: int, is_ix: bool) -> int:
        """Execute DD/FD prefixed instruction (IX/IY operations)"""
        op = self.fetch8()
        cycles = 8

        # LD IX/IY,nn
        if op == 0x21:
            nn = self.fetch16()
            self._dd_fd_result = nn
            cycles = 14

        # LD (nn),IX/IY
        elif op == 0x22:
            addr = self.fetch16()
            self.write16(addr, idx_reg)
            cycles = 20

        # LD IX/IY,(nn)
        elif op == 0x2A:
            addr = self.fetch16()
            self._dd_fd_result = self.read16(addr)
            cycles = 20

        # INC IX/IY
        elif op == 0x23:
            self._dd_fd_result = (idx_reg + 1) & 0xFFFF
            cycles = 10

        # DEC IX/IY
        elif op == 0x2B:
            self._dd_fd_result = (idx_reg - 1) & 0xFFFF
            cycles = 10

        # ADD IX/IY,rr
        elif op == 0x09:  # ADD IX/IY,BC
            result = idx_reg + self.bc
            flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
            if result > 0xFFFF: flags |= FLAG_C
            if ((idx_reg & 0xFFF) + (self.bc & 0xFFF)) > 0xFFF: flags |= FLAG_H
            self._dd_fd_result = result & 0xFFFF
            self.f = flags
            cycles = 15
        elif op == 0x19:  # ADD IX/IY,DE
            result = idx_reg + self.de
            flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
            if result > 0xFFFF: flags |= FLAG_C
            self._dd_fd_result = result & 0xFFFF
            self.f = flags
            cycles = 15
        elif op == 0x29:  # ADD IX/IY,IX/IY
            result = idx_reg + idx_reg
            flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
            if result > 0xFFFF: flags |= FLAG_C
            self._dd_fd_result = result & 0xFFFF
            self.f = flags
            cycles = 15
        elif op == 0x39:  # ADD IX/IY,SP
            result = idx_reg + self.sp
            flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
            if result > 0xFFFF: flags |= FLAG_C
            self._dd_fd_result = result & 0xFFFF
            self.f = flags
            cycles = 15

        # PUSH IX/IY
        elif op == 0xE5:
            self.push16(idx_reg)
            cycles = 15

        # POP IX/IY
        elif op == 0xE1:
            self._dd_fd_result = self.pop16()
            cycles = 14

        # JP (IX/IY)
        elif op == 0xE9:
            self.pc = idx_reg
            cycles = 8

        # LD SP,IX/IY
        elif op == 0xF9:
            self.sp = idx_reg
            cycles = 10

        # EX (SP),IX/IY
        elif op == 0xE3:
            tmp = self.read16(self.sp)
            self.write16(self.sp, idx_reg)
            self._dd_fd_result = tmp
            cycles = 23

        # LD r,(IX/IY+d) and LD (IX/IY+d),r
        elif op in [0x46, 0x4E, 0x56, 0x5E, 0x66, 0x6E, 0x7E]:  # LD r,(IX+d)
            d = self.fetch8()
            if d >= 128: d -= 256
            addr = (idx_reg + d) & 0xFFFF
            val = self.read8(addr)
            reg = (op >> 3) & 7
            self.set_reg8(reg, val)
            cycles = 19
        elif op in [0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x77]:  # LD (IX+d),r
            d = self.fetch8()
            if d >= 128: d -= 256
            addr = (idx_reg + d) & 0xFFFF
            reg = op & 7
            self.write8(addr, self.get_reg8(reg))
            cycles = 19

        # LD (IX/IY+d),n
        elif op == 0x36:
            d = self.fetch8()
            if d >= 128: d -= 256
            n = self.fetch8()
            addr = (idx_reg + d) & 0xFFFF
            self.write8(addr, n)
            cycles = 19

        # INC/DEC (IX/IY+d)
        elif op == 0x34:  # INC (IX+d)
            d = self.fetch8()
            if d >= 128: d -= 256
            addr = (idx_reg + d) & 0xFFFF
            val = self.alu_inc8(self.read8(addr))
            self.write8(addr, val)
            cycles = 23
        elif op == 0x35:  # DEC (IX+d)
            d = self.fetch8()
            if d >= 128: d -= 256
            addr = (idx_reg + d) & 0xFFFF
            val = self.alu_dec8(self.read8(addr))
            self.write8(addr, val)
            cycles = 23

        # ALU A,(IX/IY+d)
        elif 0x86 <= op <= 0xBE and (op & 7) == 6:
            d = self.fetch8()
            if d >= 128: d -= 256
            addr = (idx_reg + d) & 0xFFFF
            val = self.read8(addr)
            alu_op = (op >> 3) & 7
            if alu_op == 0: self.alu_add8(val)
            elif alu_op == 1: self.alu_add8(val, with_carry=True)
            elif alu_op == 2: self.alu_sub8(val)
            elif alu_op == 3: self.alu_sub8(val, with_carry=True)
            elif alu_op == 4: self.alu_and8(val)
            elif alu_op == 5: self.alu_xor8(val)
            elif alu_op == 6: self.alu_or8(val)
            elif alu_op == 7: self.alu_cp8(val)
            cycles = 19

        # CB prefix with IX/IY (bit operations on (IX+d))
        elif op == 0xCB:
            d = self.fetch8()
            if d >= 128: d -= 256
            cb_op = self.fetch8()
            addr = (idx_reg + d) & 0xFFFF
            val = self.read8(addr)

            group = cb_op >> 6
            subop = (cb_op >> 3) & 7

            if group == 0:  # Rotate/shift
                if subop == 0: result = self.alu_rlc(val)
                elif subop == 1: result = self.alu_rrc(val)
                elif subop == 2: result = self.alu_rl(val)
                elif subop == 3: result = self.alu_rr(val)
                elif subop == 4: result = self.alu_sla(val)
                elif subop == 5: result = self.alu_sra(val)
                elif subop == 6: result = ((val << 1) | 1) & 0xFF  # SLL
                elif subop == 7: result = self.alu_srl(val)
                self.write8(addr, result)
            elif group == 1:  # BIT
                bit = subop
                mask = 1 << bit
                flags = self.f & FLAG_C
                flags |= FLAG_H
                if not (val & mask):
                    flags |= FLAG_Z
                self.f = flags
            elif group == 2:  # RES
                bit = subop
                mask = 1 << bit
                self.write8(addr, val & ~mask)
            elif group == 3:  # SET
                bit = subop
                mask = 1 << bit
                self.write8(addr, val | mask)

            cycles = 23

        # LD IXH/IXL or IYH/IYL (undocumented)
        elif op == 0x26:  # LD IXH/IYH,n
            n = self.fetch8()
            self._dd_fd_result = (idx_reg & 0xFF) | (n << 8)
            cycles = 11
        elif op == 0x2E:  # LD IXL/IYL,n
            n = self.fetch8()
            self._dd_fd_result = (idx_reg & 0xFF00) | n
            cycles = 11

        else:
            # Unimplemented - just skip
            cycles = 8

        return cycles

    def exec_main(self, opcode: int) -> int:
        """Execute main opcode, return cycles"""
        cycles = 4  # Default

        # NOP
        if opcode == 0x00:
            cycles = 4

        # LD rr,nn
        elif opcode == 0x01:  # LD BC,nn
            self.bc = self.fetch16()
            cycles = 10
        elif opcode == 0x11:  # LD DE,nn
            self.de = self.fetch16()
            cycles = 10
        elif opcode == 0x21:  # LD HL,nn
            self.hl = self.fetch16()
            cycles = 10
        elif opcode == 0x31:  # LD SP,nn
            self.sp = self.fetch16()
            cycles = 10

        # LD r,n
        elif opcode == 0x06:  # LD B,n
            self.b = self.fetch8()
            cycles = 7
        elif opcode == 0x0E:  # LD C,n
            self.c = self.fetch8()
            cycles = 7
        elif opcode == 0x16:  # LD D,n
            self.d = self.fetch8()
            cycles = 7
        elif opcode == 0x1E:  # LD E,n
            self.e = self.fetch8()
            cycles = 7
        elif opcode == 0x26:  # LD H,n
            self.h = self.fetch8()
            cycles = 7
        elif opcode == 0x2E:  # LD L,n
            self.l = self.fetch8()
            cycles = 7
        elif opcode == 0x36:  # LD (HL),n
            self.write8(self.hl, self.fetch8())
            cycles = 10
        elif opcode == 0x3E:  # LD A,n
            self.a = self.fetch8()
            cycles = 7

        # LD r,r (0x40-0x7F except HALT)
        elif 0x40 <= opcode <= 0x7F and opcode != 0x76:
            dst = (opcode >> 3) & 7
            src = opcode & 7
            self.set_reg8(dst, self.get_reg8(src))
            cycles = 7 if (src == 6 or dst == 6) else 4

        # HALT
        elif opcode == 0x76:
            self.halted = True
            cycles = 4

        # ALU A,r
        elif 0x80 <= opcode <= 0xBF:
            src = opcode & 7
            val = self.get_reg8(src)
            op = (opcode >> 3) & 7
            if op == 0: self.alu_add8(val)
            elif op == 1: self.alu_add8(val, with_carry=True)
            elif op == 2: self.alu_sub8(val)
            elif op == 3: self.alu_sub8(val, with_carry=True)
            elif op == 4: self.alu_and8(val)
            elif op == 5: self.alu_xor8(val)
            elif op == 6: self.alu_or8(val)
            elif op == 7: self.alu_cp8(val)
            cycles = 7 if src == 6 else 4

        # INC r
        elif opcode in [0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C, 0x34, 0x3C]:
            reg = (opcode >> 3) & 7
            self.set_reg8(reg, self.alu_inc8(self.get_reg8(reg)))
            cycles = 11 if reg == 6 else 4

        # DEC r
        elif opcode in [0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D, 0x35, 0x3D]:
            reg = (opcode >> 3) & 7
            self.set_reg8(reg, self.alu_dec8(self.get_reg8(reg)))
            cycles = 11 if reg == 6 else 4

        # INC rr
        elif opcode == 0x03:  # INC BC
            self.bc = (self.bc + 1) & 0xFFFF
            cycles = 6
        elif opcode == 0x13:  # INC DE
            self.de = (self.de + 1) & 0xFFFF
            cycles = 6
        elif opcode == 0x23:  # INC HL
            self.hl = (self.hl + 1) & 0xFFFF
            cycles = 6
        elif opcode == 0x33:  # INC SP
            self.sp = (self.sp + 1) & 0xFFFF
            cycles = 6

        # DEC rr
        elif opcode == 0x0B:  # DEC BC
            self.bc = (self.bc - 1) & 0xFFFF
            cycles = 6
        elif opcode == 0x1B:  # DEC DE
            self.de = (self.de - 1) & 0xFFFF
            cycles = 6
        elif opcode == 0x2B:  # DEC HL
            self.hl = (self.hl - 1) & 0xFFFF
            cycles = 6
        elif opcode == 0x3B:  # DEC SP
            self.sp = (self.sp - 1) & 0xFFFF
            cycles = 6

        # JP nn
        elif opcode == 0xC3:
            self.pc = self.fetch16()
            cycles = 10

        # JP cc,nn
        elif opcode in [0xC2, 0xCA, 0xD2, 0xDA, 0xE2, 0xEA, 0xF2, 0xFA]:
            addr = self.fetch16()
            cond = (opcode >> 3) & 7
            if self.check_condition(cond):
                self.pc = addr
            cycles = 10

        # JR e
        elif opcode == 0x18:
            offset = self.fetch8()
            if offset >= 128:
                offset -= 256
            self.pc = (self.pc + offset) & 0xFFFF
            cycles = 12

        # JR cc,e
        elif opcode in [0x20, 0x28, 0x30, 0x38]:
            offset = self.fetch8()
            if offset >= 128:
                offset -= 256
            cond = (opcode >> 3) & 3
            if self.check_condition(cond):
                self.pc = (self.pc + offset) & 0xFFFF
                cycles = 12
            else:
                cycles = 7

        # DJNZ e
        elif opcode == 0x10:
            offset = self.fetch8()
            if offset >= 128:
                offset -= 256
            self.b = (self.b - 1) & 0xFF
            if self.b != 0:
                self.pc = (self.pc + offset) & 0xFFFF
                cycles = 13
            else:
                cycles = 8

        # CALL nn
        elif opcode == 0xCD:
            addr = self.fetch16()
            self.push16(self.pc)
            self.pc = addr
            cycles = 17

        # CALL cc,nn
        elif opcode in [0xC4, 0xCC, 0xD4, 0xDC, 0xE4, 0xEC, 0xF4, 0xFC]:
            addr = self.fetch16()
            cond = (opcode >> 3) & 7
            if self.check_condition(cond):
                self.push16(self.pc)
                self.pc = addr
                cycles = 17
            else:
                cycles = 10

        # RET
        elif opcode == 0xC9:
            self.pc = self.pop16()
            cycles = 10

        # RET cc
        elif opcode in [0xC0, 0xC8, 0xD0, 0xD8, 0xE0, 0xE8, 0xF0, 0xF8]:
            cond = (opcode >> 3) & 7
            if self.check_condition(cond):
                self.pc = self.pop16()
                cycles = 11
            else:
                cycles = 5

        # PUSH rr
        elif opcode == 0xC5:  # PUSH BC
            self.push16(self.bc)
            cycles = 11
        elif opcode == 0xD5:  # PUSH DE
            self.push16(self.de)
            cycles = 11
        elif opcode == 0xE5:  # PUSH HL
            self.push16(self.hl)
            cycles = 11
        elif opcode == 0xF5:  # PUSH AF
            self.push16(self.af)
            cycles = 11

        # POP rr
        elif opcode == 0xC1:  # POP BC
            self.bc = self.pop16()
            cycles = 10
        elif opcode == 0xD1:  # POP DE
            self.de = self.pop16()
            cycles = 10
        elif opcode == 0xE1:  # POP HL
            self.hl = self.pop16()
            cycles = 10
        elif opcode == 0xF1:  # POP AF
            self.af = self.pop16()
            cycles = 10

        # RST n
        elif opcode in [0xC7, 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF]:
            addr = opcode & 0x38
            self.push16(self.pc)
            self.pc = addr
            cycles = 11

        # ALU A,n
        elif opcode == 0xC6:  # ADD A,n
            self.alu_add8(self.fetch8())
            cycles = 7
        elif opcode == 0xCE:  # ADC A,n
            self.alu_add8(self.fetch8(), with_carry=True)
            cycles = 7
        elif opcode == 0xD6:  # SUB n
            self.alu_sub8(self.fetch8())
            cycles = 7
        elif opcode == 0xDE:  # SBC A,n
            self.alu_sub8(self.fetch8(), with_carry=True)
            cycles = 7
        elif opcode == 0xE6:  # AND n
            self.alu_and8(self.fetch8())
            cycles = 7
        elif opcode == 0xEE:  # XOR n
            self.alu_xor8(self.fetch8())
            cycles = 7
        elif opcode == 0xF6:  # OR n
            self.alu_or8(self.fetch8())
            cycles = 7
        elif opcode == 0xFE:  # CP n
            self.alu_cp8(self.fetch8())
            cycles = 7

        # Rotate A
        elif opcode == 0x07:  # RLCA
            bit7 = (self.a >> 7) & 1
            self.a = ((self.a << 1) | bit7) & 0xFF
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | (FLAG_C if bit7 else 0)
            cycles = 4
        elif opcode == 0x0F:  # RRCA
            bit0 = self.a & 1
            self.a = ((self.a >> 1) | (bit0 << 7)) & 0xFF
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | (FLAG_C if bit0 else 0)
            cycles = 4
        elif opcode == 0x17:  # RLA
            bit7 = (self.a >> 7) & 1
            carry = 1 if self.get_flag(FLAG_C) else 0
            self.a = ((self.a << 1) | carry) & 0xFF
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | (FLAG_C if bit7 else 0)
            cycles = 4
        elif opcode == 0x1F:  # RRA
            bit0 = self.a & 1
            carry = 0x80 if self.get_flag(FLAG_C) else 0
            self.a = ((self.a >> 1) | carry) & 0xFF
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | (FLAG_C if bit0 else 0)
            cycles = 4

        # CB prefix
        elif opcode == 0xCB:
            cycles = self.exec_cb()

        # DI/EI
        elif opcode == 0xF3:  # DI
            self.iff1 = False
            self.iff2 = False
            cycles = 4
        elif opcode == 0xFB:  # EI
            self.iff1 = True
            self.iff2 = True
            cycles = 4

        # EX DE,HL
        elif opcode == 0xEB:
            self.de, self.hl = self.hl, self.de
            cycles = 4

        # EX AF,AF'
        elif opcode == 0x08:
            self.af, self.af_alt = self.af_alt, self.af
            cycles = 4

        # EXX
        elif opcode == 0xD9:
            self.bc, self.bc_alt = self.bc_alt, self.bc
            self.de, self.de_alt = self.de_alt, self.de
            self.hl, self.hl_alt = self.hl_alt, self.hl
            cycles = 4

        # EX (SP),HL - exchange HL with top of stack
        elif opcode == 0xE3:
            tmp = self.read16(self.sp)
            self.write16(self.sp, self.hl)
            self.hl = tmp
            cycles = 19

        # JP (HL)
        elif opcode == 0xE9:
            self.pc = self.hl
            cycles = 4

        # LD SP,HL
        elif opcode == 0xF9:
            self.sp = self.hl
            cycles = 6

        # Memory load/store
        elif opcode == 0x02:  # LD (BC),A
            self.write8(self.bc, self.a)
            cycles = 7
        elif opcode == 0x12:  # LD (DE),A
            self.write8(self.de, self.a)
            cycles = 7
        elif opcode == 0x0A:  # LD A,(BC)
            self.a = self.read8(self.bc)
            cycles = 7
        elif opcode == 0x1A:  # LD A,(DE)
            self.a = self.read8(self.de)
            cycles = 7
        elif opcode == 0x22:  # LD (nn),HL
            addr = self.fetch16()
            self.write16(addr, self.hl)
            cycles = 16
        elif opcode == 0x2A:  # LD HL,(nn)
            addr = self.fetch16()
            self.hl = self.read16(addr)
            cycles = 16
        elif opcode == 0x32:  # LD (nn),A
            addr = self.fetch16()
            self.write8(addr, self.a)
            cycles = 13
        elif opcode == 0x3A:  # LD A,(nn)
            addr = self.fetch16()
            self.a = self.read8(addr)
            cycles = 13
        elif opcode == 0x77:  # LD (HL),A
            self.write8(self.hl, self.a)
            cycles = 7
        elif opcode == 0x7E:  # LD A,(HL)
            self.a = self.read8(self.hl)
            cycles = 7

        # ADD HL,rr
        elif opcode == 0x09:  # ADD HL,BC
            result = self.hl + self.bc
            flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
            if result > 0xFFFF: flags |= FLAG_C
            if ((self.hl & 0xFFF) + (self.bc & 0xFFF)) > 0xFFF: flags |= FLAG_H
            self.hl = result & 0xFFFF
            self.f = flags
            cycles = 11
        elif opcode == 0x19:  # ADD HL,DE
            result = self.hl + self.de
            flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
            if result > 0xFFFF: flags |= FLAG_C
            if ((self.hl & 0xFFF) + (self.de & 0xFFF)) > 0xFFF: flags |= FLAG_H
            self.hl = result & 0xFFFF
            self.f = flags
            cycles = 11
        elif opcode == 0x29:  # ADD HL,HL
            result = self.hl + self.hl
            flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
            if result > 0xFFFF: flags |= FLAG_C
            if ((self.hl & 0xFFF) * 2) > 0xFFF: flags |= FLAG_H
            self.hl = result & 0xFFFF
            self.f = flags
            cycles = 11
        elif opcode == 0x39:  # ADD HL,SP
            result = self.hl + self.sp
            flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
            if result > 0xFFFF: flags |= FLAG_C
            if ((self.hl & 0xFFF) + (self.sp & 0xFFF)) > 0xFFF: flags |= FLAG_H
            self.hl = result & 0xFFFF
            self.f = flags
            cycles = 11

        # CPL
        elif opcode == 0x2F:
            self.a = (~self.a) & 0xFF
            self.f |= FLAG_H | FLAG_N
            cycles = 4

        # DAA - Decimal Adjust Accumulator
        elif opcode == 0x27:
            a = self.a
            correction = 0
            carry = False

            if self.get_flag(FLAG_N):
                # After subtraction
                if self.get_flag(FLAG_H):
                    correction |= 0x06
                if self.get_flag(FLAG_C):
                    correction |= 0x60
                    carry = True
                a = (a - correction) & 0xFF
            else:
                # After addition
                if self.get_flag(FLAG_H) or (a & 0x0F) > 9:
                    correction |= 0x06
                if self.get_flag(FLAG_C) or a > 0x99:
                    correction |= 0x60
                    carry = True
                a = (a + correction) & 0xFF

            self.a = a
            # Update flags: Z, S from result, P/V is parity, H is undefined (we clear it)
            # C is set if there was a carry, N is preserved
            flags = self.szp_flags[a] | (FLAG_N if self.get_flag(FLAG_N) else 0)
            if carry:
                flags |= FLAG_C
            self.f = flags
            cycles = 4

        # SCF
        elif opcode == 0x37:
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | FLAG_C
            cycles = 4

        # CCF
        elif opcode == 0x3F:
            carry = FLAG_C if not self.get_flag(FLAG_C) else 0
            half = FLAG_H if self.get_flag(FLAG_C) else 0
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | carry | half
            cycles = 4

        # IN A,(n) - Full 16-bit port address: A*256 + n
        elif opcode == 0xDB:
            port_low = self.fetch8()
            port_full = (self.a << 8) | port_low  # A is high byte
            self.a = self.bus.read_io(port_full)
            cycles = 11

        # OUT (n),A - Full 16-bit port address: A*256 + n
        elif opcode == 0xD3:
            port_low = self.fetch8()
            port_full = (self.a << 8) | port_low  # A is high byte
            self.bus.write_io(port_full, self.a)
            cycles = 11

        # DD prefix (IX)
        elif opcode == 0xDD:
            cycles = self.exec_dd_fd(self.ix, True)
            # Update IX if modified
            if hasattr(self, '_dd_fd_result'):
                self.ix = self._dd_fd_result
                del self._dd_fd_result

        # ED prefix instructions
        elif opcode == 0xED:
            ed_op = self.fetch8()
            if ed_op == 0x53:  # LD (nn), DE
                addr = self.fetch16()
                self.bus.write_mem(addr, self.e)
                self.bus.write_mem(addr + 1, self.d)
                cycles = 20
            elif ed_op == 0x5B:  # LD DE, (nn)
                addr = self.fetch16()
                self.e = self.bus.read_mem(addr)
                self.d = self.bus.read_mem(addr + 1)
                cycles = 20
            elif ed_op == 0x43:  # LD (nn), BC
                addr = self.fetch16()
                self.bus.write_mem(addr, self.c)
                self.bus.write_mem(addr + 1, self.b)
                cycles = 20
            elif ed_op == 0x4B:  # LD BC, (nn)
                addr = self.fetch16()
                self.c = self.bus.read_mem(addr)
                self.b = self.bus.read_mem(addr + 1)
                cycles = 20
            elif ed_op == 0x73:  # LD (nn), SP
                addr = self.fetch16()
                self.bus.write_mem(addr, self.sp & 0xFF)
                self.bus.write_mem(addr + 1, (self.sp >> 8) & 0xFF)
                cycles = 20
            elif ed_op == 0x7B:  # LD SP, (nn)
                addr = self.fetch16()
                lo = self.bus.read_mem(addr)
                hi = self.bus.read_mem(addr + 1)
                self.sp = (hi << 8) | lo
                cycles = 20
            elif ed_op == 0x63:  # LD (nn), HL (undocumented but used)
                addr = self.fetch16()
                self.bus.write_mem(addr, self.l)
                self.bus.write_mem(addr + 1, self.h)
                cycles = 20
            elif ed_op == 0x6B:  # LD HL, (nn) (undocumented but used)
                addr = self.fetch16()
                self.l = self.bus.read_mem(addr)
                self.h = self.bus.read_mem(addr + 1)
                cycles = 20

            # Block transfer instructions
            elif ed_op == 0xA0:  # LDI - Load and Increment
                val = self.read8(self.hl)
                self.write8(self.de, val)
                self.hl = (self.hl + 1) & 0xFFFF
                self.de = (self.de + 1) & 0xFFFF
                self.bc = (self.bc - 1) & 0xFFFF
                self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_C)) | (FLAG_PV if self.bc != 0 else 0)
                cycles = 16

            elif ed_op == 0xB0:  # LDIR - Block Load, Increment, Repeat
                while True:
                    val = self.read8(self.hl)
                    self.write8(self.de, val)
                    self.hl = (self.hl + 1) & 0xFFFF
                    self.de = (self.de + 1) & 0xFFFF
                    self.bc = (self.bc - 1) & 0xFFFF
                    cycles += 21
                    if self.bc == 0:
                        break
                self.f = self.f & (FLAG_S | FLAG_Z | FLAG_C)  # Clear H, N, PV
                cycles = 16  # Final iteration

            elif ed_op == 0xA8:  # LDD - Load and Decrement
                val = self.read8(self.hl)
                self.write8(self.de, val)
                self.hl = (self.hl - 1) & 0xFFFF
                self.de = (self.de - 1) & 0xFFFF
                self.bc = (self.bc - 1) & 0xFFFF
                self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_C)) | (FLAG_PV if self.bc != 0 else 0)
                cycles = 16

            elif ed_op == 0xB8:  # LDDR - Block Load, Decrement, Repeat
                while True:
                    val = self.read8(self.hl)
                    self.write8(self.de, val)
                    self.hl = (self.hl - 1) & 0xFFFF
                    self.de = (self.de - 1) & 0xFFFF
                    self.bc = (self.bc - 1) & 0xFFFF
                    cycles += 21
                    if self.bc == 0:
                        break
                self.f = self.f & (FLAG_S | FLAG_Z | FLAG_C)
                cycles = 16

            # Compare block instructions
            elif ed_op == 0xA1:  # CPI - Compare and Increment
                val = self.read8(self.hl)
                result = (self.a - val) & 0xFF
                self.hl = (self.hl + 1) & 0xFFFF
                self.bc = (self.bc - 1) & 0xFFFF
                flags = FLAG_N
                if result == 0: flags |= FLAG_Z
                if result & 0x80: flags |= FLAG_S
                if self.bc != 0: flags |= FLAG_PV
                if (self.a & 0xF) < (val & 0xF): flags |= FLAG_H
                self.f = (self.f & FLAG_C) | flags
                cycles = 16

            elif ed_op == 0xB1:  # CPIR - Block Compare, Increment, Repeat
                while True:
                    val = self.read8(self.hl)
                    result = (self.a - val) & 0xFF
                    self.hl = (self.hl + 1) & 0xFFFF
                    self.bc = (self.bc - 1) & 0xFFFF
                    cycles += 21
                    if result == 0 or self.bc == 0:
                        break
                flags = FLAG_N
                if result == 0: flags |= FLAG_Z
                if result & 0x80: flags |= FLAG_S
                if self.bc != 0: flags |= FLAG_PV
                if (self.a & 0xF) < (val & 0xF): flags |= FLAG_H
                self.f = (self.f & FLAG_C) | flags
                cycles = 16

            # I/O block instructions
            elif ed_op == 0x44:  # NEG
                val = self.a
                self.a = (0 - val) & 0xFF
                flags = FLAG_N
                if self.a == 0: flags |= FLAG_Z
                if self.a & 0x80: flags |= FLAG_S
                if val != 0: flags |= FLAG_C
                if val == 0x80: flags |= FLAG_PV
                if (val & 0xF) != 0: flags |= FLAG_H
                self.f = flags
                cycles = 8

            elif ed_op == 0x46:  # IM 0
                self.im = 0
                cycles = 8
            elif ed_op == 0x56:  # IM 1
                self.im = 1
                cycles = 8
            elif ed_op == 0x5E:  # IM 2
                self.im = 2
                cycles = 8

            elif ed_op == 0x47:  # LD I,A
                self.i = self.a
                cycles = 9
            elif ed_op == 0x4F:  # LD R,A
                self.r = self.a
                cycles = 9
            elif ed_op == 0x57:  # LD A,I
                self.a = self.i
                flags = self.f & FLAG_C
                if self.a == 0: flags |= FLAG_Z
                if self.a & 0x80: flags |= FLAG_S
                if self.iff2: flags |= FLAG_PV
                self.f = flags
                cycles = 9
            elif ed_op == 0x5F:  # LD A,R
                self.a = self.r
                flags = self.f & FLAG_C
                if self.a == 0: flags |= FLAG_Z
                if self.a & 0x80: flags |= FLAG_S
                if self.iff2: flags |= FLAG_PV
                self.f = flags
                cycles = 9

            elif ed_op == 0x4D:  # RETI
                self.pc = self.pop16()
                cycles = 14
            elif ed_op == 0x45:  # RETN
                self.iff1 = self.iff2
                self.pc = self.pop16()
                cycles = 14

            # IN/OUT with (C)
            elif ed_op == 0x40:  # IN B,(C)
                self.b = self.bus.read_io(self.c)
                self.f = (self.f & FLAG_C) | self.szp_flags[self.b]
                cycles = 12
            elif ed_op == 0x48:  # IN C,(C)
                self.c = self.bus.read_io(self.c)
                self.f = (self.f & FLAG_C) | self.szp_flags[self.c]
                cycles = 12
            elif ed_op == 0x50:  # IN D,(C)
                self.d = self.bus.read_io(self.c)
                self.f = (self.f & FLAG_C) | self.szp_flags[self.d]
                cycles = 12
            elif ed_op == 0x58:  # IN E,(C)
                self.e = self.bus.read_io(self.c)
                self.f = (self.f & FLAG_C) | self.szp_flags[self.e]
                cycles = 12
            elif ed_op == 0x60:  # IN H,(C)
                self.h = self.bus.read_io(self.c)
                self.f = (self.f & FLAG_C) | self.szp_flags[self.h]
                cycles = 12
            elif ed_op == 0x68:  # IN L,(C)
                self.l = self.bus.read_io(self.c)
                self.f = (self.f & FLAG_C) | self.szp_flags[self.l]
                cycles = 12
            elif ed_op == 0x78:  # IN A,(C)
                self.a = self.bus.read_io(self.c)
                self.f = (self.f & FLAG_C) | self.szp_flags[self.a]
                cycles = 12

            elif ed_op == 0x41:  # OUT (C),B
                self.bus.write_io(self.c, self.b)
                cycles = 12
            elif ed_op == 0x49:  # OUT (C),C
                self.bus.write_io(self.c, self.c)
                cycles = 12
            elif ed_op == 0x51:  # OUT (C),D
                self.bus.write_io(self.c, self.d)
                cycles = 12
            elif ed_op == 0x59:  # OUT (C),E
                self.bus.write_io(self.c, self.e)
                cycles = 12
            elif ed_op == 0x61:  # OUT (C),H
                self.bus.write_io(self.c, self.h)
                cycles = 12
            elif ed_op == 0x69:  # OUT (C),L
                self.bus.write_io(self.c, self.l)
                cycles = 12
            elif ed_op == 0x79:  # OUT (C),A
                self.bus.write_io(self.c, self.a)
                cycles = 12

            # 16-bit arithmetic
            elif ed_op == 0x42:  # SBC HL,BC
                result = self.hl - self.bc - (1 if self.get_flag(FLAG_C) else 0)
                flags = FLAG_N
                if (result & 0xFFFF) == 0: flags |= FLAG_Z
                if result & 0x8000: flags |= FLAG_S
                if result < 0: flags |= FLAG_C
                if ((self.hl ^ self.bc) & (self.hl ^ result) & 0x8000): flags |= FLAG_PV
                if ((self.hl & 0xFFF) - (self.bc & 0xFFF) - (1 if self.get_flag(FLAG_C) else 0)) < 0: flags |= FLAG_H
                self.hl = result & 0xFFFF
                self.f = flags
                cycles = 15
            elif ed_op == 0x52:  # SBC HL,DE
                result = self.hl - self.de - (1 if self.get_flag(FLAG_C) else 0)
                flags = FLAG_N
                if (result & 0xFFFF) == 0: flags |= FLAG_Z
                if result & 0x8000: flags |= FLAG_S
                if result < 0: flags |= FLAG_C
                if ((self.hl ^ self.de) & (self.hl ^ result) & 0x8000): flags |= FLAG_PV
                self.hl = result & 0xFFFF
                self.f = flags
                cycles = 15
            elif ed_op == 0x62:  # SBC HL,HL
                result = self.hl - self.hl - (1 if self.get_flag(FLAG_C) else 0)
                flags = FLAG_N
                if (result & 0xFFFF) == 0: flags |= FLAG_Z
                if result & 0x8000: flags |= FLAG_S
                if result < 0: flags |= FLAG_C
                self.hl = result & 0xFFFF
                self.f = flags
                cycles = 15
            elif ed_op == 0x72:  # SBC HL,SP
                result = self.hl - self.sp - (1 if self.get_flag(FLAG_C) else 0)
                flags = FLAG_N
                if (result & 0xFFFF) == 0: flags |= FLAG_Z
                if result & 0x8000: flags |= FLAG_S
                if result < 0: flags |= FLAG_C
                self.hl = result & 0xFFFF
                self.f = flags
                cycles = 15

            elif ed_op == 0x4A:  # ADC HL,BC
                carry = 1 if self.get_flag(FLAG_C) else 0
                result = self.hl + self.bc + carry
                flags = 0
                if (result & 0xFFFF) == 0: flags |= FLAG_Z
                if result & 0x8000: flags |= FLAG_S
                if result > 0xFFFF: flags |= FLAG_C
                if ((self.hl ^ result) & (self.bc ^ result) & 0x8000): flags |= FLAG_PV
                self.hl = result & 0xFFFF
                self.f = flags
                cycles = 15
            elif ed_op == 0x5A:  # ADC HL,DE
                carry = 1 if self.get_flag(FLAG_C) else 0
                result = self.hl + self.de + carry
                flags = 0
                if (result & 0xFFFF) == 0: flags |= FLAG_Z
                if result & 0x8000: flags |= FLAG_S
                if result > 0xFFFF: flags |= FLAG_C
                if ((self.hl ^ result) & (self.de ^ result) & 0x8000): flags |= FLAG_PV
                self.hl = result & 0xFFFF
                self.f = flags
                cycles = 15
            elif ed_op == 0x6A:  # ADC HL,HL
                carry = 1 if self.get_flag(FLAG_C) else 0
                result = self.hl + self.hl + carry
                flags = 0
                if (result & 0xFFFF) == 0: flags |= FLAG_Z
                if result & 0x8000: flags |= FLAG_S
                if result > 0xFFFF: flags |= FLAG_C
                self.hl = result & 0xFFFF
                self.f = flags
                cycles = 15
            elif ed_op == 0x7A:  # ADC HL,SP
                carry = 1 if self.get_flag(FLAG_C) else 0
                result = self.hl + self.sp + carry
                flags = 0
                if (result & 0xFFFF) == 0: flags |= FLAG_Z
                if result & 0x8000: flags |= FLAG_S
                if result > 0xFFFF: flags |= FLAG_C
                self.hl = result & 0xFFFF
                self.f = flags
                cycles = 15

            else:
                # Unimplemented ED instruction - NOP
                cycles = 8

        # FD prefix (IY)
        elif opcode == 0xFD:
            cycles = self.exec_dd_fd(self.iy, False)
            # Update IY if modified
            if hasattr(self, '_dd_fd_result'):
                self.iy = self._dd_fd_result
                del self._dd_fd_result

        self.cycles += cycles
        return cycles

    def run(self, max_cycles: int = 1000000):
        """Run until halted or max_cycles reached"""
        total = 0
        while self.running and not self.halted and total < max_cycles:
            total += self.step()
        return total


if __name__ == "__main__":
    # Simple test
    bus = Bus()
    cpu = Z80(bus)

    # Test program: LD A,42; LD B,A; HALT
    program = bytes([0x3E, 42, 0x47, 0x76])
    bus.load(0, program)

    cpu.reset()
    cpu.pc = 0
    cpu.run()

    print(f"A = {cpu.a} (expected 42)")
    print(f"B = {cpu.b} (expected 42)")
    print(f"Halted = {cpu.halted}")
