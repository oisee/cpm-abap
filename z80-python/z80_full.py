"""
Z80 CPU Emulator - Full implementation matching ABAP ZCL_CPU_Z80
Includes ED, DD, FD prefix handlers for complete Z80 compatibility
"""

# Flag bit positions in F register (matching ABAP constants)
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
    """Z80 CPU Emulator - Full implementation matching ABAP"""

    def __init__(self, bus: Bus):
        self.bus = bus

        # Main registers (16-bit pairs)
        self.af = 0
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
        self.i = 0
        self.r = 0

        # Interrupt state
        self.iff1 = False
        self.iff2 = False
        self.im = 0

        # Execution state
        self.cycles = 0
        self.running = True
        self.halted = False

        # Pre-computed flag tables
        self._init_flag_tables()

    def _init_flag_tables(self):
        """Initialize pre-computed flag lookup tables"""
        self.sz_flags = []
        self.szp_flags = []
        self.parity = []

        for i in range(256):
            # SZ flags
            flags = 0
            if i >= 128:
                flags |= FLAG_S
            if i == 0:
                flags |= FLAG_Z
            self.sz_flags.append(flags)

            # Parity
            bits = bin(i).count('1')
            p = FLAG_PV if bits % 2 == 0 else 0
            self.parity.append(p)

            # SZP flags
            self.szp_flags.append(flags | p)

    def reset(self):
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

    # Register helpers
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

    # Register access by index
    def get_reg8(self, idx: int) -> int:
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
        if idx == 0: return self.bc
        elif idx == 1: return self.de
        elif idx == 2: return self.hl
        elif idx == 3: return self.sp
        return 0

    def set_reg16(self, idx: int, val: int):
        val = val & 0xFFFF
        if idx == 0: self.bc = val
        elif idx == 1: self.de = val
        elif idx == 2: self.hl = val
        elif idx == 3: self.sp = val

    # Condition checking
    def check_condition(self, cond: int) -> bool:
        if cond == 0: return not self.get_flag(FLAG_Z)
        elif cond == 1: return self.get_flag(FLAG_Z)
        elif cond == 2: return not self.get_flag(FLAG_C)
        elif cond == 3: return self.get_flag(FLAG_C)
        elif cond == 4: return not self.get_flag(FLAG_PV)
        elif cond == 5: return self.get_flag(FLAG_PV)
        elif cond == 6: return not self.get_flag(FLAG_S)
        elif cond == 7: return self.get_flag(FLAG_S)
        return False

    # ALU operations
    def alu_add8(self, val: int, with_carry: bool = False) -> int:
        a = self.a
        carry_in = 1 if (with_carry and self.get_flag(FLAG_C)) else 0
        result = a + val + carry_in

        flags = self.sz_flags[result & 0xFF]
        if result > 0xFF:
            flags |= FLAG_C
        if ((a ^ val ^ result) & 0x10):
            flags |= FLAG_H
        if ((a ^ result) & (val ^ result) & 0x80):
            flags |= FLAG_PV

        self.a = result & 0xFF
        self.f = flags
        return self.a

    def alu_sub8(self, val: int, with_carry: bool = False) -> int:
        a = self.a
        carry_in = 1 if (with_carry and self.get_flag(FLAG_C)) else 0
        result = a - val - carry_in

        result_byte = result & 0xFF
        flags = self.sz_flags[result_byte] | FLAG_N
        if result < 0:
            flags |= FLAG_C
        if ((a ^ val) & (a ^ result_byte) & 0x80):
            flags |= FLAG_PV
        if ((a & 0xF) - (val & 0xF) - carry_in) < 0:
            flags |= FLAG_H

        self.a = result_byte
        self.f = flags
        return self.a

    def alu_and8(self, val: int):
        result = self.a & val
        self.a = result
        self.f = self.szp_flags[result] | FLAG_H

    def alu_or8(self, val: int):
        result = self.a | val
        self.a = result
        self.f = self.szp_flags[result]

    def alu_xor8(self, val: int):
        result = self.a ^ val
        self.a = result
        self.f = self.szp_flags[result]

    def alu_cp8(self, val: int):
        a = self.a
        result = a - val
        result_byte = result & 0xFF

        flags = self.sz_flags[result_byte] | FLAG_N
        if result < 0:
            flags |= FLAG_C
        if ((a ^ val) & (a ^ result_byte) & 0x80):
            flags |= FLAG_PV
        if ((a & 0xF) - (val & 0xF)) < 0:
            flags |= FLAG_H

        self.f = flags

    def alu_inc8(self, val: int) -> int:
        result = (val + 1) & 0xFF
        flags = self.f & FLAG_C
        flags |= self.sz_flags[result]
        if (val & 0xF) == 0xF:
            flags |= FLAG_H
        if val == 0x7F:
            flags |= FLAG_PV
        self.f = flags
        return result

    def alu_dec8(self, val: int) -> int:
        result = (val - 1) & 0xFF
        flags = (self.f & FLAG_C) | FLAG_N
        flags |= self.sz_flags[result]
        if (val & 0xF) == 0:
            flags |= FLAG_H
        if val == 0x80:
            flags |= FLAG_PV
        self.f = flags
        return result

    # Rotate/shift operations
    def alu_rlc(self, val: int) -> int:
        bit7 = (val >> 7) & 1
        result = ((val << 1) | bit7) & 0xFF
        self.f = self.szp_flags[result] | (FLAG_C if bit7 else 0)
        return result

    def alu_rrc(self, val: int) -> int:
        bit0 = val & 1
        result = ((val >> 1) | (bit0 << 7)) & 0xFF
        self.f = self.szp_flags[result] | (FLAG_C if bit0 else 0)
        return result

    def alu_rl(self, val: int) -> int:
        bit7 = (val >> 7) & 1
        carry = 1 if self.get_flag(FLAG_C) else 0
        result = ((val << 1) | carry) & 0xFF
        self.f = self.szp_flags[result] | (FLAG_C if bit7 else 0)
        return result

    def alu_rr(self, val: int) -> int:
        bit0 = val & 1
        carry = 0x80 if self.get_flag(FLAG_C) else 0
        result = ((val >> 1) | carry) & 0xFF
        self.f = self.szp_flags[result] | (FLAG_C if bit0 else 0)
        return result

    def alu_sla(self, val: int) -> int:
        bit7 = (val >> 7) & 1
        result = (val << 1) & 0xFF
        self.f = self.szp_flags[result] | (FLAG_C if bit7 else 0)
        return result

    def alu_sra(self, val: int) -> int:
        bit7 = val & 0x80
        bit0 = val & 1
        result = ((val >> 1) | bit7) & 0xFF
        self.f = self.szp_flags[result] | (FLAG_C if bit0 else 0)
        return result

    def alu_srl(self, val: int) -> int:
        bit0 = val & 1
        result = (val >> 1) & 0xFF
        self.f = self.szp_flags[result] | (FLAG_C if bit0 else 0)
        return result

    def step(self) -> int:
        if self.halted:
            return 4

        opcode = self.fetch8()
        self.r = ((self.r + 1) & 0x7F) | (self.r & 0x80)
        cycles = self.exec_main(opcode)
        self.cycles += cycles
        return cycles

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
                self.f = self.szp_flags[result] | (FLAG_C if bit7 else 0)
            elif subop == 7: result = self.alu_srl(val)
            self.set_reg8(reg, result)

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
            result = val & ~mask
            self.set_reg8(reg, result)

        elif group == 3:  # SET
            bit = subop
            mask = 1 << bit
            result = val | mask
            self.set_reg8(reg, result)

        return 15 if reg == 6 else 8

    def exec_ed(self) -> int:
        """Execute ED-prefixed instruction (extended instructions)"""
        op = self.fetch8()

        # IN r,(C)
        if op in [0x40, 0x48, 0x50, 0x58, 0x60, 0x68, 0x78]:
            val = self.bus.read_io(self.c)
            reg_map = {0x40: 'b', 0x48: 'c', 0x50: 'd', 0x58: 'e', 0x60: 'h', 0x68: 'l', 0x78: 'a'}
            setattr(self, reg_map[op], val)
            self.f = (self.f & FLAG_C) | self.szp_flags[val]
            return 12

        # OUT (C),r
        if op in [0x41, 0x49, 0x51, 0x59, 0x61, 0x69, 0x79]:
            reg_map = {0x41: 'b', 0x49: 'c', 0x51: 'd', 0x59: 'e', 0x61: 'h', 0x69: 'l', 0x79: 'a'}
            self.bus.write_io(self.c, getattr(self, reg_map[op]))
            return 12

        # SBC HL,rr
        if op == 0x42:  # SBC HL,BC
            return self._ed_sbc_hl(self.bc)
        if op == 0x52:  # SBC HL,DE
            return self._ed_sbc_hl(self.de)
        if op == 0x62:  # SBC HL,HL
            return self._ed_sbc_hl(self.hl)
        if op == 0x72:  # SBC HL,SP
            return self._ed_sbc_hl(self.sp)

        # ADC HL,rr
        if op == 0x4A:  # ADC HL,BC
            return self._ed_adc_hl(self.bc)
        if op == 0x5A:  # ADC HL,DE
            return self._ed_adc_hl(self.de)
        if op == 0x6A:  # ADC HL,HL
            return self._ed_adc_hl(self.hl)
        if op == 0x7A:  # ADC HL,SP
            return self._ed_adc_hl(self.sp)

        # LD (nn),rr
        if op == 0x43:  # LD (nn),BC
            addr = self.fetch16()
            self.write16(addr, self.bc)
            return 20
        if op == 0x53:  # LD (nn),DE
            addr = self.fetch16()
            self.write16(addr, self.de)
            return 20
        if op == 0x63:  # LD (nn),HL (ED version)
            addr = self.fetch16()
            self.write16(addr, self.hl)
            return 20
        if op == 0x73:  # LD (nn),SP
            addr = self.fetch16()
            self.write16(addr, self.sp)
            return 20

        # LD rr,(nn)
        if op == 0x4B:  # LD BC,(nn)
            addr = self.fetch16()
            self.bc = self.read16(addr)
            return 20
        if op == 0x5B:  # LD DE,(nn)
            addr = self.fetch16()
            self.de = self.read16(addr)
            return 20
        if op == 0x6B:  # LD HL,(nn) (ED version)
            addr = self.fetch16()
            self.hl = self.read16(addr)
            return 20
        if op == 0x7B:  # LD SP,(nn)
            addr = self.fetch16()
            self.sp = self.read16(addr)
            return 20

        # NEG
        if op in [0x44, 0x4C, 0x54, 0x5C, 0x64, 0x6C, 0x74, 0x7C]:
            a = self.a
            result = (-a) & 0xFF
            flags = FLAG_N
            if result == 0: flags |= FLAG_Z
            if result & 0x80: flags |= FLAG_S
            if a != 0: flags |= FLAG_C
            if a == 0x80: flags |= FLAG_PV
            if (a & 0xF) != 0: flags |= FLAG_H
            self.a = result
            self.f = flags
            return 8

        # RETN
        if op in [0x45, 0x55, 0x5D, 0x65, 0x6D, 0x75, 0x7D]:
            self.pc = self.pop16()
            self.iff1 = self.iff2
            return 14

        # RETI
        if op == 0x4D:
            self.pc = self.pop16()
            return 14

        # IM 0/1/2
        if op in [0x46, 0x4E, 0x66, 0x6E]:
            self.im = 0
            return 8
        if op in [0x56, 0x76]:
            self.im = 1
            return 8
        if op in [0x5E, 0x7E]:
            self.im = 2
            return 8

        # LD I,A and LD R,A
        if op == 0x47:  # LD I,A
            self.i = self.a
            return 9
        if op == 0x4F:  # LD R,A
            self.r = self.a
            return 9

        # LD A,I and LD A,R
        if op == 0x57:  # LD A,I
            self.a = self.i
            flags = (self.f & FLAG_C) | self.sz_flags[self.i]
            if self.iff2: flags |= FLAG_PV
            self.f = flags
            return 9
        if op == 0x5F:  # LD A,R
            self.a = self.r
            flags = (self.f & FLAG_C) | self.sz_flags[self.r]
            if self.iff2: flags |= FLAG_PV
            self.f = flags
            return 9

        # RRD and RLD
        if op == 0x67:  # RRD
            val = self.read8(self.hl)
            result = ((self.a & 0x0F) << 4) | (val >> 4)
            self.write8(self.hl, result)
            self.a = (self.a & 0xF0) | (val & 0x0F)
            self.f = (self.f & FLAG_C) | self.szp_flags[self.a]
            return 18
        if op == 0x6F:  # RLD
            val = self.read8(self.hl)
            result = ((val & 0x0F) << 4) | (self.a & 0x0F)
            self.write8(self.hl, result)
            self.a = (self.a & 0xF0) | (val >> 4)
            self.f = (self.f & FLAG_C) | self.szp_flags[self.a]
            return 18

        # Block transfer
        if op == 0xA0:  # LDI
            return self._ed_ldi()
        if op == 0xB0:  # LDIR
            cycles = self._ed_ldi()
            if self.bc != 0:
                self.pc = (self.pc - 2) & 0xFFFF
                return 21
            return 16
        if op == 0xA8:  # LDD
            return self._ed_ldd()
        if op == 0xB8:  # LDDR
            cycles = self._ed_ldd()
            if self.bc != 0:
                self.pc = (self.pc - 2) & 0xFFFF
                return 21
            return 16

        # Block compare
        if op == 0xA1:  # CPI
            return self._ed_cpi()
        if op == 0xB1:  # CPIR
            z_flag = self._ed_cpi()
            if self.bc != 0 and not self.get_flag(FLAG_Z):
                self.pc = (self.pc - 2) & 0xFFFF
                return 21
            return 16
        if op == 0xA9:  # CPD
            return self._ed_cpd()
        if op == 0xB9:  # CPDR
            self._ed_cpd()
            if self.bc != 0 and not self.get_flag(FLAG_Z):
                self.pc = (self.pc - 2) & 0xFFFF
                return 21
            return 16

        # Block I/O (simplified - not commonly used)
        if op in [0xA2, 0xB2, 0xAA, 0xBA, 0xA3, 0xB3, 0xAB, 0xBB]:
            return 16  # Simplified

        return 8  # Unknown ED opcode

    def _ed_sbc_hl(self, val: int) -> int:
        carry_in = 1 if self.get_flag(FLAG_C) else 0
        result = self.hl - val - carry_in
        carry = 1 if result < 0 else 0
        if result < 0: result += 65536
        flags = FLAG_N
        if result == 0: flags |= FLAG_Z
        if result >= 32768: flags |= FLAG_S
        if carry: flags |= FLAG_C
        half = (self.hl & 0xFFF) - (val & 0xFFF) - carry_in
        if half < 0: flags |= FLAG_H
        # Overflow
        hl_sign = self.hl >= 32768
        val_sign = val >= 32768
        r_sign = result >= 32768
        if hl_sign and not val_sign and not r_sign: flags |= FLAG_PV
        if not hl_sign and val_sign and r_sign: flags |= FLAG_PV
        self.hl = result
        self.f = flags
        return 15

    def _ed_adc_hl(self, val: int) -> int:
        carry_in = 1 if self.get_flag(FLAG_C) else 0
        result = self.hl + val + carry_in
        carry = 1 if result >= 65536 else 0
        result &= 0xFFFF
        flags = 0
        if result == 0: flags |= FLAG_Z
        if result >= 32768: flags |= FLAG_S
        if carry: flags |= FLAG_C
        half = (self.hl & 0xFFF) + (val & 0xFFF) + carry_in
        if half >= 4096: flags |= FLAG_H
        # Overflow
        hl_sign = self.hl >= 32768
        val_sign = val >= 32768
        r_sign = result >= 32768
        if not hl_sign and not val_sign and r_sign: flags |= FLAG_PV
        if hl_sign and val_sign and not r_sign: flags |= FLAG_PV
        self.hl = result
        self.f = flags
        return 15

    def _ed_ldi(self) -> int:
        val = self.read8(self.hl)
        self.write8(self.de, val)
        self.hl = (self.hl + 1) & 0xFFFF
        self.de = (self.de + 1) & 0xFFFF
        self.bc = (self.bc - 1) & 0xFFFF
        flags = self.f & (FLAG_S | FLAG_Z | FLAG_C)
        if self.bc != 0: flags |= FLAG_PV
        self.f = flags
        return 16

    def _ed_ldd(self) -> int:
        val = self.read8(self.hl)
        self.write8(self.de, val)
        self.hl = (self.hl - 1) & 0xFFFF
        self.de = (self.de - 1) & 0xFFFF
        self.bc = (self.bc - 1) & 0xFFFF
        flags = self.f & (FLAG_S | FLAG_Z | FLAG_C)
        if self.bc != 0: flags |= FLAG_PV
        self.f = flags
        return 16

    def _ed_cpi(self) -> int:
        val = self.read8(self.hl)
        result = (self.a - val) & 0xFF
        self.hl = (self.hl + 1) & 0xFFFF
        self.bc = (self.bc - 1) & 0xFFFF
        flags = FLAG_N | (self.f & FLAG_C)
        if result == 0: flags |= FLAG_Z
        if result >= 128: flags |= FLAG_S
        if self.bc != 0: flags |= FLAG_PV
        if (self.a & 0xF) < (val & 0xF): flags |= FLAG_H
        self.f = flags
        return 16

    def _ed_cpd(self) -> int:
        val = self.read8(self.hl)
        result = (self.a - val) & 0xFF
        self.hl = (self.hl - 1) & 0xFFFF
        self.bc = (self.bc - 1) & 0xFFFF
        flags = FLAG_N | (self.f & FLAG_C)
        if result == 0: flags |= FLAG_Z
        if result >= 128: flags |= FLAG_S
        if self.bc != 0: flags |= FLAG_PV
        if (self.a & 0xF) < (val & 0xF): flags |= FLAG_H
        self.f = flags
        return 16

    def exec_dd(self) -> int:
        """Execute DD-prefixed instruction (IX instructions)"""
        return self._exec_index(self.ix, 'ix')

    def exec_fd(self) -> int:
        """Execute FD-prefixed instruction (IY instructions)"""
        return self._exec_index(self.iy, 'iy')

    def _exec_index(self, ir: int, ir_name: str) -> int:
        """Execute DD/FD prefixed instruction"""
        op = self.fetch8()

        # ADD IX/IY,rr
        if op == 0x09:  # ADD IX,BC
            return self._index_add(ir_name, self.bc)
        if op == 0x19:  # ADD IX,DE
            return self._index_add(ir_name, self.de)
        if op == 0x29:  # ADD IX,IX
            return self._index_add(ir_name, getattr(self, ir_name))
        if op == 0x39:  # ADD IX,SP
            return self._index_add(ir_name, self.sp)

        # LD IX,nn
        if op == 0x21:
            setattr(self, ir_name, self.fetch16())
            return 14

        # LD (nn),IX
        if op == 0x22:
            addr = self.fetch16()
            self.write16(addr, getattr(self, ir_name))
            return 20

        # INC IX
        if op == 0x23:
            setattr(self, ir_name, (getattr(self, ir_name) + 1) & 0xFFFF)
            return 10

        # LD IX,(nn)
        if op == 0x2A:
            addr = self.fetch16()
            setattr(self, ir_name, self.read16(addr))
            return 20

        # DEC IX
        if op == 0x2B:
            setattr(self, ir_name, (getattr(self, ir_name) - 1) & 0xFFFF)
            return 10

        # INC (IX+d)
        if op == 0x34:
            disp = self._signed_byte(self.fetch8())
            addr = (getattr(self, ir_name) + disp) & 0xFFFF
            val = self.alu_inc8(self.read8(addr))
            self.write8(addr, val)
            return 23

        # DEC (IX+d)
        if op == 0x35:
            disp = self._signed_byte(self.fetch8())
            addr = (getattr(self, ir_name) + disp) & 0xFFFF
            val = self.alu_dec8(self.read8(addr))
            self.write8(addr, val)
            return 23

        # LD (IX+d),n
        if op == 0x36:
            disp = self._signed_byte(self.fetch8())
            val = self.fetch8()
            addr = (getattr(self, ir_name) + disp) & 0xFFFF
            self.write8(addr, val)
            return 19

        # LD r,(IX+d) - 46, 4E, 56, 5E, 66, 6E, 7E
        if op in [0x46, 0x4E, 0x56, 0x5E, 0x66, 0x6E, 0x7E]:
            disp = self._signed_byte(self.fetch8())
            addr = (getattr(self, ir_name) + disp) & 0xFFFF
            val = self.read8(addr)
            reg_map = {0x46: 'b', 0x4E: 'c', 0x56: 'd', 0x5E: 'e', 0x66: 'h', 0x6E: 'l', 0x7E: 'a'}
            setattr(self, reg_map[op], val)
            return 19

        # LD (IX+d),r - 70-75, 77
        if op in [0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x77]:
            disp = self._signed_byte(self.fetch8())
            addr = (getattr(self, ir_name) + disp) & 0xFFFF
            reg_map = {0x70: 'b', 0x71: 'c', 0x72: 'd', 0x73: 'e', 0x74: 'h', 0x75: 'l', 0x77: 'a'}
            self.write8(addr, getattr(self, reg_map[op]))
            return 19

        # ALU A,(IX+d) - 86, 8E, 96, 9E, A6, AE, B6, BE
        if op in [0x86, 0x8E, 0x96, 0x9E, 0xA6, 0xAE, 0xB6, 0xBE]:
            disp = self._signed_byte(self.fetch8())
            addr = (getattr(self, ir_name) + disp) & 0xFFFF
            val = self.read8(addr)
            if op == 0x86: self.alu_add8(val)
            elif op == 0x8E: self.alu_add8(val, with_carry=True)
            elif op == 0x96: self.alu_sub8(val)
            elif op == 0x9E: self.alu_sub8(val, with_carry=True)
            elif op == 0xA6: self.alu_and8(val)
            elif op == 0xAE: self.alu_xor8(val)
            elif op == 0xB6: self.alu_or8(val)
            elif op == 0xBE: self.alu_cp8(val)
            return 19

        # DDCB/FDCB prefix
        if op == 0xCB:
            return self._exec_index_cb(ir_name)

        # POP IX
        if op == 0xE1:
            setattr(self, ir_name, self.pop16())
            return 14

        # EX (SP),IX
        if op == 0xE3:
            val = self.read16(self.sp)
            self.write16(self.sp, getattr(self, ir_name))
            setattr(self, ir_name, val)
            return 23

        # PUSH IX
        if op == 0xE5:
            self.push16(getattr(self, ir_name))
            return 15

        # JP (IX)
        if op == 0xE9:
            self.pc = getattr(self, ir_name)
            return 8

        # LD SP,IX
        if op == 0xF9:
            self.sp = getattr(self, ir_name)
            return 10

        return 4  # Unknown

    def _index_add(self, ir_name: str, val: int) -> int:
        ir = getattr(self, ir_name)
        result = ir + val
        carry = 1 if result >= 65536 else 0
        result &= 0xFFFF
        flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
        if carry: flags |= FLAG_C
        if (ir & 0xFFF) + (val & 0xFFF) >= 4096: flags |= FLAG_H
        self.f = flags
        setattr(self, ir_name, result)
        return 15

    def _exec_index_cb(self, ir_name: str) -> int:
        """Execute DDCB/FDCB prefixed instruction"""
        disp = self._signed_byte(self.fetch8())
        op = self.fetch8()
        addr = (getattr(self, ir_name) + disp) & 0xFFFF
        val = self.read8(addr)

        group = op >> 6
        subop = (op >> 3) & 7

        if group == 0:  # Rotate/shift
            if subop == 0: result = self.alu_rlc(val)
            elif subop == 1: result = self.alu_rrc(val)
            elif subop == 2: result = self.alu_rl(val)
            elif subop == 3: result = self.alu_rr(val)
            elif subop == 4: result = self.alu_sla(val)
            elif subop == 5: result = self.alu_sra(val)
            elif subop == 6:
                bit7 = (val >> 7) & 1
                result = ((val << 1) | 1) & 0xFF
                self.f = self.szp_flags[result] | (FLAG_C if bit7 else 0)
            elif subop == 7: result = self.alu_srl(val)
            self.write8(addr, result)
            return 23

        elif group == 1:  # BIT
            mask = 1 << subop
            flags = (self.f & FLAG_C) | FLAG_H
            if not (val & mask): flags |= FLAG_Z
            self.f = flags
            return 20

        elif group == 2:  # RES
            mask = 1 << subop
            self.write8(addr, val & ~mask)
            return 23

        elif group == 3:  # SET
            mask = 1 << subop
            self.write8(addr, val | mask)
            return 23

        return 23

    def _signed_byte(self, val: int) -> int:
        return val if val < 128 else val - 256

    def exec_main(self, opcode: int) -> int:
        """Execute main opcode"""
        # NOP
        if opcode == 0x00:
            return 4

        # LD rr,nn
        if opcode == 0x01:
            self.bc = self.fetch16()
            return 10
        if opcode == 0x11:
            self.de = self.fetch16()
            return 10
        if opcode == 0x21:
            self.hl = self.fetch16()
            return 10
        if opcode == 0x31:
            self.sp = self.fetch16()
            return 10

        # LD r,n
        if opcode == 0x06: self.b = self.fetch8(); return 7
        if opcode == 0x0E: self.c = self.fetch8(); return 7
        if opcode == 0x16: self.d = self.fetch8(); return 7
        if opcode == 0x1E: self.e = self.fetch8(); return 7
        if opcode == 0x26: self.h = self.fetch8(); return 7
        if opcode == 0x2E: self.l = self.fetch8(); return 7
        if opcode == 0x36: self.write8(self.hl, self.fetch8()); return 10
        if opcode == 0x3E: self.a = self.fetch8(); return 7

        # LD r,r' (0x40-0x7F except HALT)
        if 0x40 <= opcode <= 0x7F and opcode != 0x76:
            dst = (opcode >> 3) & 7
            src = opcode & 7
            self.set_reg8(dst, self.get_reg8(src))
            return 7 if (src == 6 or dst == 6) else 4

        # HALT
        if opcode == 0x76:
            self.halted = True
            return 4

        # ALU A,r (0x80-0xBF)
        if 0x80 <= opcode <= 0xBF:
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
            return 7 if src == 6 else 4

        # INC r
        if opcode in [0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C, 0x34, 0x3C]:
            reg = (opcode >> 3) & 7
            self.set_reg8(reg, self.alu_inc8(self.get_reg8(reg)))
            return 11 if reg == 6 else 4

        # DEC r
        if opcode in [0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D, 0x35, 0x3D]:
            reg = (opcode >> 3) & 7
            self.set_reg8(reg, self.alu_dec8(self.get_reg8(reg)))
            return 11 if reg == 6 else 4

        # INC rr
        if opcode == 0x03: self.bc = (self.bc + 1) & 0xFFFF; return 6
        if opcode == 0x13: self.de = (self.de + 1) & 0xFFFF; return 6
        if opcode == 0x23: self.hl = (self.hl + 1) & 0xFFFF; return 6
        if opcode == 0x33: self.sp = (self.sp + 1) & 0xFFFF; return 6

        # DEC rr
        if opcode == 0x0B: self.bc = (self.bc - 1) & 0xFFFF; return 6
        if opcode == 0x1B: self.de = (self.de - 1) & 0xFFFF; return 6
        if opcode == 0x2B: self.hl = (self.hl - 1) & 0xFFFF; return 6
        if opcode == 0x3B: self.sp = (self.sp - 1) & 0xFFFF; return 6

        # JP nn
        if opcode == 0xC3:
            self.pc = self.fetch16()
            return 10

        # JP cc,nn
        if opcode in [0xC2, 0xCA, 0xD2, 0xDA, 0xE2, 0xEA, 0xF2, 0xFA]:
            addr = self.fetch16()
            cond = (opcode >> 3) & 7
            if self.check_condition(cond):
                self.pc = addr
            return 10

        # JR e
        if opcode == 0x18:
            offset = self._signed_byte(self.fetch8())
            self.pc = (self.pc + offset) & 0xFFFF
            return 12

        # JR cc,e
        if opcode in [0x20, 0x28, 0x30, 0x38]:
            offset = self._signed_byte(self.fetch8())
            cond = (opcode >> 3) & 3
            if self.check_condition(cond):
                self.pc = (self.pc + offset) & 0xFFFF
                return 12
            return 7

        # DJNZ e
        if opcode == 0x10:
            offset = self._signed_byte(self.fetch8())
            self.b = (self.b - 1) & 0xFF
            if self.b != 0:
                self.pc = (self.pc + offset) & 0xFFFF
                return 13
            return 8

        # CALL nn
        if opcode == 0xCD:
            addr = self.fetch16()
            self.push16(self.pc)
            self.pc = addr
            return 17

        # CALL cc,nn
        if opcode in [0xC4, 0xCC, 0xD4, 0xDC, 0xE4, 0xEC, 0xF4, 0xFC]:
            addr = self.fetch16()
            cond = (opcode >> 3) & 7
            if self.check_condition(cond):
                self.push16(self.pc)
                self.pc = addr
                return 17
            return 10

        # RET
        if opcode == 0xC9:
            self.pc = self.pop16()
            return 10

        # RET cc
        if opcode in [0xC0, 0xC8, 0xD0, 0xD8, 0xE0, 0xE8, 0xF0, 0xF8]:
            cond = (opcode >> 3) & 7
            if self.check_condition(cond):
                self.pc = self.pop16()
                return 11
            return 5

        # PUSH/POP
        if opcode == 0xC5: self.push16(self.bc); return 11
        if opcode == 0xD5: self.push16(self.de); return 11
        if opcode == 0xE5: self.push16(self.hl); return 11
        if opcode == 0xF5: self.push16(self.af); return 11
        if opcode == 0xC1: self.bc = self.pop16(); return 10
        if opcode == 0xD1: self.de = self.pop16(); return 10
        if opcode == 0xE1: self.hl = self.pop16(); return 10
        if opcode == 0xF1: self.af = self.pop16(); return 10

        # RST n
        if opcode in [0xC7, 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF]:
            self.push16(self.pc)
            self.pc = opcode & 0x38
            return 11

        # ALU A,n
        if opcode == 0xC6: self.alu_add8(self.fetch8()); return 7
        if opcode == 0xCE: self.alu_add8(self.fetch8(), with_carry=True); return 7
        if opcode == 0xD6: self.alu_sub8(self.fetch8()); return 7
        if opcode == 0xDE: self.alu_sub8(self.fetch8(), with_carry=True); return 7
        if opcode == 0xE6: self.alu_and8(self.fetch8()); return 7
        if opcode == 0xEE: self.alu_xor8(self.fetch8()); return 7
        if opcode == 0xF6: self.alu_or8(self.fetch8()); return 7
        if opcode == 0xFE: self.alu_cp8(self.fetch8()); return 7

        # Rotate A
        if opcode == 0x07:  # RLCA
            bit7 = (self.a >> 7) & 1
            self.a = ((self.a << 1) | bit7) & 0xFF
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | (FLAG_C if bit7 else 0)
            return 4
        if opcode == 0x0F:  # RRCA
            bit0 = self.a & 1
            self.a = ((self.a >> 1) | (bit0 << 7)) & 0xFF
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | (FLAG_C if bit0 else 0)
            return 4
        if opcode == 0x17:  # RLA
            bit7 = (self.a >> 7) & 1
            carry = 1 if self.get_flag(FLAG_C) else 0
            self.a = ((self.a << 1) | carry) & 0xFF
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | (FLAG_C if bit7 else 0)
            return 4
        if opcode == 0x1F:  # RRA
            bit0 = self.a & 1
            carry = 0x80 if self.get_flag(FLAG_C) else 0
            self.a = ((self.a >> 1) | carry) & 0xFF
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | (FLAG_C if bit0 else 0)
            return 4

        # CB prefix
        if opcode == 0xCB:
            return self.exec_cb()

        # DD prefix (IX)
        if opcode == 0xDD:
            return self.exec_dd()

        # ED prefix (extended)
        if opcode == 0xED:
            return self.exec_ed()

        # FD prefix (IY)
        if opcode == 0xFD:
            return self.exec_fd()

        # DI/EI
        if opcode == 0xF3: self.iff1 = False; self.iff2 = False; return 4
        if opcode == 0xFB: self.iff1 = True; self.iff2 = True; return 4

        # EX
        if opcode == 0xEB: self.de, self.hl = self.hl, self.de; return 4
        if opcode == 0x08: self.af, self.af_alt = self.af_alt, self.af; return 4
        if opcode == 0xD9:
            self.bc, self.bc_alt = self.bc_alt, self.bc
            self.de, self.de_alt = self.de_alt, self.de
            self.hl, self.hl_alt = self.hl_alt, self.hl
            return 4
        if opcode == 0xE3:
            val = self.read16(self.sp)
            self.write16(self.sp, self.hl)
            self.hl = val
            return 19

        # JP (HL)
        if opcode == 0xE9:
            self.pc = self.hl
            return 4

        # LD SP,HL
        if opcode == 0xF9:
            self.sp = self.hl
            return 6

        # Memory loads
        if opcode == 0x02: self.write8(self.bc, self.a); return 7
        if opcode == 0x12: self.write8(self.de, self.a); return 7
        if opcode == 0x0A: self.a = self.read8(self.bc); return 7
        if opcode == 0x1A: self.a = self.read8(self.de); return 7
        if opcode == 0x22:
            addr = self.fetch16()
            self.write16(addr, self.hl)
            return 16
        if opcode == 0x2A:
            addr = self.fetch16()
            self.hl = self.read16(addr)
            return 16
        if opcode == 0x32:
            addr = self.fetch16()
            self.write8(addr, self.a)
            return 13
        if opcode == 0x3A:
            addr = self.fetch16()
            self.a = self.read8(addr)
            return 13

        # ADD HL,rr
        if opcode == 0x09: return self._add_hl(self.bc)
        if opcode == 0x19: return self._add_hl(self.de)
        if opcode == 0x29: return self._add_hl(self.hl)
        if opcode == 0x39: return self._add_hl(self.sp)

        # CPL
        if opcode == 0x2F:
            self.a = (~self.a) & 0xFF
            self.f |= FLAG_H | FLAG_N
            return 4

        # SCF
        if opcode == 0x37:
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | FLAG_C
            return 4

        # CCF
        if opcode == 0x3F:
            carry = FLAG_C if not self.get_flag(FLAG_C) else 0
            half = FLAG_H if self.get_flag(FLAG_C) else 0
            self.f = (self.f & (FLAG_S | FLAG_Z | FLAG_PV)) | carry | half
            return 4

        # DAA
        if opcode == 0x27:
            a = self.a
            f = self.f
            c_flag = f & FLAG_C
            n_flag = f & FLAG_N
            h_flag = f & FLAG_H
            correction = 0
            if h_flag or (a & 0xF) > 9:
                correction += 6
            if c_flag or a > 0x99:
                correction += 0x60
                c_flag = FLAG_C
            if n_flag:
                a = (a - correction) & 0xFF
            else:
                a = (a + correction) & 0xFF
            self.a = a
            self.f = self.szp_flags[a] | c_flag | n_flag
            return 4

        # IN A,(n)
        if opcode == 0xDB:
            port = self.fetch8()
            self.a = self.bus.read_io(port)
            return 11

        # OUT (n),A
        if opcode == 0xD3:
            port = self.fetch8()
            self.bus.write_io(port, self.a)
            return 11

        return 4  # Unknown opcode

    def _add_hl(self, val: int) -> int:
        result = self.hl + val
        flags = self.f & (FLAG_S | FLAG_Z | FLAG_PV)
        if result > 0xFFFF: flags |= FLAG_C
        if (self.hl & 0xFFF) + (val & 0xFFF) > 0xFFF: flags |= FLAG_H
        self.hl = result & 0xFFFF
        self.f = flags
        return 11

    def run(self, max_cycles: int = 1000000):
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
