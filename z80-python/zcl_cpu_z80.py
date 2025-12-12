"""
Z80 CPU Emulator - Python equivalent of ZCL_CPU_Z80
Main CPU implementation with prefix handler delegation
Matches ABAP architecture for easy synchronization
"""

from zif_cpu_z80_core import ZifCpuZ80Core, FLAG_C, FLAG_N, FLAG_PV, FLAG_H, FLAG_Z, FLAG_S
from zif_cpu_z80_bus import ZifCpuZ80Bus
from zcl_cpu_z80_prefix_cb import ZclCpuZ80PrefixCb
from zcl_cpu_z80_prefix_dd import ZclCpuZ80PrefixDd
from zcl_cpu_z80_prefix_ed import ZclCpuZ80PrefixEd
from zcl_cpu_z80_prefix_fd import ZclCpuZ80PrefixFd


class ZclCpuZ80(ZifCpuZ80Core):
    """
    Z80 CPU Emulator
    Implements the Zilog Z80 processor with main opcodes + prefix handlers
    Uses bus interface for memory-mapped I/O and port I/O
    Design based on proven 6502 emulator architecture (hybrid approach)
    """

    def __init__(self, bus: ZifCpuZ80Bus):
        self.mo_bus = bus

        # Main registers (16-bit pairs stored as integers: high*256+low)
        self.mv_af = 0
        self.mv_bc = 0
        self.mv_de = 0
        self.mv_hl = 0

        # Alternate register set
        self.mv_af_alt = 0
        self.mv_bc_alt = 0
        self.mv_de_alt = 0
        self.mv_hl_alt = 0

        # Index registers
        self.mv_ix = 0
        self.mv_iy = 0

        # Other registers
        self.mv_sp = 0xFFFF
        self.mv_pc = 0
        self.mv_i = 0
        self.mv_r = 0

        # Interrupt state
        self.mv_iff1 = False
        self.mv_iff2 = False
        self.mv_im = 0

        # Execution state
        self.mv_cycles = 0
        self.mv_running = True
        self.mv_halted = False

        # Initialize lookup tables
        self._init_tables()

        # Initialize prefix handlers
        self.mo_cb_handler = ZclCpuZ80PrefixCb(self)
        self.mo_dd_handler = ZclCpuZ80PrefixDd(self)
        self.mo_ed_handler = ZclCpuZ80PrefixEd(self)
        self.mo_fd_handler = ZclCpuZ80PrefixFd(self)

        self.reset()

    def _init_tables(self):
        """Initialize pre-computed flag lookup tables"""
        self.mt_sz_flags = []
        self.mt_szp_flags = []
        self.mt_parity = []

        for i in range(256):
            # SZ flags
            flags = 0
            if i >= 128:
                flags |= FLAG_S
            if i == 0:
                flags |= FLAG_Z
            self.mt_sz_flags.append(flags)

            # Parity
            bits = bin(i).count('1')
            p = FLAG_PV if bits % 2 == 0 else 0
            self.mt_parity.append(p)

            # SZP flags
            self.mt_szp_flags.append(flags | p)

    def reset(self):
        """Reset CPU to initial state"""
        self.mv_af = 0xFFFF
        self.mv_bc = 0
        self.mv_de = 0
        self.mv_hl = 0
        self.mv_af_alt = 0
        self.mv_bc_alt = 0
        self.mv_de_alt = 0
        self.mv_hl_alt = 0
        self.mv_ix = 0
        self.mv_iy = 0
        self.mv_sp = 0xFFFF
        self.mv_pc = 0
        self.mv_i = 0
        self.mv_r = 0
        self.mv_iff1 = False
        self.mv_iff2 = False
        self.mv_im = 0
        self.mv_cycles = 0
        self.mv_running = True
        self.mv_halted = False

    # Helper methods
    def _get_high(self, pair: int) -> int:
        return (pair >> 8) & 0xFF

    def _get_low(self, pair: int) -> int:
        return pair & 0xFF

    def _set_high(self, pair: int, val: int) -> int:
        return (pair & 0xFF) | ((val & 0xFF) << 8)

    def _set_low(self, pair: int, val: int) -> int:
        return (pair & 0xFF00) | (val & 0xFF)

    # Memory access
    def read_mem(self, addr: int) -> int:
        return self.mo_bus.read_mem(addr & 0xFFFF)

    def write_mem(self, addr: int, val: int):
        self.mo_bus.write_mem(addr & 0xFFFF, val & 0xFF)

    def read_io(self, port: int) -> int:
        return self.mo_bus.read_io(port & 0xFF)

    def write_io(self, port: int, val: int):
        self.mo_bus.write_io(port & 0xFF, val & 0xFF)

    def _read16(self, addr: int) -> int:
        lo = self.read_mem(addr)
        hi = self.read_mem(addr + 1)
        return (hi << 8) | lo

    def _write16(self, addr: int, val: int):
        self.write_mem(addr, val & 0xFF)
        self.write_mem(addr + 1, (val >> 8) & 0xFF)

    # Fetch with PC increment
    def fetch_byte(self) -> int:
        val = self.read_mem(self.mv_pc)
        self.mv_pc = (self.mv_pc + 1) & 0xFFFF
        return val

    def fetch_word(self) -> int:
        lo = self.fetch_byte()
        hi = self.fetch_byte()
        return (hi << 8) | lo

    # Stack operations
    def push(self, val: int):
        self.mv_sp = (self.mv_sp - 2) & 0xFFFF
        self._write16(self.mv_sp, val)

    def pop(self) -> int:
        val = self._read16(self.mv_sp)
        self.mv_sp = (self.mv_sp + 2) & 0xFFFF
        return val

    # Register access implementation
    def get_af(self) -> int: return self.mv_af
    def set_af(self, val: int): self.mv_af = val & 0xFFFF
    def get_bc(self) -> int: return self.mv_bc
    def set_bc(self, val: int): self.mv_bc = val & 0xFFFF
    def get_de(self) -> int: return self.mv_de
    def set_de(self, val: int): self.mv_de = val & 0xFFFF
    def get_hl(self) -> int: return self.mv_hl
    def set_hl(self, val: int): self.mv_hl = val & 0xFFFF
    def get_ix(self) -> int: return self.mv_ix
    def set_ix(self, val: int): self.mv_ix = val & 0xFFFF
    def get_iy(self) -> int: return self.mv_iy
    def set_iy(self, val: int): self.mv_iy = val & 0xFFFF
    def get_sp(self) -> int: return self.mv_sp
    def set_sp(self, val: int): self.mv_sp = val & 0xFFFF
    def get_pc(self) -> int: return self.mv_pc
    def set_pc(self, val: int): self.mv_pc = val & 0xFFFF

    def get_a(self) -> int: return self._get_high(self.mv_af)
    def set_a(self, val: int): self.mv_af = self._set_high(self.mv_af, val)
    def get_f(self) -> int: return self._get_low(self.mv_af)
    def set_f(self, val: int): self.mv_af = self._set_low(self.mv_af, val)
    def get_b(self) -> int: return self._get_high(self.mv_bc)
    def set_b(self, val: int): self.mv_bc = self._set_high(self.mv_bc, val)
    def get_c(self) -> int: return self._get_low(self.mv_bc)
    def set_c(self, val: int): self.mv_bc = self._set_low(self.mv_bc, val)
    def get_d(self) -> int: return self._get_high(self.mv_de)
    def set_d(self, val: int): self.mv_de = self._set_high(self.mv_de, val)
    def get_e(self) -> int: return self._get_low(self.mv_de)
    def set_e(self, val: int): self.mv_de = self._set_low(self.mv_de, val)
    def get_h(self) -> int: return self._get_high(self.mv_hl)
    def set_h(self, val: int): self.mv_hl = self._set_high(self.mv_hl, val)
    def get_l(self) -> int: return self._get_low(self.mv_hl)
    def set_l(self, val: int): self.mv_hl = self._set_low(self.mv_hl, val)

    def get_i(self) -> int: return self.mv_i
    def set_i(self, val: int): self.mv_i = val & 0xFF
    def get_r(self) -> int: return self.mv_r
    def set_r(self, val: int): self.mv_r = val & 0xFF

    def get_iff1(self) -> bool: return self.mv_iff1
    def set_iff1(self, val: bool): self.mv_iff1 = val
    def get_iff2(self) -> bool: return self.mv_iff2
    def set_iff2(self, val: bool): self.mv_iff2 = val
    def get_im(self) -> int: return self.mv_im
    def set_im(self, val: int): self.mv_im = val % 3

    # Flag helpers
    def get_flag(self, flag: int) -> bool:
        return (self.get_f() & flag) != 0

    def set_flag(self, flag: int, val: bool):
        f = self.get_f()
        if val:
            f = f | flag
        else:
            f = f & ~flag
        self.set_f(f)

    def add_cycles(self, cycles: int):
        self.mv_cycles += cycles

    # Register access by index
    def _get_reg8(self, idx: int) -> int:
        if idx == 0: return self.get_b()
        elif idx == 1: return self.get_c()
        elif idx == 2: return self.get_d()
        elif idx == 3: return self.get_e()
        elif idx == 4: return self.get_h()
        elif idx == 5: return self.get_l()
        elif idx == 6: return self.read_mem(self.mv_hl)
        elif idx == 7: return self.get_a()
        return 0

    def _set_reg8(self, idx: int, val: int):
        val = val & 0xFF
        if idx == 0: self.set_b(val)
        elif idx == 1: self.set_c(val)
        elif idx == 2: self.set_d(val)
        elif idx == 3: self.set_e(val)
        elif idx == 4: self.set_h(val)
        elif idx == 5: self.set_l(val)
        elif idx == 6: self.write_mem(self.mv_hl, val)
        elif idx == 7: self.set_a(val)

    def _check_condition(self, cond: int) -> bool:
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
    def alu_add8(self, val: int, carry: int = 0) -> int:
        a = self.get_a()
        carry_in = 1 if (carry and self.get_flag(FLAG_C)) else 0
        result = a + val + carry_in

        flags = self.mt_sz_flags[result & 0xFF]
        if result > 0xFF:
            flags |= FLAG_C
        if ((a ^ val ^ result) & 0x10):
            flags |= FLAG_H
        if ((a ^ result) & (val ^ result) & 0x80):
            flags |= FLAG_PV

        self.set_a(result & 0xFF)
        self.set_f(flags)
        return result & 0xFF

    def alu_sub8(self, val: int, carry: int = 0) -> int:
        a = self.get_a()
        carry_in = 1 if (carry and self.get_flag(FLAG_C)) else 0
        result = a - val - carry_in

        result_byte = result & 0xFF
        flags = self.mt_sz_flags[result_byte] | FLAG_N
        if result < 0:
            flags |= FLAG_C
        if ((a ^ val) & (a ^ result_byte) & 0x80):
            flags |= FLAG_PV
        if ((a & 0xF) - (val & 0xF) - carry_in) < 0:
            flags |= FLAG_H

        self.set_a(result_byte)
        self.set_f(flags)
        return result_byte

    def alu_and8(self, val: int) -> int:
        result = self.get_a() & val
        self.set_a(result)
        self.set_f(self.mt_szp_flags[result] | FLAG_H)
        return result

    def alu_or8(self, val: int) -> int:
        result = self.get_a() | val
        self.set_a(result)
        self.set_f(self.mt_szp_flags[result])
        return result

    def alu_xor8(self, val: int) -> int:
        result = self.get_a() ^ val
        self.set_a(result)
        self.set_f(self.mt_szp_flags[result])
        return result

    def alu_cp8(self, val: int):
        a = self.get_a()
        result = a - val
        result_byte = result & 0xFF

        flags = self.mt_sz_flags[result_byte] | FLAG_N
        if result < 0:
            flags |= FLAG_C
        if ((a ^ val) & (a ^ result_byte) & 0x80):
            flags |= FLAG_PV
        if ((a & 0xF) - (val & 0xF)) < 0:
            flags |= FLAG_H

        self.set_f(flags)

    def alu_inc8(self, val: int) -> int:
        result = (val + 1) & 0xFF
        flags = self.get_f() & FLAG_C
        flags |= self.mt_sz_flags[result]
        if (val & 0xF) == 0xF:
            flags |= FLAG_H
        if val == 0x7F:
            flags |= FLAG_PV
        self.set_f(flags)
        return result

    def alu_dec8(self, val: int) -> int:
        result = (val - 1) & 0xFF
        flags = (self.get_f() & FLAG_C) | FLAG_N
        flags |= self.mt_sz_flags[result]
        if (val & 0xF) == 0:
            flags |= FLAG_H
        if val == 0x80:
            flags |= FLAG_PV
        self.set_f(flags)
        return result

    # Rotate/shift operations
    def alu_rlc(self, val: int) -> int:
        bit7 = (val >> 7) & 1
        result = ((val << 1) | bit7) & 0xFF
        self.set_f(self.mt_szp_flags[result] | (FLAG_C if bit7 else 0))
        return result

    def alu_rrc(self, val: int) -> int:
        bit0 = val & 1
        result = ((val >> 1) | (bit0 << 7)) & 0xFF
        self.set_f(self.mt_szp_flags[result] | (FLAG_C if bit0 else 0))
        return result

    def alu_rl(self, val: int) -> int:
        bit7 = (val >> 7) & 1
        carry = 1 if self.get_flag(FLAG_C) else 0
        result = ((val << 1) | carry) & 0xFF
        self.set_f(self.mt_szp_flags[result] | (FLAG_C if bit7 else 0))
        return result

    def alu_rr(self, val: int) -> int:
        bit0 = val & 1
        carry = 0x80 if self.get_flag(FLAG_C) else 0
        result = ((val >> 1) | carry) & 0xFF
        self.set_f(self.mt_szp_flags[result] | (FLAG_C if bit0 else 0))
        return result

    def alu_sla(self, val: int) -> int:
        bit7 = (val >> 7) & 1
        result = (val << 1) & 0xFF
        self.set_f(self.mt_szp_flags[result] | (FLAG_C if bit7 else 0))
        return result

    def alu_sra(self, val: int) -> int:
        bit7 = val & 0x80
        bit0 = val & 1
        result = ((val >> 1) | bit7) & 0xFF
        self.set_f(self.mt_szp_flags[result] | (FLAG_C if bit0 else 0))
        return result

    def alu_srl(self, val: int) -> int:
        bit0 = val & 1
        result = (val >> 1) & 0xFF
        self.set_f(self.mt_szp_flags[result] | (FLAG_C if bit0 else 0))
        return result

    def step(self) -> int:
        """Execute one instruction, return cycles consumed"""
        if not self.mv_running:
            return 0

        if self.mv_halted:
            self.mv_cycles += 4
            return 4

        opcode = self.fetch_byte()
        self.mv_r = ((self.mv_r + 1) & 0x7F) | (self.mv_r & 0x80)
        cycles = self._exec_main(opcode)
        self.mv_cycles += cycles
        return cycles

    def _exec_main(self, opcode: int) -> int:
        """Execute main opcode, return cycles"""
        # NOP
        if opcode == 0x00:
            return 4

        # LD rr,nn
        if opcode == 0x01: self.mv_bc = self.fetch_word(); return 10
        if opcode == 0x11: self.mv_de = self.fetch_word(); return 10
        if opcode == 0x21: self.mv_hl = self.fetch_word(); return 10
        if opcode == 0x31: self.mv_sp = self.fetch_word(); return 10

        # LD r,n
        if opcode == 0x06: self.set_b(self.fetch_byte()); return 7
        if opcode == 0x0E: self.set_c(self.fetch_byte()); return 7
        if opcode == 0x16: self.set_d(self.fetch_byte()); return 7
        if opcode == 0x1E: self.set_e(self.fetch_byte()); return 7
        if opcode == 0x26: self.set_h(self.fetch_byte()); return 7
        if opcode == 0x2E: self.set_l(self.fetch_byte()); return 7
        if opcode == 0x36: self.write_mem(self.mv_hl, self.fetch_byte()); return 10
        if opcode == 0x3E: self.set_a(self.fetch_byte()); return 7

        # LD r,r' (0x40-0x7F except HALT)
        if 0x40 <= opcode <= 0x7F and opcode != 0x76:
            dst = (opcode >> 3) & 7
            src = opcode & 7
            self._set_reg8(dst, self._get_reg8(src))
            return 7 if (src == 6 or dst == 6) else 4

        # HALT
        if opcode == 0x76:
            self.mv_halted = True
            return 4

        # ALU A,r (0x80-0xBF)
        if 0x80 <= opcode <= 0xBF:
            src = opcode & 7
            val = self._get_reg8(src)
            op = (opcode >> 3) & 7
            if op == 0: self.alu_add8(val)
            elif op == 1: self.alu_add8(val, 1)
            elif op == 2: self.alu_sub8(val)
            elif op == 3: self.alu_sub8(val, 1)
            elif op == 4: self.alu_and8(val)
            elif op == 5: self.alu_xor8(val)
            elif op == 6: self.alu_or8(val)
            elif op == 7: self.alu_cp8(val)
            return 7 if src == 6 else 4

        # INC r
        if opcode in [0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C, 0x34, 0x3C]:
            reg = (opcode >> 3) & 7
            self._set_reg8(reg, self.alu_inc8(self._get_reg8(reg)))
            return 11 if reg == 6 else 4

        # DEC r
        if opcode in [0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D, 0x35, 0x3D]:
            reg = (opcode >> 3) & 7
            self._set_reg8(reg, self.alu_dec8(self._get_reg8(reg)))
            return 11 if reg == 6 else 4

        # INC/DEC rr
        if opcode == 0x03: self.mv_bc = (self.mv_bc + 1) & 0xFFFF; return 6
        if opcode == 0x13: self.mv_de = (self.mv_de + 1) & 0xFFFF; return 6
        if opcode == 0x23: self.mv_hl = (self.mv_hl + 1) & 0xFFFF; return 6
        if opcode == 0x33: self.mv_sp = (self.mv_sp + 1) & 0xFFFF; return 6
        if opcode == 0x0B: self.mv_bc = (self.mv_bc - 1) & 0xFFFF; return 6
        if opcode == 0x1B: self.mv_de = (self.mv_de - 1) & 0xFFFF; return 6
        if opcode == 0x2B: self.mv_hl = (self.mv_hl - 1) & 0xFFFF; return 6
        if opcode == 0x3B: self.mv_sp = (self.mv_sp - 1) & 0xFFFF; return 6

        # DJNZ
        if opcode == 0x10:
            offset = self.fetch_byte()
            if offset >= 128: offset -= 256
            self.set_b((self.get_b() - 1) & 0xFF)
            if self.get_b() != 0:
                self.mv_pc = (self.mv_pc + offset) & 0xFFFF
                return 13
            return 8

        # JP/JR/CALL/RET
        if opcode == 0xC3: self.mv_pc = self.fetch_word(); return 10
        if opcode == 0x18:
            offset = self.fetch_byte()
            if offset >= 128: offset -= 256
            self.mv_pc = (self.mv_pc + offset) & 0xFFFF
            return 12

        # JP cc,nn
        if opcode in [0xC2, 0xCA, 0xD2, 0xDA, 0xE2, 0xEA, 0xF2, 0xFA]:
            addr = self.fetch_word()
            if self._check_condition((opcode >> 3) & 7):
                self.mv_pc = addr
            return 10

        # JR cc,e
        if opcode in [0x20, 0x28, 0x30, 0x38]:
            offset = self.fetch_byte()
            if offset >= 128: offset -= 256
            if self._check_condition((opcode >> 3) & 3):
                self.mv_pc = (self.mv_pc + offset) & 0xFFFF
                return 12
            return 7

        # CALL nn
        if opcode == 0xCD:
            addr = self.fetch_word()
            self.push(self.mv_pc)
            self.mv_pc = addr
            return 17

        # CALL cc,nn
        if opcode in [0xC4, 0xCC, 0xD4, 0xDC, 0xE4, 0xEC, 0xF4, 0xFC]:
            addr = self.fetch_word()
            if self._check_condition((opcode >> 3) & 7):
                self.push(self.mv_pc)
                self.mv_pc = addr
                return 17
            return 10

        # RET
        if opcode == 0xC9: self.mv_pc = self.pop(); return 10

        # RET cc
        if opcode in [0xC0, 0xC8, 0xD0, 0xD8, 0xE0, 0xE8, 0xF0, 0xF8]:
            if self._check_condition((opcode >> 3) & 7):
                self.mv_pc = self.pop()
                return 11
            return 5

        # PUSH/POP
        if opcode == 0xC5: self.push(self.mv_bc); return 11
        if opcode == 0xD5: self.push(self.mv_de); return 11
        if opcode == 0xE5: self.push(self.mv_hl); return 11
        if opcode == 0xF5: self.push(self.mv_af); return 11
        if opcode == 0xC1: self.mv_bc = self.pop(); return 10
        if opcode == 0xD1: self.mv_de = self.pop(); return 10
        if opcode == 0xE1: self.mv_hl = self.pop(); return 10
        if opcode == 0xF1: self.mv_af = self.pop(); return 10

        # RST n
        if opcode in [0xC7, 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF]:
            self.push(self.mv_pc)
            self.mv_pc = opcode & 0x38
            return 11

        # ALU A,n
        if opcode == 0xC6: self.alu_add8(self.fetch_byte()); return 7
        if opcode == 0xCE: self.alu_add8(self.fetch_byte(), 1); return 7
        if opcode == 0xD6: self.alu_sub8(self.fetch_byte()); return 7
        if opcode == 0xDE: self.alu_sub8(self.fetch_byte(), 1); return 7
        if opcode == 0xE6: self.alu_and8(self.fetch_byte()); return 7
        if opcode == 0xEE: self.alu_xor8(self.fetch_byte()); return 7
        if opcode == 0xF6: self.alu_or8(self.fetch_byte()); return 7
        if opcode == 0xFE: self.alu_cp8(self.fetch_byte()); return 7

        # Rotate A
        if opcode == 0x07:  # RLCA
            bit7 = (self.get_a() >> 7) & 1
            self.set_a(((self.get_a() << 1) | bit7) & 0xFF)
            f = self.get_f() & (FLAG_S | FLAG_Z | FLAG_PV)
            if bit7: f |= FLAG_C
            self.set_f(f)
            return 4
        if opcode == 0x0F:  # RRCA
            bit0 = self.get_a() & 1
            self.set_a(((self.get_a() >> 1) | (bit0 << 7)) & 0xFF)
            f = self.get_f() & (FLAG_S | FLAG_Z | FLAG_PV)
            if bit0: f |= FLAG_C
            self.set_f(f)
            return 4
        if opcode == 0x17:  # RLA
            bit7 = (self.get_a() >> 7) & 1
            carry = 1 if self.get_flag(FLAG_C) else 0
            self.set_a(((self.get_a() << 1) | carry) & 0xFF)
            f = self.get_f() & (FLAG_S | FLAG_Z | FLAG_PV)
            if bit7: f |= FLAG_C
            self.set_f(f)
            return 4
        if opcode == 0x1F:  # RRA
            bit0 = self.get_a() & 1
            carry = 0x80 if self.get_flag(FLAG_C) else 0
            self.set_a(((self.get_a() >> 1) | carry) & 0xFF)
            f = self.get_f() & (FLAG_S | FLAG_Z | FLAG_PV)
            if bit0: f |= FLAG_C
            self.set_f(f)
            return 4

        # Prefix opcodes - delegate to handlers
        if opcode == 0xCB: return self.mo_cb_handler.execute()
        if opcode == 0xDD: return self.mo_dd_handler.execute()
        if opcode == 0xED: return self.mo_ed_handler.execute()
        if opcode == 0xFD: return self.mo_fd_handler.execute()

        # DI/EI
        if opcode == 0xF3: self.mv_iff1 = False; self.mv_iff2 = False; return 4
        if opcode == 0xFB: self.mv_iff1 = True; self.mv_iff2 = True; return 4

        # EX
        if opcode == 0xEB: self.mv_de, self.mv_hl = self.mv_hl, self.mv_de; return 4
        if opcode == 0x08: self.mv_af, self.mv_af_alt = self.mv_af_alt, self.mv_af; return 4
        if opcode == 0xD9:
            self.mv_bc, self.mv_bc_alt = self.mv_bc_alt, self.mv_bc
            self.mv_de, self.mv_de_alt = self.mv_de_alt, self.mv_de
            self.mv_hl, self.mv_hl_alt = self.mv_hl_alt, self.mv_hl
            return 4
        if opcode == 0xE3:
            val = self._read16(self.mv_sp)
            self._write16(self.mv_sp, self.mv_hl)
            self.mv_hl = val
            return 19

        # JP (HL), LD SP,HL
        if opcode == 0xE9: self.mv_pc = self.mv_hl; return 4
        if opcode == 0xF9: self.mv_sp = self.mv_hl; return 6

        # Memory loads
        if opcode == 0x02: self.write_mem(self.mv_bc, self.get_a()); return 7
        if opcode == 0x12: self.write_mem(self.mv_de, self.get_a()); return 7
        if opcode == 0x0A: self.set_a(self.read_mem(self.mv_bc)); return 7
        if opcode == 0x1A: self.set_a(self.read_mem(self.mv_de)); return 7
        if opcode == 0x22: self._write16(self.fetch_word(), self.mv_hl); return 16
        if opcode == 0x2A: self.mv_hl = self._read16(self.fetch_word()); return 16
        if opcode == 0x32: self.write_mem(self.fetch_word(), self.get_a()); return 13
        if opcode == 0x3A: self.set_a(self.read_mem(self.fetch_word())); return 13

        # ADD HL,rr
        if opcode == 0x09: return self._add_hl(self.mv_bc)
        if opcode == 0x19: return self._add_hl(self.mv_de)
        if opcode == 0x29: return self._add_hl(self.mv_hl)
        if opcode == 0x39: return self._add_hl(self.mv_sp)

        # CPL, SCF, CCF, DAA
        if opcode == 0x2F:
            self.set_a((~self.get_a()) & 0xFF)
            self.set_f(self.get_f() | FLAG_H | FLAG_N)
            return 4
        if opcode == 0x37:
            self.set_f((self.get_f() & (FLAG_S | FLAG_Z | FLAG_PV)) | FLAG_C)
            return 4
        if opcode == 0x3F:
            f = self.get_f()
            carry = FLAG_C if not (f & FLAG_C) else 0
            half = FLAG_H if (f & FLAG_C) else 0
            self.set_f((f & (FLAG_S | FLAG_Z | FLAG_PV)) | carry | half)
            return 4
        if opcode == 0x27:
            return self._daa()

        # IN/OUT
        if opcode == 0xDB: self.set_a(self.mo_bus.read_io(self.fetch_byte())); return 11
        if opcode == 0xD3: self.mo_bus.write_io(self.fetch_byte(), self.get_a()); return 11

        return 4  # Unknown opcode

    def _add_hl(self, val: int) -> int:
        result = self.mv_hl + val
        f = self.get_f() & (FLAG_S | FLAG_Z | FLAG_PV)
        if result > 0xFFFF: f |= FLAG_C
        if (self.mv_hl & 0xFFF) + (val & 0xFFF) > 0xFFF: f |= FLAG_H
        self.mv_hl = result & 0xFFFF
        self.set_f(f)
        return 11

    def _daa(self) -> int:
        a = self.get_a()
        f = self.get_f()
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
        self.set_a(a)
        self.set_f(self.mt_szp_flags[a] | c_flag | n_flag)
        return 4

    def run(self, max_cycles: int = 1000000):
        """Run until halted or max_cycles reached"""
        total = 0
        while self.mv_running and not self.mv_halted and total < max_cycles:
            total += self.step()
        return total

    def is_halted(self) -> bool:
        return self.mv_halted

    def provide_input(self, text: str):
        self.mo_bus.provide_input(text)
        self.mv_halted = False


if __name__ == "__main__":
    from zcl_cpu_z80_bus_simple import ZclCpuZ80BusSimple

    # Simple test
    bus = ZclCpuZ80BusSimple()
    cpu = ZclCpuZ80(bus)

    # Test program: LD A,42; LD B,A; HALT
    program = bytes([0x3E, 42, 0x47, 0x76])
    bus.load(0, program)

    cpu.reset()
    cpu.mv_pc = 0
    cpu.run()

    print(f"A = {cpu.get_a()} (expected 42)")
    print(f"B = {cpu.get_b()} (expected 42)")
    print(f"Halted = {cpu.is_halted()}")
