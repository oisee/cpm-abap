"""
Z80 DD Prefix Handler - Python equivalent of ZCL_CPU_Z80_PREFIX_DD
Handles IX-indexed instructions
"""

from zif_cpu_z80_core import ZifCpuZ80Core, ZifCpuZ80Prefix
from zif_cpu_z80_core import FLAG_C, FLAG_H, FLAG_Z, FLAG_S, FLAG_PV


class ZclCpuZ80PrefixDd(ZifCpuZ80Prefix):
    """DD prefix handler for IX instructions"""

    def __init__(self, cpu: ZifCpuZ80Core):
        self.cpu = cpu
        self._init_flag_tables()

    def _init_flag_tables(self):
        """Initialize pre-computed SZP flag table"""
        self.szp_flags = []
        self.sz_flags = []
        for i in range(256):
            flags = 0
            if i >= 128:
                flags |= FLAG_S
            if i == 0:
                flags |= FLAG_Z
            self.sz_flags.append(flags)
            bits = bin(i).count('1')
            if bits % 2 == 0:
                flags |= FLAG_PV
            self.szp_flags.append(flags)

    def _get_index_reg(self) -> int:
        return self.cpu.get_ix()

    def _set_index_reg(self, val: int):
        self.cpu.set_ix(val)

    def _signed_byte(self, val: int) -> int:
        return val if val < 128 else val - 256

    def _get_reg8(self, idx: int, disp: int = 0) -> int:
        """Get 8-bit register, with IX+d for index 6"""
        if idx == 0: return self.cpu.get_b()
        elif idx == 1: return self.cpu.get_c()
        elif idx == 2: return self.cpu.get_d()
        elif idx == 3: return self.cpu.get_e()
        elif idx == 4: return (self._get_index_reg() >> 8) & 0xFF  # IXH
        elif idx == 5: return self._get_index_reg() & 0xFF  # IXL
        elif idx == 6:
            addr = (self._get_index_reg() + disp) & 0xFFFF
            return self.cpu.read_mem(addr)
        elif idx == 7: return self.cpu.get_a()
        return 0

    def _set_reg8(self, idx: int, val: int, disp: int = 0):
        """Set 8-bit register, with IX+d for index 6"""
        val = val & 0xFF
        if idx == 0: self.cpu.set_b(val)
        elif idx == 1: self.cpu.set_c(val)
        elif idx == 2: self.cpu.set_d(val)
        elif idx == 3: self.cpu.set_e(val)
        elif idx == 4:
            ix = self._get_index_reg()
            self._set_index_reg((val << 8) | (ix & 0xFF))
        elif idx == 5:
            ix = self._get_index_reg()
            self._set_index_reg((ix & 0xFF00) | val)
        elif idx == 6:
            addr = (self._get_index_reg() + disp) & 0xFFFF
            self.cpu.write_mem(addr, val)
        elif idx == 7: self.cpu.set_a(val)

    def execute(self) -> int:
        op = self.cpu.fetch_byte()
        ix = self._get_index_reg()

        # ADD IX,rr
        if op == 0x09:  # ADD IX,BC
            return self._add_ix(self.cpu.get_bc())
        if op == 0x19:  # ADD IX,DE
            return self._add_ix(self.cpu.get_de())
        if op == 0x29:  # ADD IX,IX
            return self._add_ix(ix)
        if op == 0x39:  # ADD IX,SP
            return self._add_ix(self.cpu.get_sp())

        # LD IX,nn
        if op == 0x21:
            self._set_index_reg(self.cpu.fetch_word())
            return 14

        # LD (nn),IX
        if op == 0x22:
            addr = self.cpu.fetch_word()
            self.cpu.write_mem(addr, ix & 0xFF)
            self.cpu.write_mem((addr + 1) & 0xFFFF, (ix >> 8) & 0xFF)
            return 20

        # INC IX
        if op == 0x23:
            self._set_index_reg((ix + 1) & 0xFFFF)
            return 10

        # LD IX,(nn)
        if op == 0x2A:
            addr = self.cpu.fetch_word()
            lo = self.cpu.read_mem(addr)
            hi = self.cpu.read_mem((addr + 1) & 0xFFFF)
            self._set_index_reg((hi << 8) | lo)
            return 20

        # DEC IX
        if op == 0x2B:
            self._set_index_reg((ix - 1) & 0xFFFF)
            return 10

        # INC (IX+d)
        if op == 0x34:
            disp = self._signed_byte(self.cpu.fetch_byte())
            val = self._get_reg8(6, disp)
            result = self.cpu.alu_inc8(val)
            self._set_reg8(6, result, disp)
            return 23

        # DEC (IX+d)
        if op == 0x35:
            disp = self._signed_byte(self.cpu.fetch_byte())
            val = self._get_reg8(6, disp)
            result = self.cpu.alu_dec8(val)
            self._set_reg8(6, result, disp)
            return 23

        # LD (IX+d),n
        if op == 0x36:
            disp = self._signed_byte(self.cpu.fetch_byte())
            val = self.cpu.fetch_byte()
            self._set_reg8(6, val, disp)
            return 19

        # LD (IX+d),r - 70-75, 77
        if op in [0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x77]:
            disp = self._signed_byte(self.cpu.fetch_byte())
            reg_map = {0x70: 'get_b', 0x71: 'get_c', 0x72: 'get_d', 0x73: 'get_e',
                       0x74: 'get_h', 0x75: 'get_l', 0x77: 'get_a'}
            val = getattr(self.cpu, reg_map[op])()
            self._set_reg8(6, val, disp)
            return 19

        # LD r,(IX+d) - 46, 4E, 56, 5E, 66, 6E, 7E
        if op in [0x46, 0x4E, 0x56, 0x5E, 0x66, 0x6E, 0x7E]:
            disp = self._signed_byte(self.cpu.fetch_byte())
            val = self._get_reg8(6, disp)
            reg_map = {0x46: 'set_b', 0x4E: 'set_c', 0x56: 'set_d', 0x5E: 'set_e',
                       0x66: 'set_h', 0x6E: 'set_l', 0x7E: 'set_a'}
            getattr(self.cpu, reg_map[op])(val)
            return 19

        # ALU A,(IX+d) - 86, 8E, 96, 9E, A6, AE, B6, BE
        if op == 0x86:  # ADD A,(IX+d)
            disp = self._signed_byte(self.cpu.fetch_byte())
            self.cpu.alu_add8(self._get_reg8(6, disp))
            return 19
        if op == 0x8E:  # ADC A,(IX+d)
            disp = self._signed_byte(self.cpu.fetch_byte())
            self.cpu.alu_add8(self._get_reg8(6, disp), 1)
            return 19
        if op == 0x96:  # SUB (IX+d)
            disp = self._signed_byte(self.cpu.fetch_byte())
            self.cpu.alu_sub8(self._get_reg8(6, disp))
            return 19
        if op == 0x9E:  # SBC A,(IX+d)
            disp = self._signed_byte(self.cpu.fetch_byte())
            self.cpu.alu_sub8(self._get_reg8(6, disp), 1)
            return 19
        if op == 0xA6:  # AND (IX+d)
            disp = self._signed_byte(self.cpu.fetch_byte())
            self.cpu.alu_and8(self._get_reg8(6, disp))
            return 19
        if op == 0xAE:  # XOR (IX+d)
            disp = self._signed_byte(self.cpu.fetch_byte())
            self.cpu.alu_xor8(self._get_reg8(6, disp))
            return 19
        if op == 0xB6:  # OR (IX+d)
            disp = self._signed_byte(self.cpu.fetch_byte())
            self.cpu.alu_or8(self._get_reg8(6, disp))
            return 19
        if op == 0xBE:  # CP (IX+d)
            disp = self._signed_byte(self.cpu.fetch_byte())
            self.cpu.alu_cp8(self._get_reg8(6, disp))
            return 19

        # DDCB prefix
        if op == 0xCB:
            return self._exec_ddcb()

        # POP IX
        if op == 0xE1:
            self._set_index_reg(self.cpu.pop())
            return 14

        # EX (SP),IX
        if op == 0xE3:
            sp = self.cpu.get_sp()
            lo = self.cpu.read_mem(sp)
            hi = self.cpu.read_mem((sp + 1) & 0xFFFF)
            val = (hi << 8) | lo
            self.cpu.write_mem(sp, ix & 0xFF)
            self.cpu.write_mem((sp + 1) & 0xFFFF, (ix >> 8) & 0xFF)
            self._set_index_reg(val)
            return 23

        # PUSH IX
        if op == 0xE5:
            self.cpu.push(ix)
            return 15

        # JP (IX)
        if op == 0xE9:
            self.cpu.set_pc(ix)
            return 8

        # LD SP,IX
        if op == 0xF9:
            self.cpu.set_sp(ix)
            return 10

        # Unhandled DD opcode
        return 4

    def _add_ix(self, val: int) -> int:
        """ADD IX,rr"""
        ix = self._get_index_reg()
        result = ix + val
        carry = 1 if result >= 65536 else 0
        result &= 0xFFFF
        half = 1 if (ix & 0xFFF) + (val & 0xFFF) >= 4096 else 0

        f = self.cpu.get_f()
        f = f & (FLAG_S | FLAG_Z | FLAG_PV)  # Preserve S, Z, P/V
        if half:
            f |= FLAG_H
        if carry:
            f |= FLAG_C
        self.cpu.set_f(f)
        self._set_index_reg(result)
        return 15

    def _exec_ddcb(self) -> int:
        """Execute DDCB prefixed instruction"""
        disp = self._signed_byte(self.cpu.fetch_byte())
        op = self.cpu.fetch_byte()

        addr = (self._get_index_reg() + disp) & 0xFFFF
        val = self.cpu.read_mem(addr)

        group = op >> 6
        subop = (op >> 3) & 7

        if group == 0:  # Rotate/shift
            if subop == 0:
                result = self.cpu.alu_rlc(val)
            elif subop == 1:
                result = self.cpu.alu_rrc(val)
            elif subop == 2:
                result = self.cpu.alu_rl(val)
            elif subop == 3:
                result = self.cpu.alu_rr(val)
            elif subop == 4:
                result = self.cpu.alu_sla(val)
            elif subop == 5:
                result = self.cpu.alu_sra(val)
            elif subop == 6:  # SLL (undocumented)
                bit7 = (val >> 7) & 1
                result = ((val << 1) | 1) & 0xFF
                flags = self.szp_flags[result]
                if bit7:
                    flags |= FLAG_C
                self.cpu.set_f(flags)
            elif subop == 7:
                result = self.cpu.alu_srl(val)
            self.cpu.write_mem(addr, result)
            return 23

        elif group == 1:  # BIT
            mask = 1 << subop
            flags = self.cpu.get_f() & FLAG_C
            flags |= FLAG_H
            if not (val & mask):
                flags |= FLAG_Z
            self.cpu.set_f(flags)
            return 20

        elif group == 2:  # RES
            mask = 1 << subop
            result = val & ~mask
            self.cpu.write_mem(addr, result)
            return 23

        elif group == 3:  # SET
            mask = 1 << subop
            result = val | mask
            self.cpu.write_mem(addr, result)
            return 23

        return 23
