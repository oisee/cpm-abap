"""
Z80 CB Prefix Handler - Python equivalent of ZCL_CPU_Z80_PREFIX_CB
Handles bit manipulation, rotate, and shift instructions
"""

from zif_cpu_z80_core import ZifCpuZ80Core, ZifCpuZ80Prefix, FLAG_C, FLAG_Z, FLAG_H


class ZclCpuZ80PrefixCb(ZifCpuZ80Prefix):
    """CB prefix handler for bit operations"""

    def __init__(self, cpu: ZifCpuZ80Core):
        self.cpu = cpu
        self._init_flag_tables()

    def _init_flag_tables(self):
        """Initialize pre-computed SZP flag table"""
        self.szp_flags = []
        for i in range(256):
            flags = 0
            if i >= 128:
                flags |= ZifCpuZ80Core.c_flag_s
            if i == 0:
                flags |= ZifCpuZ80Core.c_flag_z
            # Parity
            bits = bin(i).count('1')
            if bits % 2 == 0:
                flags |= ZifCpuZ80Core.c_flag_pv
            self.szp_flags.append(flags)

    def _get_reg8(self, idx: int) -> int:
        """Get 8-bit register by index: B=0, C=1, D=2, E=3, H=4, L=5, (HL)=6, A=7"""
        if idx == 0: return self.cpu.get_b()
        elif idx == 1: return self.cpu.get_c()
        elif idx == 2: return self.cpu.get_d()
        elif idx == 3: return self.cpu.get_e()
        elif idx == 4: return self.cpu.get_h()
        elif idx == 5: return self.cpu.get_l()
        elif idx == 6: return self.cpu.read_mem(self.cpu.get_hl())
        elif idx == 7: return self.cpu.get_a()
        return 0

    def _set_reg8(self, idx: int, val: int):
        """Set 8-bit register by index"""
        val = val & 0xFF
        if idx == 0: self.cpu.set_b(val)
        elif idx == 1: self.cpu.set_c(val)
        elif idx == 2: self.cpu.set_d(val)
        elif idx == 3: self.cpu.set_e(val)
        elif idx == 4: self.cpu.set_h(val)
        elif idx == 5: self.cpu.set_l(val)
        elif idx == 6: self.cpu.write_mem(self.cpu.get_hl(), val)
        elif idx == 7: self.cpu.set_a(val)

    def execute(self) -> int:
        op = self.cpu.fetch_byte()
        reg = op & 7
        val = self._get_reg8(reg)

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
            self._set_reg8(reg, result)

        elif group == 1:  # BIT
            bit = subop
            mask = 1 << bit
            flags = self.cpu.get_f() & FLAG_C  # Preserve C
            flags |= FLAG_H
            if not (val & mask):
                flags |= FLAG_Z
            self.cpu.set_f(flags)

        elif group == 2:  # RES
            bit = subop
            mask = 1 << bit
            result = val & ~mask
            self._set_reg8(reg, result)

        elif group == 3:  # SET
            bit = subop
            mask = 1 << bit
            result = val | mask
            self._set_reg8(reg, result)

        return 15 if reg == 6 else 8
