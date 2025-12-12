"""
Z80 ED Prefix Handler - Python equivalent of ZCL_CPU_Z80_PREFIX_ED
Handles extended instructions: block transfers, I/O, 16-bit arithmetic
"""

from zif_cpu_z80_core import ZifCpuZ80Core, ZifCpuZ80Prefix
from zif_cpu_z80_core import FLAG_C, FLAG_N, FLAG_PV, FLAG_H, FLAG_Z, FLAG_S


class ZclCpuZ80PrefixEd(ZifCpuZ80Prefix):
    """ED prefix handler for extended instructions"""

    def __init__(self, cpu: ZifCpuZ80Core):
        self.cpu = cpu
        self._init_flag_tables()

    def _init_flag_tables(self):
        """Initialize pre-computed SZP flag table"""
        self.szp_flags = []
        for i in range(256):
            flags = 0
            if i >= 128:
                flags |= FLAG_S
            if i == 0:
                flags |= FLAG_Z
            bits = bin(i).count('1')
            if bits % 2 == 0:
                flags |= FLAG_PV
            self.szp_flags.append(flags)

    def execute(self) -> int:
        op = self.cpu.fetch_byte()

        # IN r,(C) - Input from port C to register
        if op == 0x40:  # IN B,(C)
            val = self.cpu.read_io(self.cpu.get_c())
            self.cpu.set_b(val)
            self._set_in_flags(val)
            return 12
        if op == 0x48:  # IN C,(C)
            val = self.cpu.read_io(self.cpu.get_c())
            self.cpu.set_c(val)
            self._set_in_flags(val)
            return 12
        if op == 0x50:  # IN D,(C)
            val = self.cpu.read_io(self.cpu.get_c())
            self.cpu.set_d(val)
            self._set_in_flags(val)
            return 12
        if op == 0x58:  # IN E,(C)
            val = self.cpu.read_io(self.cpu.get_c())
            self.cpu.set_e(val)
            self._set_in_flags(val)
            return 12
        if op == 0x60:  # IN H,(C)
            val = self.cpu.read_io(self.cpu.get_c())
            self.cpu.set_h(val)
            self._set_in_flags(val)
            return 12
        if op == 0x68:  # IN L,(C)
            val = self.cpu.read_io(self.cpu.get_c())
            self.cpu.set_l(val)
            self._set_in_flags(val)
            return 12
        if op == 0x78:  # IN A,(C)
            val = self.cpu.read_io(self.cpu.get_c())
            self.cpu.set_a(val)
            self._set_in_flags(val)
            return 12

        # OUT (C),r - Output register to port C
        if op == 0x41:
            self.cpu.write_io(self.cpu.get_c(), self.cpu.get_b())
            return 12
        if op == 0x49:
            self.cpu.write_io(self.cpu.get_c(), self.cpu.get_c())
            return 12
        if op == 0x51:
            self.cpu.write_io(self.cpu.get_c(), self.cpu.get_d())
            return 12
        if op == 0x59:
            self.cpu.write_io(self.cpu.get_c(), self.cpu.get_e())
            return 12
        if op == 0x61:
            self.cpu.write_io(self.cpu.get_c(), self.cpu.get_h())
            return 12
        if op == 0x69:
            self.cpu.write_io(self.cpu.get_c(), self.cpu.get_l())
            return 12
        if op == 0x79:
            self.cpu.write_io(self.cpu.get_c(), self.cpu.get_a())
            return 12

        # SBC HL,rr - 16-bit subtract with carry
        if op == 0x42:
            return self._sbc_hl(self.cpu.get_bc())
        if op == 0x52:
            return self._sbc_hl(self.cpu.get_de())
        if op == 0x62:
            return self._sbc_hl(self.cpu.get_hl())
        if op == 0x72:
            return self._sbc_hl(self.cpu.get_sp())

        # ADC HL,rr - 16-bit add with carry
        if op == 0x4A:
            return self._adc_hl(self.cpu.get_bc())
        if op == 0x5A:
            return self._adc_hl(self.cpu.get_de())
        if op == 0x6A:
            return self._adc_hl(self.cpu.get_hl())
        if op == 0x7A:
            return self._adc_hl(self.cpu.get_sp())

        # LD (nn),rr
        if op == 0x43:  # LD (nn),BC
            addr = self.cpu.fetch_word()
            self._write16(addr, self.cpu.get_bc())
            return 20
        if op == 0x53:  # LD (nn),DE
            addr = self.cpu.fetch_word()
            self._write16(addr, self.cpu.get_de())
            return 20
        if op == 0x63:  # LD (nn),HL (ED version)
            addr = self.cpu.fetch_word()
            self._write16(addr, self.cpu.get_hl())
            return 20
        if op == 0x73:  # LD (nn),SP
            addr = self.cpu.fetch_word()
            self._write16(addr, self.cpu.get_sp())
            return 20

        # LD rr,(nn)
        if op == 0x4B:  # LD BC,(nn)
            addr = self.cpu.fetch_word()
            self.cpu.set_bc(self._read16(addr))
            return 20
        if op == 0x5B:  # LD DE,(nn)
            addr = self.cpu.fetch_word()
            self.cpu.set_de(self._read16(addr))
            return 20
        if op == 0x6B:  # LD HL,(nn) (ED version)
            addr = self.cpu.fetch_word()
            self.cpu.set_hl(self._read16(addr))
            return 20
        if op == 0x7B:  # LD SP,(nn)
            addr = self.cpu.fetch_word()
            self.cpu.set_sp(self._read16(addr))
            return 20

        # NEG - Negate A
        if op in [0x44, 0x4C, 0x54, 0x5C, 0x64, 0x6C, 0x74, 0x7C]:
            a = self.cpu.get_a()
            result = (-a) & 0xFF
            flags = FLAG_N
            if result == 0:
                flags |= FLAG_Z
            if result >= 128:
                flags |= FLAG_S
            if a != 0:
                flags |= FLAG_C
            if a == 0x80:
                flags |= FLAG_PV
            if (a & 0xF) != 0:
                flags |= FLAG_H
            self.cpu.set_a(result)
            self.cpu.set_f(flags)
            return 8

        # RETN
        if op in [0x45, 0x55, 0x5D, 0x65, 0x6D, 0x75, 0x7D]:
            self.cpu.set_pc(self.cpu.pop())
            self.cpu.set_iff1(self.cpu.get_iff2())
            return 14

        # RETI
        if op == 0x4D:
            self.cpu.set_pc(self.cpu.pop())
            return 14

        # IM 0/1/2 - Set interrupt mode
        if op in [0x46, 0x4E, 0x66, 0x6E]:
            self.cpu.set_im(0)
            return 8
        if op in [0x56, 0x76]:
            self.cpu.set_im(1)
            return 8
        if op in [0x5E, 0x7E]:
            self.cpu.set_im(2)
            return 8

        # LD I,A and LD R,A
        if op == 0x47:  # LD I,A
            self.cpu.set_i(self.cpu.get_a())
            return 9
        if op == 0x4F:  # LD R,A
            self.cpu.set_r(self.cpu.get_a())
            return 9

        # LD A,I and LD A,R
        if op == 0x57:  # LD A,I
            val = self.cpu.get_i()
            self.cpu.set_a(val)
            flags = (self.cpu.get_f() & FLAG_C) | self.szp_flags[val]
            if self.cpu.get_iff2():
                flags |= FLAG_PV
            # Clear PV from szp_flags, set based on IFF2
            flags = flags & ~FLAG_PV
            if self.cpu.get_iff2():
                flags |= FLAG_PV
            self.cpu.set_f(flags)
            return 9
        if op == 0x5F:  # LD A,R
            val = self.cpu.get_r()
            self.cpu.set_a(val)
            flags = (self.cpu.get_f() & FLAG_C)
            if val == 0:
                flags |= FLAG_Z
            if val >= 128:
                flags |= FLAG_S
            if self.cpu.get_iff2():
                flags |= FLAG_PV
            self.cpu.set_f(flags)
            return 9

        # RRD and RLD - Rotate decimal
        if op == 0x67:  # RRD
            a = self.cpu.get_a()
            hl = self.cpu.get_hl()
            val = self.cpu.read_mem(hl)
            result = ((a & 0x0F) << 4) | (val >> 4)
            self.cpu.write_mem(hl, result)
            a = (a & 0xF0) | (val & 0x0F)
            self.cpu.set_a(a)
            flags = (self.cpu.get_f() & FLAG_C) | self.szp_flags[a]
            self.cpu.set_f(flags)
            return 18

        if op == 0x6F:  # RLD
            a = self.cpu.get_a()
            hl = self.cpu.get_hl()
            val = self.cpu.read_mem(hl)
            result = ((val & 0x0F) << 4) | (a & 0x0F)
            self.cpu.write_mem(hl, result)
            a = (a & 0xF0) | (val >> 4)
            self.cpu.set_a(a)
            flags = (self.cpu.get_f() & FLAG_C) | self.szp_flags[a]
            self.cpu.set_f(flags)
            return 18

        # Block transfer instructions
        if op == 0xA0:  # LDI
            return self._ldi()
        if op == 0xB0:  # LDIR
            self._ldi()
            if self.cpu.get_bc() != 0:
                self.cpu.set_pc((self.cpu.get_pc() - 2) & 0xFFFF)
                return 21
            return 16
        if op == 0xA8:  # LDD
            return self._ldd()
        if op == 0xB8:  # LDDR
            self._ldd()
            if self.cpu.get_bc() != 0:
                self.cpu.set_pc((self.cpu.get_pc() - 2) & 0xFFFF)
                return 21
            return 16

        # Block compare instructions
        if op == 0xA1:  # CPI
            return self._cpi()
        if op == 0xB1:  # CPIR
            self._cpi()
            if self.cpu.get_bc() != 0 and not (self.cpu.get_f() & FLAG_Z):
                self.cpu.set_pc((self.cpu.get_pc() - 2) & 0xFFFF)
                return 21
            return 16
        if op == 0xA9:  # CPD
            return self._cpd()
        if op == 0xB9:  # CPDR
            self._cpd()
            if self.cpu.get_bc() != 0 and not (self.cpu.get_f() & FLAG_Z):
                self.cpu.set_pc((self.cpu.get_pc() - 2) & 0xFFFF)
                return 21
            return 16

        # Block I/O instructions
        if op == 0xA2:  # INI
            return self._ini()
        if op == 0xB2:  # INIR
            self._ini()
            if self.cpu.get_b() != 0:
                self.cpu.set_pc((self.cpu.get_pc() - 2) & 0xFFFF)
                return 21
            return 16
        if op == 0xAA:  # IND
            return self._ind()
        if op == 0xBA:  # INDR
            self._ind()
            if self.cpu.get_b() != 0:
                self.cpu.set_pc((self.cpu.get_pc() - 2) & 0xFFFF)
                return 21
            return 16
        if op == 0xA3:  # OUTI
            return self._outi()
        if op == 0xB3:  # OTIR
            self._outi()
            if self.cpu.get_b() != 0:
                self.cpu.set_pc((self.cpu.get_pc() - 2) & 0xFFFF)
                return 21
            return 16
        if op == 0xAB:  # OUTD
            return self._outd()
        if op == 0xBB:  # OTDR
            self._outd()
            if self.cpu.get_b() != 0:
                self.cpu.set_pc((self.cpu.get_pc() - 2) & 0xFFFF)
                return 21
            return 16

        # Unhandled ED opcode
        return 8

    def _set_in_flags(self, val: int):
        """Set flags after IN instruction"""
        flags = (self.cpu.get_f() & FLAG_C) | self.szp_flags[val]
        self.cpu.set_f(flags)

    def _read16(self, addr: int) -> int:
        lo = self.cpu.read_mem(addr)
        hi = self.cpu.read_mem((addr + 1) & 0xFFFF)
        return (hi << 8) | lo

    def _write16(self, addr: int, val: int):
        self.cpu.write_mem(addr, val & 0xFF)
        self.cpu.write_mem((addr + 1) & 0xFFFF, (val >> 8) & 0xFF)

    def _sbc_hl(self, val: int) -> int:
        """SBC HL,rr"""
        hl = self.cpu.get_hl()
        carry_in = 1 if (self.cpu.get_f() & FLAG_C) else 0
        result = hl - val - carry_in

        carry = 0
        if result < 0:
            carry = 1
            result += 65536

        flags = FLAG_N
        if result == 0:
            flags |= FLAG_Z
        if result >= 32768:
            flags |= FLAG_S
        if carry:
            flags |= FLAG_C
        half = (hl & 0xFFF) - (val & 0xFFF) - carry_in
        if half < 0:
            flags |= FLAG_H
        # Overflow
        hl_sign = hl >= 32768
        val_sign = val >= 32768
        r_sign = result >= 32768
        if hl_sign and not val_sign and not r_sign:
            flags |= FLAG_PV
        if not hl_sign and val_sign and r_sign:
            flags |= FLAG_PV

        self.cpu.set_hl(result)
        self.cpu.set_f(flags)
        return 15

    def _adc_hl(self, val: int) -> int:
        """ADC HL,rr"""
        hl = self.cpu.get_hl()
        carry_in = 1 if (self.cpu.get_f() & FLAG_C) else 0
        result = hl + val + carry_in

        carry = 0
        if result >= 65536:
            carry = 1
            result &= 0xFFFF

        flags = 0
        if result == 0:
            flags |= FLAG_Z
        if result >= 32768:
            flags |= FLAG_S
        if carry:
            flags |= FLAG_C
        half = (hl & 0xFFF) + (val & 0xFFF) + carry_in
        if half >= 4096:
            flags |= FLAG_H
        # Overflow
        hl_sign = hl >= 32768
        val_sign = val >= 32768
        r_sign = result >= 32768
        if not hl_sign and not val_sign and r_sign:
            flags |= FLAG_PV
        if hl_sign and val_sign and not r_sign:
            flags |= FLAG_PV

        self.cpu.set_hl(result)
        self.cpu.set_f(flags)
        return 15

    def _ldi(self) -> int:
        """LDI - Load and increment"""
        hl = self.cpu.get_hl()
        de = self.cpu.get_de()
        bc = self.cpu.get_bc()

        val = self.cpu.read_mem(hl)
        self.cpu.write_mem(de, val)

        self.cpu.set_hl((hl + 1) & 0xFFFF)
        self.cpu.set_de((de + 1) & 0xFFFF)
        bc = (bc - 1) & 0xFFFF
        self.cpu.set_bc(bc)

        f = self.cpu.get_f()
        f = (f & (FLAG_S | FLAG_Z | FLAG_C))  # Clear H, P/V, N
        if bc != 0:
            f |= FLAG_PV
        self.cpu.set_f(f)
        return 16

    def _ldd(self) -> int:
        """LDD - Load and decrement"""
        hl = self.cpu.get_hl()
        de = self.cpu.get_de()
        bc = self.cpu.get_bc()

        val = self.cpu.read_mem(hl)
        self.cpu.write_mem(de, val)

        self.cpu.set_hl((hl - 1) & 0xFFFF)
        self.cpu.set_de((de - 1) & 0xFFFF)
        bc = (bc - 1) & 0xFFFF
        self.cpu.set_bc(bc)

        f = self.cpu.get_f()
        f = (f & (FLAG_S | FLAG_Z | FLAG_C))
        if bc != 0:
            f |= FLAG_PV
        self.cpu.set_f(f)
        return 16

    def _cpi(self) -> int:
        """CPI - Compare and increment"""
        a = self.cpu.get_a()
        hl = self.cpu.get_hl()
        bc = self.cpu.get_bc()

        val = self.cpu.read_mem(hl)
        result = (a - val) & 0xFF

        self.cpu.set_hl((hl + 1) & 0xFFFF)
        bc = (bc - 1) & 0xFFFF
        self.cpu.set_bc(bc)

        flags = FLAG_N | (self.cpu.get_f() & FLAG_C)
        if result == 0:
            flags |= FLAG_Z
        if result >= 128:
            flags |= FLAG_S
        if bc != 0:
            flags |= FLAG_PV
        if (a & 0xF) < (val & 0xF):
            flags |= FLAG_H
        self.cpu.set_f(flags)
        return 16

    def _cpd(self) -> int:
        """CPD - Compare and decrement"""
        a = self.cpu.get_a()
        hl = self.cpu.get_hl()
        bc = self.cpu.get_bc()

        val = self.cpu.read_mem(hl)
        result = (a - val) & 0xFF

        self.cpu.set_hl((hl - 1) & 0xFFFF)
        bc = (bc - 1) & 0xFFFF
        self.cpu.set_bc(bc)

        flags = FLAG_N | (self.cpu.get_f() & FLAG_C)
        if result == 0:
            flags |= FLAG_Z
        if result >= 128:
            flags |= FLAG_S
        if bc != 0:
            flags |= FLAG_PV
        if (a & 0xF) < (val & 0xF):
            flags |= FLAG_H
        self.cpu.set_f(flags)
        return 16

    def _ini(self) -> int:
        """INI - Input and increment"""
        hl = self.cpu.get_hl()
        val = self.cpu.read_io(self.cpu.get_c())
        self.cpu.write_mem(hl, val)
        self.cpu.set_hl((hl + 1) & 0xFFFF)
        b = (self.cpu.get_b() - 1) & 0xFF
        self.cpu.set_b(b)
        flags = FLAG_N
        if b == 0:
            flags |= FLAG_Z
        self.cpu.set_f(flags)
        return 16

    def _ind(self) -> int:
        """IND - Input and decrement"""
        hl = self.cpu.get_hl()
        val = self.cpu.read_io(self.cpu.get_c())
        self.cpu.write_mem(hl, val)
        self.cpu.set_hl((hl - 1) & 0xFFFF)
        b = (self.cpu.get_b() - 1) & 0xFF
        self.cpu.set_b(b)
        flags = FLAG_N
        if b == 0:
            flags |= FLAG_Z
        self.cpu.set_f(flags)
        return 16

    def _outi(self) -> int:
        """OUTI - Output and increment"""
        hl = self.cpu.get_hl()
        val = self.cpu.read_mem(hl)
        self.cpu.write_io(self.cpu.get_c(), val)
        self.cpu.set_hl((hl + 1) & 0xFFFF)
        b = (self.cpu.get_b() - 1) & 0xFF
        self.cpu.set_b(b)
        flags = FLAG_N
        if b == 0:
            flags |= FLAG_Z
        self.cpu.set_f(flags)
        return 16

    def _outd(self) -> int:
        """OUTD - Output and decrement"""
        hl = self.cpu.get_hl()
        val = self.cpu.read_mem(hl)
        self.cpu.write_io(self.cpu.get_c(), val)
        self.cpu.set_hl((hl - 1) & 0xFFFF)
        b = (self.cpu.get_b() - 1) & 0xFF
        self.cpu.set_b(b)
        flags = FLAG_N
        if b == 0:
            flags |= FLAG_Z
        self.cpu.set_f(flags)
        return 16
