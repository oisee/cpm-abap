"""
6502 Opcode Handlers

Each handler is a Python function that implements one 6502 opcode.
The handler receives the emulator instance and performs the operation.

Handler contract:
- Read operand(s) from code stream using emu.pop_operand_byte/word()
- Perform the operation
- Update flags as needed
- Return True to continue, False to stop
"""

from emu6502.runtime import (
    Emu6502, Traps,
    FLAG_C, FLAG_Z, FLAG_N, FLAG_V,
    DATA_BASE
)


# =============================================================================
# Load/Store Operations
# =============================================================================

def lda_imm(emu: Emu6502) -> bool:
    """$A9 - LDA #imm"""
    val = emu.pop_operand_byte()
    emu.a = val
    emu.update_nz(val)
    return True


def lda_zp(emu: Emu6502) -> bool:
    """$A5 - LDA zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    emu.a = val
    emu.update_nz(val)
    return True


def lda_zpx(emu: Emu6502) -> bool:
    """$B5 - LDA zp,X"""
    addr = (emu.pop_operand_byte() + emu.x) & 0xFF
    val = emu.read_6502(addr)
    emu.a = val
    emu.update_nz(val)
    return True


def lda_abs(emu: Emu6502) -> bool:
    """$AD - LDA abs"""
    addr = emu.pop_operand_word()
    val = emu.read_6502(addr)
    emu.a = val
    emu.update_nz(val)
    return True


def lda_abx(emu: Emu6502) -> bool:
    """$BD - LDA abs,X"""
    addr = (emu.pop_operand_word() + emu.x) & 0xFFFF
    val = emu.read_6502(addr)
    emu.a = val
    emu.update_nz(val)
    return True


def lda_aby(emu: Emu6502) -> bool:
    """$B9 - LDA abs,Y"""
    addr = (emu.pop_operand_word() + emu.y) & 0xFFFF
    val = emu.read_6502(addr)
    emu.a = val
    emu.update_nz(val)
    return True


def ldx_imm(emu: Emu6502) -> bool:
    """$A2 - LDX #imm"""
    val = emu.pop_operand_byte()
    emu.x = val
    emu.update_nz(val)
    return True


def ldx_zp(emu: Emu6502) -> bool:
    """$A6 - LDX zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    emu.x = val
    emu.update_nz(val)
    return True


def ldy_imm(emu: Emu6502) -> bool:
    """$A0 - LDY #imm"""
    val = emu.pop_operand_byte()
    emu.y = val
    emu.update_nz(val)
    return True


def ldy_zp(emu: Emu6502) -> bool:
    """$A4 - LDY zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    emu.y = val
    emu.update_nz(val)
    return True


def sta_zp(emu: Emu6502) -> bool:
    """$85 - STA zp"""
    addr = emu.pop_operand_byte()
    emu.write_6502(addr, emu.a)
    return True


def sta_zpx(emu: Emu6502) -> bool:
    """$95 - STA zp,X"""
    addr = (emu.pop_operand_byte() + emu.x) & 0xFF
    emu.write_6502(addr, emu.a)
    return True


def sta_abs(emu: Emu6502) -> bool:
    """$8D - STA abs"""
    addr = emu.pop_operand_word()
    emu.write_6502(addr, emu.a)
    return True


def sta_abx(emu: Emu6502) -> bool:
    """$9D - STA abs,X"""
    addr = (emu.pop_operand_word() + emu.x) & 0xFFFF
    emu.write_6502(addr, emu.a)
    return True


def sta_aby(emu: Emu6502) -> bool:
    """$99 - STA abs,Y"""
    addr = (emu.pop_operand_word() + emu.y) & 0xFFFF
    emu.write_6502(addr, emu.a)
    return True


def stx_zp(emu: Emu6502) -> bool:
    """$86 - STX zp"""
    addr = emu.pop_operand_byte()
    emu.write_6502(addr, emu.x)
    return True


def stx_abs(emu: Emu6502) -> bool:
    """$8E - STX abs"""
    addr = emu.pop_operand_word()
    emu.write_6502(addr, emu.x)
    return True


def sty_zp(emu: Emu6502) -> bool:
    """$84 - STY zp"""
    addr = emu.pop_operand_byte()
    emu.write_6502(addr, emu.y)
    return True


def sty_abs(emu: Emu6502) -> bool:
    """$8C - STY abs"""
    addr = emu.pop_operand_word()
    emu.write_6502(addr, emu.y)
    return True


# =============================================================================
# Transfer Operations
# =============================================================================

def tax(emu: Emu6502) -> bool:
    """$AA - TAX"""
    emu.x = emu.a
    emu.update_nz(emu.x)
    return True


def tay(emu: Emu6502) -> bool:
    """$A8 - TAY"""
    emu.y = emu.a
    emu.update_nz(emu.y)
    return True


def txa(emu: Emu6502) -> bool:
    """$8A - TXA"""
    emu.a = emu.x
    emu.update_nz(emu.a)
    return True


def tya(emu: Emu6502) -> bool:
    """$98 - TYA"""
    emu.a = emu.y
    emu.update_nz(emu.a)
    return True


def tsx(emu: Emu6502) -> bool:
    """$BA - TSX"""
    emu.x = emu.sp
    emu.update_nz(emu.x)
    return True


def txs(emu: Emu6502) -> bool:
    """$9A - TXS"""
    emu.sp = emu.x
    return True


# =============================================================================
# Increment/Decrement
# =============================================================================

def inx(emu: Emu6502) -> bool:
    """$E8 - INX"""
    emu.x = (emu.x + 1) & 0xFF
    emu.update_nz(emu.x)
    return True


def iny(emu: Emu6502) -> bool:
    """$C8 - INY"""
    emu.y = (emu.y + 1) & 0xFF
    emu.update_nz(emu.y)
    return True


def dex(emu: Emu6502) -> bool:
    """$CA - DEX"""
    emu.x = (emu.x - 1) & 0xFF
    emu.update_nz(emu.x)
    return True


def dey(emu: Emu6502) -> bool:
    """$88 - DEY"""
    emu.y = (emu.y - 1) & 0xFF
    emu.update_nz(emu.y)
    return True


def inc_zp(emu: Emu6502) -> bool:
    """$E6 - INC zp"""
    addr = emu.pop_operand_byte()
    val = (emu.read_6502(addr) + 1) & 0xFF
    emu.write_6502(addr, val)
    emu.update_nz(val)
    return True


def inc_abs(emu: Emu6502) -> bool:
    """$EE - INC abs"""
    addr = emu.pop_operand_word()
    val = (emu.read_6502(addr) + 1) & 0xFF
    emu.write_6502(addr, val)
    emu.update_nz(val)
    return True


def dec_zp(emu: Emu6502) -> bool:
    """$C6 - DEC zp"""
    addr = emu.pop_operand_byte()
    val = (emu.read_6502(addr) - 1) & 0xFF
    emu.write_6502(addr, val)
    emu.update_nz(val)
    return True


def dec_abs(emu: Emu6502) -> bool:
    """$CE - DEC abs"""
    addr = emu.pop_operand_word()
    val = (emu.read_6502(addr) - 1) & 0xFF
    emu.write_6502(addr, val)
    emu.update_nz(val)
    return True


# =============================================================================
# Compare Operations
# =============================================================================

def _compare(emu: Emu6502, reg: int, val: int):
    """Helper for CMP/CPX/CPY"""
    result = reg - val
    emu.set_flag(FLAG_C, reg >= val)
    emu.set_flag(FLAG_Z, (result & 0xFF) == 0)
    emu.set_flag(FLAG_N, (result & 0x80) != 0)


def cmp_imm(emu: Emu6502) -> bool:
    """$C9 - CMP #imm"""
    val = emu.pop_operand_byte()
    _compare(emu, emu.a, val)
    return True


def cmp_zp(emu: Emu6502) -> bool:
    """$C5 - CMP zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    _compare(emu, emu.a, val)
    return True


def cmp_abs(emu: Emu6502) -> bool:
    """$CD - CMP abs"""
    addr = emu.pop_operand_word()
    val = emu.read_6502(addr)
    _compare(emu, emu.a, val)
    return True


def cpx_imm(emu: Emu6502) -> bool:
    """$E0 - CPX #imm"""
    val = emu.pop_operand_byte()
    _compare(emu, emu.x, val)
    return True


def cpx_zp(emu: Emu6502) -> bool:
    """$E4 - CPX zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    _compare(emu, emu.x, val)
    return True


def cpy_imm(emu: Emu6502) -> bool:
    """$C0 - CPY #imm"""
    val = emu.pop_operand_byte()
    _compare(emu, emu.y, val)
    return True


def cpy_zp(emu: Emu6502) -> bool:
    """$C4 - CPY zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    _compare(emu, emu.y, val)
    return True


# =============================================================================
# Branch Operations
# =============================================================================

def _branch(emu: Emu6502, condition: bool) -> bool:
    """Helper for all branch instructions"""
    # Get relative offset (signed)
    offset = emu.pop_operand_byte()
    if offset >= 128:
        offset -= 256

    if condition:
        # Adjust Z80 SP to new position
        # offset is in 6502 bytes, multiply by 2 for threaded code
        emu.vm.cpu.sp = (emu.vm.cpu.sp + offset * 2) & 0xFFFF

    return True


def bne(emu: Emu6502) -> bool:
    """$D0 - BNE"""
    return _branch(emu, not emu.get_flag(FLAG_Z))


def beq(emu: Emu6502) -> bool:
    """$F0 - BEQ"""
    return _branch(emu, emu.get_flag(FLAG_Z))


def bcc(emu: Emu6502) -> bool:
    """$90 - BCC"""
    return _branch(emu, not emu.get_flag(FLAG_C))


def bcs(emu: Emu6502) -> bool:
    """$B0 - BCS"""
    return _branch(emu, emu.get_flag(FLAG_C))


def bpl(emu: Emu6502) -> bool:
    """$10 - BPL"""
    return _branch(emu, not emu.get_flag(FLAG_N))


def bmi(emu: Emu6502) -> bool:
    """$30 - BMI"""
    return _branch(emu, emu.get_flag(FLAG_N))


def bvc(emu: Emu6502) -> bool:
    """$50 - BVC"""
    return _branch(emu, not emu.get_flag(FLAG_V))


def bvs(emu: Emu6502) -> bool:
    """$70 - BVS"""
    return _branch(emu, emu.get_flag(FLAG_V))


# =============================================================================
# Jump/Call Operations
# =============================================================================

def jmp_abs(emu: Emu6502) -> bool:
    """$4C - JMP abs"""
    target = emu.pop_operand_word()
    # Convert 6502 address to Z80 threaded code address
    # Each 6502 byte = 2 bytes in threaded code
    z80_addr = 0xC000 + (target * 2)
    emu.vm.cpu.sp = z80_addr
    return True


def jmp_ind(emu: Emu6502) -> bool:
    """$6C - JMP (ind)"""
    ptr = emu.pop_operand_word()
    # 6502 bug: doesn't cross page boundary
    lo = emu.read_6502(ptr)
    hi = emu.read_6502((ptr & 0xFF00) | ((ptr + 1) & 0xFF))
    target = (hi << 8) | lo
    z80_addr = 0xC000 + (target * 2)
    emu.vm.cpu.sp = z80_addr
    return True


def jsr(emu: Emu6502) -> bool:
    """$20 - JSR abs"""
    target = emu.pop_operand_word()

    # Check for ROM traps
    if target >= 0xFC00:
        return _handle_trap(emu, target)

    # Save current Z80 SP (return address) to shadow stack
    emu.shadow_push(emu.vm.cpu.sp)

    # Jump to target
    z80_addr = 0xC000 + (target * 2)
    emu.vm.cpu.sp = z80_addr
    return True


def rts(emu: Emu6502) -> bool:
    """$60 - RTS"""
    # Pop return address from shadow stack
    ret_addr = emu.shadow_pull()
    emu.vm.cpu.sp = ret_addr
    return True


def _handle_trap(emu: Emu6502, addr: int) -> bool:
    """Handle ROM call trap"""
    if addr == 0xFDED:
        Traps.cout(emu)
    elif addr == 0xFD6A:
        Traps.getln(emu)
    elif addr == 0xFC58:
        Traps.home(emu)
    # else: unknown trap, ignore
    return True


# =============================================================================
# Stack Operations
# =============================================================================

def pha(emu: Emu6502) -> bool:
    """$48 - PHA"""
    emu.push(emu.a)
    return True


def pla(emu: Emu6502) -> bool:
    """$68 - PLA"""
    emu.a = emu.pull()
    emu.update_nz(emu.a)
    return True


def php(emu: Emu6502) -> bool:
    """$08 - PHP"""
    emu.push(emu.p | 0x10)  # B flag set when pushed by PHP
    return True


def plp(emu: Emu6502) -> bool:
    """$28 - PLP"""
    emu.p = emu.pull()
    return True


# =============================================================================
# Flag Operations
# =============================================================================

def clc(emu: Emu6502) -> bool:
    """$18 - CLC"""
    emu.set_flag(FLAG_C, False)
    return True


def sec(emu: Emu6502) -> bool:
    """$38 - SEC"""
    emu.set_flag(FLAG_C, True)
    return True


def cli(emu: Emu6502) -> bool:
    """$58 - CLI"""
    emu.set_flag(0x04, False)
    return True


def sei(emu: Emu6502) -> bool:
    """$78 - SEI"""
    emu.set_flag(0x04, True)
    return True


def clv(emu: Emu6502) -> bool:
    """$B8 - CLV"""
    emu.set_flag(FLAG_V, False)
    return True


def cld(emu: Emu6502) -> bool:
    """$D8 - CLD"""
    emu.set_flag(0x08, False)
    return True


def sed(emu: Emu6502) -> bool:
    """$F8 - SED (NO-OP - decimal mode not implemented)"""
    emu.set_flag(0x08, True)
    return True


# =============================================================================
# ALU Operations (without decimal mode)
# =============================================================================

def adc_imm(emu: Emu6502) -> bool:
    """$69 - ADC #imm"""
    val = emu.pop_operand_byte()
    _adc(emu, val)
    return True


def adc_zp(emu: Emu6502) -> bool:
    """$65 - ADC zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    _adc(emu, val)
    return True


def adc_abs(emu: Emu6502) -> bool:
    """$6D - ADC abs"""
    addr = emu.pop_operand_word()
    val = emu.read_6502(addr)
    _adc(emu, val)
    return True


def _adc(emu: Emu6502, val: int):
    """ADC helper (binary mode only)"""
    a = emu.a
    c = 1 if emu.get_flag(FLAG_C) else 0
    result = a + val + c

    emu.set_flag(FLAG_C, result > 0xFF)
    emu.set_flag(FLAG_V, ((a ^ result) & (val ^ result) & 0x80) != 0)
    emu.a = result & 0xFF
    emu.update_nz(emu.a)


def sbc_imm(emu: Emu6502) -> bool:
    """$E9 - SBC #imm"""
    val = emu.pop_operand_byte()
    _sbc(emu, val)
    return True


def sbc_zp(emu: Emu6502) -> bool:
    """$E5 - SBC zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    _sbc(emu, val)
    return True


def sbc_abs(emu: Emu6502) -> bool:
    """$ED - SBC abs"""
    addr = emu.pop_operand_word()
    val = emu.read_6502(addr)
    _sbc(emu, val)
    return True


def _sbc(emu: Emu6502, val: int):
    """SBC helper (binary mode only)"""
    a = emu.a
    c = 1 if emu.get_flag(FLAG_C) else 0
    result = a - val - (1 - c)

    emu.set_flag(FLAG_C, result >= 0)
    emu.set_flag(FLAG_V, ((a ^ result) & (a ^ val) & 0x80) != 0)
    emu.a = result & 0xFF
    emu.update_nz(emu.a)


# =============================================================================
# Logic Operations
# =============================================================================

def and_imm(emu: Emu6502) -> bool:
    """$29 - AND #imm"""
    val = emu.pop_operand_byte()
    emu.a = emu.a & val
    emu.update_nz(emu.a)
    return True


def and_zp(emu: Emu6502) -> bool:
    """$25 - AND zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    emu.a = emu.a & val
    emu.update_nz(emu.a)
    return True


def ora_imm(emu: Emu6502) -> bool:
    """$09 - ORA #imm"""
    val = emu.pop_operand_byte()
    emu.a = emu.a | val
    emu.update_nz(emu.a)
    return True


def ora_zp(emu: Emu6502) -> bool:
    """$05 - ORA zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    emu.a = emu.a | val
    emu.update_nz(emu.a)
    return True


def eor_imm(emu: Emu6502) -> bool:
    """$49 - EOR #imm"""
    val = emu.pop_operand_byte()
    emu.a = emu.a ^ val
    emu.update_nz(emu.a)
    return True


def eor_zp(emu: Emu6502) -> bool:
    """$45 - EOR zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    emu.a = emu.a ^ val
    emu.update_nz(emu.a)
    return True


# =============================================================================
# Bit Test
# =============================================================================

def bit_zp(emu: Emu6502) -> bool:
    """$24 - BIT zp"""
    addr = emu.pop_operand_byte()
    val = emu.read_6502(addr)
    emu.set_flag(FLAG_Z, (emu.a & val) == 0)
    emu.set_flag(FLAG_N, (val & 0x80) != 0)
    emu.set_flag(FLAG_V, (val & 0x40) != 0)
    return True


def bit_abs(emu: Emu6502) -> bool:
    """$2C - BIT abs"""
    addr = emu.pop_operand_word()
    val = emu.read_6502(addr)
    emu.set_flag(FLAG_Z, (emu.a & val) == 0)
    emu.set_flag(FLAG_N, (val & 0x80) != 0)
    emu.set_flag(FLAG_V, (val & 0x40) != 0)
    return True


# =============================================================================
# Shift/Rotate Operations
# =============================================================================

def asl_acc(emu: Emu6502) -> bool:
    """$0A - ASL A"""
    emu.set_flag(FLAG_C, (emu.a & 0x80) != 0)
    emu.a = (emu.a << 1) & 0xFF
    emu.update_nz(emu.a)
    return True


def lsr_acc(emu: Emu6502) -> bool:
    """$4A - LSR A"""
    emu.set_flag(FLAG_C, (emu.a & 0x01) != 0)
    emu.a = emu.a >> 1
    emu.update_nz(emu.a)
    return True


def rol_acc(emu: Emu6502) -> bool:
    """$2A - ROL A"""
    c = 1 if emu.get_flag(FLAG_C) else 0
    emu.set_flag(FLAG_C, (emu.a & 0x80) != 0)
    emu.a = ((emu.a << 1) | c) & 0xFF
    emu.update_nz(emu.a)
    return True


def ror_acc(emu: Emu6502) -> bool:
    """$6A - ROR A"""
    c = 0x80 if emu.get_flag(FLAG_C) else 0
    emu.set_flag(FLAG_C, (emu.a & 0x01) != 0)
    emu.a = (emu.a >> 1) | c
    emu.update_nz(emu.a)
    return True


# =============================================================================
# Misc
# =============================================================================

def nop(emu: Emu6502) -> bool:
    """$EA - NOP"""
    return True


def brk(emu: Emu6502) -> bool:
    """$00 - BRK"""
    return False  # Stop execution


# =============================================================================
# Handler Table
# =============================================================================

HANDLERS = {
    # Load/Store
    0xA9: lda_imm, 0xA5: lda_zp, 0xB5: lda_zpx,
    0xAD: lda_abs, 0xBD: lda_abx, 0xB9: lda_aby,
    0xA2: ldx_imm, 0xA6: ldx_zp,
    0xA0: ldy_imm, 0xA4: ldy_zp,
    0x85: sta_zp, 0x95: sta_zpx, 0x8D: sta_abs,
    0x9D: sta_abx, 0x99: sta_aby,
    0x86: stx_zp, 0x8E: stx_abs,
    0x84: sty_zp, 0x8C: sty_abs,

    # Transfer
    0xAA: tax, 0xA8: tay, 0x8A: txa, 0x98: tya,
    0xBA: tsx, 0x9A: txs,

    # Inc/Dec
    0xE8: inx, 0xC8: iny, 0xCA: dex, 0x88: dey,
    0xE6: inc_zp, 0xEE: inc_abs,
    0xC6: dec_zp, 0xCE: dec_abs,

    # Compare
    0xC9: cmp_imm, 0xC5: cmp_zp, 0xCD: cmp_abs,
    0xE0: cpx_imm, 0xE4: cpx_zp,
    0xC0: cpy_imm, 0xC4: cpy_zp,

    # Branch
    0xD0: bne, 0xF0: beq, 0x90: bcc, 0xB0: bcs,
    0x10: bpl, 0x30: bmi, 0x50: bvc, 0x70: bvs,

    # Jump/Call
    0x4C: jmp_abs, 0x6C: jmp_ind,
    0x20: jsr, 0x60: rts,

    # Stack
    0x48: pha, 0x68: pla, 0x08: php, 0x28: plp,

    # Flags
    0x18: clc, 0x38: sec, 0x58: cli, 0x78: sei,
    0xB8: clv, 0xD8: cld, 0xF8: sed,

    # ALU
    0x69: adc_imm, 0x65: adc_zp, 0x6D: adc_abs,
    0xE9: sbc_imm, 0xE5: sbc_zp, 0xED: sbc_abs,

    # Logic
    0x29: and_imm, 0x25: and_zp,
    0x09: ora_imm, 0x05: ora_zp,
    0x49: eor_imm, 0x45: eor_zp,

    # Bit
    0x24: bit_zp, 0x2C: bit_abs,

    # Shift
    0x0A: asl_acc, 0x4A: lsr_acc,
    0x2A: rol_acc, 0x6A: ror_acc,

    # Misc
    0xEA: nop, 0x00: brk,
}


def get_handler(opcode: int):
    """Get handler function for opcode"""
    return HANDLERS.get(opcode)


def count_implemented() -> int:
    """Count implemented handlers"""
    return len(HANDLERS)


if __name__ == "__main__":
    print(f"6502 Handlers: {count_implemented()} implemented")
