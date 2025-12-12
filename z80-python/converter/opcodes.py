"""
6502 Opcode Table
Complete table of all 256 opcodes with addressing modes
"""

from dataclasses import dataclass
from enum import Enum, auto


class AddrMode(Enum):
    """6502 Addressing Modes"""
    IMP = auto()    # Implied: CLC, RTS (1 byte)
    ACC = auto()    # Accumulator: ASL A (1 byte)
    IMM = auto()    # Immediate: LDA #$xx (2 bytes)
    ZP = auto()     # Zero Page: LDA $xx (2 bytes)
    ZPX = auto()    # Zero Page,X: LDA $xx,X (2 bytes)
    ZPY = auto()    # Zero Page,Y: LDX $xx,Y (2 bytes)
    ABS = auto()    # Absolute: LDA $xxxx (3 bytes)
    ABX = auto()    # Absolute,X: LDA $xxxx,X (3 bytes)
    ABY = auto()    # Absolute,Y: LDA $xxxx,Y (3 bytes)
    IND = auto()    # Indirect: JMP ($xxxx) (3 bytes)
    IZX = auto()    # Indexed Indirect: LDA ($xx,X) (2 bytes)
    IZY = auto()    # Indirect Indexed: LDA ($xx),Y (2 bytes)
    REL = auto()    # Relative: BNE $xx (2 bytes)


# Bytes per addressing mode
ADDR_MODE_BYTES = {
    AddrMode.IMP: 1,
    AddrMode.ACC: 1,
    AddrMode.IMM: 2,
    AddrMode.ZP: 2,
    AddrMode.ZPX: 2,
    AddrMode.ZPY: 2,
    AddrMode.ABS: 3,
    AddrMode.ABX: 3,
    AddrMode.ABY: 3,
    AddrMode.IND: 3,
    AddrMode.IZX: 2,
    AddrMode.IZY: 2,
    AddrMode.REL: 2,
}


@dataclass
class Opcode:
    """6502 Opcode definition"""
    mnemonic: str       # e.g., "LDA", "STA", "JMP"
    mode: AddrMode      # Addressing mode
    cycles: int         # Base cycle count (for reference)

    @property
    def bytes(self) -> int:
        return ADDR_MODE_BYTES[self.mode]


# Illegal/undefined opcode marker
ILL = None


def _op(mnemonic: str, mode: AddrMode, cycles: int) -> Opcode:
    return Opcode(mnemonic, mode, cycles)


# Complete 6502 opcode table (256 entries)
# Based on http://www.6502.org/tutorials/6502opcodes.html
OPCODES: dict[int, Opcode | None] = {
    # 0x00 - 0x0F
    0x00: _op("BRK", AddrMode.IMP, 7),
    0x01: _op("ORA", AddrMode.IZX, 6),
    0x02: ILL,
    0x03: ILL,
    0x04: ILL,  # NOP zp (illegal)
    0x05: _op("ORA", AddrMode.ZP, 3),
    0x06: _op("ASL", AddrMode.ZP, 5),
    0x07: ILL,
    0x08: _op("PHP", AddrMode.IMP, 3),
    0x09: _op("ORA", AddrMode.IMM, 2),
    0x0A: _op("ASL", AddrMode.ACC, 2),
    0x0B: ILL,
    0x0C: ILL,  # NOP abs (illegal)
    0x0D: _op("ORA", AddrMode.ABS, 4),
    0x0E: _op("ASL", AddrMode.ABS, 6),
    0x0F: ILL,

    # 0x10 - 0x1F
    0x10: _op("BPL", AddrMode.REL, 2),
    0x11: _op("ORA", AddrMode.IZY, 5),
    0x12: ILL,
    0x13: ILL,
    0x14: ILL,
    0x15: _op("ORA", AddrMode.ZPX, 4),
    0x16: _op("ASL", AddrMode.ZPX, 6),
    0x17: ILL,
    0x18: _op("CLC", AddrMode.IMP, 2),
    0x19: _op("ORA", AddrMode.ABY, 4),
    0x1A: ILL,  # NOP (illegal)
    0x1B: ILL,
    0x1C: ILL,
    0x1D: _op("ORA", AddrMode.ABX, 4),
    0x1E: _op("ASL", AddrMode.ABX, 7),
    0x1F: ILL,

    # 0x20 - 0x2F
    0x20: _op("JSR", AddrMode.ABS, 6),
    0x21: _op("AND", AddrMode.IZX, 6),
    0x22: ILL,
    0x23: ILL,
    0x24: _op("BIT", AddrMode.ZP, 3),
    0x25: _op("AND", AddrMode.ZP, 3),
    0x26: _op("ROL", AddrMode.ZP, 5),
    0x27: ILL,
    0x28: _op("PLP", AddrMode.IMP, 4),
    0x29: _op("AND", AddrMode.IMM, 2),
    0x2A: _op("ROL", AddrMode.ACC, 2),
    0x2B: ILL,
    0x2C: _op("BIT", AddrMode.ABS, 4),
    0x2D: _op("AND", AddrMode.ABS, 4),
    0x2E: _op("ROL", AddrMode.ABS, 6),
    0x2F: ILL,

    # 0x30 - 0x3F
    0x30: _op("BMI", AddrMode.REL, 2),
    0x31: _op("AND", AddrMode.IZY, 5),
    0x32: ILL,
    0x33: ILL,
    0x34: ILL,
    0x35: _op("AND", AddrMode.ZPX, 4),
    0x36: _op("ROL", AddrMode.ZPX, 6),
    0x37: ILL,
    0x38: _op("SEC", AddrMode.IMP, 2),
    0x39: _op("AND", AddrMode.ABY, 4),
    0x3A: ILL,
    0x3B: ILL,
    0x3C: ILL,
    0x3D: _op("AND", AddrMode.ABX, 4),
    0x3E: _op("ROL", AddrMode.ABX, 7),
    0x3F: ILL,

    # 0x40 - 0x4F
    0x40: _op("RTI", AddrMode.IMP, 6),
    0x41: _op("EOR", AddrMode.IZX, 6),
    0x42: ILL,
    0x43: ILL,
    0x44: ILL,
    0x45: _op("EOR", AddrMode.ZP, 3),
    0x46: _op("LSR", AddrMode.ZP, 5),
    0x47: ILL,
    0x48: _op("PHA", AddrMode.IMP, 3),
    0x49: _op("EOR", AddrMode.IMM, 2),
    0x4A: _op("LSR", AddrMode.ACC, 2),
    0x4B: ILL,
    0x4C: _op("JMP", AddrMode.ABS, 3),
    0x4D: _op("EOR", AddrMode.ABS, 4),
    0x4E: _op("LSR", AddrMode.ABS, 6),
    0x4F: ILL,

    # 0x50 - 0x5F
    0x50: _op("BVC", AddrMode.REL, 2),
    0x51: _op("EOR", AddrMode.IZY, 5),
    0x52: ILL,
    0x53: ILL,
    0x54: ILL,
    0x55: _op("EOR", AddrMode.ZPX, 4),
    0x56: _op("LSR", AddrMode.ZPX, 6),
    0x57: ILL,
    0x58: _op("CLI", AddrMode.IMP, 2),
    0x59: _op("EOR", AddrMode.ABY, 4),
    0x5A: ILL,
    0x5B: ILL,
    0x5C: ILL,
    0x5D: _op("EOR", AddrMode.ABX, 4),
    0x5E: _op("LSR", AddrMode.ABX, 7),
    0x5F: ILL,

    # 0x60 - 0x6F
    0x60: _op("RTS", AddrMode.IMP, 6),
    0x61: _op("ADC", AddrMode.IZX, 6),
    0x62: ILL,
    0x63: ILL,
    0x64: ILL,
    0x65: _op("ADC", AddrMode.ZP, 3),
    0x66: _op("ROR", AddrMode.ZP, 5),
    0x67: ILL,
    0x68: _op("PLA", AddrMode.IMP, 4),
    0x69: _op("ADC", AddrMode.IMM, 2),
    0x6A: _op("ROR", AddrMode.ACC, 2),
    0x6B: ILL,
    0x6C: _op("JMP", AddrMode.IND, 5),
    0x6D: _op("ADC", AddrMode.ABS, 4),
    0x6E: _op("ROR", AddrMode.ABS, 6),
    0x6F: ILL,

    # 0x70 - 0x7F
    0x70: _op("BVS", AddrMode.REL, 2),
    0x71: _op("ADC", AddrMode.IZY, 5),
    0x72: ILL,
    0x73: ILL,
    0x74: ILL,
    0x75: _op("ADC", AddrMode.ZPX, 4),
    0x76: _op("ROR", AddrMode.ZPX, 6),
    0x77: ILL,
    0x78: _op("SEI", AddrMode.IMP, 2),
    0x79: _op("ADC", AddrMode.ABY, 4),
    0x7A: ILL,
    0x7B: ILL,
    0x7C: ILL,
    0x7D: _op("ADC", AddrMode.ABX, 4),
    0x7E: _op("ROR", AddrMode.ABX, 7),
    0x7F: ILL,

    # 0x80 - 0x8F
    0x80: ILL,
    0x81: _op("STA", AddrMode.IZX, 6),
    0x82: ILL,
    0x83: ILL,
    0x84: _op("STY", AddrMode.ZP, 3),
    0x85: _op("STA", AddrMode.ZP, 3),
    0x86: _op("STX", AddrMode.ZP, 3),
    0x87: ILL,
    0x88: _op("DEY", AddrMode.IMP, 2),
    0x89: ILL,
    0x8A: _op("TXA", AddrMode.IMP, 2),
    0x8B: ILL,
    0x8C: _op("STY", AddrMode.ABS, 4),
    0x8D: _op("STA", AddrMode.ABS, 4),
    0x8E: _op("STX", AddrMode.ABS, 4),
    0x8F: ILL,

    # 0x90 - 0x9F
    0x90: _op("BCC", AddrMode.REL, 2),
    0x91: _op("STA", AddrMode.IZY, 6),
    0x92: ILL,
    0x93: ILL,
    0x94: _op("STY", AddrMode.ZPX, 4),
    0x95: _op("STA", AddrMode.ZPX, 4),
    0x96: _op("STX", AddrMode.ZPY, 4),
    0x97: ILL,
    0x98: _op("TYA", AddrMode.IMP, 2),
    0x99: _op("STA", AddrMode.ABY, 5),
    0x9A: _op("TXS", AddrMode.IMP, 2),
    0x9B: ILL,
    0x9C: ILL,
    0x9D: _op("STA", AddrMode.ABX, 5),
    0x9E: ILL,
    0x9F: ILL,

    # 0xA0 - 0xAF
    0xA0: _op("LDY", AddrMode.IMM, 2),
    0xA1: _op("LDA", AddrMode.IZX, 6),
    0xA2: _op("LDX", AddrMode.IMM, 2),
    0xA3: ILL,
    0xA4: _op("LDY", AddrMode.ZP, 3),
    0xA5: _op("LDA", AddrMode.ZP, 3),
    0xA6: _op("LDX", AddrMode.ZP, 3),
    0xA7: ILL,
    0xA8: _op("TAY", AddrMode.IMP, 2),
    0xA9: _op("LDA", AddrMode.IMM, 2),
    0xAA: _op("TAX", AddrMode.IMP, 2),
    0xAB: ILL,
    0xAC: _op("LDY", AddrMode.ABS, 4),
    0xAD: _op("LDA", AddrMode.ABS, 4),
    0xAE: _op("LDX", AddrMode.ABS, 4),
    0xAF: ILL,

    # 0xB0 - 0xBF
    0xB0: _op("BCS", AddrMode.REL, 2),
    0xB1: _op("LDA", AddrMode.IZY, 5),
    0xB2: ILL,
    0xB3: ILL,
    0xB4: _op("LDY", AddrMode.ZPX, 4),
    0xB5: _op("LDA", AddrMode.ZPX, 4),
    0xB6: _op("LDX", AddrMode.ZPY, 4),
    0xB7: ILL,
    0xB8: _op("CLV", AddrMode.IMP, 2),
    0xB9: _op("LDA", AddrMode.ABY, 4),
    0xBA: _op("TSX", AddrMode.IMP, 2),
    0xBB: ILL,
    0xBC: _op("LDY", AddrMode.ABX, 4),
    0xBD: _op("LDA", AddrMode.ABX, 4),
    0xBE: _op("LDX", AddrMode.ABY, 4),
    0xBF: ILL,

    # 0xC0 - 0xCF
    0xC0: _op("CPY", AddrMode.IMM, 2),
    0xC1: _op("CMP", AddrMode.IZX, 6),
    0xC2: ILL,
    0xC3: ILL,
    0xC4: _op("CPY", AddrMode.ZP, 3),
    0xC5: _op("CMP", AddrMode.ZP, 3),
    0xC6: _op("DEC", AddrMode.ZP, 5),
    0xC7: ILL,
    0xC8: _op("INY", AddrMode.IMP, 2),
    0xC9: _op("CMP", AddrMode.IMM, 2),
    0xCA: _op("DEX", AddrMode.IMP, 2),
    0xCB: ILL,
    0xCC: _op("CPY", AddrMode.ABS, 4),
    0xCD: _op("CMP", AddrMode.ABS, 4),
    0xCE: _op("DEC", AddrMode.ABS, 6),
    0xCF: ILL,

    # 0xD0 - 0xDF
    0xD0: _op("BNE", AddrMode.REL, 2),
    0xD1: _op("CMP", AddrMode.IZY, 5),
    0xD2: ILL,
    0xD3: ILL,
    0xD4: ILL,
    0xD5: _op("CMP", AddrMode.ZPX, 4),
    0xD6: _op("DEC", AddrMode.ZPX, 6),
    0xD7: ILL,
    0xD8: _op("CLD", AddrMode.IMP, 2),
    0xD9: _op("CMP", AddrMode.ABY, 4),
    0xDA: ILL,
    0xDB: ILL,
    0xDC: ILL,
    0xDD: _op("CMP", AddrMode.ABX, 4),
    0xDE: _op("DEC", AddrMode.ABX, 7),
    0xDF: ILL,

    # 0xE0 - 0xEF
    0xE0: _op("CPX", AddrMode.IMM, 2),
    0xE1: _op("SBC", AddrMode.IZX, 6),
    0xE2: ILL,
    0xE3: ILL,
    0xE4: _op("CPX", AddrMode.ZP, 3),
    0xE5: _op("SBC", AddrMode.ZP, 3),
    0xE6: _op("INC", AddrMode.ZP, 5),
    0xE7: ILL,
    0xE8: _op("INX", AddrMode.IMP, 2),
    0xE9: _op("SBC", AddrMode.IMM, 2),
    0xEA: _op("NOP", AddrMode.IMP, 2),
    0xEB: ILL,
    0xEC: _op("CPX", AddrMode.ABS, 4),
    0xED: _op("SBC", AddrMode.ABS, 4),
    0xEE: _op("INC", AddrMode.ABS, 6),
    0xEF: ILL,

    # 0xF0 - 0xFF
    0xF0: _op("BEQ", AddrMode.REL, 2),
    0xF1: _op("SBC", AddrMode.IZY, 5),
    0xF2: ILL,
    0xF3: ILL,
    0xF4: ILL,
    0xF5: _op("SBC", AddrMode.ZPX, 4),
    0xF6: _op("INC", AddrMode.ZPX, 6),
    0xF7: ILL,
    0xF8: _op("SED", AddrMode.IMP, 2),
    0xF9: _op("SBC", AddrMode.ABY, 4),
    0xFA: ILL,
    0xFB: ILL,
    0xFC: ILL,
    0xFD: _op("SBC", AddrMode.ABX, 4),
    0xFE: _op("INC", AddrMode.ABX, 7),
    0xFF: ILL,
}


def get_opcode(code: int) -> Opcode | None:
    """Get opcode info, returns None for illegal opcodes"""
    return OPCODES.get(code)


def is_legal(code: int) -> bool:
    """Check if opcode is legal"""
    return OPCODES.get(code) is not None


def count_legal() -> int:
    """Count legal opcodes"""
    return sum(1 for op in OPCODES.values() if op is not None)


# Statistics
if __name__ == "__main__":
    legal = count_legal()
    print(f"6502 Opcode Table")
    print(f"Legal opcodes: {legal}/256")
    print()

    # Group by mnemonic
    mnemonics: dict[str, list[int]] = {}
    for code, op in OPCODES.items():
        if op:
            if op.mnemonic not in mnemonics:
                mnemonics[op.mnemonic] = []
            mnemonics[op.mnemonic].append(code)

    print(f"Unique mnemonics: {len(mnemonics)}")
    print()

    # Show addressing modes per mnemonic
    for mnem in sorted(mnemonics.keys()):
        codes = mnemonics[mnem]
        modes = [OPCODES[c].mode.name for c in codes]
        print(f"{mnem:4s}: {', '.join(modes)}")
