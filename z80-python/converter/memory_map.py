"""
Memory Map and Address Translation for 6502 Emulator

Handler Address Formula:
    Handler_Address = (High_Byte << 8) | Low_Byte
    Where:
        Low_Byte  = Opcode (self-documenting!)
        High_Byte = $70 | (Opcode & $0F)

Z80 Memory Layout:
    $0000-$5FFF  System/Screen (on real Spectrum)
    $6000-$6FFF  Runtime: long handlers, traps, utilities
    $7000-$7FFF  Handler Table (16-way interleaved, 4 KB)
    $8000-$BFFF  6502 DATA Memory (raw bytes, 16 KB)
    $C000-$FFFF  6502 CODE Stream (threaded, banked)
"""


# Memory region constants
RUNTIME_BASE = 0x6000       # Long handlers, traps
HANDLER_TABLE_BASE = 0x7000  # Handler table start
DATA_BASE = 0x8000          # 6502 data memory
CODE_BASE = 0xC000          # Threaded code stream

# Handler table: 16 pages of 256 bytes each
HANDLER_PAGE_SIZE = 256
HANDLER_SLOT_SIZE = 16      # 16 bytes per handler slot


def opcode_to_handler(opcode: int) -> int:
    """
    Convert 6502 opcode to Z80 handler address.

    Formula: handler = ($70 | (opcode & $0F)) << 8 | opcode

    Examples:
        $00 (BRK) -> $7000
        $A9 (LDA#) -> $79A9
        $85 (STA zp) -> $7585
        $E8 (INX) -> $78E8
        $4C (JMP) -> $7C4C
    """
    low = opcode & 0xFF
    high = 0x70 | (opcode & 0x0F)
    return (high << 8) | low


def handler_to_opcode(addr: int) -> int:
    """
    Reverse: extract opcode from handler address.
    (For debugging/disassembly)
    """
    return addr & 0xFF


def addr_6502_to_data(addr: int) -> int:
    """
    Convert 6502 data address to Z80 address.

    6502 $0000-$3FFF -> Z80 $8000-$BFFF

    Note: Only first 16KB of 6502 address space is directly
    mapped. Higher addresses need bank switching.
    """
    return DATA_BASE + (addr & 0x3FFF)


def addr_6502_to_code(addr: int) -> int:
    """
    Convert 6502 code address to threaded code address.

    Each 6502 byte becomes 2 bytes in threaded code,
    so the address is multiplied by 2.

    6502 $0800 -> Z80 $C000 + ($0800 * 2) = $D000
    """
    return CODE_BASE + (addr * 2)


def code_to_6502_addr(z80_addr: int) -> int:
    """
    Reverse: convert Z80 code address back to 6502 address.
    """
    return (z80_addr - CODE_BASE) // 2


def get_handler_page(opcode: int) -> int:
    """
    Get handler page number (0-15) for opcode.
    Page = opcode & $0F
    """
    return opcode & 0x0F


def get_handler_slot(opcode: int) -> int:
    """
    Get slot within page (0-15) for opcode.
    Slot = opcode >> 4
    """
    return opcode >> 4


def handler_slot_address(opcode: int) -> int:
    """
    Get address of handler slot start.
    """
    page = get_handler_page(opcode)
    slot = get_handler_slot(opcode)
    return HANDLER_TABLE_BASE + (page * HANDLER_PAGE_SIZE) + (slot * HANDLER_SLOT_SIZE)


# Test and demonstrate
if __name__ == "__main__":
    print("6502 to Z80 Address Translation")
    print("=" * 50)
    print()

    # Test opcodes
    test_opcodes = [
        (0x00, "BRK"),
        (0xA9, "LDA #imm"),
        (0x85, "STA zp"),
        (0xE8, "INX"),
        (0x4C, "JMP abs"),
        (0x20, "JSR abs"),
        (0x60, "RTS"),
        (0xD0, "BNE rel"),
    ]

    print("Handler Address Calculation:")
    print(f"{'Opcode':8s} {'Mnemonic':10s} {'Handler':8s} {'Page':6s} {'Slot':6s}")
    print("-" * 50)

    for opcode, mnem in test_opcodes:
        handler = opcode_to_handler(opcode)
        page = get_handler_page(opcode)
        slot = get_handler_slot(opcode)
        print(f"${opcode:02X}      {mnem:10s} ${handler:04X}     {page:2d}     {slot:2d}")

    print()
    print("Data Address Translation (6502 -> Z80):")
    print(f"  $0000 (Zero Page)   -> ${addr_6502_to_data(0x0000):04X}")
    print(f"  $0100 (Stack)       -> ${addr_6502_to_data(0x0100):04X}")
    print(f"  $0200 (Input buf)   -> ${addr_6502_to_data(0x0200):04X}")
    print(f"  $0800 (Program)     -> ${addr_6502_to_data(0x0800):04X}")

    print()
    print("Code Address Translation (6502 -> Z80 threaded):")
    print(f"  $0800 -> ${addr_6502_to_code(0x0800):04X}")
    print(f"  $1000 -> ${addr_6502_to_code(0x1000):04X}")
    print(f"  $2000 -> ${addr_6502_to_code(0x2000):04X}")

    print()
    print("Handler Table Layout:")
    print(f"  Page $70: opcodes $x0 (BRK, BPL, JSR, BMI, RTI, BVC, RTS, BVS, ...)")
    print(f"  Page $71: opcodes $x1 (ORA izx, AND izx, EOR izx, ADC izx, ...)")
    print(f"  ...")
    print(f"  Page $79: opcodes $x9 (ORA imm, AND imm, EOR imm, ADC imm, LDA imm, ...)")
    print(f"  ...")
    print(f"  Page $7F: opcodes $xF")
