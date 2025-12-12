#!/usr/bin/env python3
"""
Test the 6502 threaded code runtime on VZ80

Simple test: print "HI" using COUT trap
"""

from vz80 import VZ80Bus
from z80 import Z80
from runtime_6502_data import (
    RUNTIME_CODE, RUNTIME_ORG,
    HLE_CODE, HLE_ORG,
    HANDLERS_CODE, HANDLERS_ORG,
    HANDLER_TABLE, TABLE_ORG,
    LABELS
)

# =============================================================================
# 6502 TEST PROGRAM
# =============================================================================

# Simple 6502 program: print "HI" then halt
#
# $0800:  LDA #$48    ; 'H'
#         JSR $FDED   ; COUT
#         LDA #$49    ; 'I'
#         JSR $FDED   ; COUT
#         LDA #$0A    ; newline
#         JSR $FDED   ; COUT
#         BRK         ; halt

TEST_6502 = bytes([
    0xA9, 0x48,       # LDA #'H'
    0x20, 0xED, 0xFD, # JSR $FDED (COUT)
    0xA9, 0x49,       # LDA #'I'
    0x20, 0xED, 0xFD, # JSR $FDED
    0xA9, 0x0A,       # LDA #$0A (newline)
    0x20, 0xED, 0xFD, # JSR $FDED
    0x00,             # BRK
])

# =============================================================================
# CONVERTER
# =============================================================================

def convert_6502_to_threaded(code: bytes, org: int = 0x0800) -> bytes:
    """
    Convert 6502 binary to threaded code format.
    Each byte becomes a 2-byte handler address.
    """
    result = bytearray()

    for byte in code:
        # Handler address = ($70 | (byte & $0F)) << 8 | byte
        handler_high = 0x70 | (byte & 0x0F)
        handler_addr = (handler_high << 8) | byte

        # Little-endian
        result.append(handler_addr & 0xFF)
        result.append((handler_addr >> 8) & 0xFF)

    return bytes(result)


# =============================================================================
# TRAP TABLE
# =============================================================================

def create_trap_table() -> bytes:
    """
    Create trap table at $8C00 for ROM calls ($FC00-$FFFF).
    Maps Apple II ROM addresses to our HLE handlers.

    Each entry is 2 bytes (handler address).
    Indexed by: (rom_addr & $03FF)
    """
    # First fill entire table with TRAP_UNIMPL
    unimpl = LABELS.get('TRAP_UNIMPL', 0x6050)
    table = bytearray(1024)
    for i in range(0, 1024):
        table[i] = unimpl & 0xFF if i % 2 == 0 else (unimpl >> 8) & 0xFF

    # Key traps (will overwrite UNIMPL entries)
    traps = {
        0xFDED: LABELS['TRAP_COUT'],    # COUT
        0xFD6A: LABELS['TRAP_GETLN'],   # GETLN
        0xFC58: LABELS['TRAP_HOME'],    # HOME
        0xFD0C: LABELS['TRAP_RDKEY'],   # RDKEY
    }

    for rom_addr, handler in traps.items():
        # Offset in trap table: (rom_addr & $03FF)
        offset = rom_addr & 0x03FF
        table[offset] = handler & 0xFF
        table[offset + 1] = (handler >> 8) & 0xFF
        print(f"    Trap ${rom_addr:04X} -> offset {offset}: ${handler:04X}")

    return bytes(table)


# =============================================================================
# MAIN TEST
# =============================================================================

def main():
    print("=" * 60)
    print("6502 Threaded Code Runtime Test")
    print("=" * 60)

    # Create VZ80
    bus = VZ80Bus()
    cpu = Z80(bus)

    # Track output
    output = []
    def on_output(ch):
        # ch is already a character string from vz80
        output.append(ch)
        print(ch, end='', flush=True)
    bus.on_output = on_output

    # Load runtime components into fixed memory
    print("\nLoading runtime...")

    # HLE traps at $6000
    for i, b in enumerate(HLE_CODE):
        bus.fixed_memory[HLE_ORG + i] = b
    print(f"  HLE traps: ${HLE_ORG:04X}-${HLE_ORG + len(HLE_CODE):04X} ({len(HLE_CODE)} bytes)")

    # Opcode handlers at $6100
    for i, b in enumerate(HANDLERS_CODE):
        bus.fixed_memory[HANDLERS_ORG + i] = b
    print(f"  Handlers:  ${HANDLERS_ORG:04X}-${HANDLERS_ORG + len(HANDLERS_CODE):04X} ({len(HANDLERS_CODE)} bytes)")

    # Handler table at $7000
    for i, b in enumerate(HANDLER_TABLE):
        bus.fixed_memory[TABLE_ORG + i] = b
    print(f"  Table:     ${TABLE_ORG:04X}-${TABLE_ORG + len(HANDLER_TABLE):04X} ({len(HANDLER_TABLE)} bytes)")

    # Runtime at $8300
    for i, b in enumerate(RUNTIME_CODE):
        bus.fixed_memory[RUNTIME_ORG + i] = b
    print(f"  Runtime:   ${RUNTIME_ORG:04X}-${RUNTIME_ORG + len(RUNTIME_CODE):04X} ({len(RUNTIME_CODE)} bytes)")

    # Trap table at $8C00
    trap_table = create_trap_table()
    for i, b in enumerate(trap_table):
        bus.fixed_memory[0x8C00 + i] = b
    print(f"  Traps:     $8C00-$8FFF ({len(trap_table)} bytes)")

    # Debug: show trap entries
    print("\n  Trap table entries:")
    for name, rom_addr in [('COUT', 0xFDED), ('GETLN', 0xFD6A), ('HOME', 0xFC58)]:
        offset = rom_addr & 0x03FF
        mem_addr = 0x8C00 + offset
        lo = bus.fixed_memory[mem_addr]
        hi = bus.fixed_memory[mem_addr + 1]
        target = lo | (hi << 8)
        expected = LABELS.get(f'TRAP_{name}', 0)
        print(f"    {name} ${rom_addr:04X} -> ${mem_addr:04X}: ${target:04X} (expected ${expected:04X})")

    # Convert 6502 program to threaded code
    print("\nConverting 6502 program...")
    print(f"  Original: {len(TEST_6502)} bytes")
    for i, b in enumerate(TEST_6502):
        print(f"    ${0x0800+i:04X}: ${b:02X}")

    threaded = convert_6502_to_threaded(TEST_6502)
    print(f"  Threaded: {len(threaded)} bytes")

    # Load threaded code into bank 0 at $C000
    # (6502 $0800 -> Z80 $C000 + $0800*2 = $D000)
    # Actually for simplicity, put it at start of bank 0
    code_addr = 0xC000
    for i, b in enumerate(threaded):
        bus.banked_memory[0][i] = b
    print(f"  Loaded at: ${code_addr:04X} (bank 0)")

    # Initialize 6502 state
    bus.fixed_memory[0x8F00] = 0  # ZP_SYNCED = 0
    bus.fixed_memory[0x8F01] = 0  # STACK_SYNCED = 0
    bus.fixed_memory[0x8F02] = 0xFF  # REG_S = $FF (stack pointer)

    # Set up Z80 to start executing
    # SP = address of threaded code (for RET-threading)
    cpu.sp = code_addr

    # Select bank 0
    bus.current_bank = 0

    # Start with a RET to kick off threading
    # Put RET at $8200 and set PC there
    bus.fixed_memory[0x8200] = 0xC9  # RET
    cpu.pc = 0x8200

    print("\n" + "=" * 60)
    print("Running... (output below)")
    print("=" * 60)
    print()

    # Execute!
    max_cycles = 10000
    cycles = 0
    halted = False

    # Debug: trace first N instructions
    TRACE_COUNT = 50

    try:
        while cycles < max_cycles and not halted:
            # Check for HALT
            opcode = bus.read_mem(cpu.pc)
            if opcode == 0x76:  # HALT
                print(f"\n[HALT at ${cpu.pc:04X}]")
                halted = True
                break

            # Trace
            if cycles < TRACE_COUNT:
                # Get next few bytes
                b1 = bus.read_mem(cpu.pc + 1) if cpu.pc + 1 < 0x10000 else 0
                b2 = bus.read_mem(cpu.pc + 2) if cpu.pc + 2 < 0x10000 else 0
                stack_val = bus.read_mem(cpu.sp) | (bus.read_mem(cpu.sp + 1) << 8) if cpu.sp < 0xFFFE else 0
                print(f"[{cycles:3d}] PC=${cpu.pc:04X} ({opcode:02X} {b1:02X} {b2:02X}) SP=${cpu.sp:04X} [${stack_val:04X}] A=${cpu.a:02X} BC=${cpu.b:02X}{cpu.c:02X} DE={cpu.de:04X} HL={cpu.hl:04X}")

            # Step one instruction
            cpu.step()
            cycles += 1

    except Exception as e:
        print(f"\n[ERROR at cycle {cycles}: {e}]")
        print(f"  PC: ${cpu.pc:04X}, SP: ${cpu.sp:04X}")
        print(f"  A: ${cpu.a:02X}, B: ${cpu.b:02X}, C: ${cpu.c:02X}")
        import traceback
        traceback.print_exc()

    print("\n" + "=" * 60)
    print(f"Finished after {cycles} cycles")
    print(f"Output: {''.join(output)!r}")
    print("=" * 60)


if __name__ == '__main__':
    main()
