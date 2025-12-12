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

# Test program with actual subroutine (tests JSR/RTS with shadow stack)
# Main: JSR to PRINT_ABC, which prints "ABC" and returns
#
# ORG $0800
# main:
#         JSR print_abc   ; Call subroutine at $080A
#         LDA #$0A        ; newline
#         JSR $FDED       ; COUT
#         BRK
#
# print_abc: ($080A)
#         LDA #$41        ; 'A'
#         JSR $FDED
#         LDA #$42        ; 'B'
#         JSR $FDED
#         LDA #$43        ; 'C'
#         JSR $FDED
#         RTS

TEST_6502_SUBROUTINE = bytes([
    # main ($0800)
    0x20, 0x0A, 0x08, # JSR $080A (print_abc)
    0xA9, 0x0A,       # LDA #$0A (newline)
    0x20, 0xED, 0xFD, # JSR $FDED (COUT)
    0x00,             # BRK
    0x00,             # padding to $080A
    # print_abc ($080A)
    0xA9, 0x41,       # LDA #'A'
    0x20, 0xED, 0xFD, # JSR $FDED
    0xA9, 0x42,       # LDA #'B'
    0x20, 0xED, 0xFD, # JSR $FDED
    0xA9, 0x43,       # LDA #'C'
    0x20, 0xED, 0xFD, # JSR $FDED
    0x60,             # RTS
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

def run_test(test_code: bytes, test_name: str, trace_count: int = 50, verbose: bool = True):
    """Run a 6502 test program"""
    print("=" * 60)
    print(f"Test: {test_name}")
    print("=" * 60)

    # Create VZ80
    bus = VZ80Bus()
    cpu = Z80(bus)

    # Track output
    output = []
    def on_output(ch):
        output.append(ch)
        print(ch, end='', flush=True)
    bus.on_output = on_output

    # Load runtime components into fixed memory
    if verbose:
        print("\nLoading runtime...")

    # HLE traps at $6000
    for i, b in enumerate(HLE_CODE):
        bus.fixed_memory[HLE_ORG + i] = b

    # Opcode handlers at $6100
    for i, b in enumerate(HANDLERS_CODE):
        bus.fixed_memory[HANDLERS_ORG + i] = b

    # Handler table at $7000
    for i, b in enumerate(HANDLER_TABLE):
        bus.fixed_memory[TABLE_ORG + i] = b

    # Runtime at $8300
    for i, b in enumerate(RUNTIME_CODE):
        bus.fixed_memory[RUNTIME_ORG + i] = b

    # Trap table at $8C00
    trap_table = create_trap_table()
    for i, b in enumerate(trap_table):
        bus.fixed_memory[0x8C00 + i] = b

    if verbose:
        print(f"  Loaded runtime ({len(HLE_CODE) + len(HANDLERS_CODE) + len(HANDLER_TABLE) + len(RUNTIME_CODE) + len(trap_table)} bytes)")

    # Convert 6502 program to threaded code
    threaded = convert_6502_to_threaded(test_code)
    if verbose:
        print(f"  Converted {len(test_code)} -> {len(threaded)} bytes")

    # Load threaded code into bank 0
    # 6502 code starting at $0800 maps to Z80 $D000 in bank 0
    # Formula: Z80_addr = $C000 + (6502_addr & $1FFF) * 2
    # For $0800: Z80_addr = $C000 + $1000 = $D000
    code_offset = 0x0800 * 2  # = $1000
    code_addr = 0xC000 + code_offset  # = $D000
    for i, b in enumerate(threaded):
        bus.banked_memory[0][code_offset + i] = b

    # Initialize 6502 state
    bus.fixed_memory[0x8F00] = 0  # ZP_SYNCED = 0
    bus.fixed_memory[0x8F01] = 0  # STACK_SYNCED = 0
    bus.fixed_memory[0x8F02] = 0xFF  # REG_S = $FF (stack pointer)

    # Initialize shadow stack pointer (grows downward from $8EFE)
    bus.fixed_memory[0x8EFE] = 0xFE  # low byte
    bus.fixed_memory[0x8EFF] = 0x8E  # high byte -> $8EFE (top of stack)

    # Set up Z80 to start executing
    cpu.sp = code_addr
    bus.current_bank = 0

    # Start with a RET to kick off threading
    bus.fixed_memory[0x8200] = 0xC9  # RET
    cpu.pc = 0x8200

    print("\nRunning...")
    print()

    # Execute!
    max_cycles = 10000
    cycles = 0
    halted = False

    try:
        while cycles < max_cycles and not halted:
            opcode = bus.read_mem(cpu.pc)
            if opcode == 0x76:  # HALT
                halted = True
                break

            # Trace
            if cycles < trace_count:
                b1 = bus.read_mem(cpu.pc + 1) if cpu.pc + 1 < 0x10000 else 0
                b2 = bus.read_mem(cpu.pc + 2) if cpu.pc + 2 < 0x10000 else 0
                stack_val = bus.read_mem(cpu.sp) | (bus.read_mem(cpu.sp + 1) << 8) if cpu.sp < 0xFFFE else 0
                print(f"[{cycles:3d}] PC=${cpu.pc:04X} SP=${cpu.sp:04X} A=${cpu.a:02X}")

            cpu.step()
            cycles += 1

    except Exception as e:
        print(f"\n[ERROR at cycle {cycles}: {e}]")
        print(f"  PC: ${cpu.pc:04X}, SP: ${cpu.sp:04X}")
        import traceback
        traceback.print_exc()
        return None

    result = ''.join(output)
    print(f"\n[HALT after {cycles} cycles]")
    print(f"Output: {result!r}")
    print("=" * 60)
    return result


def main():
    # Test 1: Simple HI program (trap-only JSR)
    print("\n" + "=" * 60)
    print("TEST 1: Simple 'HI' program")
    print("=" * 60)
    result1 = run_test(TEST_6502, "Print HI", trace_count=0)
    assert result1 == "HI\n", f"Expected 'HI\\n', got {result1!r}"
    print("PASSED!")

    # Test 2: Subroutine test (JSR/RTS with shadow stack)
    print("\n" + "=" * 60)
    print("TEST 2: Subroutine test (JSR/RTS)")
    print("=" * 60)
    result2 = run_test(TEST_6502_SUBROUTINE, "Print ABC via subroutine", trace_count=100)
    expected2 = "ABC\n"
    if result2 == expected2:
        print("PASSED!")
    else:
        print(f"FAILED: Expected {expected2!r}, got {result2!r}")


if __name__ == '__main__':
    main()
