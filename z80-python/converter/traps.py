"""
HLE Trap Mapping for Apple II ROM Calls

Instead of emulating the entire Apple II ROM, we intercept
specific ROM calls and handle them natively.

Trap addresses are mapped to page $00 (Zero Page handlers).
When 6502 code calls JSR $FDED, we emit handler for $00xx
which triggers our trap handler instead.

Memory Layout:
  $0000-$00FF in 6502 space = Zero Page
  Handlers $7000-$70FF = Zero Page handler space

We reserve some Zero Page addresses for traps:
  $00 = BRK (already used)
  $01 = TRAP_COUT      (Apple II $FDED)
  $02 = TRAP_GETLN     (Apple II $FD6A)
  $03 = TRAP_RDKEY     (Apple II $FD0C)
  $04 = TRAP_HOME      (Apple II $FC58)
  ...
  $0F = TRAP_EXIT      (clean exit)
"""

from dataclasses import dataclass


@dataclass
class Trap:
    """ROM call trap definition"""
    name: str           # Human-readable name
    rom_addr: int       # Original ROM address (e.g., $FDED)
    trap_num: int       # Trap number ($01-$0F)
    description: str    # What it does


# Apple II ROM traps
APPLE2_TRAPS = {
    0xFDED: Trap("COUT", 0xFDED, 0x01, "Output character from A register"),
    0xFD6A: Trap("GETLN", 0xFD6A, 0x02, "Read line into $0200, return length in X"),
    0xFD0C: Trap("RDKEY", 0xFD0C, 0x03, "Read single character, return in A"),
    0xFC58: Trap("HOME", 0xFC58, 0x04, "Clear screen"),
    0xFCA8: Trap("WAIT", 0xFCA8, 0x05, "Delay (A * 2.5 cycles)"),
    0xFE2C: Trap("MOVE", 0xFE2C, 0x06, "Block memory move"),
    0xFE84: Trap("SETINV", 0xFE84, 0x07, "Set inverse text mode"),
    0xFE80: Trap("SETNORM", 0xFE80, 0x08, "Set normal text mode"),
}

# Special trap numbers
TRAP_BRK = 0x00     # BRK instruction
TRAP_EXIT = 0x0F    # Clean program exit


def get_trap(rom_addr: int) -> Trap | None:
    """Get trap info for ROM address"""
    return APPLE2_TRAPS.get(rom_addr)


def is_trap_address(addr: int) -> bool:
    """Check if address should be trapped"""
    return addr in APPLE2_TRAPS


def rom_to_trap_addr(rom_addr: int) -> int | None:
    """Convert ROM address to trap address ($00xx)"""
    trap = APPLE2_TRAPS.get(rom_addr)
    if trap:
        return trap.trap_num
    return None


def trap_to_handler(trap_num: int) -> int:
    """Get Z80 handler address for trap number"""
    # Trap $01 → handler $7001
    # Uses same formula: ($70 | (trap & $0F)) << 8 | trap
    # But since trap < $10, high byte is always $70
    return 0x7000 | trap_num


# Display trap table
if __name__ == "__main__":
    print("Apple II ROM Trap Table")
    print("=" * 60)
    print(f"{'ROM Addr':10s} {'Trap':6s} {'Handler':8s} {'Name':10s} {'Description'}")
    print("-" * 60)

    for rom_addr, trap in sorted(APPLE2_TRAPS.items()):
        handler = trap_to_handler(trap.trap_num)
        print(f"${rom_addr:04X}      ${trap.trap_num:02X}    ${handler:04X}     {trap.name:10s} {trap.description}")

    print()
    print("Special traps:")
    print(f"  $00 = BRK (break/exit)")
    print(f"  $0F = EXIT (clean exit)")
