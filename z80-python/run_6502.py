#!/usr/bin/env python3
"""
6502 Emulator on Z80 Threaded Code - Runner

Usage:
    python3 run_6502.py                     # Run built-in Hello World
    python3 run_6502.py program.bin         # Load binary at $0800
    python3 run_6502.py program.bin -o 0x0800  # Load at specific address
    python3 run_6502.py -i                  # Interactive mode (echo program)

Apple II Memory Map:
    $0000-$00FF  Zero Page
    $0100-$01FF  Stack
    $0200-$02FF  Input buffer
    $0300-$03FF  Vectors
    $0800-$BFFF  User programs (default load address)
    $C000-$CFFF  I/O slots
    $D000-$FFFF  ROM (we trap these with HLE)

Key ROM Entry Points (trapped to native Z80):
    $FB39  SETTXT  - Set text mode
    $FC58  HOME    - Clear screen
    $FD0C  RDKEY   - Read key (blocking)
    $FD6A  GETLN   - Get line input
    $FDED  COUT    - Print character
    $FE89  SETKBD  - Reset input
    $FE93  SETVID  - Reset output
"""

import sys
import os
import argparse
import tty
import termios

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
# BUILT-IN TEST PROGRAMS
# =============================================================================

# Hello World - prints "Hello, 6502!" and exits
HELLO_WORLD_6502 = bytes([
    # Print "Hello, 6502!\n"
    0xA9, ord('H'),    0x20, 0xED, 0xFD,  # LDA #'H' / JSR COUT
    0xA9, ord('e'),    0x20, 0xED, 0xFD,
    0xA9, ord('l'),    0x20, 0xED, 0xFD,
    0xA9, ord('l'),    0x20, 0xED, 0xFD,
    0xA9, ord('o'),    0x20, 0xED, 0xFD,
    0xA9, ord(','),    0x20, 0xED, 0xFD,
    0xA9, ord(' '),    0x20, 0xED, 0xFD,
    0xA9, ord('6'),    0x20, 0xED, 0xFD,
    0xA9, ord('5'),    0x20, 0xED, 0xFD,
    0xA9, ord('0'),    0x20, 0xED, 0xFD,
    0xA9, ord('2'),    0x20, 0xED, 0xFD,
    0xA9, ord('!'),    0x20, 0xED, 0xFD,
    0xA9, 0x0A,        0x20, 0xED, 0xFD,  # newline
    0x00,  # BRK - halt
])

# Echo program - reads keys, echoes them, doubles digits, ESC to exit
# Note: RDKEY returns 0xFF when no input available (VZ80 behavior)
# Memory layout at $0800:
#   $0800: JSR $FD0C (3)  - loop:
#   $0803: CMP #$FF (2)   - check for "no input"
#   $0805: BEQ loop (2)   - wait if no input
#   $0807: CMP #$1B (2)   - ESC?
#   $0809: BEQ done (2)   - exit on ESC
#   $080B: JSR $FDED (3)  - echo character
#   $080E: CMP #$30 (2)   - below '0'?
#   $0810: BCC loop (2)   - not a digit
#   $0812: CMP #$3A (2)   - above '9'?
#   $0814: BCS loop (2)   - not a digit
#   $0816: JSR $FDED (3)  - print digit again (double!)
#   $0819: JMP $0800 (3)
#   $081C: LDA #$0A (2)   - done:
#   $081E: JSR $FDED (3)
#   $0821: BRK (1)
ECHO_6502 = bytes([
    # loop: ($0800)
    0x20, 0x0C, 0xFD,  # JSR $FD0C (RDKEY) - read key into A
    0xC9, 0xFF,        # CMP #$FF - no input? (VZ80 returns $FF when empty)
    0xF0, 0xF9,        # BEQ loop - offset -7 (wait for input)
    0xC9, 0x1B,        # CMP #$1B - ESC?
    0xF0, 0x11,        # BEQ done - offset +17 (exit on ESC)
    0x20, 0xED, 0xFD,  # JSR $FDED (COUT) - echo character
    0xC9, 0x30,        # CMP #'0'
    0x90, 0xEE,        # BCC loop - offset -18 (below '0', not a digit)
    0xC9, 0x3A,        # CMP #'9'+1
    0xB0, 0xEA,        # BCS loop - offset -22 (above '9', not a digit)
    0x20, 0xED, 0xFD,  # JSR $FDED - print digit again (double it!)
    0x4C, 0x00, 0x08,  # JMP loop
    # done: ($081C)
    0xA9, 0x0A,        # LDA #$0A - newline
    0x20, 0xED, 0xFD,  # JSR COUT
    0x00,              # BRK - halt
])

def _build_hello_name():
    """Build Hello Name program with inline strings (no ZP needed for strings)"""
    ORG = 0x0800
    code = bytearray()

    def here():
        return ORG + len(code)

    NAME_BUF = 0x00C0  # ZP for name buffer only

    # Placeholders for string addresses (will be filled after code)
    prompt_addr_pos = None
    hello_addr_pos = None

    # === Print prompt (inline string, absolute,X addressing) ===
    code.extend([0xA2, 0x00])              # LDX #0
    print_prompt = here()
    code.extend([0xBD, 0x00, 0x00])        # LDA $xxxx,X (placeholder)
    prompt_addr_pos = len(code) - 2
    code.extend([0xF0, 0x07])              # BEQ read_name (+7)
    code.extend([0x20, 0xED, 0xFD])        # JSR COUT
    code.extend([0xE8])                    # INX
    code.extend([0x4C, print_prompt & 0xFF, print_prompt >> 8])

    # === Read name ===
    read_name = here()
    code.extend([0xA2, 0x00])              # LDX #0
    read_loop = here()
    code.extend([0x20, 0x0C, 0xFD])        # JSR RDKEY
    code.extend([0xC9, 0xFF])              # CMP #$FF
    code.extend([0xF0, (read_loop - (here() + 2)) & 0xFF])  # BEQ read_loop
    code.extend([0xC9, 0x0D])              # CMP #$0D (CR)
    beq_got_name1 = len(code)
    code.extend([0xF0, 0x00])              # BEQ got_name
    code.extend([0xC9, 0x0A])              # CMP #$0A (LF)
    beq_got_name2 = len(code)
    code.extend([0xF0, 0x00])              # BEQ got_name
    code.extend([0x95, NAME_BUF])          # STA $C0,X (ZP buffer)
    code.extend([0x20, 0xED, 0xFD])        # JSR COUT (echo)
    code.extend([0xE8])                    # INX
    code.extend([0x4C, read_loop & 0xFF, read_loop >> 8])

    # got_name:
    got_name = here()
    code[beq_got_name1 + 1] = (got_name - (ORG + beq_got_name1 + 2)) & 0xFF
    code[beq_got_name2 + 1] = (got_name - (ORG + beq_got_name2 + 2)) & 0xFF

    code.extend([0xA9, 0x00])              # LDA #0
    code.extend([0x95, NAME_BUF])          # STA $C0,X (null terminate)
    code.extend([0xA9, 0x0A])              # LDA #$0A (newline)
    code.extend([0x20, 0xED, 0xFD])        # JSR COUT

    # === Print "Hello, " (inline string) ===
    code.extend([0xA2, 0x00])              # LDX #0
    print_hello = here()
    code.extend([0xBD, 0x00, 0x00])        # LDA $xxxx,X (placeholder)
    hello_addr_pos = len(code) - 2
    code.extend([0xF0, 0x07])              # BEQ print_name (+7)
    code.extend([0x20, 0xED, 0xFD])        # JSR COUT
    code.extend([0xE8])                    # INX
    code.extend([0x4C, print_hello & 0xFF, print_hello >> 8])

    # === Print name (from ZP buffer) ===
    print_name = here()
    code.extend([0xA2, 0x00])              # LDX #0
    print_name_loop = here()
    code.extend([0xB5, NAME_BUF])          # LDA $C0,X
    code.extend([0xF0, 0x07])              # BEQ done (+7)
    code.extend([0x20, 0xED, 0xFD])        # JSR COUT
    code.extend([0xE8])                    # INX
    code.extend([0x4C, print_name_loop & 0xFF, print_name_loop >> 8])

    # done:
    code.extend([0xA9, ord('!')])          # LDA #'!'
    code.extend([0x20, 0xED, 0xFD])        # JSR COUT
    code.extend([0xA9, 0x0A])              # LDA #$0A
    code.extend([0x20, 0xED, 0xFD])        # JSR COUT
    code.extend([0x00])                    # BRK

    # === Inline strings (right after code) ===
    PROMPT_ADDR = ORG + len(code)
    code.extend(b'What is your name? \x00')

    HELLO_ADDR = ORG + len(code)
    code.extend(b'Hello, \x00')

    # Fill in string addresses
    code[prompt_addr_pos] = PROMPT_ADDR & 0xFF
    code[prompt_addr_pos + 1] = PROMPT_ADDR >> 8
    code[hello_addr_pos] = HELLO_ADDR & 0xFF
    code[hello_addr_pos + 1] = HELLO_ADDR >> 8

    return bytes(code)

HELLO_NAME_6502 = _build_hello_name()

# Counter - counts 0-9 and prints
COUNTER_6502 = bytes([
    0xA2, 0x00,        # LDX #0
    # loop:
    0x8A,              # TXA
    0x18,              # CLC
    0x69, 0x30,        # ADC #'0'
    0x20, 0xED, 0xFD,  # JSR COUT
    0xE8,              # INX
    0xE0, 0x0A,        # CPX #10
    0xD0, 0xF4,        # BNE loop
    0xA9, 0x0A,        # LDA #$0A
    0x20, 0xED, 0xFD,  # JSR COUT
    0x00,              # BRK
])


# =============================================================================
# EMULATOR CLASS
# =============================================================================

class Emu6502Runner:
    """
    6502 Emulator running on Z80 via RET-threaded code translation.

    Architecture:
        6502 binary -> Converter -> Threaded code (2x expansion)
                                        |
                                        v
                        Z80 Runtime (native handlers)
                        + HLE Traps (COUT, RDKEY, etc.)
                                        |
                                        v
                                  VZ80 Emulator
    """

    # Apple II ROM trap addresses
    TRAPS = {
        0xFDED: 'TRAP_COUT',    # Character output
        0xFD0C: 'TRAP_RDKEY',   # Read key
        0xFD6A: 'TRAP_GETLN',   # Get line
        0xFC58: 'TRAP_HOME',    # Clear screen / home cursor
        # Microsoft BASIC I/O addresses
        0xFFF0: 'TRAP_COUT',    # CHAROUT - same as COUT
        0xFFF1: 'TRAP_RDKEY',   # CHARIN - same as RDKEY
    }

    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.bus = VZ80Bus()
        self.cpu = Z80(self.bus)
        self.output = []
        self.halted = False

        # Set up output callback
        self.bus.on_output = self._on_output

        # Load runtime
        self._load_runtime()

    def _log(self, msg: str):
        if self.verbose:
            print(f"[EMU] {msg}")

    def _on_output(self, ch: str):
        """Called when 6502 outputs a character via COUT"""
        self.output.append(ch)
        # Convert LF to CR+LF for proper terminal output in raw mode
        if ch == '\n':
            print('\r\n', end='', flush=True)
        else:
            print(ch, end='', flush=True)

    def _load_runtime(self):
        """Load Z80 runtime components into fixed memory"""
        self._log(f"Loading HLE handlers at ${HLE_ORG:04X} ({len(HLE_CODE)} bytes)")
        for i, b in enumerate(HLE_CODE):
            self.bus.fixed_memory[HLE_ORG + i] = b

        self._log(f"Loading opcode handlers at ${HANDLERS_ORG:04X} ({len(HANDLERS_CODE)} bytes)")
        for i, b in enumerate(HANDLERS_CODE):
            self.bus.fixed_memory[HANDLERS_ORG + i] = b

        self._log(f"Loading handler table at ${TABLE_ORG:04X} ({len(HANDLER_TABLE)} bytes)")
        for i, b in enumerate(HANDLER_TABLE):
            self.bus.fixed_memory[TABLE_ORG + i] = b

        self._log(f"Loading runtime at ${RUNTIME_ORG:04X} ({len(RUNTIME_CODE)} bytes)")
        for i, b in enumerate(RUNTIME_CODE):
            self.bus.fixed_memory[RUNTIME_ORG + i] = b

        # Set up trap table at $8C00 (for ROM addresses $FC00-$FFFF)
        self._setup_trap_table()

        # Initialize 6502 state
        self._init_6502_state()

    def _setup_trap_table(self):
        """
        Set up trap table at $8C00 for Apple II ROM calls.

        When 6502 does JSR $FDxx, the JSR handler checks if target >= $FC00.
        If so, it looks up the handler address in the trap table.

        Trap table layout: 1024 bytes covering $FC00-$FFFF
        Each entry is 2 bytes (handler address, little-endian)
        """
        self._log("Setting up trap table at $8C00")

        # Fill with TRAP_UNIMPL first
        unimpl = LABELS.get('TRAP_UNIMPL', 0x6050)
        for i in range(0, 1024, 2):
            self.bus.fixed_memory[0x8C00 + i] = unimpl & 0xFF
            self.bus.fixed_memory[0x8C00 + i + 1] = (unimpl >> 8) & 0xFF

        # Set up known traps
        for rom_addr, trap_name in self.TRAPS.items():
            handler = LABELS.get(trap_name)
            if handler:
                offset = rom_addr & 0x3FF
                self.bus.fixed_memory[0x8C00 + offset] = handler & 0xFF
                self.bus.fixed_memory[0x8C00 + offset + 1] = (handler >> 8) & 0xFF
                self._log(f"  ${rom_addr:04X} ({trap_name}) -> ${handler:04X}")

    def _init_6502_state(self):
        """Initialize 6502 CPU state in Z80 memory"""
        # 6502 registers stored at $8F00-$8F05
        self.bus.fixed_memory[0x8F00] = 0     # REG_A
        self.bus.fixed_memory[0x8F01] = 0     # REG_X
        self.bus.fixed_memory[0x8F02] = 0xFF  # REG_S (stack pointer)
        self.bus.fixed_memory[0x8F03] = 0     # REG_Y
        self.bus.fixed_memory[0x8F04] = 0     # REG_P (flags)

        # Shadow stack pointer at $8EFE-$8EFF (grows downward from $8EFE)
        self.bus.fixed_memory[0x8EFE] = 0xFE
        self.bus.fixed_memory[0x8EFF] = 0x8E

        # Entry point: RET instruction at $8200 to start threading
        self.bus.fixed_memory[0x8200] = 0xC9  # Z80 RET

    def convert_to_threaded(self, code: bytes) -> bytes:
        """
        Convert 6502 binary to RET-threaded code.

        Each 6502 byte becomes a 2-byte handler address:
            handler_addr = (0x70 | (byte & 0x0F)) << 8 | byte

        This gives addresses like $70A9 for LDA #imm ($A9),
        which points into the 16-way interleaved handler table at $7000.
        """
        result = bytearray()
        for byte in code:
            # Handler address in table at $7000
            handler_high = 0x70 | (byte & 0x0F)
            result.append(byte)           # low byte
            result.append(handler_high)   # high byte
        return bytes(result)

    def load_program(self, code: bytes, org: int = 0x0800):
        """
        Load 6502 program into emulator.

        Args:
            code: Raw 6502 binary
            org: Load address in 6502 memory space (default $0800)
        """
        self._log(f"Loading {len(code)} bytes at ${org:04X}")

        # Convert to threaded code
        threaded = self.convert_to_threaded(code)
        self._log(f"Converted to {len(threaded)} bytes of threaded code")

        # Load threaded code into correct banks
        # Each bank covers 8KB of 6502 address space (16KB Z80 / 2)
        # Bank 0: $0000-$1FFF, Bank 1: $2000-$3FFF, etc.
        for i in range(0, len(code)):
            addr_6502 = org + i
            bank = addr_6502 >> 13           # Bank = addr / 8192
            offset_in_bank = addr_6502 & 0x1FFF  # Offset within 8KB range
            z80_offset = offset_in_bank * 2  # Threaded code is 2x

            # Write the two bytes of threaded code
            if bank < len(self.bus.banked_memory) and z80_offset + 1 < len(self.bus.banked_memory[bank]):
                self.bus.banked_memory[bank][z80_offset] = threaded[i * 2]
                self.bus.banked_memory[bank][z80_offset + 1] = threaded[i * 2 + 1]

        # Set up Z80 to start execution
        # Start address is in bank (org >> 13), offset (org & 0x1FFF) * 2
        start_bank = org >> 13
        start_offset = (org & 0x1FFF) * 2
        self.start_addr = 0xC000 + start_offset
        self.start_bank = start_bank
        self._log(f"Start address: Z80 ${self.start_addr:04X} in bank {start_bank}")

    def run(self, max_cycles: int = 1_000_000) -> str:
        """
        Run the loaded program until BRK (halt) or max cycles.

        Returns:
            Output string from program
        """
        self.output.clear()
        self.halted = False

        # Set up Z80 initial state
        self.cpu.sp = self.start_addr
        self.cpu.pc = 0x8200  # Points to RET instruction to start threading
        self.bus.current_bank = getattr(self, 'start_bank', 0)

        # Initialize CODE_BANK_ADDR ($8F04) with starting bank
        CODE_BANK_ADDR = 0x8F04
        self.bus.write_mem(CODE_BANK_ADDR, self.bus.current_bank)

        self._log(f"Starting execution: PC=${self.cpu.pc:04X} SP=${self.cpu.sp:04X}")

        cycles = 0
        while cycles < max_cycles and not self.halted:
            # Check for HALT (BRK handler issues Z80 HALT)
            if self.bus.read_mem(self.cpu.pc) == 0x76:
                self.halted = True
                break

            self.cpu.step()
            cycles += 1

        self._log(f"Halted after {cycles} cycles")
        return ''.join(self.output)

    def run_interactive(self, code: bytes, org: int = 0x0800):
        """
        Run program with interactive terminal I/O.

        Uses raw terminal mode for character-by-character input.
        ESC key exits the program.
        """
        self.load_program(code, org)
        self.run_interactive_loaded()

    def run_interactive_loaded(self):
        """
        Run already-loaded program with interactive terminal I/O.

        Call load_program() first, then this method.
        """
        # Set up raw terminal input
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)

        import select

        def check_input_ready():
            """Check if stdin has data available (non-blocking)"""
            r, _, _ = select.select([fd], [], [], 0)
            return len(r) > 0

        def read_key():
            """Read single key from terminal (blocking)"""
            try:
                ch = os.read(fd, 1)
                if ch:
                    c = ch[0]
                    # Convert Enter/LF to CR for 6502
                    if c == 10 or c == 13:
                        return 0x0D
                    return c
            except:
                pass
            return 0xFF  # No input

        try:
            tty.setraw(fd)

            # Set up input with status checking
            self.bus.on_input = read_key
            self.bus.check_input_ready = check_input_ready
            self.run(max_cycles=100_000_000)

        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
            print()  # Newline after raw mode


# =============================================================================
# MAIN
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        description='6502 Emulator on Z80 Threaded Code',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s                      Run built-in Hello World
  %(prog)s -e                   Run built-in Echo program (interactive)
  %(prog)s -c                   Run built-in Counter (0-9)
  %(prog)s program.bin          Load and run binary at $0800
  %(prog)s program.bin -o 0x300 Load at $0300
  %(prog)s -i input.txt prog.bin  Run with input from file
  %(prog)s msbasic.bin -E 0x2730 -I  Run MS BASIC (entry at $2730)

Apple II ROM traps supported:
  $FDED  COUT   - Print character (A register)
  $FD0C  RDKEY  - Read key into A (blocking)
  $FD6A  GETLN  - Read line into $0200
  $FC58  HOME   - Clear screen
        """
    )

    parser.add_argument('binary', nargs='?', help='6502 binary file to run')
    parser.add_argument('-o', '--org', default='0x0800',
                        help='Load address in hex (default: 0x0800)')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Verbose output')
    parser.add_argument('-e', '--echo', action='store_true',
                        help='Run built-in echo program (interactive)')
    parser.add_argument('-c', '--counter', action='store_true',
                        help='Run built-in counter program')
    parser.add_argument('-n', '--name', action='store_true',
                        help='Run built-in "Hello Name" program (interactive)')
    parser.add_argument('-i', '--input', metavar='FILE',
                        help='Read input from file instead of keyboard')
    parser.add_argument('-I', '--interactive', action='store_true',
                        help='Run in interactive mode (for programs that need keyboard input)')
    parser.add_argument('-E', '--entry', metavar='ADDR',
                        help='Entry point address (default: same as load address)')

    args = parser.parse_args()

    # Parse origin address
    org = int(args.org, 0)  # Auto-detect hex/decimal

    # Parse entry point (defaults to org if not specified)
    entry = int(args.entry, 0) if args.entry else None

    # Create emulator
    emu = Emu6502Runner(verbose=args.verbose)

    # Determine what program to run
    if args.binary:
        # Load binary file
        if not os.path.exists(args.binary):
            print(f"Error: File not found: {args.binary}", file=sys.stderr)
            sys.exit(1)

        with open(args.binary, 'rb') as f:
            code = f.read()

        print(f"Loaded {len(code)} bytes from {args.binary}")
        print(f"Running from ${org:04X}...")
        print("-" * 40)
        interactive = args.interactive

    elif args.echo:
        code = ECHO_6502
        print("6502 Echo Program")
        print("Type characters - digits are doubled, ESC/Ctrl-C to exit")
        print("-" * 40)
        interactive = not args.input  # Interactive unless input file provided

    elif args.counter:
        code = COUNTER_6502
        print("6502 Counter Program")
        print("-" * 40)
        interactive = False

    elif args.name:
        code = HELLO_NAME_6502
        print("6502 Hello Name Program")
        print("Enter your name and press Enter")
        print("-" * 40)
        interactive = not args.input

    else:
        # Default: Hello World
        code = HELLO_WORLD_6502
        print("6502 Hello World")
        print("-" * 40)
        interactive = False

    # Helper to apply entry point override
    def apply_entry_override(emu, entry_addr):
        if entry_addr is not None:
            emu.start_bank = entry_addr >> 13
            emu.start_addr = 0xC000 + ((entry_addr & 0x1FFF) * 2)
            print(f"Entry point: ${entry_addr:04X}")

    # Handle input source
    if args.input:
        # Read input from file - overrides interactive mode
        with open(args.input, 'rb') as f:
            emu.bus.input_buffer = f.read().decode('latin-1')
        emu.load_program(code, org)
        apply_entry_override(emu, entry)
        emu.run()
    elif interactive:
        # Interactive mode for echo program
        emu.load_program(code, org)
        apply_entry_override(emu, entry)
        emu.run_interactive_loaded()
    else:
        # Non-interactive run
        emu.load_program(code, org)
        apply_entry_override(emu, entry)
        emu.run()

    print("-" * 40)
    print("Done.")


if __name__ == '__main__':
    main()
