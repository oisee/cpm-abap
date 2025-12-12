"""
VZ80 - Virtual Z80 Machine
Implementation based on 6502 Emulator Design Report

Hardware Specification:
- CPU: Z80
- Fixed RAM: $0000-$BFFF (48 KB, always mapped)
- Banked RAM: $C000-$FFFF (16 KB window, 256 pages = 4 MB total)

I/O Ports:
- Port $00: Bank Select (0-255)
- Port $01: Console I/O (blocking)
"""

from z80 import Z80, Bus, FLAG_Z, FLAG_S, FLAG_C


class VZ80Bus(Bus):
    """
    Virtual Z80 Bus with banked memory and console I/O

    Memory Map:
        $0000-$BFFF: Fixed RAM (48 KB)
        $C000-$FFFF: Banked RAM (16 KB window into 4 MB)

    I/O Ports:
        $00: Bank select (read/write current page 0-255)
        $01: Console I/O (write=output char, read=input char)
    """

    BANK_SIZE = 16384  # 16 KB per bank
    NUM_BANKS = 256    # 256 banks = 4 MB total banked memory

    def __init__(self):
        # Don't call super().__init__() - we manage our own memory

        # Fixed memory: $0000-$BFFF (48 KB)
        self.fixed_memory = bytearray(0xC000)

        # Banked memory: 256 pages x 16 KB = 4 MB
        self.banked_memory = [bytearray(self.BANK_SIZE) for _ in range(self.NUM_BANKS)]

        # Current bank (port $00)
        self.current_bank = 0

        # Console I/O buffers
        self.input_buffer = ""
        self.output_buffer = ""

        # Callback for real-time I/O (optional)
        self.on_output = None  # Called with character when output occurs
        self.on_input = None   # Called to request input (should return char or None)

    def read_mem(self, addr: int) -> int:
        """Read byte from memory"""
        addr = addr & 0xFFFF

        if addr < 0xC000:
            # Fixed memory
            return self.fixed_memory[addr]
        else:
            # Banked memory
            offset = addr - 0xC000
            return self.banked_memory[self.current_bank][offset]

    def write_mem(self, addr: int, val: int):
        """Write byte to memory"""
        addr = addr & 0xFFFF
        val = val & 0xFF

        if addr < 0xC000:
            # Fixed memory
            self.fixed_memory[addr] = val
        else:
            # Banked memory
            offset = addr - 0xC000
            self.banked_memory[self.current_bank][offset] = val

    def read_io(self, port: int) -> int:
        """Read from I/O port"""
        port = port & 0xFF

        if port == 0x00:
            # Bank select - return current bank
            return self.current_bank

        elif port == 0x01:
            # Console input
            if self.on_input:
                # Use callback for real-time input
                ch = self.on_input()
                return ch if ch is not None else 0xFF
            elif self.input_buffer:
                # Take from buffer
                ch = ord(self.input_buffer[0])
                self.input_buffer = self.input_buffer[1:]
                return ch
            else:
                # No input available
                return 0xFF

        return 0xFF

    def write_io(self, port: int, val: int):
        """Write to I/O port"""
        port = port & 0xFF
        val = val & 0xFF

        if port == 0x00:
            # Bank select
            self.current_bank = val

        elif port == 0x01:
            # Console output
            if self.on_output:
                self.on_output(chr(val))
            else:
                self.output_buffer += chr(val)

    def load(self, addr: int, data: bytes):
        """Load data into memory starting at addr"""
        for i, b in enumerate(data):
            self.write_mem(addr + i, b)

    def load_bank(self, bank: int, data: bytes, offset: int = 0):
        """Load data directly into a specific bank"""
        if 0 <= bank < self.NUM_BANKS:
            for i, b in enumerate(data):
                if offset + i < self.BANK_SIZE:
                    self.banked_memory[bank][offset + i] = b

    def provide_input(self, text: str):
        """Add text to input buffer"""
        self.input_buffer += text

    def get_output(self) -> str:
        """Get accumulated output"""
        return self.output_buffer

    def clear_output(self):
        """Clear output buffer"""
        self.output_buffer = ""

    def get_total_memory(self) -> int:
        """Return total memory size in bytes"""
        return len(self.fixed_memory) + (self.NUM_BANKS * self.BANK_SIZE)


class VZ80:
    """
    Virtual Z80 Machine

    A complete virtual computer with:
    - Z80 CPU
    - 48 KB fixed RAM + 4 MB banked RAM
    - Simple 2-port I/O for bank switching and console
    """

    def __init__(self):
        self.bus = VZ80Bus()
        self.cpu = Z80(self.bus)
        self.running = False
        self.debug = False

    def reset(self):
        """Reset the machine"""
        self.cpu.reset()
        self.cpu.pc = 0x0000
        self.cpu.sp = 0xBFFF  # Stack at top of fixed memory
        self.bus.current_bank = 0
        self.running = True

    def load(self, addr: int, data: bytes):
        """Load program/data into memory"""
        self.bus.load(addr, data)

    def load_code(self, data: bytes, org: int = 0x0100):
        """Load code at specified origin"""
        self.bus.load(org, data)
        self.cpu.pc = org

    def step(self) -> int:
        """Execute single instruction, return cycles"""
        if self.cpu.halted:
            self.running = False
            return 4

        cycles = self.cpu.step()

        if self.debug:
            self._print_state()

        return cycles

    def run(self, max_cycles: int = 10_000_000) -> str:
        """Run until HALT or max cycles"""
        self.bus.clear_output()
        total_cycles = 0

        while self.running and total_cycles < max_cycles:
            cycles = self.step()
            total_cycles += cycles

            if self.cpu.halted:
                break

        return self.bus.get_output()

    def run_interactive(self):
        """Run with interactive console I/O"""
        import sys

        def output_char(ch):
            sys.stdout.write(ch)
            sys.stdout.flush()

        def input_char():
            # Non-blocking check would be better, but for simple test:
            if self.bus.input_buffer:
                ch = ord(self.bus.input_buffer[0])
                self.bus.input_buffer = self.bus.input_buffer[1:]
                return ch
            return 0xFF

        self.bus.on_output = output_char
        self.bus.on_input = input_char

        self.run()

    def _print_state(self):
        """Print CPU state for debugging"""
        cpu = self.cpu
        print(f"PC={cpu.pc:04X} SP={cpu.sp:04X} "
              f"AF={cpu.af:04X} BC={cpu.bc:04X} "
              f"DE={cpu.de:04X} HL={cpu.hl:04X} "
              f"Bank={self.bus.current_bank}")

    # Convenience properties
    @property
    def a(self): return self.cpu.a

    @property
    def bc(self): return self.cpu.bc

    @property
    def de(self): return self.cpu.de

    @property
    def hl(self): return self.cpu.hl

    @property
    def pc(self): return self.cpu.pc

    @property
    def sp(self): return self.cpu.sp

    @property
    def bank(self): return self.bus.current_bank


def assemble_simple(source: str) -> bytes:
    """
    Very simple Z80 assembler for testing
    Supports: LD, OUT, IN, HALT, JR, DJNZ, INC, DEC, ADD, CP
    """
    opcodes = {
        # LD r,n
        'LD A,': lambda n: bytes([0x3E, n]),
        'LD B,': lambda n: bytes([0x06, n]),
        'LD C,': lambda n: bytes([0x0E, n]),
        'LD D,': lambda n: bytes([0x16, n]),
        'LD E,': lambda n: bytes([0x1E, n]),
        'LD H,': lambda n: bytes([0x26, n]),
        'LD L,': lambda n: bytes([0x2E, n]),

        # LD rr,nn
        'LD BC,': lambda n: bytes([0x01, n & 0xFF, (n >> 8) & 0xFF]),
        'LD DE,': lambda n: bytes([0x11, n & 0xFF, (n >> 8) & 0xFF]),
        'LD HL,': lambda n: bytes([0x21, n & 0xFF, (n >> 8) & 0xFF]),
        'LD SP,': lambda n: bytes([0x31, n & 0xFF, (n >> 8) & 0xFF]),

        # I/O
        'OUT (': lambda p: bytes([0xD3, p]),  # OUT (n),A
        'IN A,(': lambda p: bytes([0xDB, p]), # IN A,(n)

        # Control
        'HALT': lambda: bytes([0x76]),
        'NOP': lambda: bytes([0x00]),
        'RET': lambda: bytes([0xC9]),

        # Jumps
        'JP ': lambda n: bytes([0xC3, n & 0xFF, (n >> 8) & 0xFF]),
        'JR ': lambda n: bytes([0x18, n & 0xFF]),
        'DJNZ ': lambda n: bytes([0x10, n & 0xFF]),

        # ALU
        'INC A': lambda: bytes([0x3C]),
        'INC B': lambda: bytes([0x04]),
        'DEC B': lambda: bytes([0x05]),
        'CP ': lambda n: bytes([0xFE, n]),
        'ADD A,': lambda n: bytes([0xC6, n]),
        'SUB ': lambda n: bytes([0xD6, n]),
    }

    result = bytearray()

    for line in source.strip().split('\n'):
        line = line.strip()
        if not line or line.startswith(';'):
            continue

        # Remove comments
        if ';' in line:
            line = line[:line.index(';')].strip()

        matched = False
        for pattern, gen in opcodes.items():
            if line.startswith(pattern):
                if pattern in ['HALT', 'NOP', 'RET', 'INC A', 'INC B', 'DEC B']:
                    result.extend(gen())
                else:
                    # Extract operand
                    rest = line[len(pattern):].rstrip(')')
                    # Parse number (hex or decimal)
                    if rest.startswith('$') or rest.startswith('0x'):
                        num = int(rest.replace('$', '0x'), 16)
                    else:
                        num = int(rest)
                    result.extend(gen(num))
                matched = True
                break

        if not matched:
            raise ValueError(f"Unknown instruction: {line}")

    return bytes(result)


# Test programs
def test_hello_world():
    """Test: Print 'Hello!' via port $01"""
    vm = VZ80()
    vm.reset()

    # Manual assembly: print "Hello!" and halt
    program = bytes([
        # Print 'H'
        0x3E, ord('H'),      # LD A, 'H'
        0xD3, 0x01,          # OUT ($01), A
        # Print 'e'
        0x3E, ord('e'),      # LD A, 'e'
        0xD3, 0x01,          # OUT ($01), A
        # Print 'l'
        0x3E, ord('l'),      # LD A, 'l'
        0xD3, 0x01,          # OUT ($01), A
        # Print 'l'
        0x3E, ord('l'),      # LD A, 'l'
        0xD3, 0x01,          # OUT ($01), A
        # Print 'o'
        0x3E, ord('o'),      # LD A, 'o'
        0xD3, 0x01,          # OUT ($01), A
        # Print '!'
        0x3E, ord('!'),      # LD A, '!'
        0xD3, 0x01,          # OUT ($01), A
        # Print newline
        0x3E, 0x0A,          # LD A, 10
        0xD3, 0x01,          # OUT ($01), A
        # HALT
        0x76                 # HALT
    ])

    vm.load_code(program, org=0x0100)
    output = vm.run()

    print(f"Output: {repr(output)}")
    assert output == "Hello!\n", f"Expected 'Hello!\\n', got {repr(output)}"
    print("PASS: Hello World test")
    return True


def test_bank_switching():
    """Test: Write to different banks and verify"""
    vm = VZ80()
    vm.reset()

    # Write $AA to bank 0, $BB to bank 1, read back
    program = bytes([
        # Select bank 0
        0x3E, 0x00,          # LD A, 0
        0xD3, 0x00,          # OUT ($00), A  ; select bank 0

        # Write $AA to $C000
        0x3E, 0xAA,          # LD A, $AA
        0x21, 0x00, 0xC0,    # LD HL, $C000
        0x77,                # LD (HL), A

        # Select bank 1
        0x3E, 0x01,          # LD A, 1
        0xD3, 0x00,          # OUT ($00), A  ; select bank 1

        # Write $BB to $C000 (now in bank 1)
        0x3E, 0xBB,          # LD A, $BB
        0x77,                # LD (HL), A

        # Verify bank 1 has $BB
        0x7E,                # LD A, (HL)
        0xFE, 0xBB,          # CP $BB
        0x20, 0x05,          # JR NZ, fail

        # Switch back to bank 0 and verify $AA
        0x3E, 0x00,          # LD A, 0
        0xD3, 0x00,          # OUT ($00), A
        0x7E,                # LD A, (HL)
        0xFE, 0xAA,          # CP $AA
        0x20, 0x05,          # JR NZ, fail

        # Success - print 'OK'
        0x3E, ord('O'),      # LD A, 'O'
        0xD3, 0x01,          # OUT ($01), A
        0x3E, ord('K'),      # LD A, 'K'
        0xD3, 0x01,          # OUT ($01), A
        0x76,                # HALT

        # Fail - print 'FAIL'
        0x3E, ord('F'),      # LD A, 'F'
        0xD3, 0x01,          # OUT ($01), A
        0x76,                # HALT
    ])

    vm.load_code(program, org=0x0100)
    output = vm.run()

    print(f"Output: {repr(output)}")
    assert output == "OK", f"Expected 'OK', got {repr(output)}"
    print("PASS: Bank switching test")
    return True


def test_counter():
    """Test: Count from 0 to 9 and print digits"""
    vm = VZ80()
    vm.reset()

    # Count 0-9, print each digit
    program = bytes([
        0x06, 0x0A,          # LD B, 10       ; counter
        0x3E, ord('0'),      # LD A, '0'      ; start digit

        # loop:
        0xD3, 0x01,          # OUT ($01), A   ; print digit
        0x3C,                # INC A          ; next digit
        0x10, 0xFB,          # DJNZ loop      ; -5 bytes back

        # Print newline and halt
        0x3E, 0x0A,          # LD A, 10
        0xD3, 0x01,          # OUT ($01), A
        0x76,                # HALT
    ])

    vm.load_code(program, org=0x0100)
    output = vm.run()

    print(f"Output: {repr(output)}")
    assert output == "0123456789\n", f"Expected '0123456789\\n', got {repr(output)}"
    print("PASS: Counter test")
    return True


def test_input_echo():
    """Test: Echo input character"""
    vm = VZ80()
    vm.reset()

    # Pre-load input
    vm.bus.provide_input("ABC")

    # Read 3 chars, echo them back
    # Address layout:
    #   0-1: LD B,3
    #   2-3: IN A,($01)   <- loop
    #   4-5: CP $FF
    #   6-7: JR Z,loop    (target=2, PC_after=8, offset=2-8=-6=0xFA)
    #   8-9: OUT ($01),A
    #   10-11: DJNZ loop  (target=2, PC_after=12, offset=2-12=-10=0xF6)
    #   12: HALT
    program = bytes([
        0x06, 0x03,          # LD B, 3        ; 3 characters

        # loop:
        0xDB, 0x01,          # IN A, ($01)    ; read char
        0xFE, 0xFF,          # CP $FF         ; no input?
        0x28, 0xFA,          # JR Z, loop     ; wait for input (-6)
        0xD3, 0x01,          # OUT ($01), A   ; echo
        0x10, 0xF6,          # DJNZ loop      ; (-10)

        0x76,                # HALT
    ])

    vm.load_code(program, org=0x0100)
    output = vm.run()

    print(f"Output: {repr(output)}")
    assert output == "ABC", f"Expected 'ABC', got {repr(output)}"
    print("PASS: Input echo test")
    return True


def run_all_tests():
    """Run all VZ80 tests"""
    print("=" * 50)
    print("VZ80 Virtual Machine Tests")
    print("=" * 50)

    total_memory = VZ80Bus().get_total_memory()
    print(f"Total memory: {total_memory:,} bytes ({total_memory // 1024 // 1024} MB + {(total_memory % (1024*1024)) // 1024} KB)")
    print()

    tests = [
        ("Hello World", test_hello_world),
        ("Bank Switching", test_bank_switching),
        ("Counter", test_counter),
        ("Input Echo", test_input_echo),
    ]

    passed = 0
    failed = 0

    for name, test_func in tests:
        print(f"\n--- {name} ---")
        try:
            if test_func():
                passed += 1
        except Exception as e:
            print(f"FAIL: {e}")
            failed += 1

    print("\n" + "=" * 50)
    print(f"Results: {passed} passed, {failed} failed")
    print("=" * 50)

    return failed == 0


if __name__ == "__main__":
    run_all_tests()
