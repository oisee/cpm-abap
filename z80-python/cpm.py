"""
CP/M Emulator using Z80 CPU
Implements BDOS console functions for running CP/M programs
"""

from z80 import Z80, Bus


class CPMBus(Bus):
    """CP/M-aware bus with BDOS call interception"""

    def __init__(self):
        super().__init__()
        self.bdos_intercept = True

    def setup_cpm(self):
        """Set up CP/M memory map"""
        # Warm boot at 0000h - JP to BIOS (we'll intercept)
        self.memory[0x0000] = 0xC3  # JP
        self.memory[0x0001] = 0x00
        self.memory[0x0002] = 0xFF  # JP FF00 (BIOS warm boot)

        # BDOS entry at 0005h - JP to BDOS handler
        self.memory[0x0005] = 0xC3  # JP
        self.memory[0x0006] = 0x00
        self.memory[0x0007] = 0xFE  # JP FE00 (BDOS entry)

        # BDOS/BIOS stubs - just HALT so we can intercept
        self.memory[0xFE00] = 0x76  # HALT at BDOS entry
        self.memory[0xFF00] = 0x76  # HALT at BIOS warm boot


class CPM:
    """CP/M 2.2 Emulator"""

    # BDOS function numbers
    BDOS_SYSTEM_RESET = 0
    BDOS_CONSOLE_INPUT = 1
    BDOS_CONSOLE_OUTPUT = 2
    BDOS_DIRECT_IO = 6
    BDOS_PRINT_STRING = 9
    BDOS_READ_BUFFER = 10
    BDOS_CONSOLE_STATUS = 11
    BDOS_VERSION = 12

    def __init__(self):
        self.bus = CPMBus()
        self.cpu = Z80(self.bus)
        self.running = True
        self.debug = False

    def reset(self):
        """Reset CP/M system"""
        self.bus.setup_cpm()
        self.cpu.reset()
        self.cpu.pc = 0x0100  # TPA start
        self.cpu.sp = 0xFFFF
        self.running = True

    def load_program(self, data: bytes, addr: int = 0x0100):
        """Load program at address (default TPA at 0100h)"""
        self.bus.load(addr, data)

    def load_file(self, filename: str, addr: int = 0x0100):
        """Load binary file as CP/M program"""
        with open(filename, 'rb') as f:
            data = f.read()
        self.load_program(data, addr)
        return len(data)

    def bdos_call(self, func: int) -> bool:
        """Handle BDOS function call. Returns True to continue, False to exit."""

        if self.debug:
            print(f"[BDOS] Function {func}, DE={self.cpu.de:04X}")

        if func == self.BDOS_SYSTEM_RESET:
            # Warm boot - exit program
            return False

        elif func == self.BDOS_CONSOLE_INPUT:
            # Read character from console
            ch = self._console_input()
            self.cpu.a = ch
            self.cpu.l = ch  # Also returned in L

        elif func == self.BDOS_CONSOLE_OUTPUT:
            # Output character in E
            ch = self.cpu.e
            self._console_output(ch)

        elif func == self.BDOS_DIRECT_IO:
            # Direct console I/O
            if self.cpu.e == 0xFF:
                # Input request
                if self.bus.input_buffer:
                    ch = ord(self.bus.input_buffer[0])
                    self.bus.input_buffer = self.bus.input_buffer[1:]
                    self.cpu.a = ch
                else:
                    self.cpu.a = 0  # No character available
            else:
                # Output character
                self._console_output(self.cpu.e)

        elif func == self.BDOS_PRINT_STRING:
            # Print string at DE until '$'
            addr = self.cpu.de
            while True:
                ch = self.bus.read_mem(addr)
                if ch == 0x24:  # '$' terminator
                    break
                self._console_output(ch)
                addr += 1
                if addr > 0xFFFF:
                    break

        elif func == self.BDOS_READ_BUFFER:
            # Read console buffer
            # Buffer format: [max_len][actual_len][chars...]
            buf_addr = self.cpu.de
            max_len = self.bus.read_mem(buf_addr)
            if max_len == 0:
                max_len = 128

            # Read line from input
            line = self._read_line(max_len)

            # Store in buffer
            self.bus.write_mem(buf_addr + 1, len(line))
            for i, ch in enumerate(line):
                self.bus.write_mem(buf_addr + 2 + i, ord(ch))

        elif func == self.BDOS_CONSOLE_STATUS:
            # Return FF if character available, 0 otherwise
            if self.bus.input_buffer:
                self.cpu.a = 0xFF
            else:
                self.cpu.a = 0

        elif func == self.BDOS_VERSION:
            # Return CP/M version 2.2
            self.cpu.hl = 0x0022
            self.cpu.a = 0x22

        else:
            if self.debug:
                print(f"[BDOS] Unimplemented function {func}")
            self.cpu.a = 0

        return True

    def _console_input(self) -> int:
        """Read single character from input"""
        if self.bus.input_buffer:
            ch = ord(self.bus.input_buffer[0])
            self.bus.input_buffer = self.bus.input_buffer[1:]
            return ch
        # Block until input available (in real impl would wait)
        # For now, return 0 or prompt
        try:
            ch = input()[0] if input() else '\r'
            return ord(ch)
        except:
            return 0

    def _console_output(self, ch: int):
        """Output character to console"""
        if ch == 0x0D:  # CR
            pass  # Ignore CR, handle LF
        elif ch == 0x0A:  # LF
            self.bus.output_buffer += '\n'
        elif 32 <= ch < 127:
            self.bus.output_buffer += chr(ch)
        elif ch == 0x09:  # Tab
            self.bus.output_buffer += '    '

    def _read_line(self, max_len: int) -> str:
        """Read line of input"""
        if self.bus.input_buffer:
            # Take from buffer
            line = ""
            while self.bus.input_buffer and len(line) < max_len:
                ch = self.bus.input_buffer[0]
                self.bus.input_buffer = self.bus.input_buffer[1:]
                if ch == '\n' or ch == '\r':
                    break
                line += ch
            return line
        # Interactive input
        try:
            return input()[:max_len]
        except:
            return ""

    def step(self) -> bool:
        """Execute one instruction. Returns False when program exits."""

        # Check for BDOS/BIOS intercept points
        if self.cpu.pc == 0xFE00:  # BDOS entry
            func = self.cpu.c
            if not self.bdos_call(func):
                return False
            # Return from BDOS
            self.cpu.pc = self.cpu.pop16()
            return True

        if self.cpu.pc == 0xFF00:  # BIOS warm boot
            return False

        # Normal instruction execution
        self.cpu.step()

        return not self.cpu.halted

    def run(self, max_cycles: int = 10000000) -> str:
        """Run program until exit or max cycles. Returns output."""
        self.bus.clear_output()
        cycles = 0

        while self.running and cycles < max_cycles:
            if not self.step():
                break
            cycles += self.cpu.cycles

        return self.bus.get_output()

    def provide_input(self, text: str):
        """Provide input for the program"""
        self.bus.provide_input(text)


def run_com_file(filename: str, input_text: str = "") -> str:
    """Run a .COM file and return output"""
    cpm = CPM()
    cpm.reset()
    cpm.load_file(filename)
    if input_text:
        cpm.provide_input(input_text)
    return cpm.run()


if __name__ == "__main__":
    import sys

    # Test with inline "Hello World" program
    cpm = CPM()
    cpm.reset()
    cpm.debug = True

    # Simple test program:
    # LD DE, msg
    # LD C, 9    ; Print string
    # CALL 5     ; BDOS
    # LD C, 0    ; System reset
    # CALL 5     ; BDOS
    # msg: DB 'Hello, CP/M World!$'

    program = bytes([
        0x11, 0x0D, 0x01,  # LD DE, 010Dh (msg address)
        0x0E, 0x09,        # LD C, 9 (print string)
        0xCD, 0x05, 0x00,  # CALL 0005h (BDOS)
        0x0E, 0x00,        # LD C, 0 (system reset)
        0xCD, 0x05, 0x00,  # CALL 0005h (BDOS)
        # msg at 010Dh:
        ord('H'), ord('e'), ord('l'), ord('l'), ord('o'),
        ord(','), ord(' '), ord('C'), ord('P'), ord('/'),
        ord('M'), ord(' '), ord('W'), ord('o'), ord('r'),
        ord('l'), ord('d'), ord('!'), ord('$')
    ])

    cpm.load_program(program)
    output = cpm.run()

    print(f"\nProgram output: {output}")
