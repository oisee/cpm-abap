"""
CP/M Emulator with File System Support
Extends cpm.py with BDOS file functions for running ZORK1.COM
"""

import os
from z80 import Z80, Bus


class CPMFileBus(Bus):
    """CP/M-aware bus with BDOS call interception and file system"""

    def __init__(self):
        super().__init__()
        self.dma_addr = 0x0080  # Default DMA address

    def setup_cpm(self):
        """Set up CP/M memory map"""
        # Warm boot at 0000h - JP to BIOS
        self.memory[0x0000] = 0xC3  # JP
        self.memory[0x0001] = 0x00
        self.memory[0x0002] = 0xFF  # JP FF00 (BIOS warm boot)

        # I/O byte at 0003h (not used)
        self.memory[0x0003] = 0x00

        # Current disk at 0004h
        self.memory[0x0004] = 0x00  # Drive A:

        # BDOS entry at 0005h - JP to BDOS handler
        self.memory[0x0005] = 0xC3  # JP
        self.memory[0x0006] = 0x00
        self.memory[0x0007] = 0xFE  # JP FE00 (BDOS entry)

        # Default FCB at 005Ch (36 bytes)
        # Default DMA buffer at 0080h (128 bytes)

        # BDOS stub at FE00h - RET (we intercept by address)
        self.memory[0xFE00] = 0xC9  # RET

        # BIOS warm boot stub at FF00h - RET
        self.memory[0xFF00] = 0xC9  # RET


class CPMFile:
    """CP/M File System emulation"""

    def __init__(self, directory: str = "."):
        self.directory = directory
        self.open_files = {}  # FCB address -> file handle
        self.fcb_to_path = {}  # FCB address -> file path

    def fcb_to_filename(self, bus: Bus, fcb_addr: int) -> str:
        """Extract filename from FCB structure"""
        # FCB: byte 0 = drive, bytes 1-8 = name, bytes 9-11 = extension
        name = ""
        for i in range(1, 9):
            ch = bus.read_mem(fcb_addr + i) & 0x7F
            if ch == 0x20:  # Space = end
                break
            name += chr(ch)

        ext = ""
        for i in range(9, 12):
            ch = bus.read_mem(fcb_addr + i) & 0x7F
            if ch == 0x20:
                break
            ext += chr(ch)

        if ext:
            return f"{name}.{ext}"
        return name

    def find_file(self, filename: str) -> str:
        """Find file in directory (case-insensitive)"""
        filename_upper = filename.upper()
        for f in os.listdir(self.directory):
            if f.upper() == filename_upper:
                return os.path.join(self.directory, f)
        return None

    def open_file(self, bus: Bus, fcb_addr: int) -> int:
        """Open file (BDOS function 15). Returns 0-3 on success, 0xFF on failure."""
        filename = self.fcb_to_filename(bus, fcb_addr)
        filepath = self.find_file(filename)

        if filepath is None or not os.path.exists(filepath):
            return 0xFF

        try:
            fh = open(filepath, 'rb')
            self.open_files[fcb_addr] = fh
            self.fcb_to_path[fcb_addr] = filepath

            # Initialize FCB fields
            bus.write_mem(fcb_addr + 12, 0)  # s1 = 0
            bus.write_mem(fcb_addr + 13, 0)  # s2 = 0
            bus.write_mem(fcb_addr + 14, 0)  # rc = 0 (will be updated)
            bus.write_mem(fcb_addr + 32, 0)  # cr = 0 (current record)

            # Get file size in records
            fh.seek(0, 2)  # End of file
            size = fh.tell()
            records = (size + 127) // 128
            fh.seek(0)  # Back to start

            # Store extent info (simplified)
            extent_records = min(records, 128)
            bus.write_mem(fcb_addr + 15, extent_records & 0xFF)  # rc (records in extent)

            return 0  # Success (directory entry 0)

        except Exception as e:
            print(f"[FILE] Error opening {filename}: {e}")
            return 0xFF

    def close_file(self, bus: Bus, fcb_addr: int) -> int:
        """Close file (BDOS function 16). Returns 0 on success, 0xFF on failure."""
        if fcb_addr in self.open_files:
            self.open_files[fcb_addr].close()
            del self.open_files[fcb_addr]
            if fcb_addr in self.fcb_to_path:
                del self.fcb_to_path[fcb_addr]
            return 0
        return 0xFF

    def read_sequential(self, bus: Bus, fcb_addr: int) -> int:
        """Read sequential record (BDOS function 20). Returns 0 on success, non-zero on EOF."""
        if fcb_addr not in self.open_files:
            return 1  # File not open

        fh = self.open_files[fcb_addr]
        data = fh.read(128)

        if len(data) == 0:
            return 1  # EOF

        # Write to DMA address
        for i, b in enumerate(data):
            bus.write_mem(bus.dma_addr + i, b)

        # Pad with 0x1A (CP/M EOF marker) if less than 128 bytes
        for i in range(len(data), 128):
            bus.write_mem(bus.dma_addr + i, 0x1A)

        # Update current record in FCB
        cr = bus.read_mem(fcb_addr + 32)
        bus.write_mem(fcb_addr + 32, (cr + 1) & 0xFF)

        return 0  # Success

    def read_random(self, bus: Bus, fcb_addr: int) -> int:
        """Random read (BDOS function 33). Returns 0 on success."""
        if fcb_addr not in self.open_files:
            return 6  # Seek to unwritten extent

        # Get random record number from FCB bytes 33-35
        r0 = bus.read_mem(fcb_addr + 33)
        r1 = bus.read_mem(fcb_addr + 34)
        r2 = bus.read_mem(fcb_addr + 35)
        record = r0 | (r1 << 8) | (r2 << 16)

        fh = self.open_files[fcb_addr]
        offset = record * 128

        try:
            fh.seek(offset)
            data = fh.read(128)

            if len(data) == 0:
                return 1  # EOF / reading unwritten data

            # Write to DMA address
            for i, b in enumerate(data):
                bus.write_mem(bus.dma_addr + i, b)

            # Pad with 0x1A
            for i in range(len(data), 128):
                bus.write_mem(bus.dma_addr + i, 0x1A)

            return 0

        except Exception:
            return 6  # Seek error

    def get_file_size(self, bus: Bus, fcb_addr: int) -> int:
        """Get file size in records (BDOS function 35)."""
        filename = self.fcb_to_filename(bus, fcb_addr)
        filepath = self.find_file(filename)

        if filepath is None:
            return 0xFF

        try:
            size = os.path.getsize(filepath)
            records = (size + 127) // 128

            # Store in FCB r0, r1, r2 (bytes 33-35)
            bus.write_mem(fcb_addr + 33, records & 0xFF)
            bus.write_mem(fcb_addr + 34, (records >> 8) & 0xFF)
            bus.write_mem(fcb_addr + 35, (records >> 16) & 0xFF)

            return 0

        except Exception:
            return 0xFF


class CPMWithFiles:
    """CP/M 2.2 Emulator with File System Support"""

    # BDOS function numbers
    BDOS_SYSTEM_RESET = 0
    BDOS_CONSOLE_INPUT = 1
    BDOS_CONSOLE_OUTPUT = 2
    BDOS_READER_INPUT = 3
    BDOS_PUNCH_OUTPUT = 4
    BDOS_LIST_OUTPUT = 5
    BDOS_DIRECT_IO = 6
    BDOS_GET_IOBYTE = 7
    BDOS_SET_IOBYTE = 8
    BDOS_PRINT_STRING = 9
    BDOS_READ_BUFFER = 10
    BDOS_CONSOLE_STATUS = 11
    BDOS_VERSION = 12
    BDOS_RESET_DISK = 13
    BDOS_SELECT_DISK = 14
    BDOS_OPEN_FILE = 15
    BDOS_CLOSE_FILE = 16
    BDOS_SEARCH_FIRST = 17
    BDOS_SEARCH_NEXT = 18
    BDOS_DELETE_FILE = 19
    BDOS_READ_SEQ = 20
    BDOS_WRITE_SEQ = 21
    BDOS_MAKE_FILE = 22
    BDOS_RENAME_FILE = 23
    BDOS_GET_LOGIN = 24
    BDOS_GET_DISK = 25
    BDOS_SET_DMA = 26
    BDOS_GET_ALLOC = 27
    BDOS_WRITE_PROT = 28
    BDOS_GET_RO = 29
    BDOS_SET_ATTR = 30
    BDOS_GET_DPB = 31
    BDOS_GET_USER = 32
    BDOS_READ_RAND = 33
    BDOS_WRITE_RAND = 34
    BDOS_FILE_SIZE = 35
    BDOS_SET_RAND = 36
    BDOS_RESET_DRIVE = 37

    def __init__(self, file_directory: str = "."):
        self.bus = CPMFileBus()
        self.cpu = Z80(self.bus)
        self.files = CPMFile(file_directory)
        self.running = True
        self.debug = False
        self.current_disk = 0  # A:
        self.user_number = 0

    def reset(self):
        """Reset CP/M system"""
        self.bus.setup_cpm()
        self.cpu.reset()
        self.cpu.pc = 0x0100  # TPA start
        self.cpu.sp = 0xFFFF
        # Push warm boot address (like CCP does)
        self.cpu.push16(0x0000)
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

    def setup_fcb(self, fcb_addr: int, filename: str):
        """Set up FCB with filename at address"""
        # Clear FCB
        for i in range(36):
            self.bus.write_mem(fcb_addr + i, 0)

        # Parse filename (format: D:FILENAME.EXT)
        drive = 0  # Default drive
        name = filename.upper()

        if len(name) >= 2 and name[1] == ':':
            drive = ord(name[0]) - ord('A') + 1
            name = name[2:]

        # Split name.ext
        if '.' in name:
            parts = name.split('.')
            fname = parts[0][:8]
            ext = parts[1][:3] if len(parts) > 1 else ""
        else:
            fname = name[:8]
            ext = ""

        # Write drive
        self.bus.write_mem(fcb_addr, drive)

        # Write filename (padded with spaces)
        for i in range(8):
            ch = ord(fname[i]) if i < len(fname) else 0x20
            self.bus.write_mem(fcb_addr + 1 + i, ch)

        # Write extension (padded with spaces)
        for i in range(3):
            ch = ord(ext[i]) if i < len(ext) else 0x20
            self.bus.write_mem(fcb_addr + 9 + i, ch)

    def bdos_call(self, func: int) -> bool:
        """Handle BDOS function call. Returns True to continue, False to exit."""

        if self.debug:
            print(f"[BDOS] Function {func}, DE={self.cpu.de:04X}")

        if func == self.BDOS_SYSTEM_RESET:
            return False

        elif func == self.BDOS_CONSOLE_INPUT:
            ch = self._console_input()
            self.cpu.a = ch
            self.cpu.l = ch

        elif func == self.BDOS_CONSOLE_OUTPUT:
            self._console_output(self.cpu.e)

        elif func == self.BDOS_READER_INPUT:
            # Paper tape reader - return EOF
            self.cpu.a = 0x1A

        elif func == self.BDOS_PUNCH_OUTPUT:
            # Paper tape punch - ignore
            pass

        elif func == self.BDOS_LIST_OUTPUT:
            # Printer - output to console
            self._console_output(self.cpu.e)

        elif func == self.BDOS_DIRECT_IO:
            if self.cpu.e == 0xFF:
                if self.bus.input_buffer:
                    ch = ord(self.bus.input_buffer[0])
                    self.bus.input_buffer = self.bus.input_buffer[1:]
                    self.cpu.a = ch
                else:
                    self.cpu.a = 0
            elif self.cpu.e == 0xFE:
                # Check input status
                self.cpu.a = 0xFF if self.bus.input_buffer else 0
            elif self.cpu.e == 0xFD:
                # Get input, wait
                ch = self._console_input()
                self.cpu.a = ch
            else:
                self._console_output(self.cpu.e)

        elif func == self.BDOS_GET_IOBYTE:
            self.cpu.a = self.bus.read_mem(0x0003)

        elif func == self.BDOS_SET_IOBYTE:
            self.bus.write_mem(0x0003, self.cpu.e)

        elif func == self.BDOS_PRINT_STRING:
            addr = self.cpu.de
            while True:
                ch = self.bus.read_mem(addr)
                if ch == 0x24:  # '$'
                    break
                self._console_output(ch)
                addr += 1
                if addr > 0xFFFF:
                    break

        elif func == self.BDOS_READ_BUFFER:
            buf_addr = self.cpu.de
            max_len = self.bus.read_mem(buf_addr)
            if max_len == 0:
                max_len = 128
            line = self._read_line(max_len)
            self.bus.write_mem(buf_addr + 1, len(line))
            for i, ch in enumerate(line):
                self.bus.write_mem(buf_addr + 2 + i, ord(ch))

        elif func == self.BDOS_CONSOLE_STATUS:
            self.cpu.a = 0xFF if self.bus.input_buffer else 0

        elif func == self.BDOS_VERSION:
            # CP/M 2.2
            self.cpu.hl = 0x0022
            self.cpu.a = 0x22

        elif func == self.BDOS_RESET_DISK:
            self.cpu.a = 0

        elif func == self.BDOS_SELECT_DISK:
            self.current_disk = self.cpu.e
            self.cpu.a = 0

        elif func == self.BDOS_OPEN_FILE:
            result = self.files.open_file(self.bus, self.cpu.de)
            self.cpu.a = result
            if self.debug:
                fname = self.files.fcb_to_filename(self.bus, self.cpu.de)
                print(f"[BDOS] Open '{fname}' -> {result}")

        elif func == self.BDOS_CLOSE_FILE:
            result = self.files.close_file(self.bus, self.cpu.de)
            self.cpu.a = result

        elif func == self.BDOS_SEARCH_FIRST:
            # Not implemented - return not found
            self.cpu.a = 0xFF

        elif func == self.BDOS_SEARCH_NEXT:
            self.cpu.a = 0xFF

        elif func == self.BDOS_DELETE_FILE:
            self.cpu.a = 0xFF

        elif func == self.BDOS_READ_SEQ:
            result = self.files.read_sequential(self.bus, self.cpu.de)
            self.cpu.a = result
            if self.debug and result != 0:
                print(f"[BDOS] Read seq -> EOF")

        elif func == self.BDOS_WRITE_SEQ:
            # Not implemented
            self.cpu.a = 0xFF

        elif func == self.BDOS_MAKE_FILE:
            # Not implemented
            self.cpu.a = 0xFF

        elif func == self.BDOS_RENAME_FILE:
            self.cpu.a = 0xFF

        elif func == self.BDOS_GET_LOGIN:
            # Return all drives logged in
            self.cpu.hl = 0x0001  # Just A:

        elif func == self.BDOS_GET_DISK:
            self.cpu.a = self.current_disk

        elif func == self.BDOS_SET_DMA:
            self.bus.dma_addr = self.cpu.de
            if self.debug:
                print(f"[BDOS] Set DMA -> {self.cpu.de:04X}")

        elif func == self.BDOS_GET_ALLOC:
            # Return pointer to allocation vector (fake it)
            self.cpu.hl = 0x0000

        elif func == self.BDOS_WRITE_PROT:
            pass

        elif func == self.BDOS_GET_RO:
            self.cpu.hl = 0x0000

        elif func == self.BDOS_SET_ATTR:
            self.cpu.a = 0

        elif func == self.BDOS_GET_DPB:
            # Return pointer to disk parameter block (fake)
            self.cpu.hl = 0x0000

        elif func == self.BDOS_GET_USER:
            if self.cpu.e == 0xFF:
                self.cpu.a = self.user_number
            else:
                self.user_number = self.cpu.e

        elif func == self.BDOS_READ_RAND:
            result = self.files.read_random(self.bus, self.cpu.de)
            self.cpu.a = result
            if self.debug:
                r0 = self.bus.read_mem(self.cpu.de + 33)
                r1 = self.bus.read_mem(self.cpu.de + 34)
                rec = r0 | (r1 << 8)
                print(f"[BDOS] Read random rec={rec} -> {result}")

        elif func == self.BDOS_WRITE_RAND:
            self.cpu.a = 0xFF

        elif func == self.BDOS_FILE_SIZE:
            result = self.files.get_file_size(self.bus, self.cpu.de)
            self.cpu.a = result

        elif func == self.BDOS_SET_RAND:
            # Set random record from sequential position
            # r0-r2 = (extent * 128) + cr
            fcb = self.cpu.de
            ex = self.bus.read_mem(fcb + 12)  # extent
            cr = self.bus.read_mem(fcb + 32)  # current record
            record = (ex * 128) + cr
            self.bus.write_mem(fcb + 33, record & 0xFF)
            self.bus.write_mem(fcb + 34, (record >> 8) & 0xFF)
            self.bus.write_mem(fcb + 35, 0)
            self.cpu.a = 0

        elif func == self.BDOS_RESET_DRIVE:
            self.cpu.a = 0

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
        try:
            line = input()
            if line:
                return ord(line[0])
            return 0x0D  # Return CR if empty
        except EOFError:
            return 0x1A  # Ctrl-Z (EOF)

    def _console_output(self, ch: int):
        """Output character to console"""
        if ch == 0x0D:
            pass  # Ignore CR
        elif ch == 0x0A:
            self.bus.output_buffer += '\n'
            if self.debug:
                print()  # Also print in debug
        elif ch == 0x08:  # Backspace
            if self.bus.output_buffer:
                self.bus.output_buffer = self.bus.output_buffer[:-1]
        elif ch == 0x07:  # Bell
            pass
        elif 32 <= ch < 127:
            self.bus.output_buffer += chr(ch)
            if self.debug:
                print(chr(ch), end='', flush=True)
        elif ch == 0x09:
            self.bus.output_buffer += '    '

    def _read_line(self, max_len: int) -> str:
        """Read line of input"""
        if self.bus.input_buffer:
            line = ""
            while self.bus.input_buffer and len(line) < max_len:
                ch = self.bus.input_buffer[0]
                self.bus.input_buffer = self.bus.input_buffer[1:]
                if ch == '\n' or ch == '\r':
                    break
                line += ch
            return line
        try:
            return input()[:max_len]
        except EOFError:
            return ""

    def step(self) -> bool:
        """Execute one instruction. Returns False when program exits."""

        # Check for BDOS/BIOS intercept points
        if self.cpu.pc == 0xFE00:
            func = self.cpu.c
            if not self.bdos_call(func):
                return False
            # RET instruction will execute and return naturally
            self.cpu.step()
            return True

        if self.cpu.pc == 0xFF00:
            return False

        # Debug: track execution
        if hasattr(self, '_trace') and self._trace:
            op = self.bus.read_mem(self.cpu.pc)
            if op in [0xDD, 0xED, 0xFD]:
                op2 = self.bus.read_mem(self.cpu.pc + 1)
                print(f"PC={self.cpu.pc:04X} OP={op:02X} {op2:02X}")
            # else:
            #     print(f"PC={self.cpu.pc:04X} OP={op:02X}")

        self.cpu.step()
        return not self.cpu.halted

    def run(self, max_cycles: int = 100000000) -> str:
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


def run_zork(zork_dir: str, commands: list = None):
    """Run ZORK1.COM with optional commands"""
    cpm = CPMWithFiles(zork_dir)
    cpm.reset()
    cpm.debug = True

    # Load ZORK1.COM
    zork_path = os.path.join(zork_dir, "ZORK1.COM")
    if not os.path.exists(zork_path):
        print(f"Error: {zork_path} not found")
        return

    size = cpm.load_file(zork_path)
    print(f"Loaded ZORK1.COM: {size} bytes")

    # Set up default FCB with ZORK1.DAT
    cpm.setup_fcb(0x005C, "ZORK1.DAT")

    # Provide commands if any
    if commands:
        for cmd in commands:
            cpm.provide_input(cmd + "\n")

    # Run
    output = cpm.run()
    print("\n=== Output ===")
    print(output)


if __name__ == "__main__":
    import sys

    if len(sys.argv) > 1:
        # Run ZORK from directory
        run_zork(sys.argv[1], sys.argv[2:])
    else:
        # Test with HELLO.COM
        print("Testing HELLO.COM...")
        cpm = CPMWithFiles("../test-programs")
        cpm.reset()
        cpm.load_file("../test-programs/HELLO.COM")
        output = cpm.run()
        print(f"Output: [{output}]")
