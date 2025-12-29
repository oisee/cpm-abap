"""
CP/M Emulator using ABAP-architecture-aligned Z80
Uses ZclCpuZ80 (matches ABAP ZCL_CPU_Z80 design)
"""

import os
from zcl_cpu_z80 import ZclCpuZ80
from zif_cpu_z80_bus import ZifCpuZ80Bus


class CpmBus(ZifCpuZ80Bus):
    """CP/M memory bus - 64KB RAM"""

    def __init__(self):
        self.memory = bytearray(65536)
        self.dma_addr = 0x0080  # Default DMA address
        self.input_buffer = ""
        self.output_buffer = ""

    def read_mem(self, addr: int) -> int:
        return self.memory[addr & 0xFFFF]

    def write_mem(self, addr: int, val: int):
        self.memory[addr & 0xFFFF] = val & 0xFF

    def read_io(self, port: int) -> int:
        return 0xFF

    def write_io(self, port: int, val: int):
        pass

    def load(self, addr: int, data: bytes):
        for i, b in enumerate(data):
            self.memory[addr + i] = b

    def setup_cpm(self):
        """Set up CP/M memory map"""
        # Warm boot at 0000h - JP to BIOS
        self.memory[0x0000] = 0xC3  # JP
        self.memory[0x0001] = 0x00
        self.memory[0x0002] = 0xFF  # JP FF00 (BIOS warm boot)

        # I/O byte at 0003h
        self.memory[0x0003] = 0x00

        # Current disk at 0004h
        self.memory[0x0004] = 0x00  # Drive A:

        # BDOS entry at 0005h - JP to BDOS handler
        self.memory[0x0005] = 0xC3  # JP
        self.memory[0x0006] = 0x00
        self.memory[0x0007] = 0xFE  # JP FE00 (BDOS entry)

        # BDOS stub at FE00h - RET
        self.memory[0xFE00] = 0xC9  # RET

        # BIOS warm boot stub at FF00h - RET
        self.memory[0xFF00] = 0xC9  # RET

    def provide_input(self, text: str):
        self.input_buffer += text

    def get_output(self) -> str:
        return self.output_buffer

    def clear_output(self):
        self.output_buffer = ""


class CPMFile:
    """CP/M File System emulation"""

    def __init__(self, directory: str = "."):
        self.directory = directory
        self.open_files = {}  # FCB address -> file handle
        self.fcb_to_path = {}

    def fcb_to_filename(self, bus: CpmBus, fcb_addr: int) -> str:
        """Extract filename from FCB structure"""
        name = ""
        for i in range(1, 9):
            ch = bus.read_mem(fcb_addr + i) & 0x7F
            if ch == 0x20:
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

    def open_file(self, bus: CpmBus, fcb_addr: int) -> int:
        """Open file (BDOS function 15)"""
        filename = self.fcb_to_filename(bus, fcb_addr)

        # Check if already open at this FCB (e.g., after MAKE)
        if fcb_addr in self.open_files:
            # Already open, just reset position
            self.open_files[fcb_addr].seek(0)
            bus.write_mem(fcb_addr + 32, 0)  # cr = 0
            return 0

        filepath = self.find_file(filename)

        if filepath is None or not os.path.exists(filepath):
            return 0xFF

        try:
            fh = open(filepath, 'r+b')  # Open for read/write
            self.open_files[fcb_addr] = fh
            self.fcb_to_path[fcb_addr] = filepath

            # Initialize FCB fields
            bus.write_mem(fcb_addr + 12, 0)  # s1 = 0
            bus.write_mem(fcb_addr + 13, 0)  # s2 = 0
            bus.write_mem(fcb_addr + 14, 0)  # rc = 0
            bus.write_mem(fcb_addr + 32, 0)  # cr = 0

            # Get file size in records
            fh.seek(0, 2)
            size = fh.tell()
            records = (size + 127) // 128
            fh.seek(0)

            extent_records = min(records, 128)
            bus.write_mem(fcb_addr + 15, extent_records & 0xFF)

            return 0

        except Exception as e:
            # Try read-only if r+b fails
            try:
                fh = open(filepath, 'rb')
                self.open_files[fcb_addr] = fh
                self.fcb_to_path[fcb_addr] = filepath
                bus.write_mem(fcb_addr + 12, 0)
                bus.write_mem(fcb_addr + 13, 0)
                bus.write_mem(fcb_addr + 14, 0)
                bus.write_mem(fcb_addr + 32, 0)
                fh.seek(0, 2)
                size = fh.tell()
                records = (size + 127) // 128
                fh.seek(0)
                bus.write_mem(fcb_addr + 15, min(records, 128) & 0xFF)
                return 0
            except:
                print(f"[FILE] Error opening {filename}: {e}")
                return 0xFF

    def close_file(self, bus: CpmBus, fcb_addr: int) -> int:
        """Close file (BDOS function 16)"""
        if fcb_addr in self.open_files:
            self.open_files[fcb_addr].close()
            del self.open_files[fcb_addr]
            if fcb_addr in self.fcb_to_path:
                del self.fcb_to_path[fcb_addr]
            return 0
        return 0xFF

    def read_sequential(self, bus: CpmBus, fcb_addr: int) -> int:
        """Read sequential record (BDOS function 20)"""
        if fcb_addr not in self.open_files:
            return 1

        fh = self.open_files[fcb_addr]
        data = fh.read(128)

        if len(data) == 0:
            return 1  # EOF

        # Write to DMA address
        for i, b in enumerate(data):
            bus.write_mem(bus.dma_addr + i, b)

        # Pad with 0x1A
        for i in range(len(data), 128):
            bus.write_mem(bus.dma_addr + i, 0x1A)

        # Update current record in FCB
        cr = bus.read_mem(fcb_addr + 32)
        bus.write_mem(fcb_addr + 32, (cr + 1) & 0xFF)

        return 0

    def read_random(self, bus: CpmBus, fcb_addr: int) -> int:
        """Random read (BDOS function 33)"""
        if fcb_addr not in self.open_files:
            return 6

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
                return 1

            for i, b in enumerate(data):
                bus.write_mem(bus.dma_addr + i, b)

            for i in range(len(data), 128):
                bus.write_mem(bus.dma_addr + i, 0x1A)

            return 0

        except Exception:
            return 6

    def get_file_size(self, bus: CpmBus, fcb_addr: int) -> int:
        """Get file size in records (BDOS function 35)"""
        filename = self.fcb_to_filename(bus, fcb_addr)
        filepath = self.find_file(filename)

        if filepath is None:
            return 0xFF

        try:
            size = os.path.getsize(filepath)
            records = (size + 127) // 128

            bus.write_mem(fcb_addr + 33, records & 0xFF)
            bus.write_mem(fcb_addr + 34, (records >> 8) & 0xFF)
            bus.write_mem(fcb_addr + 35, (records >> 16) & 0xFF)

            return 0
        except Exception:
            return 0xFF

    def make_file(self, bus: CpmBus, fcb_addr: int) -> int:
        """Create new file (BDOS function 22)"""
        filename = self.fcb_to_filename(bus, fcb_addr)
        filepath = os.path.join(self.directory, filename.upper())

        try:
            fh = open(filepath, 'w+b')  # Read/write mode
            self.open_files[fcb_addr] = fh
            self.fcb_to_path[fcb_addr] = filepath
            self.write_mode = getattr(self, 'write_mode', {})
            self.write_mode[fcb_addr] = True  # Mark as write mode

            # Initialize FCB fields
            bus.write_mem(fcb_addr + 12, 0)  # s1 = 0
            bus.write_mem(fcb_addr + 13, 0)  # s2 = 0
            bus.write_mem(fcb_addr + 14, 0)  # rc = 0
            bus.write_mem(fcb_addr + 32, 0)  # cr = 0

            return 0
        except Exception as e:
            print(f"[FILE] Error creating {filename}: {e}")
            return 0xFF

    def write_sequential(self, bus: CpmBus, fcb_addr: int) -> int:
        """Write sequential record (BDOS function 21)"""
        if fcb_addr not in self.open_files:
            return 1

        fh = self.open_files[fcb_addr]

        # Read 128 bytes from DMA
        data = bytes([bus.read_mem(bus.dma_addr + i) for i in range(128)])

        try:
            fh.write(data)

            # Update current record in FCB
            cr = bus.read_mem(fcb_addr + 32)
            bus.write_mem(fcb_addr + 32, (cr + 1) & 0xFF)

            return 0
        except Exception:
            return 1

    def write_random(self, bus: CpmBus, fcb_addr: int) -> int:
        """Random write (BDOS function 34)"""
        if fcb_addr not in self.open_files:
            return 6

        r0 = bus.read_mem(fcb_addr + 33)
        r1 = bus.read_mem(fcb_addr + 34)
        r2 = bus.read_mem(fcb_addr + 35)
        record = r0 | (r1 << 8) | (r2 << 16)

        fh = self.open_files[fcb_addr]
        offset = record * 128

        # Read 128 bytes from DMA
        data = bytes([bus.read_mem(bus.dma_addr + i) for i in range(128)])

        try:
            fh.seek(offset)
            fh.write(data)
            return 0
        except Exception:
            return 6


class CpmEmulator:
    """CP/M 2.2 Emulator using ABAP-aligned Z80"""

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
        self.bus = CpmBus()
        self.cpu = ZclCpuZ80(self.bus)
        self.files = CPMFile(file_directory)
        self.running = True
        self.debug = False
        self.current_disk = 0
        self.user_number = 0

    def reset(self):
        """Reset CP/M system"""
        self.bus.setup_cpm()
        self.cpu.reset()
        self.cpu.set_pc(0x0100)  # TPA start
        self.cpu.set_sp(0xFFFF)
        # Push warm boot address
        self.cpu.push(0x0000)
        self.running = True

    def load_program(self, data: bytes, addr: int = 0x0100):
        """Load program at address"""
        self.bus.load(addr, data)

    def load_file(self, filename: str, addr: int = 0x0100) -> int:
        """Load binary file as CP/M program"""
        with open(filename, 'rb') as f:
            data = f.read()
        self.load_program(data, addr)
        return len(data)

    def setup_fcb(self, fcb_addr: int, filename: str):
        """Set up FCB with filename"""
        for i in range(36):
            self.bus.write_mem(fcb_addr + i, 0)

        drive = 0
        name = filename.upper()

        if len(name) >= 2 and name[1] == ':':
            drive = ord(name[0]) - ord('A') + 1
            name = name[2:]

        if '.' in name:
            parts = name.split('.')
            fname = parts[0][:8]
            ext = parts[1][:3] if len(parts) > 1 else ""
        else:
            fname = name[:8]
            ext = ""

        self.bus.write_mem(fcb_addr, drive)

        for i in range(8):
            ch = ord(fname[i]) if i < len(fname) else 0x20
            self.bus.write_mem(fcb_addr + 1 + i, ch)

        for i in range(3):
            ch = ord(ext[i]) if i < len(ext) else 0x20
            self.bus.write_mem(fcb_addr + 9 + i, ch)

    def bdos_call(self, func: int) -> bool:
        """Handle BDOS function call. Returns True to continue, False to exit."""

        if self.debug:
            print(f"[BDOS] Function {func}, DE={self.cpu.get_de():04X}")

        if func == self.BDOS_SYSTEM_RESET:
            return False

        elif func == self.BDOS_CONSOLE_INPUT:
            ch = self._console_input()
            self.cpu.set_a(ch)
            self.cpu.set_l(ch)

        elif func == self.BDOS_CONSOLE_OUTPUT:
            self._console_output(self.cpu.get_e())

        elif func == self.BDOS_READER_INPUT:
            self.cpu.set_a(0x1A)

        elif func == self.BDOS_PUNCH_OUTPUT:
            pass

        elif func == self.BDOS_LIST_OUTPUT:
            self._console_output(self.cpu.get_e())

        elif func == self.BDOS_DIRECT_IO:
            e = self.cpu.get_e()
            if e == 0xFF:
                if self.bus.input_buffer:
                    ch = ord(self.bus.input_buffer[0])
                    self.bus.input_buffer = self.bus.input_buffer[1:]
                    self.cpu.set_a(ch)
                else:
                    self.cpu.set_a(0)
            elif e == 0xFE:
                self.cpu.set_a(0xFF if self.bus.input_buffer else 0)
            elif e == 0xFD:
                ch = self._console_input()
                self.cpu.set_a(ch)
            else:
                self._console_output(e)

        elif func == self.BDOS_GET_IOBYTE:
            self.cpu.set_a(self.bus.read_mem(0x0003))

        elif func == self.BDOS_SET_IOBYTE:
            self.bus.write_mem(0x0003, self.cpu.get_e())

        elif func == self.BDOS_PRINT_STRING:
            addr = self.cpu.get_de()
            while True:
                ch = self.bus.read_mem(addr)
                if ch == 0x24:  # '$'
                    break
                self._console_output(ch)
                addr += 1
                if addr > 0xFFFF:
                    break

        elif func == self.BDOS_READ_BUFFER:
            buf_addr = self.cpu.get_de()
            max_len = self.bus.read_mem(buf_addr)
            if max_len == 0:
                max_len = 128
            line = self._read_line(max_len)
            self.bus.write_mem(buf_addr + 1, len(line))
            for i, ch in enumerate(line):
                self.bus.write_mem(buf_addr + 2 + i, ord(ch))

        elif func == self.BDOS_CONSOLE_STATUS:
            self.cpu.set_a(0xFF if self.bus.input_buffer else 0)

        elif func == self.BDOS_VERSION:
            self.cpu.set_hl(0x0022)
            self.cpu.set_a(0x22)

        elif func == self.BDOS_RESET_DISK:
            self.cpu.set_a(0)

        elif func == self.BDOS_SELECT_DISK:
            self.current_disk = self.cpu.get_e()
            self.cpu.set_a(0)

        elif func == self.BDOS_OPEN_FILE:
            result = self.files.open_file(self.bus, self.cpu.get_de())
            self.cpu.set_a(result)
            if self.debug:
                fname = self.files.fcb_to_filename(self.bus, self.cpu.get_de())
                print(f"[BDOS] Open '{fname}' -> {result}")

        elif func == self.BDOS_CLOSE_FILE:
            result = self.files.close_file(self.bus, self.cpu.get_de())
            self.cpu.set_a(result)

        elif func == self.BDOS_SEARCH_FIRST:
            self.cpu.set_a(0xFF)

        elif func == self.BDOS_SEARCH_NEXT:
            self.cpu.set_a(0xFF)

        elif func == self.BDOS_DELETE_FILE:
            self.cpu.set_a(0xFF)

        elif func == self.BDOS_READ_SEQ:
            result = self.files.read_sequential(self.bus, self.cpu.get_de())
            self.cpu.set_a(result)
            if self.debug and result != 0:
                print(f"[BDOS] Read seq -> EOF")

        elif func == self.BDOS_WRITE_SEQ:
            result = self.files.write_sequential(self.bus, self.cpu.get_de())
            self.cpu.set_a(result)

        elif func == self.BDOS_MAKE_FILE:
            result = self.files.make_file(self.bus, self.cpu.get_de())
            self.cpu.set_a(result)
            if self.debug:
                fname = self.files.fcb_to_filename(self.bus, self.cpu.get_de())
                print(f"[BDOS] Make '{fname}' -> {result}")

        elif func == self.BDOS_RENAME_FILE:
            self.cpu.set_a(0xFF)

        elif func == self.BDOS_GET_LOGIN:
            self.cpu.set_hl(0x0001)

        elif func == self.BDOS_GET_DISK:
            self.cpu.set_a(self.current_disk)

        elif func == self.BDOS_SET_DMA:
            self.bus.dma_addr = self.cpu.get_de()
            if self.debug:
                print(f"[BDOS] Set DMA -> {self.cpu.get_de():04X}")

        elif func == self.BDOS_GET_ALLOC:
            self.cpu.set_hl(0x0000)

        elif func == self.BDOS_WRITE_PROT:
            pass

        elif func == self.BDOS_GET_RO:
            self.cpu.set_hl(0x0000)

        elif func == self.BDOS_SET_ATTR:
            self.cpu.set_a(0)

        elif func == self.BDOS_GET_DPB:
            self.cpu.set_hl(0x0000)

        elif func == self.BDOS_GET_USER:
            if self.cpu.get_e() == 0xFF:
                self.cpu.set_a(self.user_number)
            else:
                self.user_number = self.cpu.get_e()

        elif func == self.BDOS_READ_RAND:
            result = self.files.read_random(self.bus, self.cpu.get_de())
            self.cpu.set_a(result)
            if self.debug:
                r0 = self.bus.read_mem(self.cpu.get_de() + 33)
                r1 = self.bus.read_mem(self.cpu.get_de() + 34)
                rec = r0 | (r1 << 8)
                print(f"[BDOS] Read random rec={rec} -> {result}")

        elif func == self.BDOS_WRITE_RAND:
            result = self.files.write_random(self.bus, self.cpu.get_de())
            self.cpu.set_a(result)

        elif func == self.BDOS_FILE_SIZE:
            result = self.files.get_file_size(self.bus, self.cpu.get_de())
            self.cpu.set_a(result)

        elif func == self.BDOS_SET_RAND:
            fcb = self.cpu.get_de()
            ex = self.bus.read_mem(fcb + 12)
            cr = self.bus.read_mem(fcb + 32)
            record = (ex * 128) + cr
            self.bus.write_mem(fcb + 33, record & 0xFF)
            self.bus.write_mem(fcb + 34, (record >> 8) & 0xFF)
            self.bus.write_mem(fcb + 35, 0)
            self.cpu.set_a(0)

        elif func == self.BDOS_RESET_DRIVE:
            self.cpu.set_a(0)

        else:
            if self.debug:
                print(f"[BDOS] Unimplemented function {func}")
            self.cpu.set_a(0)

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
            return 0x0D
        except EOFError:
            return 0x1A

    def _console_output(self, ch: int):
        """Output character to console"""
        if ch == 0x0D:
            pass
        elif ch == 0x0A:
            self.bus.output_buffer += '\n'
            if self.debug:
                print()
        elif ch == 0x08:
            if self.bus.output_buffer:
                self.bus.output_buffer = self.bus.output_buffer[:-1]
        elif ch == 0x07:
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

        pc = self.cpu.get_pc()

        # Check for BDOS/BIOS intercept points
        if pc == 0xFE00:
            func = self.cpu.get_c()
            if not self.bdos_call(func):
                return False
            # Execute RET instruction
            self.cpu.step()
            return True

        if pc == 0xFF00:
            return False

        self.cpu.step()
        return self.cpu.mv_running and not self.cpu.mv_halted

    def run(self, max_cycles: int = 100000000) -> str:
        """Run program until exit or max cycles. Returns output."""
        self.bus.clear_output()
        cycles = 0

        while self.running and cycles < max_cycles:
            if not self.step():
                break
            cycles += self.cpu.mv_cycles

        return self.bus.get_output()

    def provide_input(self, text: str):
        """Provide input for the program"""
        self.bus.provide_input(text)


def run_hello():
    """Test with HELLO.COM"""
    print("Testing HELLO.COM with ABAP-aligned Z80...")
    cpm = CpmEmulator("../test-programs")
    cpm.reset()
    cpm.load_file("../test-programs/HELLO.COM")
    output = cpm.run()
    print(f"Output: [{output}]")
    return output


def run_zork(zork_dir: str, commands: list = None):
    """Run ZORK1.COM with optional commands"""
    cpm = CpmEmulator(zork_dir)
    cpm.reset()
    cpm.debug = True

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
        if sys.argv[1] == "--hello":
            run_hello()
        else:
            run_zork(sys.argv[1], sys.argv[2:])
    else:
        run_hello()
