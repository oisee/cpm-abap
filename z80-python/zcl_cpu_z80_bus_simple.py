"""
Simple Z80 Bus Implementation - Python equivalent of ZCL_CPU_Z80_BUS_SIMPLE
Memory: 64KB addressable (0x0000-0xFFFF)
I/O: 256 ports (0x00-0xFF)
"""

from zif_cpu_z80_bus import ZifCpuZ80Bus


class ZclCpuZ80BusSimple(ZifCpuZ80Bus):
    """Simple bus with 64KB RAM and 256 I/O ports"""

    def __init__(self):
        self.memory = bytearray(65536)
        self.io_ports = bytearray(256)
        self.input_buffer = ""
        self.output_buffer = ""

    def read_mem(self, addr: int) -> int:
        return self.memory[addr & 0xFFFF]

    def write_mem(self, addr: int, val: int):
        self.memory[addr & 0xFFFF] = val & 0xFF

    def read_io(self, port: int) -> int:
        return self.io_ports[port & 0xFF]

    def write_io(self, port: int, val: int):
        self.io_ports[port & 0xFF] = val & 0xFF

    def load(self, addr: int, data: bytes):
        """Load binary data at address"""
        for i, b in enumerate(data):
            self.memory[(addr + i) & 0xFFFF] = b

    def provide_input(self, text: str):
        self.input_buffer += text

    def get_output(self) -> str:
        return self.output_buffer

    def clear_output(self):
        self.output_buffer = ""
