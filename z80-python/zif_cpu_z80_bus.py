"""
Z80 Bus Interface - Python equivalent of ZIF_CPU_Z80_BUS
"""

from abc import ABC, abstractmethod


class ZifCpuZ80Bus(ABC):
    """Bus interface for memory and I/O - matches ZIF_CPU_Z80_BUS"""

    @abstractmethod
    def read_mem(self, addr: int) -> int:
        """Read byte from memory"""
        pass

    @abstractmethod
    def write_mem(self, addr: int, val: int):
        """Write byte to memory"""
        pass

    @abstractmethod
    def read_io(self, port: int) -> int:
        """Read from I/O port"""
        pass

    @abstractmethod
    def write_io(self, port: int, val: int):
        """Write to I/O port"""
        pass

    @abstractmethod
    def provide_input(self, text: str):
        """Provide input text for console"""
        pass

    @abstractmethod
    def get_output(self) -> str:
        """Get output text from console"""
        pass

    @abstractmethod
    def clear_output(self):
        """Clear output buffer"""
        pass
