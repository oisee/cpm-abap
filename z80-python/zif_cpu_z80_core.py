"""
Z80 CPU Core Interface - Python equivalent of ZIF_CPU_Z80_CORE
Defines the interface between main CPU and prefix handlers
"""

from abc import ABC, abstractmethod

# Flag bit positions in F register (matching ABAP constants)
FLAG_C = 1      # Carry
FLAG_N = 2      # Add/Subtract
FLAG_PV = 4     # Parity/Overflow
FLAG_F3 = 8     # Undocumented (bit 3)
FLAG_H = 16     # Half-carry
FLAG_F5 = 32    # Undocumented (bit 5)
FLAG_Z = 64     # Zero
FLAG_S = 128    # Sign


class ZifCpuZ80Core(ABC):
    """Interface for Z80 CPU core - matches ZIF_CPU_Z80_CORE"""

    # Constants (class attributes)
    c_flag_c = FLAG_C
    c_flag_n = FLAG_N
    c_flag_pv = FLAG_PV
    c_flag_f3 = FLAG_F3
    c_flag_h = FLAG_H
    c_flag_f5 = FLAG_F5
    c_flag_z = FLAG_Z
    c_flag_s = FLAG_S

    # Memory access
    @abstractmethod
    def read_mem(self, addr: int) -> int: pass

    @abstractmethod
    def write_mem(self, addr: int, val: int): pass

    # I/O access
    @abstractmethod
    def read_io(self, port: int) -> int: pass

    @abstractmethod
    def write_io(self, port: int, val: int): pass

    # Fetch operations
    @abstractmethod
    def fetch_byte(self) -> int: pass

    @abstractmethod
    def fetch_word(self) -> int: pass

    # Register access - 16-bit pairs
    @abstractmethod
    def get_af(self) -> int: pass
    @abstractmethod
    def set_af(self, val: int): pass

    @abstractmethod
    def get_bc(self) -> int: pass
    @abstractmethod
    def set_bc(self, val: int): pass

    @abstractmethod
    def get_de(self) -> int: pass
    @abstractmethod
    def set_de(self, val: int): pass

    @abstractmethod
    def get_hl(self) -> int: pass
    @abstractmethod
    def set_hl(self, val: int): pass

    @abstractmethod
    def get_ix(self) -> int: pass
    @abstractmethod
    def set_ix(self, val: int): pass

    @abstractmethod
    def get_iy(self) -> int: pass
    @abstractmethod
    def set_iy(self, val: int): pass

    @abstractmethod
    def get_sp(self) -> int: pass
    @abstractmethod
    def set_sp(self, val: int): pass

    @abstractmethod
    def get_pc(self) -> int: pass
    @abstractmethod
    def set_pc(self, val: int): pass

    # Register access - 8-bit
    @abstractmethod
    def get_a(self) -> int: pass
    @abstractmethod
    def set_a(self, val: int): pass

    @abstractmethod
    def get_f(self) -> int: pass
    @abstractmethod
    def set_f(self, val: int): pass

    @abstractmethod
    def get_b(self) -> int: pass
    @abstractmethod
    def set_b(self, val: int): pass

    @abstractmethod
    def get_c(self) -> int: pass
    @abstractmethod
    def set_c(self, val: int): pass

    @abstractmethod
    def get_d(self) -> int: pass
    @abstractmethod
    def set_d(self, val: int): pass

    @abstractmethod
    def get_e(self) -> int: pass
    @abstractmethod
    def set_e(self, val: int): pass

    @abstractmethod
    def get_h(self) -> int: pass
    @abstractmethod
    def set_h(self, val: int): pass

    @abstractmethod
    def get_l(self) -> int: pass
    @abstractmethod
    def set_l(self, val: int): pass

    # Special registers
    @abstractmethod
    def get_i(self) -> int: pass
    @abstractmethod
    def set_i(self, val: int): pass

    @abstractmethod
    def get_r(self) -> int: pass
    @abstractmethod
    def set_r(self, val: int): pass

    # Interrupt flags
    @abstractmethod
    def get_iff1(self) -> bool: pass
    @abstractmethod
    def set_iff1(self, val: bool): pass

    @abstractmethod
    def get_iff2(self) -> bool: pass
    @abstractmethod
    def set_iff2(self, val: bool): pass

    @abstractmethod
    def get_im(self) -> int: pass
    @abstractmethod
    def set_im(self, val: int): pass

    # Stack operations
    @abstractmethod
    def push(self, val: int): pass

    @abstractmethod
    def pop(self) -> int: pass

    # Flag operations
    @abstractmethod
    def get_flag(self, flag: int) -> bool: pass

    @abstractmethod
    def set_flag(self, flag: int, val: bool): pass

    # ALU operations
    @abstractmethod
    def alu_add8(self, val: int, carry: int = 0) -> int: pass

    @abstractmethod
    def alu_sub8(self, val: int, carry: int = 0) -> int: pass

    @abstractmethod
    def alu_and8(self, val: int) -> int: pass

    @abstractmethod
    def alu_or8(self, val: int) -> int: pass

    @abstractmethod
    def alu_xor8(self, val: int) -> int: pass

    @abstractmethod
    def alu_cp8(self, val: int): pass

    @abstractmethod
    def alu_inc8(self, val: int) -> int: pass

    @abstractmethod
    def alu_dec8(self, val: int) -> int: pass

    # Rotate/shift
    @abstractmethod
    def alu_rlc(self, val: int) -> int: pass

    @abstractmethod
    def alu_rrc(self, val: int) -> int: pass

    @abstractmethod
    def alu_rl(self, val: int) -> int: pass

    @abstractmethod
    def alu_rr(self, val: int) -> int: pass

    @abstractmethod
    def alu_sla(self, val: int) -> int: pass

    @abstractmethod
    def alu_sra(self, val: int) -> int: pass

    @abstractmethod
    def alu_srl(self, val: int) -> int: pass

    # Cycle counting
    @abstractmethod
    def add_cycles(self, cycles: int): pass


class ZifCpuZ80Prefix(ABC):
    """Interface for prefix handlers - matches ZIF_CPU_Z80_PREFIX"""

    @abstractmethod
    def execute(self) -> int:
        """Execute prefixed instruction, return cycles consumed"""
        pass
