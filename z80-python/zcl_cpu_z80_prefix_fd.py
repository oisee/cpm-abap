"""
Z80 FD Prefix Handler - Python equivalent of ZCL_CPU_Z80_PREFIX_FD
Handles IY-indexed instructions (same as DD but for IY)
"""

from zcl_cpu_z80_prefix_dd import ZclCpuZ80PrefixDd
from zif_cpu_z80_core import ZifCpuZ80Core


class ZclCpuZ80PrefixFd(ZclCpuZ80PrefixDd):
    """FD prefix handler for IY instructions - extends DD handler"""

    def __init__(self, cpu: ZifCpuZ80Core):
        super().__init__(cpu)

    def _get_index_reg(self) -> int:
        """Override to use IY instead of IX"""
        return self.cpu.get_iy()

    def _set_index_reg(self, val: int):
        """Override to use IY instead of IX"""
        self.cpu.set_iy(val)
