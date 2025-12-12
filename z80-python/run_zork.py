#!/usr/bin/env python3
"""Run ZORK1.COM with the ABAP-aligned Z80 emulator"""

import os
from cpm_z80 import CpmEmulator


def run_zork(commands=None, max_instr=1000000):
    """Run ZORK1.COM"""
    zork_dir = "../test-games"
    cpm = CpmEmulator(zork_dir)
    cpm.reset()

    zork_path = os.path.join(zork_dir, "ZORK1.COM")
    size = cpm.load_file(zork_path)
    print(f"Loaded ZORK1.COM: {size} bytes")

    cpm.setup_fcb(0x005C, "ZORK1.DAT")

    # Provide commands as input
    if commands:
        for cmd in commands:
            cpm.provide_input(cmd + "\n")

    instr_count = 0
    while cpm.running and instr_count < max_instr:
        pc = cpm.cpu.get_pc()

        if pc == 0xFE00:
            func = cpm.cpu.get_c()
            if not cpm.bdos_call(func):
                break
            cpm.cpu.step()
            instr_count += 1
            continue

        if pc == 0xFF00:
            break

        cpm.cpu.step()
        instr_count += 1

        if cpm.cpu.mv_halted or not cpm.cpu.mv_running:
            break

    print(f"\n--- Executed {instr_count} instructions ---\n")
    print(cpm.bus.get_output())


if __name__ == "__main__":
    import sys
    commands = sys.argv[1:] if len(sys.argv) > 1 else ["look"]
    run_zork(commands, max_instr=2000000)
