#!/usr/bin/env python3
"""Debug ZORK1.COM execution"""

import os
from cpm_z80 import CpmEmulator


def debug_zork():
    """Debug ZORK1.COM execution"""
    zork_dir = "../test-games"
    cpm = CpmEmulator(zork_dir)
    cpm.reset()
    cpm.debug = True

    zork_path = os.path.join(zork_dir, "ZORK1.COM")
    size = cpm.load_file(zork_path)
    print(f"Loaded ZORK1.COM: {size} bytes")

    # Set up default FCB with ZORK1.DAT
    cpm.setup_fcb(0x005C, "ZORK1.DAT")

    # Run with instruction tracing after first few BDOS calls
    bdos_count = 0
    instr_count = 0
    trace_after = 7  # Start tracing after 7 BDOS calls (past initial file reads)
    max_trace = 200

    while cpm.running:
        pc = cpm.cpu.get_pc()

        # Check for BDOS intercept
        if pc == 0xFE00:
            func = cpm.cpu.get_c()
            bdos_count += 1
            if not cpm.bdos_call(func):
                break
            cpm.cpu.step()
            continue

        if pc == 0xFF00:
            print("Hit warm boot")
            break

        # Trace execution after some BDOS calls
        if bdos_count >= trace_after and instr_count < max_trace:
            op = cpm.bus.read_mem(pc)
            a = cpm.cpu.get_a()
            bc = cpm.cpu.get_bc()
            de = cpm.cpu.get_de()
            hl = cpm.cpu.get_hl()
            sp = cpm.cpu.get_sp()

            # Show prefixed opcodes
            if op in [0xCB, 0xDD, 0xED, 0xFD]:
                op2 = cpm.bus.read_mem(pc + 1)
                if op in [0xDD, 0xFD] and op2 == 0xCB:
                    op3 = cpm.bus.read_mem(pc + 2)
                    op4 = cpm.bus.read_mem(pc + 3)
                    print(f"PC={pc:04X} [{op:02X} {op2:02X} {op3:02X} {op4:02X}] A={a:02X} BC={bc:04X} DE={de:04X} HL={hl:04X} SP={sp:04X}")
                else:
                    print(f"PC={pc:04X} [{op:02X} {op2:02X}] A={a:02X} BC={bc:04X} DE={de:04X} HL={hl:04X} SP={sp:04X}")
            else:
                print(f"PC={pc:04X} [{op:02X}] A={a:02X} BC={bc:04X} DE={de:04X} HL={hl:04X} SP={sp:04X}")
            instr_count += 1

        cpm.cpu.step()

        if cpm.cpu.mv_halted:
            print("CPU halted")
            break

        if not cpm.cpu.mv_running:
            print("CPU stopped running")
            break

    print(f"\n=== Stats ===")
    print(f"BDOS calls: {bdos_count}")
    print(f"Traced instructions: {instr_count}")
    print(f"\n=== Output ===")
    print(cpm.bus.get_output())


if __name__ == "__main__":
    debug_zork()
