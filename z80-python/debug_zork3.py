#!/usr/bin/env python3
"""Debug ZORK1.COM execution - check for loops"""

import os
from cpm_z80 import CpmEmulator


def debug_zork():
    """Debug ZORK1.COM execution"""
    zork_dir = "../test-games"
    cpm = CpmEmulator(zork_dir)
    cpm.reset()

    zork_path = os.path.join(zork_dir, "ZORK1.COM")
    size = cpm.load_file(zork_path)
    print(f"Loaded ZORK1.COM: {size} bytes")

    cpm.setup_fcb(0x005C, "ZORK1.DAT")

    bdos_count = 0
    instr_count = 0
    max_instr = 200000  # 200K instructions

    # Track PC history to detect loops
    pc_history = {}
    last_pc = -1
    repeat_count = 0

    while cpm.running and instr_count < max_instr:
        pc = cpm.cpu.get_pc()

        if pc == 0xFE00:
            func = cpm.cpu.get_c()
            bdos_count += 1
            de = cpm.cpu.get_de()

            if func == 33:  # Read random
                r0 = cpm.bus.read_mem(de + 33)
                r1 = cpm.bus.read_mem(de + 34)
                rec = r0 | (r1 << 8)
                print(f"[BDOS {func}] Read random rec={rec} (instr #{instr_count})")
            elif func in [2, 9]:  # Console output
                print(f"[BDOS {func}] Console output (instr #{instr_count})")
            else:
                print(f"[BDOS {func}] DE={de:04X} (instr #{instr_count})")

            if not cpm.bdos_call(func):
                break
            cpm.cpu.step()
            instr_count += 1
            continue

        if pc == 0xFF00:
            print("Hit warm boot")
            break

        # Track PC to detect loops
        if pc == last_pc:
            repeat_count += 1
        else:
            repeat_count = 0
            last_pc = pc

        if repeat_count > 10:
            print(f"Detected repeat at PC={pc:04X} after {instr_count} instructions")
            op = cpm.bus.read_mem(pc)
            print(f"Opcode: {op:02X}")
            break

        # Track hot spots
        pc_history[pc] = pc_history.get(pc, 0) + 1

        cpm.cpu.step()
        instr_count += 1

        if cpm.cpu.mv_halted:
            print("CPU halted")
            break

    # Show hot spots
    print(f"\n=== Top 20 Hot PCs (out of {len(pc_history)} unique) ===")
    sorted_pcs = sorted(pc_history.items(), key=lambda x: -x[1])
    for pc, count in sorted_pcs[:20]:
        op = cpm.bus.read_mem(pc)
        print(f"PC={pc:04X} ({op:02X}): {count} times")

    print(f"\n=== Stats ===")
    print(f"BDOS calls: {bdos_count}")
    print(f"Instructions: {instr_count}")
    print(f"\n=== Output ===")
    print(cpm.bus.get_output())


if __name__ == "__main__":
    debug_zork()
