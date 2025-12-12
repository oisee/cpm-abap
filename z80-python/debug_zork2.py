#!/usr/bin/env python3
"""Debug ZORK1.COM execution - longer run"""

import os
from cpm_z80 import CpmEmulator


def debug_zork():
    """Debug ZORK1.COM execution"""
    zork_dir = "../test-games"
    cpm = CpmEmulator(zork_dir)
    cpm.reset()
    # cpm.debug = True  # Verbose BDOS

    zork_path = os.path.join(zork_dir, "ZORK1.COM")
    size = cpm.load_file(zork_path)
    print(f"Loaded ZORK1.COM: {size} bytes")

    cpm.setup_fcb(0x005C, "ZORK1.DAT")

    bdos_count = 0
    total_cycles = 0
    max_cycles = 10000000  # 10M cycles

    while cpm.running and total_cycles < max_cycles:
        pc = cpm.cpu.get_pc()

        if pc == 0xFE00:
            func = cpm.cpu.get_c()
            bdos_count += 1

            # Print all BDOS calls
            de = cpm.cpu.get_de()
            if func == 2:  # Console output
                ch = cpm.cpu.get_e()
                print(f"[BDOS {func}] Console output: {ch:02X} '{chr(ch) if 32 <= ch < 127 else '?'}'")
            elif func == 9:  # Print string
                addr = de
                s = ""
                while cpm.bus.read_mem(addr) != 0x24:
                    s += chr(cpm.bus.read_mem(addr))
                    addr += 1
                    if len(s) > 80:
                        break
                print(f"[BDOS {func}] Print string: {s[:40]}...")
            elif func in [15, 16, 20, 33]:  # File ops
                fname = cpm.files.fcb_to_filename(cpm.bus, de)
                if func == 33:
                    r0 = cpm.bus.read_mem(de + 33)
                    r1 = cpm.bus.read_mem(de + 34)
                    rec = r0 | (r1 << 8)
                    print(f"[BDOS {func}] Read random rec={rec}")
                else:
                    print(f"[BDOS {func}] File op on {fname}")
            else:
                print(f"[BDOS {func}] DE={de:04X}")

            if not cpm.bdos_call(func):
                break
            cpm.cpu.step()
            continue

        if pc == 0xFF00:
            print("Hit warm boot")
            break

        cpm.cpu.step()
        total_cycles += cpm.cpu.mv_cycles

        if cpm.cpu.mv_halted:
            print("CPU halted")
            break

        if not cpm.cpu.mv_running:
            print("CPU stopped running")
            break

    print(f"\n=== Stats ===")
    print(f"BDOS calls: {bdos_count}")
    print(f"Total cycles: {total_cycles}")
    print(f"\n=== Output ===")
    print(cpm.bus.get_output())


if __name__ == "__main__":
    debug_zork()
