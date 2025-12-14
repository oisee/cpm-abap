#!/usr/bin/env python3
"""Test Microsoft BASIC on Z80 6502 emulator"""

from run_6502 import Emu6502Runner

with open("msbasic/msbasic.bin", "rb") as f:
    code = f.read()

print(f"Loaded {len(code)} bytes of MS BASIC")

emu = Emu6502Runner(verbose=False)
emu.load_program(code, org=0x0800)

# Override entry point to COLD START at $2730
entry_6502 = 0x2730
emu.start_bank = entry_6502 >> 13
emu.start_addr = 0xC000 + ((entry_6502 & 0x1FFF) * 2)

# Provide input for BASIC startup
# BASIC prompts for memory size, then asks "WANT SIN-COS-TAN-ATN (Y/N)"
emu.bus.input_buffer = "\r\rN\rPRINT 1+1\r"

# Trace I/O operations
original_write_io = emu.bus.write_io
original_read_io = emu.bus.read_io
write_count = [0]
read_count = [0]

def traced_write_io(port, val):
    if port == 0x01:
        write_count[0] += 1
        if write_count[0] <= 200:
            ch = chr(val) if 32 <= val < 127 else f"${val:02X}"
            print(f"  OUT port 1 (COUT): {ch}")
    return original_write_io(port, val)

def traced_read_io(port):
    result = original_read_io(port)
    if port == 0x01:
        read_count[0] += 1
        if read_count[0] <= 20:
            ch = chr(result) if 32 <= result < 127 else f"${result:02X}"
            print(f"  IN port 1 (CHIN): {ch}")
    return result

emu.bus.write_io = traced_write_io
emu.bus.read_io = traced_read_io

print(f"Entry point: 6502 ${entry_6502:04X}")
print("Running MS BASIC for 500000 cycles...")
print("=" * 40)

output = emu.run(max_cycles=10_000_000)

print("=" * 40)
print(f"Total COUT calls: {write_count[0]}")
out_str = "".join(emu.output)
print(f"Output ({len(out_str)} chars): {repr(out_str[:200])}")
