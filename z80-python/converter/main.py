#!/usr/bin/env python3
"""
6502 to Z80 Threaded Code Converter - CLI

Usage:
    python3 main.py input.bin [-o output.bin] [--org 0x0800] [--disasm]
"""

import argparse
import sys
from pathlib import Path

from opcodes import OPCODES
from translator import Translator, hexdump


def main():
    parser = argparse.ArgumentParser(
        description="Convert 6502 binary to Z80 threaded code"
    )
    parser.add_argument("input", help="Input 6502 binary file")
    parser.add_argument("-o", "--output", help="Output threaded code file")
    parser.add_argument("--org", type=lambda x: int(x, 0), default=0x0800,
                        help="6502 origin address (default: 0x0800)")
    parser.add_argument("--disasm", action="store_true",
                        help="Show disassembly")
    parser.add_argument("--hexdump", action="store_true",
                        help="Show hex dump of output")

    args = parser.parse_args()

    # Read input
    input_path = Path(args.input)
    if not input_path.exists():
        print(f"Error: File not found: {input_path}", file=sys.stderr)
        return 1

    binary = input_path.read_bytes()
    print(f"Input: {input_path} ({len(binary)} bytes)")
    print(f"Origin: ${args.org:04X}")

    # Translate
    translator = Translator()

    if args.disasm:
        print("\nDisassembly:")
        print("-" * 60)
        instructions = translator.disassemble(binary, org=args.org)
        for instr in instructions:
            info = OPCODES.get(instr.opcode)
            nbytes = info.bytes if info else 1
            idx = instr.pc_6502 - args.org
            orig = ' '.join(f'{b:02X}' for b in binary[idx:idx + nbytes])

            operand_str = ""
            if instr.operand is not None:
                if instr.mode.name == "REL":
                    rel = instr.operand if instr.operand < 128 else instr.operand - 256
                    target = instr.pc_6502 + 2 + rel
                    operand_str = f"${target:04X}"
                elif instr.mode.name == "IMM":
                    operand_str = f"#${instr.operand:02X}"
                elif instr.mode.name in ("ZP", "ZPX", "ZPY"):
                    operand_str = f"${instr.operand:02X}"
                else:
                    operand_str = f"${instr.operand:04X}"

            mode_suffix = {
                "ZPX": ",X", "ZPY": ",Y",
                "ABX": ",X", "ABY": ",Y",
                "IZX": ",X)", "IZY": "),Y",
            }.get(instr.mode.name, "")

            if instr.mode.name in ("IZX", "IZY"):
                operand_str = f"(${instr.operand:02X}{mode_suffix}"
            elif mode_suffix:
                operand_str += mode_suffix

            print(f"${instr.pc_6502:04X}  {orig:12s} {instr.mnemonic:4s} {operand_str}")

    threaded = translator.translate(binary, org=args.org)
    ratio = len(threaded) / len(binary) if binary else 0

    print(f"\nOutput: {len(threaded)} bytes (expansion: {ratio:.2f}x)")

    if translator.warnings:
        print("\nWarnings:")
        for w in translator.warnings:
            print(f"  {w}")

    if args.hexdump:
        print("\nThreaded code:")
        print(hexdump(threaded, 0xD000))

    # Write output
    if args.output:
        output_path = Path(args.output)
        output_path.write_bytes(threaded)
        print(f"\nWritten to: {output_path}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
