"""
6502 to Threaded Code Translator

Converts 6502 binary code into Z80 threaded code format.
Each 6502 instruction becomes:
  - 2 bytes: handler address
  - 0, 2, or 4 bytes: operand (depending on addressing mode)

Output format:
  For each instruction:
    [handler_lo] [handler_hi] [operand_lo] [operand_hi]?
"""

from dataclasses import dataclass
from opcodes import OPCODES, AddrMode, ADDR_MODE_BYTES
from memory_map import opcode_to_handler


@dataclass
class TranslatedInstruction:
    """Result of translating one 6502 instruction"""
    pc_6502: int            # Original 6502 address
    opcode: int             # 6502 opcode
    mnemonic: str           # Instruction mnemonic
    mode: AddrMode          # Addressing mode
    operand: int | None     # Operand value (if any)
    handler_addr: int       # Z80 handler address
    output_bytes: bytes     # Translated bytes


class Translator:
    """
    Translates 6502 binary to Z80 threaded code.
    """

    def __init__(self):
        self.org = 0x0800  # Default origin for 6502 code
        self.warnings: list[str] = []

    def translate(self, binary: bytes, org: int = 0x0800) -> bytes:
        """
        Translate entire 6502 binary to threaded code.

        Args:
            binary: Raw 6502 machine code
            org: Origin address (where code is loaded in 6502 memory)

        Returns:
            Threaded code bytes
        """
        self.org = org
        self.warnings = []
        output = bytearray()
        pc = 0

        while pc < len(binary):
            instr = self.translate_instruction(binary, pc, org + pc)
            output.extend(instr.output_bytes)
            pc += ADDR_MODE_BYTES.get(instr.mode, 1)

        return bytes(output)

    def translate_instruction(self, binary: bytes, offset: int, addr: int) -> TranslatedInstruction:
        """
        Translate a single 6502 instruction.

        Args:
            binary: Full binary data
            offset: Offset within binary
            addr: 6502 address of instruction

        Returns:
            TranslatedInstruction with all details
        """
        opcode = binary[offset]
        info = OPCODES.get(opcode)

        if info is None:
            # Illegal opcode - emit as data (handler for illegal op)
            self.warnings.append(f"Illegal opcode ${opcode:02X} at ${addr:04X}")
            return TranslatedInstruction(
                pc_6502=addr,
                opcode=opcode,
                mnemonic="???",
                mode=AddrMode.IMP,
                operand=None,
                handler_addr=opcode_to_handler(opcode),
                output_bytes=self._emit_handler(opcode)
            )

        handler = opcode_to_handler(opcode)
        operand = None
        output = bytearray()

        # Emit handler address (little-endian)
        output.extend(self._emit_handler(opcode))

        # Handle operand based on addressing mode
        operand_bytes = info.bytes - 1

        if operand_bytes == 1:
            # 1-byte operand (immediate, zero page, relative)
            operand = binary[offset + 1] if offset + 1 < len(binary) else 0
            # Emit as 16-bit word (for uniform POP BC handling)
            output.append(operand & 0xFF)
            output.append(0x00)

        elif operand_bytes == 2:
            # 2-byte operand (absolute address)
            lo = binary[offset + 1] if offset + 1 < len(binary) else 0
            hi = binary[offset + 2] if offset + 2 < len(binary) else 0
            operand = (hi << 8) | lo
            output.append(lo)
            output.append(hi)

        return TranslatedInstruction(
            pc_6502=addr,
            opcode=opcode,
            mnemonic=info.mnemonic,
            mode=info.mode,
            operand=operand,
            handler_addr=handler,
            output_bytes=bytes(output)
        )

    def _emit_handler(self, opcode: int) -> bytes:
        """Emit handler address as little-endian bytes"""
        handler = opcode_to_handler(opcode)
        return bytes([handler & 0xFF, (handler >> 8) & 0xFF])

    def disassemble(self, binary: bytes, org: int = 0x0800) -> list[TranslatedInstruction]:
        """
        Disassemble and translate, returning list of instructions.
        Useful for debugging and visualization.
        """
        self.org = org
        self.warnings = []
        result = []
        pc = 0

        while pc < len(binary):
            instr = self.translate_instruction(binary, pc, org + pc)
            result.append(instr)
            info = OPCODES.get(instr.opcode)
            pc += info.bytes if info else 1

        return result


def hexdump(data: bytes, org: int = 0, bytes_per_line: int = 16) -> str:
    """Format bytes as hex dump"""
    lines = []
    for i in range(0, len(data), bytes_per_line):
        chunk = data[i:i + bytes_per_line]
        hex_part = ' '.join(f'{b:02X}' for b in chunk)
        lines.append(f"${org + i:04X}: {hex_part}")
    return '\n'.join(lines)


# Test with simple 6502 programs
if __name__ == "__main__":
    print("6502 to Threaded Code Translator")
    print("=" * 60)

    # Test program 1: Simple loop
    # LDA #$41      ; Load 'A'
    # JSR $FDED     ; COUT (print char)
    # INX           ; increment X
    # BNE $0800     ; loop (relative: -8 bytes)
    # BRK           ; break

    test1 = bytes([
        0xA9, 0x41,       # LDA #$41
        0x20, 0xED, 0xFD, # JSR $FDED
        0xE8,             # INX
        0xD0, 0xF8,       # BNE -8 (back to LDA)
        0x00,             # BRK
    ])

    print("\nTest Program 1: Print 'A' loop")
    print("-" * 60)
    print("6502 source:")
    print(hexdump(test1, 0x0800))

    translator = Translator()
    instructions = translator.disassemble(test1, org=0x0800)

    print("\nDisassembly with threaded code:")
    print(f"{'Addr':6s} {'Opcode':8s} {'Mnemonic':10s} {'Operand':8s} {'Handler':8s} {'Output'}")
    print("-" * 60)

    for instr in instructions:
        operand_str = f"${instr.operand:04X}" if instr.operand is not None else ""
        output_hex = ' '.join(f'{b:02X}' for b in instr.output_bytes)
        print(f"${instr.pc_6502:04X}  ${instr.opcode:02X}      {instr.mnemonic:10s} {operand_str:8s} ${instr.handler_addr:04X}    {output_hex}")

    threaded = translator.translate(test1, org=0x0800)
    print(f"\nThreaded code output ({len(threaded)} bytes):")
    print(hexdump(threaded, 0xD000))  # Code would be at $D000 in Z80

    # Test program 2: Hello World data
    # LDX #$00      ; X = 0
    # loop:
    # LDA $0820,X   ; Load char from message
    # BEQ done      ; If zero, done
    # JSR $FDED     ; Print it
    # INX           ; Next char
    # BNE loop      ; Continue
    # done:
    # RTS

    print("\n" + "=" * 60)
    print("Test Program 2: Hello World")
    print("-" * 60)

    test2 = bytes([
        0xA2, 0x00,       # LDX #$00
        0xBD, 0x20, 0x08, # LDA $0820,X
        0xF0, 0x06,       # BEQ +6 (to RTS)
        0x20, 0xED, 0xFD, # JSR $FDED
        0xE8,             # INX
        0xD0, 0xF5,       # BNE -11 (to LDA)
        0x60,             # RTS
    ])

    instructions2 = translator.disassemble(test2, org=0x0800)

    print(f"{'Addr':6s} {'Bytes':12s} {'Mnemonic':10s} {'Handler':8s}")
    print("-" * 60)

    for instr in instructions2:
        info = OPCODES.get(instr.opcode)
        if info:
            nbytes = info.bytes
        else:
            nbytes = 1
        # Get original bytes
        idx = instr.pc_6502 - 0x0800
        orig_bytes = ' '.join(f'{b:02X}' for b in test2[idx:idx + nbytes])
        operand_str = ""
        if instr.operand is not None:
            if instr.mode == AddrMode.REL:
                # Show relative as signed
                rel = instr.operand if instr.operand < 128 else instr.operand - 256
                operand_str = f"{rel:+d}"
            elif instr.mode == AddrMode.IMM:
                operand_str = f"#${instr.operand:02X}"
            else:
                operand_str = f"${instr.operand:04X}"
        print(f"${instr.pc_6502:04X}  {orig_bytes:12s} {instr.mnemonic:4s} {operand_str:8s} ${instr.handler_addr:04X}")

    threaded2 = translator.translate(test2, org=0x0800)
    print(f"\nThreaded code ({len(threaded2)} bytes, expansion: {len(threaded2)/len(test2):.1f}x):")
    print(hexdump(threaded2, 0xD000))

    if translator.warnings:
        print("\nWarnings:")
        for w in translator.warnings:
            print(f"  {w}")
