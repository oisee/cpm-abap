# Z-Machine Specification Summary

This document summarizes the Z-machine virtual machine specification (v1.1) for implementing an interpreter in ABAP.

## Overview

The Z-machine is a virtual machine designed by Infocom in 1979 for running text adventure games. It's one of the most portable virtual machines ever created, with interpreters on hundreds of platforms.

**Key characteristics:**
- 8-bit virtual processor with 16-bit operations
- Stack-based architecture with local variables
- Built-in text compression (ZSCII encoding)
- Object-oriented game world model
- Multiple versions (1-8), with v3 being most common for classic games

## Memory Model

### Memory Regions

```
+------------------+ 0x00000
|     Header       |  (64 bytes)
+------------------+ 0x00040
|  Dynamic Memory  |  (read/write)
|  - Global vars   |
|  - Object table  |
|  - Property data |
+------------------+ Static memory mark (header $0E)
|  Static Memory   |  (read-only)
|  - Dictionary    |
|  - Grammar       |
+------------------+ High memory mark (header $04)
|  High Memory     |  (code/strings only)
|  - Routines      |
|  - Packed strings|
+------------------+ End of file
```

### Memory Limits by Version

| Version | Dynamic + Static | Total Story File |
|---------|------------------|------------------|
| 1-3     | 64 KB max        | 128 KB max       |
| 4-5     | 64 KB max        | 256 KB max       |
| 6-7     | 64 KB max        | 512 KB max       |
| 8       | 64 KB max        | 512 KB max       |

### Addressing Modes

| Type | Description | Usage |
|------|-------------|-------|
| **Byte address** | Direct byte offset (0-65535) | General memory access |
| **Word address** | Byte address ÷ 2 | Abbreviation tables |
| **Packed address** | Version-dependent formula | Routines and strings |

**Packed address formulas:**
- v1-3: `2 × P`
- v4-5: `4 × P`
- v6-7: `4 × P + offset` (separate offsets for routines/strings)
- v8: `8 × P`

## Header Format (64 bytes)

| Offset | Size | Contents |
|--------|------|----------|
| $00 | 1 | Version number (1-8) |
| $01 | 1 | Flags 1 (capabilities) |
| $04 | 2 | High memory base address |
| $06 | 2 | Initial PC (or main routine in v6) |
| $08 | 2 | Dictionary address |
| $0A | 2 | Object table address |
| $0C | 2 | Global variables table address |
| $0E | 2 | Static memory base address |
| $10 | 2 | Flags 2 (features) |
| $18 | 2 | Abbreviations table address |
| $1A | 2 | File length (÷ constant) |
| $1C | 2 | File checksum |
| $1E | 1 | Interpreter number |
| $1F | 1 | Interpreter version |
| $20 | 1 | Screen height (lines) |
| $21 | 1 | Screen width (chars) |
| $24 | 2 | Screen width (units) |
| $26 | 2 | Screen height (units) |
| $28-$2F | | Font/color information |

### Interpreter Numbers

| Number | Platform |
|--------|----------|
| 1 | DEC-20 |
| 2 | Apple IIe |
| 3 | Macintosh |
| 4 | Amiga |
| 5 | Atari ST |
| 6 | IBM PC |
| 7 | Commodore 128 |
| 8 | Commodore 64 |
| 9 | Apple IIc |
| 10 | Apple IIgs |
| 11 | Tandy Color |

For our ABAP interpreter, we could use a custom number (e.g., 20 for "SAP").

## Instruction Encoding

### Operand Types (2 bits each)

| Bits | Type | Size |
|------|------|------|
| 00 | Large constant | 2 bytes (0-65535) |
| 01 | Small constant | 1 byte (0-255) |
| 10 | Variable | 1 byte (var number) |
| 11 | Omitted | 0 bytes |

### Variable Numbers

| Range | Meaning |
|-------|---------|
| $00 | Stack (pop/push) |
| $01-$0F | Local variables (1-15) |
| $10-$FF | Global variables (16-255) |

### Instruction Forms

**Short form** (1 byte opcode):
- Top bits = `10`
- Bits 4-5 = operand type
- Bits 0-3 = opcode number
- 0OP if type=11, else 1OP

**Long form** (1 byte opcode):
- Top bits = `00`, `01`
- Bit 6 = first operand type (0=small, 1=variable)
- Bit 5 = second operand type
- Bits 0-4 = opcode number
- Always 2OP

**Variable form** (2+ bytes):
- Top bits = `11`
- Bit 5 = 0 for 2OP, 1 for VAR
- Bits 0-4 = opcode number
- Next byte = 4 operand types (2 bits each)

**Extended form** (v5+):
- First byte = $BE (190)
- Second byte = opcode number
- Third byte = operand types

### Store and Branch

**Store byte:** Single byte after instruction specifying destination variable.

**Branch data:**
- Bit 7 of first byte: 1=branch on true, 0=branch on false
- Bit 6: 1=short offset (6 bits), 0=long offset (14 bits signed)
- Offset 0 = return false
- Offset 1 = return true
- Other offsets: jump to `PC + offset - 2`

## Text Encoding (ZSCII)

### Z-character Structure

Each word (2 bytes) contains 3 Z-characters:
```
  First byte     Second byte
  7 6 5 4 3 2 1 0  7 6 5 4 3 2 1 0
  E|--ZC1--|--ZC2--|--ZC3--|
```
- Bit 15 (E): End marker (1 = last word)
- Bits 14-10: Z-character 1
- Bits 9-5: Z-character 2
- Bits 4-0: Z-character 3

### Alphabets

**A0 (lowercase, default):**
```
  6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
  a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z
```

**A1 (uppercase):**
```
  6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
  A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   W   X   Y   Z
```

**A2 (punctuation):**
```
  6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
  ^   CR  0   1   2   3   4   5   6   7   8   9   .   ,   !   ?   _   #   '   "   /   \   -   :   (   )
```
(Z-char 6 in A2 = special 10-bit ZSCII escape)

### Special Z-characters

| Z-char | Meaning |
|--------|---------|
| 0 | Space |
| 1 | Abbreviation 0-31 (v3+) |
| 2 | Abbreviation 32-63 (v3+) / Shift to A1 (v1-2) |
| 3 | Abbreviation 64-95 (v3+) / Shift to A2 (v1-2) |
| 4 | Shift to A1 |
| 5 | Shift to A2 |
| 6 (in A2) | 10-bit ZSCII follows |
| 7 (in A2) | Newline |

### Abbreviations

Formula: `32 × (z - 1) + next_zchar`

This gives entry 0-95 in abbreviations table.

## Object Model

### Object Entry Format

**Versions 1-3** (9 bytes per object, max 255 objects):
```
Bytes 0-3:   32 attribute flags (bits 0-31)
Byte 4:      Parent object number
Byte 5:      Sibling object number
Byte 6:      Child object number
Bytes 7-8:   Property table address
```

**Versions 4+** (14 bytes per object, max 65535 objects):
```
Bytes 0-5:   48 attribute flags (bits 0-47)
Bytes 6-7:   Parent object number
Bytes 8-9:   Sibling object number
Bytes 10-11: Child object number
Bytes 12-13: Property table address
```

### Property Table Format

Each object's property table contains:
1. **Short name:** Length byte + Z-encoded text
2. **Properties:** In descending numerical order

**Property block (v1-3):**
```
Size byte: bits 7-5 = size-1, bits 4-0 = property number
Data: 1-8 bytes
```

**Property block (v4+):**
```
First byte: bit 7 clear
  - bits 5-0 = property number
  - bit 6 = 0 for 1 byte, 1 for 2 bytes
OR
First byte: bit 7 set
  - bits 5-0 = property number
Second byte: bits 5-0 = length (0 means 64)
```

## Dictionary Format

Located at address in header $08:

```
1 byte:      Number of word-separator characters
n bytes:     Word-separator ZSCII codes
1 byte:      Entry length (bytes per entry)
2 bytes:     Number of entries
entries:     Sorted by encoded text value
```

Each entry:
- **v1-3:** 4 bytes encoded text (6 Z-chars) + data
- **v4+:** 6 bytes encoded text (9 Z-chars) + data

## Routines and Stack

### Routine Header

```
1 byte:      Number of local variables (0-15)
[v1-4 only] n×2 bytes: Initial values for locals
```

### Call Stack Frame

Each call creates a frame containing:
- Return PC
- Result variable (or discard flag)
- Local variables (1-15)
- Evaluation stack for that frame

### Stack Operations

- Variable $00 refers to stack top
- Reading $00 = pop
- Writing $00 = push
- Each routine has its own stack portion

## Version 3 Specifics (Target)

For Zork I-III compatibility, we focus on v3:

- **Max 255 objects** with 32 attributes each
- **4-byte encoded dictionary words** (6 Z-chars)
- **32 abbreviations** (single table)
- **Packed addresses** = 2 × P
- **Status line:** Score/moves or time display
- **Save/restore:** Branch instruction (not store)
- **No extended opcodes**

## References

- [Z-Machine Standards Document v1.1](https://inform-fiction.org/zmachine/standards/z1point1/)
- [Jared Reisinger's Z-Spec](https://zspec.jaredreisinger.com/)
- [Inform 6 Technical Manual](https://inform-fiction.org/manual/)
- [Frotz Interpreter Source](https://gitlab.com/DavidGriffith/frotz)

---

*Document created for ABAP Z-Machine Interpreter project*
*Version: 1.0*
*Date: 2025-12-07*
