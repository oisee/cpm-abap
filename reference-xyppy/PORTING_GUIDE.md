# Z-Machine Porting Guide: Python → ABAP

## Overview

`z3_minimal.py` is a ~1100 line Z-Machine V3 interpreter that can be ported to ABAP.

## Class Mapping

| Python Class | ABAP Class | Purpose |
|--------------|------------|---------|
| `Memory` | `ZCL_ZORK_00_MEMORY` | Story file memory, header parsing |
| `Stack` + `Frame` | `ZCL_ZORK_00_STACK` | Call stack and evaluation stack |
| `Objects` | `ZCL_ZORK_00_OBJECTS` | Object table, attributes, properties |
| `Text` | `ZCL_ZORK_00_DECODER` | Z-string encoding/decoding |
| `Dictionary` | Part of executor | Tokenization for READ opcode |
| `ZMachine` | `ZCL_ZORK_00_EXECUTOR` | Main interpreter, opcodes |

## Critical Implementation Details

### 1. Alphabet Indexing (Bug We Fixed!)

**Wrong:**
```python
A0 = ' ????? abcdefghijklmnopqrstuvwxyz'  # 32 chars
result.append(A0[zc])  # Direct index
```

**Correct:**
```python
A0 = 'abcdefghijklmnopqrstuvwxyz'  # 26 chars
result.append(A0[zc - 6])  # Subtract 6!
```

Z-characters 6-31 map to alphabet indices 0-25.

### 2. Big-Endian Bit Ordering for Attributes

Attribute 0 is bit 7 of byte 0, attribute 7 is bit 0 of byte 0:
```python
byte_idx = attr // 8
bit_idx = 7 - (attr % 8)  # Big-endian within byte!
```

**Example:** Attribute 3 in byte 0:
- bit_idx = 7 - 3 = 4
- mask = 1 << 4 = 0x10

### 3. Object Table Layout (V3)

```
Base + 0: Property defaults (31 words = 62 bytes)
Base + 62: First object entry

Each object (9 bytes):
  +0: Attributes (4 bytes = 32 bits)
  +4: Parent (1 byte)
  +5: Sibling (1 byte)
  +6: Child (1 byte)
  +7: Property pointer (2 bytes)
```

### 4. Dictionary Lookup

Text is encoded to 4 bytes (2 words) for V3:
- Max 6 characters
- 3 Z-characters per word (5 bits each = 15 bits + 1 end bit)
- Pad with 5 (shift char) if < 6 chars

```python
# Encode: char -> (A0.index(char) + 6)
# Pack: (zc1 << 10) | (zc2 << 5) | zc3
# Set end bit on last word: | 0x8000
```

### 5. Branch Offsets

```python
if byte1 & 0x40:  # Single byte
    offset = byte1 & 0x3F
else:  # Two bytes, signed
    offset = ((byte1 & 0x3F) << 8) | byte2
    if offset & 0x2000:  # Sign extend
        offset -= 0x4000
```

Special values:
- offset 0 = return false (rfalse)
- offset 1 = return true (rtrue)

### 6. Signed Arithmetic

Z-Machine uses 16-bit signed values:
```python
def to_signed(val):
    if val >= 0x8000:
        return val - 0x10000
    return val
```

For ABAP, use `CONV i( )` or handle explicitly.

## Opcode Summary (V3)

### 0OP (0x00-0x0F)
| Op | Name | Description |
|----|------|-------------|
| 00 | rtrue | Return 1 |
| 01 | rfalse | Return 0 |
| 02 | print | Print inline string |
| 03 | print_ret | Print + newline + return 1 |
| 05 | save | Save game (branch) |
| 06 | restore | Restore game (branch) |
| 08 | ret_popped | Return stack top |
| 0A | quit | Exit game |
| 0B | new_line | Print newline |

### 1OP (0x00-0x0F)
| Op | Name | Description |
|----|------|-------------|
| 00 | jz | Jump if zero |
| 01 | get_sibling | Store sibling, branch if exists |
| 02 | get_child | Store child, branch if exists |
| 03 | get_parent | Store parent |
| 05 | inc | Increment variable |
| 06 | dec | Decrement variable |
| 09 | remove_obj | Remove from parent |
| 0A | print_obj | Print object name |
| 0B | ret | Return value |
| 0C | jump | Unconditional jump |
| 0D | print_paddr | Print packed address |

### 2OP (0x01-0x1F)
| Op | Name | Description |
|----|------|-------------|
| 01 | je | Jump if equal (up to 4 operands) |
| 02 | jl | Jump if less |
| 03 | jg | Jump if greater |
| 06 | jin | Jump if obj in parent |
| 07 | test | Test bitmap |
| 08 | or | Bitwise OR |
| 09 | and | Bitwise AND |
| 0A | test_attr | Test attribute |
| 0B | set_attr | Set attribute |
| 0C | clear_attr | Clear attribute |
| 0D | store | Store to variable |
| 0E | insert_obj | Insert object as child |
| 0F | loadw | Load word from array |
| 10 | loadb | Load byte from array |
| 11 | get_prop | Get property value |
| 14 | add | Add |
| 15 | sub | Subtract |
| 16 | mul | Multiply |
| 17 | div | Divide (signed) |
| 18 | mod | Modulo (signed) |

### VAR (0x00-0x1F)
| Op | Name | Description |
|----|------|-------------|
| 00 | call | Call routine |
| 01 | storew | Store word to array |
| 02 | storeb | Store byte to array |
| 03 | put_prop | Set property value |
| 04 | sread | Read input line (V3) |
| 05 | print_char | Print character |
| 06 | print_num | Print signed number |
| 07 | random | Random number |
| 08 | push | Push to stack |
| 09 | pull | Pop from stack |

## Testing Checklist

1. ☐ Header parsing (version, addresses, serial)
2. ☐ Memory read/write (u8, u16, w8, w16)
3. ☐ Stack operations (push, pop, frames)
4. ☐ Object attributes (test, set, clear)
5. ☐ Object tree (parent, sibling, child, insert, remove)
6. ☐ Properties (get, put, next)
7. ☐ Text decoding (basic, abbreviations, 10-bit)
8. ☐ Text encoding (for dictionary)
9. ☐ Dictionary lookup
10. ☐ Tokenization (READ opcode)
11. ☐ All V3 opcodes
12. ☐ MiniZork: LOOK works
13. ☐ MiniZork: INVENTORY works
14. ☐ MiniZork: Navigation works
15. ☐ MiniZork: Object manipulation works

## Files

- `z3_minimal.py` - Working reference implementation
- `../test-games/minizork.z3` - Test story file
- This guide - `PORTING_GUIDE.md`
