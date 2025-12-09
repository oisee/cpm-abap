# Z-Machine Porting Status

*Last updated: 2025-12-08*

## Current Approach

1. **Created Python reference** - z3_minimal.py (~1100 lines)
2. **Tested thoroughly** - test_minizork.py (4 test scenarios, all pass)
3. **Porting to ABAP** - Compare with existing ABAP implementation

## Progress

### Completed
- [x] Evaluated existing Z-Machine implementations (xyppy, msinilo/zmachine)
- [x] Created minimal Z3 Python interpreter (z3-reference/z3_minimal.py)
- [x] Fixed Python alphabet indexing bug (zc-6 instead of direct index)
- [x] Created automated test suite (z3-reference/test_minizork.py)
- [x] All 4 MiniZork tests pass in Python
- [x] Reviewed existing ABAP classes (ZCL_ZORK_00_*)
- [x] Created PORTING_GUIDE.md documentation

### In Progress
- [ ] Fix story file truncation (16 bytes missing in ABAP)

### Pending
- [ ] Compare tokenization logic (Python vs ABAP)
- [ ] Test fixed ABAP implementation in SAP
- [ ] Verify MiniZork gameplay in SAP

## Known Issues

### 1. Story File Truncation
The base64-encoded MiniZork story in `ZCL_ZORK_00_GAME_MINIZORK` is 16 bytes short:
- Expected: 52216 bytes
- Actual: 52200 bytes

This causes issues at the end of the file (affects some data tables).

### 2. Original Bug (Now Fixed in Python)
The "grue death at West of House" bug was traced to:
- Attribute 3 is the "lit" flag (not attribute 1)
- Big-endian bit ordering was correct
- The actual root cause was likely in tokenization or the truncated story file

## Key Files

### Reference Implementation (Python)
| File | Description |
|------|-------------|
| z3-reference/z3_minimal.py | Working Z3 interpreter (~1100 lines) |
| z3-reference/test_minizork.py | 4 automated test scenarios |
| z3-reference/PORTING_GUIDE.md | Class mappings and implementation details |

### ABAP Implementation
| Object | Type | Description |
|--------|------|-------------|
| ZCL_ZORK_00_MEMORY | CLAS | Memory management |
| ZCL_ZORK_00_STACK | CLAS | Call/evaluation stack |
| ZCL_ZORK_00_DECODER | CLAS | Text decoder (ZSCII) |
| ZCL_ZORK_00_OBJECTS | CLAS | Object table handling |
| ZCL_ZORK_00_EXECUTOR | CLAS | Instruction executor |
| ZCL_ZORK_00_GAME_MINIZORK | CLAS | MiniZork story file |
| ZORK_00_CONSOLE | PROG | Main console program |

### Test Story
- `test-games/minizork.z3` - MiniZork v3 story file (52216 bytes)

## Critical Implementation Details

### 1. Alphabet Indexing
Z-characters 6-31 map to alphabet indices 0-25:
```python
# Python (CORRECT)
A0 = 'abcdefghijklmnopqrstuvwxyz'  # 26 chars
result.append(A0[zc - 6])  # Subtract 6!
```

### 2. Big-Endian Attributes
Attribute 0 is bit 7 of byte 0:
```python
byte_idx = attr // 8
bit_idx = 7 - (attr % 8)  # Big-endian within byte
```

### 3. Dictionary Encoding (V3)
- 6 characters max
- Encoded to 4 bytes (2 words)
- 3 z-chars per word (5 bits each)
- Pad with 5 (shift char) if shorter

### 4. Branch Offsets
```python
if byte1 & 0x40:  # Single byte
    offset = byte1 & 0x3F
else:  # Two bytes, signed
    offset = ((byte1 & 0x3F) << 8) | byte2
    if offset & 0x2000:  # Sign extend
        offset -= 0x4000
```

## Test Scenarios (All Pass in Python)

1. **Basic Navigation** - Circle the house (N, SW, S, E)
2. **Mailbox Interaction** - Open mailbox, take/read leaflet
3. **Forest and Tree** - Go to forest, climb tree, take egg
4. **Enter House** - Open window, enter, get lantern/sword

## Next Steps

1. **Fix story truncation**: Re-encode full 52216-byte story to base64
2. **Compare tokenization**:
   - Python: `encode_to_zchars()` in z3_minimal.py
   - ABAP: `encode_word_to_zchars` in ZCL_ZORK_00_EXECUTOR
3. **Test in SAP**: Run ZORK_00_CONSOLE and verify "look" command works
4. **Debug if needed**: Use breakpoints to trace execution

## How to Test

### Python Reference
```bash
cd z3-reference
python3 z3_minimal.py ../test-games/minizork.z3
# Interactive: type "look", "inventory", "north", etc.

# Automated tests:
python3 test_minizork.py
```

### ABAP (via MCP-ADT)
```
# Get source
mcp__a4h-abap-adt__GetSource(object_type="CLAS", name="ZCL_ZORK_00_EXECUTOR")

# Run unit tests
mcp__a4h-abap-adt__RunUnitTests(object_url="/sap/bc/adt/oo/classes/ZCL_ZORK_00_EXECUTOR")

# Set breakpoint for debugging
mcp__a4h-abap-adt__SetExternalBreakpoint(kind="line", object_uri="...", line=N)
```

## Notes

- xyppy (Python) works perfectly with MiniZork - used as behavioral reference
- msinilo/zmachine (Go) crashes on INVENTORY - not suitable as reference
- ABAP decode_zstring uses CASE statements (correct, different style from Python)
- The 16-byte truncation is the most likely cause of remaining bugs
