# Z-Machine Interpreter Implementation Plan

## Project Overview

Build a Z-machine v3 interpreter in ABAP to run classic Infocom text adventures (Zork I, II, III) natively in SAP systems.

## Comparison: i8080 vs Z-machine

| Aspect | i8080 Emulator | Z-machine Interpreter |
|--------|----------------|----------------------|
| Complexity | ~100 opcodes | ~80 opcodes (v3) |
| Memory | 64 KB flat | 128 KB segmented |
| Encoding | Fixed 1-3 bytes | Variable 1-23 bytes |
| Stack | Hardware stack | Virtual call stack |
| Text | ASCII | ZSCII (compressed) |
| Objects | None | Object tree system |
| I/O | Port-based | Abstract interface |

**Key insight:** Z-machine is higher-level but the instruction decoder is more complex.

## Implementation Phases

### Phase 1: Core Infrastructure (Foundation)

**Goal:** Basic memory, header parsing, and instruction decoding.

#### 1.1 Memory Management
- [ ] `zcl_zmachine_memory` class
- [ ] Load story file from hex string
- [ ] Read/write byte operations
- [ ] Read/write word operations (big-endian)
- [ ] Memory region validation (dynamic/static/high)
- [ ] Tests for all memory operations

**Estimated opcodes:** 0 (infrastructure only)

#### 1.2 Header Parsing
- [ ] Parse all header fields
- [ ] Extract key addresses (dictionary, objects, globals)
- [ ] Validate version number (v3)
- [ ] Set interpreter identification
- [ ] Tests for header parsing

#### 1.3 Instruction Decoder
- [ ] `zcl_zmachine_decoder` class
- [ ] Short form decoding
- [ ] Long form decoding
- [ ] Variable form decoding
- [ ] Operand extraction
- [ ] Store byte parsing
- [ ] Branch data parsing
- [ ] Tests for all instruction forms

### Phase 2: Execution Engine (Core)

**Goal:** Variable access, branching, basic arithmetic.

#### 2.1 Variable System
- [ ] Stack variable (var 0) - push/pop
- [ ] Local variables (vars 1-15)
- [ ] Global variables (vars 16-255)
- [ ] `zcl_zmachine_stack` class
- [ ] Tests for variable operations

#### 2.2 Arithmetic Opcodes (7 opcodes)
- [ ] `add` - 2OP:20
- [ ] `sub` - 2OP:21
- [ ] `mul` - 2OP:22
- [ ] `div` - 2OP:23
- [ ] `mod` - 2OP:24
- [ ] `and` - 2OP:9
- [ ] `or` - 2OP:8
- [ ] Tests for arithmetic

#### 2.3 Comparison/Branch Opcodes (6 opcodes)
- [ ] `je` - 2OP:1 (equal, multiple operands)
- [ ] `jl` - 2OP:2 (less than, signed)
- [ ] `jg` - 2OP:3 (greater than, signed)
- [ ] `jz` - 1OP:0 (zero test)
- [ ] `jump` - 1OP:12 (unconditional)
- [ ] Tests for comparisons

#### 2.4 Load/Store Opcodes (6 opcodes)
- [ ] `loadw` - 2OP:15
- [ ] `loadb` - 2OP:16
- [ ] `storew` - VAR:1
- [ ] `storeb` - VAR:2
- [ ] `store` - 2OP:13
- [ ] `load` - 1OP:14
- [ ] Tests for load/store

### Phase 3: Routine Calls (Critical)

**Goal:** Subroutine call/return mechanism.

#### 3.1 Call Stack
- [ ] Frame structure (return PC, locals, eval stack)
- [ ] Push/pop frames
- [ ] Local variable initialization (v3: from routine header)
- [ ] Argument passing

#### 3.2 Call Opcodes (2 opcodes)
- [ ] `call` - VAR:0 (v3 primary call)
- [ ] Packed address calculation

#### 3.3 Return Opcodes (4 opcodes)
- [ ] `ret` - 1OP:11
- [ ] `rtrue` - 0OP:0
- [ ] `rfalse` - 0OP:1
- [ ] `ret_popped` - 0OP:8
- [ ] Tests for call/return

### Phase 4: Text System (Essential for Output)

**Goal:** ZSCII decoding for game output.

#### 4.1 Text Decoder
- [ ] `zcl_zmachine_text` class
- [ ] Z-character extraction from words
- [ ] Alphabet switching (A0/A1/A2)
- [ ] 10-bit ZSCII escape handling
- [ ] Abbreviation expansion
- [ ] Tests for text decoding

#### 4.2 Print Opcodes (7 opcodes)
- [ ] `print` - 0OP:2 (literal string)
- [ ] `print_ret` - 0OP:3 (print + newline + return true)
- [ ] `print_char` - VAR:5
- [ ] `print_num` - VAR:6
- [ ] `print_addr` - 1OP:7
- [ ] `print_paddr` - 1OP:13
- [ ] `new_line` - 0OP:11
- [ ] Tests for printing

### Phase 5: Object System (Game Logic)

**Goal:** Object tree manipulation for game world.

#### 5.1 Object Access
- [ ] `zcl_zmachine_objects` class
- [ ] Object address calculation
- [ ] Parent/sibling/child access
- [ ] Property table parsing
- [ ] Short name extraction

#### 5.2 Object Opcodes (15 opcodes)
- [ ] `get_parent` - 1OP:3
- [ ] `get_sibling` - 1OP:1
- [ ] `get_child` - 1OP:2
- [ ] `jin` - 2OP:6 (object in object?)
- [ ] `insert_obj` - 2OP:14
- [ ] `remove_obj` - 1OP:9
- [ ] `test_attr` - 2OP:10
- [ ] `set_attr` - 2OP:11
- [ ] `clear_attr` - 2OP:12
- [ ] `get_prop` - 2OP:17
- [ ] `put_prop` - VAR:3
- [ ] `get_prop_addr` - 2OP:18
- [ ] `get_prop_len` - 1OP:4
- [ ] `get_next_prop` - 2OP:19
- [ ] `print_obj` - 1OP:10
- [ ] Tests for object operations

### Phase 6: Input System (Interactivity)

**Goal:** Keyboard input and text parsing.

#### 6.1 Input/Output Interface
- [ ] `zif_zmachine_io` interface
- [ ] `zcl_zmachine_io_console` for testing
- [ ] Output buffering
- [ ] Input simulation for tests

#### 6.2 Text Encoder
- [ ] Word to Z-character encoding
- [ ] Dictionary lookup
- [ ] Lexical analysis (tokenization)

#### 6.3 Input Opcodes (2 opcodes)
- [ ] `sread` - VAR:4 (v3 input)
- [ ] `show_status` - 0OP:12 (v3 status line)
- [ ] Tests for input

### Phase 7: Remaining Opcodes

**Goal:** Complete v3 opcode coverage.

#### 7.1 Stack Opcodes (3 opcodes)
- [ ] `push` - VAR:8
- [ ] `pull` - VAR:9
- [ ] `pop` - 0OP:9 (v1-4)

#### 7.2 Miscellaneous (10 opcodes)
- [ ] `inc` - 1OP:5
- [ ] `dec` - 1OP:6
- [ ] `inc_chk` - 2OP:5
- [ ] `dec_chk` - 2OP:4
- [ ] `not` - 1OP:15
- [ ] `random` - VAR:7
- [ ] `restart` - 0OP:7
- [ ] `quit` - 0OP:10
- [ ] `verify` - 0OP:13
- [ ] `nop` - 0OP:4

#### 7.3 Save/Restore (2 opcodes)
- [ ] `save` - 0OP:5 (branch in v3)
- [ ] `restore` - 0OP:6 (branch in v3)

### Phase 8: Integration and Testing

**Goal:** Run real games.

#### 8.1 Test Suite
- [ ] Create minimal test story files
- [ ] Unit tests for each opcode
- [ ] Integration tests
- [ ] Czech (z-machine test suite) compatibility

#### 8.2 Game Testing
- [ ] Load Zork I story file
- [ ] Verify header parsing
- [ ] Test initial room description
- [ ] Test basic commands (n, s, e, w, look, inventory)
- [ ] Complete walkthrough test

#### 8.3 SAP Integration
- [ ] `zcl_zmachine_io_sap` for SAP GUI
- [ ] Dynpro-based interface (optional)
- [ ] CL_GUI_TEXTEDIT based interface
- [ ] Save/restore to SAP database

## Opcode Summary (v3)

### 2OP Opcodes (28 total, 20 needed)

| Dec | Hex | Mnemonic | Phase | Priority |
|-----|-----|----------|-------|----------|
| 1 | 01 | je | 2 | HIGH |
| 2 | 02 | jl | 2 | HIGH |
| 3 | 03 | jg | 2 | HIGH |
| 4 | 04 | dec_chk | 7 | MED |
| 5 | 05 | inc_chk | 7 | MED |
| 6 | 06 | jin | 5 | HIGH |
| 7 | 07 | test | 5 | HIGH |
| 8 | 08 | or | 2 | HIGH |
| 9 | 09 | and | 2 | HIGH |
| 10 | 0A | test_attr | 5 | HIGH |
| 11 | 0B | set_attr | 5 | HIGH |
| 12 | 0C | clear_attr | 5 | HIGH |
| 13 | 0D | store | 2 | HIGH |
| 14 | 0E | insert_obj | 5 | HIGH |
| 15 | 0F | loadw | 2 | HIGH |
| 16 | 10 | loadb | 2 | HIGH |
| 17 | 11 | get_prop | 5 | HIGH |
| 18 | 12 | get_prop_addr | 5 | MED |
| 19 | 13 | get_next_prop | 5 | MED |
| 20 | 14 | add | 2 | HIGH |
| 21 | 15 | sub | 2 | HIGH |
| 22 | 16 | mul | 2 | HIGH |
| 23 | 17 | div | 2 | HIGH |
| 24 | 18 | mod | 2 | HIGH |

### 1OP Opcodes (16 total, 15 needed)

| Dec | Hex | Mnemonic | Phase | Priority |
|-----|-----|----------|-------|----------|
| 0 | 00 | jz | 2 | HIGH |
| 1 | 01 | get_sibling | 5 | HIGH |
| 2 | 02 | get_child | 5 | HIGH |
| 3 | 03 | get_parent | 5 | HIGH |
| 4 | 04 | get_prop_len | 5 | MED |
| 5 | 05 | inc | 7 | MED |
| 6 | 06 | dec | 7 | MED |
| 7 | 07 | print_addr | 4 | HIGH |
| 9 | 09 | remove_obj | 5 | HIGH |
| 10 | 0A | print_obj | 5 | HIGH |
| 11 | 0B | ret | 3 | HIGH |
| 12 | 0C | jump | 2 | HIGH |
| 13 | 0D | print_paddr | 4 | HIGH |
| 14 | 0E | load | 2 | HIGH |
| 15 | 0F | not | 7 | MED |

### 0OP Opcodes (16 total, 13 needed for v3)

| Dec | Hex | Mnemonic | Phase | Priority |
|-----|-----|----------|-------|----------|
| 0 | 00 | rtrue | 3 | HIGH |
| 1 | 01 | rfalse | 3 | HIGH |
| 2 | 02 | print | 4 | HIGH |
| 3 | 03 | print_ret | 4 | HIGH |
| 4 | 04 | nop | 7 | LOW |
| 5 | 05 | save | 7 | MED |
| 6 | 06 | restore | 7 | MED |
| 7 | 07 | restart | 7 | MED |
| 8 | 08 | ret_popped | 3 | HIGH |
| 9 | 09 | pop | 7 | MED |
| 10 | 0A | quit | 7 | HIGH |
| 11 | 0B | new_line | 4 | HIGH |
| 12 | 0C | show_status | 6 | MED |
| 13 | 0D | verify | 7 | LOW |

### VAR Opcodes (10 needed for v3)

| Dec | Hex | Mnemonic | Phase | Priority |
|-----|-----|----------|-------|----------|
| 0 | 00 | call | 3 | HIGH |
| 1 | 01 | storew | 2 | HIGH |
| 2 | 02 | storeb | 2 | HIGH |
| 3 | 03 | put_prop | 5 | HIGH |
| 4 | 04 | sread | 6 | HIGH |
| 5 | 05 | print_char | 4 | HIGH |
| 6 | 06 | print_num | 4 | HIGH |
| 7 | 07 | random | 7 | MED |
| 8 | 08 | push | 7 | MED |
| 9 | 09 | pull | 7 | MED |

## Milestone Targets

### Milestone 1: "Hello World"
- Memory, header, decoder working
- `print`, `new_line`, `quit` opcodes
- Static string output
- **Test:** Print "Hello, Adventurer!" and quit

### Milestone 2: Calculator
- Arithmetic opcodes
- Variables (global, local)
- Basic call/return
- **Test:** Call routine, compute 2+2, return result

### Milestone 3: Text Adventure Shell
- Full text system
- Object system basics
- Input parsing
- **Test:** Two-room game with movement

### Milestone 4: Zork Boots
- All v3 opcodes implemented
- Zork I loads successfully
- Opening text displays
- **Test:** See "West of House" description

### Milestone 5: Zork Playable
- Full input handling
- Save/restore working
- Complete game playable
- **Test:** Complete Zork I walkthrough

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Complex instruction decoding | HIGH | Thorough unit tests |
| ZSCII edge cases | MED | Reference implementation comparison |
| Object tree corruption | HIGH | Extensive object tests |
| Performance issues | MED | Profile hot paths, optimize |
| Transpiler incompatibility | MED | Test frequently with `npm test` |

## Testing Resources

- **Czech:** Z-machine test suite for opcode verification
- **Praxix:** Another test suite
- **TerpEtude:** Standard compliance tests
- **Mini-Zork:** Simplified game for testing
- **Zork I-III:** Ultimate validation

## References

- [Z-Machine Standards 1.1](https://inform-fiction.org/zmachine/standards/z1point1/)
- [Frotz Source Code](https://gitlab.com/DavidGriffith/frotz) (C reference)
- [ZILF](https://foss.heptapod.net/zilf/zilf) (Modern ZIL compiler)
- [Infocom Fact Sheet](https://www.ifarchive.org/indexes/if-archive/infocom/)
- [Quetzal Save Format](https://www.ifarchive.org/if-archive/infocom/interpreters/specification/savefile_14.txt)

## Alternative: ZIL to ABAP Compiler?

Instead of interpreting Z-code, we could compile ZIL source to native ABAP:

**Pros:**
- Better performance
- Native SAP integration
- Debuggable ABAP code

**Cons:**
- Much more complex
- Only works with available source
- Need ZIL parser in ABAP

**Recommendation:** Start with interpreter. Consider compiler as future enhancement once interpreter proves the concept.

---

*Document created for ABAP Z-Machine Interpreter project*
*Version: 1.0*
*Date: 2025-12-07*
