# Retro Computing Emulators in ABAP

Two vintage computing emulators implemented in ABAP:
1. **Z80/i8080 CPU Emulator** - For running CP/M programs
2. **Z-Machine Interpreter** - For running Infocom interactive fiction (ZORK!)

## Current Status (2025-12-11)

### CP/M 2.2 Emulator - WORKING!
✅ **Interactive console** - ZCPM_CONSOLE with HTML display in SAP GUI
✅ **Full I/O support** - BDOS functions 1, 2, 6, 9, 10, 11 (console I/O)
✅ **File operations** - BDOS 15, 16, 20, 26, 33, 35, 36 (open, close, read, DMA)
✅ **Runs ZORK!** - Load ZORK1.COM + ZORK1.DAT from SMW0
✅ **Input waiting** - Pauses for user input, resumes on Enter

### Z-Machine Interpreter
✅ **Working!** - Plays MiniZork in SAP GUI
✅ **4 core classes** with 30+ unit tests
✅ **Proper ABAP types** - TS_/TT_/TY_ conventions with internal tables
✅ **Deployed to SAP** - Executor and all core opcodes implemented

### Z80 CPU Emulator
✅ **Full prefix opcode support** - CB, DD, ED, FD handlers
✅ **Composition architecture** - Separate handler classes via interface
✅ **38 unit tests, all passing**
✅ **SAP package: $ZCPU_Z80_00**

### i8080 CPU Emulator
✅ **86 / ~105 core i8080 opcodes implemented (82%)**
✅ **16 unit tests, all passing**
✅ **Local TDD workflow operational** (transpile + test < 1 second)

### Quick Start - CP/M Emulator (SAP)

```
1. Upload ZORK1.COM and ZORK1.DAT to SMW0
2. Run transaction SE38 → ZCPM_CONSOLE
3. Select "SMW0" source, choose ZORK1.COM
4. Play ZORK in SAP GUI!
```

### Quick Start - Local Development

```bash
# Run tests locally
npm test

# Expected output: All 16 tests passing
# ZCL_CPU_8080_V2: running zcl_cpu_8080_test->test_init
# ZCL_CPU_8080_V2: running zcl_cpu_8080_test->test_memory
# ... (all 16 tests pass)
```

## Documentation

📚 **Start here for different purposes:**

- **[CONTEXT.md](CONTEXT.md)** - Current project status, architecture decisions, session history
- **[TODO.md](TODO.md)** - Implementation plan, missing opcodes (~19 remaining), test strategy
- **[CLAUDE.md](CLAUDE.md)** - Instructions for AI assistants (Claude, GPT, etc.)
- **[BRAINSTORM.md](BRAINSTORM.md)** - Architecture analysis (why hybrid approach)
- **[TRANSPILER.md](TRANSPILER.md)** - ABAP transpiler setup and constraints
- **[SESSION_NOTES.md](SESSION_NOTES.md)** - Debugging notes from opcode 49 bug fix

## Project Structure

```
cpm-abap/
├── src/
│   │
│   │ ## CP/M Emulator (Z80-based)
│   ├── zcl_cpm_emulator.clas.abap             # CP/M 2.2 emulator with BDOS
│   ├── zcl_cpm_speedrun.clas.abap             # Speedrun test runner
│   ├── zcpm_console.prog.abap                 # Interactive console (SAP GUI)
│   ├── zcpm_zork_test.prog.abap               # ZORK test program
│   ├── zcpm_io_test.prog.abap                 # I/O test (HELLO_NAME.COM)
│   ├── zcpm_speedrun.prog.abap                # Speedrun test program
│   ├── zcpm_debug_trace.prog.abap             # Debug trace viewer
│   ├── zcpm_session_report.prog.abap          # Session report
│   │
│   │ ## Z80 CPU Emulator
│   ├── zcl_cpu_z80.clas.abap                  # Z80 CPU emulator (main)
│   ├── zcl_cpu_z80.clas.testclasses.abap      # Z80 unit tests (38 tests)
│   ├── zif_cpu_z80_bus.intf.abap              # Bus interface (memory + I/O)
│   ├── zif_cpu_z80_core.intf.abap             # Core CPU interface for handlers
│   ├── zif_cpu_z80_prefix.intf.abap           # Prefix handler interface
│   ├── zcl_cpu_z80_bus_simple.clas.abap       # Simple bus implementation
│   ├── zcl_cpu_z80_prefix_cb.clas.abap        # CB prefix (bit ops, rotates)
│   ├── zcl_cpu_z80_prefix_dd.clas.abap        # DD prefix (IX instructions)
│   ├── zcl_cpu_z80_prefix_ed.clas.abap        # ED prefix (extended ops)
│   ├── zcl_cpu_z80_prefix_fd.clas.abap        # FD prefix (IY instructions)
│   │
│   │ ## i8080 CPU Emulator
│   ├── zcl_cpu_8080_v2.clas.abap              # i8080 CPU emulator
│   ├── zcl_cpu_8080_v2.clas.testclasses.abap  # i8080 unit tests
│   │
│   │ ## Z-Machine Interpreter
│   ├── zif_zork_00_types.intf.abap            # Z-machine types (TS_/TT_/TY_)
│   ├── zif_zork_00_io.intf.abap               # Z-machine I/O interface
│   ├── zcl_zork_00_memory.clas.abap           # Z-machine memory manager
│   ├── zcl_zork_00_stack.clas.abap            # Z-machine call stack
│   ├── zcl_zork_00_decoder.clas.abap          # Z-machine instruction decoder
│   ├── zcl_zork_00_executor.clas.abap         # Z-machine instruction executor
│   ├── zcl_zork_00_io_console.clas.abap       # Console I/O implementation
│   ├── zcl_zork_00_io_html.clas.abap          # HTML/SAP GUI I/O
│   ├── zork_00_console.prog.abap              # Z-machine console program
│   └── *.testclasses.abap                     # Unit tests for each class
│
├── z80-python/                                 # Python Z80 reference impl
│   ├── cpm_z80.py                             # CP/M emulator in Python
│   ├── z80.py                                 # Z80 CPU core
│   ├── HELLO_NAME.COM                         # Test program binary
│   └── hello_name.asm                         # Test program source
│
├── output/                                     # Transpiled JavaScript
├── node_modules/                               # npm packages
├── package.json
├── abaplint.json
└── README.md
```

## Z-Machine Interpreter Architecture

The Z-machine is a virtual machine designed by Infocom for running interactive fiction games like ZORK.

### Components (SAP Package: $ZORK_00)

| Class | Purpose | Tests |
|-------|---------|-------|
| `ZIF_ZORK_00_TYPES` | Type definitions (TS_/TT_/TY_ conventions) | - |
| `ZIF_ZORK_00_IO` | I/O interface for text/screen | - |
| `ZCL_ZORK_00_MEMORY` | Story file loading, byte/word access, Z-strings | 10 |
| `ZCL_ZORK_00_STACK` | Call frames, locals, evaluation stack | ✅ |
| `ZCL_ZORK_00_DECODER` | Instruction decoding → `TT_INSTRUCTIONS` table | 11 |
| `ZCL_ZORK_00_IO_CONSOLE` | Console-based I/O implementation | 9 |

### Key Type Definitions

```abap
" Decoded instruction with operands as internal table
TYPES: BEGIN OF ts_instruction,
         address   TYPE i,
         opcode    TYPE i,
         operands  TYPE tt_operands,  " Internal table!
         has_store TYPE abap_bool,
         store_var TYPE i,
         ...
       END OF ts_instruction.

" Table of decoded instructions (for analysis/caching)
TYPES: tt_instructions TYPE STANDARD TABLE OF ts_instruction WITH KEY address.
```

### Decode → Execute Architecture

```
   Z-code bytes     ┌─────────────┐     ts_instruction      ┌──────────────┐
  ───────────────►  │   DECODER   │  ─────────────────────► │   EXECUTOR   │
                    │  decode()   │     (structured)        │  execute()   │
                    └─────────────┘                         └──────────────┘
                                                                   │
                                                                   ▼
                                                            Game output
```

## Z80 CPU Emulator Architecture

The Z80 emulator uses a **composition pattern** with separate prefix handlers for cleaner code organization.

### Components (SAP Package: $ZCPU_Z80_00)

| Class/Interface | Purpose |
|-----------------|---------|
| `ZCL_CPU_Z80` | Main CPU - registers, main opcodes, orchestration |
| `ZIF_CPU_Z80_BUS` | Bus interface for memory (64KB) and I/O (256 ports) |
| `ZIF_CPU_Z80_CORE` | Core CPU interface for prefix handlers |
| `ZIF_CPU_Z80_PREFIX` | Prefix handler interface (`execute()` method) |
| `ZCL_CPU_Z80_PREFIX_CB` | CB prefix - bit ops, rotates, shifts |
| `ZCL_CPU_Z80_PREFIX_DD` | DD prefix - IX index register ops + DDCB |
| `ZCL_CPU_Z80_PREFIX_FD` | FD prefix - IY index register ops + FDCB |
| `ZCL_CPU_Z80_PREFIX_ED` | ED prefix - block moves, I/O, extended ops |
| `ZCL_CPU_Z80_BUS_SIMPLE` | Simple bus implementation for testing |

### Prefix Handler Architecture

```
                    ┌─────────────────────────────────────────────┐
                    │              ZCL_CPU_Z80                    │
                    │  ┌─────────────────────────────────────┐   │
  Opcode fetch      │  │           exec_main()               │   │
  ───────────────►  │  │  CASE opcode                        │   │
                    │  │    WHEN 203 → mo_cb_handler         │   │
                    │  │    WHEN 221 → mo_dd_handler         │   │
                    │  │    WHEN 237 → mo_ed_handler         │   │
                    │  │    WHEN 253 → mo_fd_handler         │   │
                    │  └──────────────┬──────────────────────┘   │
                    │                 │                          │
                    │    implements   ▼                          │
                    │        ZIF_CPU_Z80_CORE                    │
                    └─────────────────┬───────────────────────────┘
                                      │
           ┌──────────────────────────┼──────────────────────────┐
           │                          │                          │
           ▼                          ▼                          ▼
  ┌─────────────────┐      ┌─────────────────┐      ┌─────────────────┐
  │ PREFIX_CB       │      │ PREFIX_DD/FD    │      │ PREFIX_ED       │
  │ - RLC/RRC/RL/RR │      │ - LD IX/IY,nn   │      │ - LDIR/LDDR     │
  │ - SLA/SRA/SRL   │      │ - ADD IX/IY,rr  │      │ - CPIR/CPDR     │
  │ - BIT/SET/RES   │      │ - LD r,(IX+d)   │      │ - IN/OUT block  │
  └─────────────────┘      │ - DDCB/FDCB     │      │ - NEG/RETN/RETI │
                           └─────────────────┘      └─────────────────┘
```

### Key Design Decisions

1. **Composition over inheritance** - Prefix handlers are separate classes, not subclasses
2. **Interface-based coupling** - Handlers access CPU via `ZIF_CPU_Z80_CORE` interface
3. **Bus abstraction** - Memory and I/O unified via `ZIF_CPU_Z80_BUS`
4. **Pre-computed flag tables** - Each handler has its own SZP flag lookup table

## What's Implemented

### CPU Core (Complete)
- ✅ All registers: AF, BC, DE, HL, PC, SP
- ✅ 64KB memory (STRING representation, transpiler-compatible)
- ✅ Pre-computed lookup tables (parity, inc, dec, carry bits)
- ✅ Memory access (byte/word, little-endian)
- ✅ Flag calculations (all 6 flags)

### Opcodes: 86 Implemented (12 Families)

1. ✅ **Load/Store 16-bit** (4) - LD BC/DE/HL/SP,nnnn
2. ✅ **Load/Store 8-bit** (7) - LD r,nn
3. ✅ **Register to Register** (64) - LD r,r family (MOV in i8080)
4. ✅ **Memory Operations** (8) - LD (BC),A / LD A,(nnnn) / etc.
5. ✅ **Increment/Decrement** (14) - INC/DEC for all registers
6. ✅ **ALU with Register** (64) - ADD/SUB/AND/OR/XOR/CP A,r
7. ✅ **ALU with Immediate** (8) - ADD/SUB/AND/OR/XOR/CP A,nn
8. ✅ **Rotate** (4) - RLCA/RRCA/RLA/RRA
9. ✅ **Control Flow** (3) - JP/CALL/RET
10. ✅ **Conditional Flow** (24) - JP/CALL/RET with conditions (NZ/Z/NC/C/PO/PE/P/M)
11. ✅ **Stack** (8) - PUSH/POP BC/DE/HL/AF
12. ✅ **Miscellaneous** (6) - NOP/HALT/DAA/CPL/STC/CMC

### What's Missing (~19 opcodes)

See **[TODO.md](TODO.md)** for detailed implementation plan.

**High Priority (10 opcodes):**
- ADD HL,rr (4 opcodes) - 16-bit arithmetic
- Exchange operations (4 opcodes) - EX DE,HL / JP (HL) / etc.
- DI/EI (2 opcodes) - Interrupt control

**Medium Priority (8 opcodes):**
- RST instructions (8 opcodes) - System calls

**Low Priority (2 opcodes):**
- I/O operations (2 opcodes) - IN/OUT

## Architecture: Hybrid Approach

Based on analysis of [RunCPM](https://github.com/MockbaTheBorg/RunCPM), this uses a **hybrid architecture**:

**Code-driven dispatch** - Direct CASE statement for opcodes
```abap
CASE iv_opcode.
  WHEN 1.  " LD BC,nnnn (Z80) / LXI B (i8080)
    mv_bc = read_word_pp( CHANGING cv_addr = mv_pc ).
    rv_cycles = 10.
```

**Table-driven flags** - Pre-computed lookup tables
```abap
" INC uses pre-computed flag table (257 entries)
lv_offset = lv_temp * 2.
lv_hex = mv_inc_table+lv_offset(2).
lv_flags = hex_to_byte( lv_hex ).
```

### Why Hybrid?

✅ **Fast** - Direct execution, no indirection
✅ **Debuggable** - Step through actual logic, not metadata
✅ **Simple** - No dynamic method calls
✅ **Efficient** - CASE optimized by ABAP kernel
✅ **Accurate** - Pre-computed flags eliminate calculation errors

See **[BRAINSTORM.md](BRAINSTORM.md)** for detailed analysis.

## Key Design Decisions

### 1. Memory as STRING (Transpiler-Compatible)

```abap
" 64KB memory = 131,072 hex characters (2 per byte)
DATA: mv_memory TYPE string.

" Read byte at address 0x1000:
lv_offset = 4096 * 2.              " Address × 2
lv_hex = mv_memory+8192(2).        " Extract 2 hex chars
rv_val = hex_to_byte( lv_hex ).    " Convert to integer
```

**Why?** Transpiler doesn't support internal table operations. STRING works perfectly.

### 2. Registers as 32-bit Integers

```abap
" Store 16-bit pairs as 32-bit integers
DATA: mv_af TYPE i.  " A=high byte (bits 8-15), F=low byte (bits 0-7)

" Access via arithmetic (transpiler-compatible)
METHOD get_high_byte.
  rv_val = iv_pair DIV 256.
  rv_val = rv_val MOD 256.  " Ensure 8-bit
ENDMETHOD.
```

**Why?** Simpler 16-bit operations, no bit operations needed.

### 3. Dual i8080/Z80 Naming

All opcodes documented with both architectures:

```abap
WHEN 1.   " LD BC,nnnn (Z80) / LXI B (i8080)
WHEN 6.   " LD B,nn (Z80) / MVI B,nn (i8080)
WHEN 195. " JP nnnn (Z80) / JMP nnnn (i8080)
```

**Why?** Prepares for Z80 extension, helps readers familiar with either CPU.

## Local Development Workflow

### Prerequisites

```bash
npm install  # Installs @abaplint/transpiler-cli and runtime
```

### Test-Driven Development Cycle

```bash
# 1. Edit ABAP code in your favorite editor
vim src/zcl_cpu_8080_v2.clas.abap

# 2. Run tests (< 1 second!)
npm test

# 3. All tests pass ✓
# 4. Commit
git add src/*.abap
git commit -m "Added feature X"
git push
```

**Benefits:**
- ⚡ **Instant feedback** - 100x faster than SAP upload
- 💻 **Work offline** - No SAP server required
- 🔄 **CI/CD ready** - Can run in GitHub Actions
- 🛠️ **Modern tools** - VS Code, git workflows

See **[TRANSPILER.md](TRANSPILER.md)** for setup details.

## Testing

### 16 Comprehensive Tests (All Passing)

1. `test_init` - CPU initialization
2. `test_memory` - Memory read/write operations
3. `test_nop` - NOP instruction
4. `test_ld_bc` - 16-bit register load
5. `test_inc_bc` - 16-bit increment with flags
6. `test_halt` - CPU halt state
7. `test_jump` - Unconditional jump (JP)
8. `test_call_ret` - Subroutine calls and returns
9. `test_program` - Complex multi-instruction program
10. `test_ld_r_r` - Register-to-register moves
11. `test_alu_add` - Addition with flags
12. `test_alu_sub` - Subtraction with flags
13. `test_alu_and` - Bitwise AND with flags
14. `test_alu_or` - Bitwise OR with flags
15. `test_alu_xor` - Bitwise XOR with flags
16. *(More tests as opcodes are added)*

### Industry Standard Test Suite (Coming Soon)

**8080 Exerciser by Ian Bartholomew** - Gold standard for i8080 validation

- Tests all instructions systematically
- Computes CRC checksums
- Prints "PASS" or shows which instruction failed
- See **[TODO.md](TODO.md)** for test suite integration plan

## Implementation Plan

See **[TODO.md](TODO.md)** for complete implementation plan.

### Short Term (4-6 hours)

✅ Implement remaining 19 i8080 opcodes
- ADD HL,rr family (4 opcodes)
- Exchange operations (4 opcodes)
- RST instructions (8 opcodes)
- I/O operations (2 opcodes)
- DI/EI (2 opcodes)

### Medium Term (1-2 weeks)

⏳ **CP/M BDOS Emulation**
- Console I/O (functions 1, 2, 9, 10, 11)
- File I/O (functions 15, 16, 20-26)
- .COM file loader
- Run real CP/M programs!

### Long Term (2-3 weeks, optional)

⏳ **Z80 Extensions**
- CB prefix - Bit operations
- ED prefix - Block operations
- DD/FD prefix - IX/IY registers
- Alternate register set

## Usage Example

```abap
" Create CPU instance
DATA(lo_cpu) = NEW zcl_cpu_8080_v2( ).

" Write a simple program to memory
" Program: LD BC,0x1234; INC BC; HALT
lo_cpu->write_byte( iv_addr = 256 iv_val = 1 ).    " LD BC,nnnn
lo_cpu->write_byte( iv_addr = 257 iv_val = 52 ).   " 0x34
lo_cpu->write_byte( iv_addr = 258 iv_val = 18 ).   " 0x12
lo_cpu->write_byte( iv_addr = 259 iv_val = 3 ).    " INC BC
lo_cpu->write_byte( iv_addr = 260 iv_val = 118 ).  " HALT

" Execute until HALT
DATA(lv_count) = lo_cpu->execute_until_halt( iv_max_instructions = 1000 ).

" Inspect results
WRITE: / 'Executed', lv_count, 'instructions'.
WRITE: / 'BC =', lo_cpu->get_bc( ).  " Should be 0x1235
```

## Performance Estimates

### JavaScript (Current - via transpiler)
- ~5-10M instructions/second
- V8 engine optimization
- Perfect for development/testing

### SAP System (When deployed)
- **Modern (2020+)**: ~500K-1M instructions/second
- **Older (2010)**: ~100K-200K instructions/second
- **Original 8080 @ 2MHz**: 500K instructions/second
- **Can emulate at original speed on modern systems!**

## For AI Assistants

**Working on this project with Claude, GPT, or another AI?**

👉 **Read [CLAUDE.md](CLAUDE.md) first!**

It contains:
- Current implementation status
- Transpiler constraints and patterns
- Code style guidelines
- Common tasks and examples
- What NOT to do

## Resources

### Code & Tools
- Repository: https://github.com/oisee/cpm-abap
- Reference: https://github.com/MockbaTheBorg/RunCPM (C implementation)
- Transpiler: https://github.com/abaplint/transpiler

### Documentation
- Z80 CPU Manual: http://www.zilog.com/docs/z80/um0080.pdf
- i8080 Opcode Reference: http://www.emulator101.com/reference/8080-by-opcode.html
- i8080 Opcode Table: https://pastraiser.com/cpu/i8080/i8080_opcodes.html
- CP/M 2.2 Manual: http://www.gaby.de/cpm/manuals/archive/cpm22htm/

### Test Suites
- 8080 Exerciser: http://www.retroarchive.org/cpm/cdrom/SIMTEL/CPMUG/
- ZEXDOC/ZEXALL: https://github.com/anotherlin/z80emu

## Contributing

This is an educational/experimental project. Contributions welcome!

**Before contributing:**
1. Read **[CONTEXT.md](CONTEXT.md)** for project status
2. Check **[TODO.md](TODO.md)** for what needs doing
3. Review **[CLAUDE.md](CLAUDE.md)** for code patterns
4. Run `npm test` to ensure all tests pass

## License

Educational/experimental project. RunCPM reference implementation is MIT licensed.

## Next Milestones

### CP/M Emulator - ACHIEVED!
✅ **DONE:** Interactive console with input waiting
✅ **DONE:** File I/O (FCB, DMA, random read)
✅ **DONE:** Runs ZORK1.COM in SAP GUI!
🎯 **Next:** More BDOS functions (directory, write)
🎯 **Future:** Run more CP/M software (Turbo Pascal, WordStar)

### Z-Machine Interpreter - ACHIEVED!
✅ **DONE:** Core interpreter running MiniZork
✅ **DONE:** All essential opcodes implemented
🎯 **Next:** Save/restore game state
🎯 **Future:** Full ZORK I support

### i8080 CPU Emulator
🎯 **Immediate:** Complete i8080 instruction set (~19 opcodes)
🎯 **Short term:** Run 8080 Exerciser test suite

---

*Retro computing in ABAP - because why not?* 🚀

*Last updated: 2025-12-11*
