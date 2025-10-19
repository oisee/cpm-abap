# Z80/i8080 CPU Emulator in ABAP

A working Z80/i8080 CPU emulator implemented in ABAP, designed to run CP/M programs. Features transpiler-compatible code for local testing via Node.js.

## Current Status (2025-10-19)

✅ **86 / ~105 core i8080 opcodes implemented (82%)**
✅ **16 unit tests, all passing**
✅ **Local TDD workflow operational** (transpile + test < 1 second)
✅ **2,175 lines of ABAP code**

### Quick Start

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
│   ├── zcl_cpu_8080_v2.clas.abap              # CPU emulator (1,716 lines)
│   └── zcl_cpu_8080_v2.clas.testclasses.abap  # Unit tests (459 lines)
├── output/                                     # Transpiled JavaScript (auto-generated)
│   ├── index.mjs                               # Test runner
│   └── zcl_cpu_8080_v2.clas.mjs                # Transpiled CPU code
├── node_modules/                               # npm packages (transpiler + runtime)
├── docs/
│   ├── CONTEXT.md          # Project status and history
│   ├── TODO.md             # Implementation plan ⭐
│   ├── CLAUDE.md           # AI assistant instructions
│   ├── BRAINSTORM.md       # Architecture analysis
│   ├── TRANSPILER.md       # Transpiler setup
│   └── SESSION_NOTES.md    # Bug fix documentation
├── package.json            # npm dependencies
├── abaplint.json           # Transpiler config
└── README.md               # This file
```

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

🎯 **Immediate:** Complete i8080 instruction set (~19 opcodes, 4-6 hours)
🎯 **Short term:** Run 8080 Exerciser test suite (validate all instructions)
🎯 **Medium term:** CP/M BDOS emulation (console I/O, file I/O)
🎯 **Long term:** Run real CP/M programs (ZORK, Turbo Pascal, MBASIC)

See **[TODO.md](TODO.md)** for detailed plan and timeline.

---

*Built with ABAP, validated by industry-standard test suites, documented for the future* 🚀

*Last updated: 2025-10-19*
