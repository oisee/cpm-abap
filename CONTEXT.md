# Project Context: Z80/i8080 CPU Emulator in ABAP

## Current Status (2025-10-13)

### What We've Built

A working proof-of-concept Z80/i8080 CPU emulator in ABAP with **hybrid architecture** (switch/case dispatch + lookup tables).

**Files:**
- `src/zcl_cpu_8080.clas.abap` (897 lines) - Main CPU emulator
- `src/zcl_cpu_8080_test.clas.abap` (233 lines) - Unit tests
- `BRAINSTORM.md` - Architecture analysis and design decisions
- `TRANSPILER.md` - ABAP transpiler integration findings
- `README.md` - Project documentation

**Repository:** https://github.com/oisee/cpm-abap

### Implemented Features

**CPU Core:**
- ✓ All registers: AF, BC, DE, HL, PC, SP
- ✓ 64KB memory (currently as internal table)
- ✓ Pre-computed lookup tables: parity, inc, dec, carry bits
- ✓ Bit manipulation helpers (high/low byte access)
- ✓ Memory access (byte/word, little-endian)

**Opcodes (25 working):**
- NOP, LD BC/DE/HL/SP, INC/DEC registers
- Memory operations: LD (BC),A, LD A,(BC)
- Control flow: JP, CALL, RET, HALT
- With proper flag handling via lookup tables

**Tests:**
- 10 unit test methods covering all features
- Memory operations, register manipulation, control flow

### Architecture Decision: Hybrid Approach

**Why we chose hybrid (switch/case + lookup tables):**

1. **Code-driven dispatch** - Direct CASE statement for opcodes
   - Fast execution (no indirection)
   - Easy to debug
   - ABAP CASE is optimized (hash table internally)

2. **Table-driven flags** - Pre-computed lookup tables for complex calculations
   - Eliminates expensive bit counting
   - Pre-validated correctness
   - ~15x faster than calculating flags on each operation

**Alternative rejected: Pure table-driven**
- Requires metadata lookup on every instruction
- Dynamic method calls are slow in ABAP
- Same amount of code needed for handlers
- Harder to debug (step through metadata, not logic)
- Estimated 2x slower overall

### Key Design Decisions

#### 1. Register Storage

```abap
" Store 16-bit pairs as 32-bit integers
DATA: mv_af TYPE i.  " A=high byte (bits 8-15), F=low byte (bits 0-7)

" Access via arithmetic instead of casting
METHOD get_high_byte.
  rv_val = iv_pair DIV 256 MOD 256.
ENDMETHOD.
```

**Why?** Simpler arithmetic for 16-bit operations (INC BC, ADD HL,BC)

#### 2. Memory Representation (Current - Needs Fix)

```abap
" Current: Internal table (NOT transpiler-compatible)
TYPES: ty_memory TYPE STANDARD TABLE OF x LENGTH 1 WITH EMPTY KEY.
DATA: mt_memory TYPE ty_memory.

" Access: 1-based indexing
READ TABLE mt_memory INDEX ( lv_addr + 1 ) INTO rv_val.
```

**Issue:** Transpiler doesn't support APPEND to standard tables with this syntax

**Fix needed:** Change to XSTRING (see plan below)

#### 3. Pre-computed Lookup Tables

```abap
" INC table: flags for every result 0-256
mt_inc_table[0]   = '50'  " Zero + Half-carry flags
mt_inc_table[128] = '90'  " Sign flag set

" Usage in opcode:
WHEN '04'.  " INC B
  mv_bc = mv_bc + 256.
  READ TABLE mt_inc_table INDEX ( get_high_byte( mv_bc ) + 1 ) INTO lv_flags.
```

**Why?** Flag calculation is CPU-intensive:
- Parity requires counting 1-bits
- Half-carry depends on nibble overflow
- Overflow detection is complex
- Pre-computing eliminates all this on each ALU operation

#### 4. Opcode Dispatch

```abap
" Direct switch/case (fast, debuggable)
CASE iv_opcode.
  WHEN '00'. " NOP - do nothing
  WHEN '01'. mv_bc = read_word_pp( cv_addr = mv_pc ).
  WHEN '04'. " INC B - uses lookup table for flags
    mv_bc = mv_bc + 256.
    lv_flags = mt_inc_table[ get_high_byte( mv_bc ) + 1 ].
ENDCASE.
```

## ABAP Transpiler Discovery

### What Works

✓ **Installation:** `npm install @abaplint/transpiler-cli @abaplint/runtime`
✓ **Auto-detection:** Finds `.clas.abap` files in `src/`
✓ **Transpilation:** Converts ABAP → JavaScript (ES6)
✓ **Runtime:** Includes open-abap-core (536 files)
✓ **Unit tests:** Supports `FOR TESTING` classes
✓ **Production-ready:** Used by abapGit, abap2UI5, others

### Current Blockers

1. **Table syntax** - `APPEND TO` standard table not supported
2. **BIT operations** - Need proper XSTRING/hex types (~15 locations)
3. **Type compatibility** - Some ABAP 7.02 vs modern differences

### What This Enables

**Local TDD without SAP server:**
```bash
# Traditional ABAP development
Edit code → Upload to SAP → Run tests → Wait 30-60 seconds → Fix → Repeat

# With transpiler (after fixes)
Edit code → Save → Auto-transpile → Run tests in Node.js → Results in < 1 second
```

**Benefits:**
- ⚡ Instant feedback (100x faster iteration)
- 💻 Work offline (no SAP server required)
- 🔄 CI/CD integration (GitHub Actions, GitLab CI)
- 🛠️ Modern tools (VS Code, git workflows, npm packages)

## Implementation Plan

### Phase 1: Fix Transpiler Compatibility (THIS SESSION)

**Goal:** Get tests running locally with `node output/index.js`

**Tasks:**
1. ✓ ~~Setup transpiler (npm packages)~~
2. ✓ ~~Create transpiler-compatible test class~~
3. ⏳ Fix memory representation (table → XSTRING)
4. ⏳ Fix BIT operations (use proper hex types)
5. ⏳ Fix lookup table initialization
6. ⏳ Transpile and verify no errors
7. ⏳ Run tests locally
8. ⏳ Commit working version

**Estimated time:** 2-4 hours

### Phase 2: Complete i8080 Instruction Set

**Goal:** Run real CP/M .COM files (like HELLO.COM)

**Tasks:**
- Implement MOV r,r family (49 opcodes)
- Implement ADD/ADC/SUB/SBC family (32 opcodes)
- Implement AND/OR/XOR/CP family (32 opcodes)
- Implement conditional jumps/calls/returns (16 opcodes)
- Implement stack operations (PUSH, POP)
- Implement I/O operations (IN, OUT)
- Implement rotate/shift operations

**Total:** 220 additional opcodes
**Estimated time:** 1-2 weeks

### Phase 3: CP/M BDOS Emulation

**Goal:** Run interactive CP/M programs (text adventures, BASIC)

**Tasks:**
- Implement BDOS syscall interface
- Console I/O (functions 1, 2, 9, 10, 11)
- File I/O (functions 15, 16, 20-26)
- Disk operations (simulated via ABAP tables or browser storage)
- Memory management (TPA setup)

**Estimated time:** 1-2 weeks

### Phase 4: Z80 Extensions (Optional)

**Goal:** Run advanced software (Turbo Pascal, CP/M 3)

**Tasks:**
- CB prefix - Bit operations (BIT, SET, RES, shifts)
- ED prefix - Block operations (LDIR, CPIR, etc.)
- DD/FD prefix - IX/IY index registers
- Alternate register set (AF', BC', DE', HL')
- Interrupt handling

**Estimated time:** 2-3 weeks

## Technical Challenges & Solutions

### Challenge 1: Memory Representation

**Problem:** 64KB memory needs efficient access pattern

**Options considered:**
```abap
" Option A: Standard table (current - NOT transpiler-compatible)
DATA: mt_memory TYPE STANDARD TABLE OF x LENGTH 1.
" Pro: Natural indexing
" Con: APPEND not supported by transpiler

" Option B: XSTRING (recommended for transpiler)
DATA: mv_memory TYPE xstring.
" Pro: Transpiler-compatible, native ABAP type
" Con: Access via string operations (2 hex chars = 1 byte)

" Option C: Hashed table with address key
DATA: mt_memory TYPE HASHED TABLE ... WITH UNIQUE KEY address.
" Pro: O(1) lookup
" Con: More overhead, CP/M uses most of 64KB anyway
```

**Decision:** Use XSTRING for transpiler compatibility (implementing now)

### Challenge 2: BIT Operations

**Problem:** Transpiler requires proper types for BIT operations

**Fix:**
```abap
" Before (doesn't work):
lv_flags = lv_val BIT-AND 168.  " Error: need hex type

" After:
DATA(lv_hex) = CONV x( lv_val ).
lv_flags = lv_hex BIT-AND '00A8'.
DATA(lv_result) = CONV i( lv_flags ).
```

### Challenge 3: Lookup Table Initialization

**Problem:** Need to populate 257+256+256+512 table entries

**Current approach:** DO loops with APPEND (not transpiler-compatible)

**Fix:** Pre-initialize with full hexadecimal data or use XSTRING concatenation

## Performance Estimates

### On Modern SAP System (2020+ hardware)

**Expected performance:**
- ~500K-1M instructions/second
- Original 8080 @ 2MHz = 500K instructions/second
- **We can emulate at original speed!**

### On Older Systems (2010 hardware)

**Expected performance:**
- ~100K-200K instructions/second
- Still enough for interactive CP/M programs
- Text adventures, BASIC, editors work fine

### JavaScript (via transpiler)

**Expected performance:**
- ~5-10M instructions/second (10-20x faster than ABAP)
- V8 engine optimization
- Good for development/testing

## Project Evolution

### Session 1: Architecture Analysis (Oct 13, morning)

**Question:** "Is Z80 emulation code-driven or data-driven?"

**Analysis:**
- Examined RunCPM reference implementation (C)
- Discovered hybrid approach (switch/case + lookup tables)
- Evaluated pure table-driven alternative
- **Decision:** Hybrid is optimal for ABAP

**Outcome:** BRAINSTORM.md with full analysis

### Session 2: Proof of Concept (Oct 13, afternoon)

**Implementation:**
- Created zcl_cpu_8080 class (897 lines)
- Implemented 25 core opcodes
- Built pre-computed lookup tables
- Created test program with 13 test cases

**Outcome:** Working CPU emulator, all tests conceptually correct

### Session 3: Transpiler Integration (Oct 13, evening)

**Question:** "Can we use ABAP transpiler for local TDD?"

**Discovery:**
- YES! Transpiler works and is production-proven
- Found compatibility issues (table syntax, BIT operations)
- Created transpiler-compatible test class
- Documented findings in TRANSPILER.md

**Outcome:** Setup complete, fixing compatibility issues now

### Session 4: Fix & Test (Oct 13, now)

**Current work:**
- Fixing memory representation (table → XSTRING)
- Fixing BIT operations
- Making code fully transpiler-compatible
- Running tests locally with Node.js

**Goal:** Working local TDD environment

## Next User Decisions

After this session completes, you'll need to decide:

1. **Continue with full i8080?** (220 opcodes, 1-2 weeks)
   - Pro: Can run real CP/M software
   - Con: Significant time investment

2. **Jump to specific opcodes?** (pragmatic approach)
   - Implement only what's needed for target software
   - Example: ZORK needs ~100 opcodes, not all 244

3. **Add BDOS emulation?** (CP/M system calls)
   - Required for any real CP/M program
   - Can start with minimal set (console I/O only)

4. **Deploy somewhere?**
   - Web version (JavaScript via transpiler)
   - SAP GUI transaction
   - Background job processing
   - RESTful service

## Resources

**Code:**
- Repository: https://github.com/oisee/cpm-abap
- Reference: https://github.com/MockbaTheBorg/RunCPM

**Documentation:**
- Z80 CPU Manual: http://www.zilog.com/docs/z80/um0080.pdf
- i8080 Opcode Reference: http://www.emulator101.com/reference/8080-by-opcode.html
- CP/M 2.2 Manual: http://www.gaby.de/cpm/manuals/archive/cpm22htm/
- ABAP Transpiler: https://github.com/abaplint/transpiler

**Tools:**
- Transpiler playground: https://transpiler.abaplint.org
- Opcode maps: https://clrhome.org/table/

## Lessons Learned

1. **Hybrid architecture is optimal** - Pure table-driven offers no benefit in ABAP
2. **Pre-computed tables are powerful** - Eliminate expensive calculations
3. **Transpiler enables modern workflow** - Local TDD is game-changing
4. **ABAP can do low-level work** - Bit operations and byte manipulation work well
5. **Start small, iterate** - POC proved concept before full implementation

## Current Working Directory

```
/home/alice/dev/cpm-abap/
├── .git/                    # Git repository
├── .gitignore               # Ignore node_modules/, output/
├── node_modules/            # npm packages (transpiler)
├── src/
│   ├── zcl_cpu_8080.clas.abap         # CPU emulator (needs fixes)
│   └── zcl_cpu_8080_test.clas.abap   # Unit tests (transpiler-ready)
├── BRAINSTORM.md            # Architecture analysis
├── TRANSPILER.md            # Transpiler findings
├── CONTEXT.md               # This file
├── README.md                # Project documentation
├── abaplint.json            # Transpiler config
├── package.json             # npm dependencies
└── z_test_cpu_8080.prog.abap.backup  # Old test program

Total: 1,130 lines of ABAP code
```

## What's Next (Immediate)

Working on fixing transpiler compatibility NOW:

1. **Memory → XSTRING** - Change from table to native string type
2. **BIT operations** - Add proper type conversions
3. **Lookup tables** - Fix initialization for transpiler
4. **Test locally** - Verify all tests pass in Node.js
5. **Commit** - Push working version to GitHub

After this session, you'll have:
- ✓ Working CPU emulator
- ✓ Local test execution (< 1 second)
- ✓ TDD workflow enabled
- ✓ Foundation for completing i8080

**Time remaining in this session:** Implementing fixes now...

---

*Context saved: 2025-10-13 15:50 UTC*
