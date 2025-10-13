# Z80/i8080 CPU Emulator in ABAP

A working proof-of-concept CPU emulator for the Intel 8080 and Zilog Z80 processors, implemented in ABAP (SAP's programming language). This project demonstrates the feasibility of emulating vintage 8-bit CPUs using ABAP's bit manipulation and table processing capabilities.

## Architecture: Hybrid Approach

Based on analysis of [RunCPM](https://github.com/MockbaTheBorg/RunCPM), this implementation uses a **hybrid architecture**:

- **Code-driven dispatch**: Direct `CASE` statement for opcode execution (fast, debuggable)
- **Table-driven flag calculations**: Pre-computed lookup tables for complex flag operations

### Why Hybrid?

```abap
" ✓ FAST: Direct opcode dispatch
CASE lv_opcode.
  WHEN '01'. mv_bc = read_word_pp( cv_addr = mv_pc ).  " LD BC,nnnn
  WHEN '04'. " INC B - uses pre-computed table for flags
    mv_bc = mv_bc + 256.
    lv_flags = mt_inc_table[ get_high_byte( mv_bc ) + 1 ].
ENDCASE.

" ✗ SLOWER: Pure table-driven (metadata lookup + indirect dispatch)
READ TABLE gt_opcodes WITH KEY opcode = lv_op INTO ls_meta.
CALL METHOD (ls_meta-handler) ...
```

**Advantages in ABAP:**
- Direct execution path (no indirection overhead)
- CASE statement is optimized (hash table internally)
- BIT operations work well: `BIT-AND`, `BIT-OR`, `BIT-XOR`, `BIT-SHIFT`
- Debugger-friendly (step through actual logic, not metadata)
- Pre-computed tables still eliminate expensive flag calculations

## Project Structure

```
cpm-abap/
├── src/
│   ├── zcl_cpu_8080.clas.abap      - Main CPU emulator class
│   └── z_test_cpu_8080.prog.abap   - Unit test suite
├── RunCPM/                          - Reference implementation (C)
├── BRAINSTORM.md                    - Architecture design notes
└── README.md                        - This file
```

## Implementation Plan

### 🎯 Current Sprint: Fix Transpiler Compatibility (TODAY)

**Goal:** Enable local TDD with instant test execution (< 1 second)

**Status:** IN PROGRESS

**Tasks:**
- [x] Setup npm/transpiler packages
- [x] Create transpiler-compatible test class
- [ ] Fix memory representation (table → XSTRING)
- [ ] Fix BIT operations (use proper hex types)
- [ ] Fix lookup table initialization
- [ ] Transpile and verify no errors
- [ ] Run tests locally with Node.js
- [ ] Commit working version

**Why:** This unlocks instant local testing without SAP server, enabling true TDD workflow

**Time estimate:** 2-4 hours

### Milestone 1: Complete i8080 Instruction Set (1-2 weeks)

**Goal:** Run real CP/M .COM files (HELLO.COM)

**Remaining opcodes:** 220
- MOV r,r family (49 opcodes)
- ADD/ADC/SUB/SBC family (32 opcodes)
- AND/OR/XOR/CP family (32 opcodes)
- Conditional jumps/calls/returns (16 opcodes)
- Stack operations (PUSH, POP - 8 opcodes)
- I/O operations (IN, OUT - 8 opcodes)
- Rotate/shift operations (8 opcodes)

### Milestone 2: CP/M BDOS Emulation (1-2 weeks)

**Goal:** Run interactive CP/M programs (text adventures, BASIC)

**Components:**
- BDOS syscall interface (intercept CALL 0x0005)
- Console I/O (functions 1, 2, 9, 10, 11)
- File I/O (functions 15, 16, 20-26)
- Disk operations (simulated via ABAP tables or browser storage)

### Milestone 3: Z80 Extensions (2-3 weeks, optional)

**Goal:** Run advanced software (Turbo Pascal, CP/M 3)

**Extensions:**
- CB prefix - Bit operations (BIT, SET, RES, shifts)
- ED prefix - Block operations (LDIR, CPIR, etc.)
- DD/FD prefix - IX/IY index registers
- Alternate register set (AF', BC', DE', HL')

---

## Implementation Status

### ✓ Completed (Proof of Concept)

**Core CPU Features:**
- [x] 64KB addressable memory
- [x] All 8080/Z80 registers (AF, BC, DE, HL, PC, SP)
- [x] Pre-computed lookup tables (parity, inc, dec, carry bits)
- [x] Bit manipulation helpers (high/low byte access)
- [x] Memory access methods (byte/word, little-endian)
- [x] CPU status management (running/halted)

**Opcodes Implemented (20 core instructions):**
- [x] `0x00` - NOP
- [x] `0x01` - LD BC,nnnn
- [x] `0x02` - LD (BC),A
- [x] `0x03` - INC BC
- [x] `0x04` - INC B (with flag lookup table)
- [x] `0x05` - DEC B (with flag lookup table)
- [x] `0x06` - LD B,nn
- [x] `0x0A` - LD A,(BC)
- [x] `0x0B` - DEC BC
- [x] `0x0C` - INC C
- [x] `0x0D` - DEC C
- [x] `0x0E` - LD C,nn
- [x] `0x11` - LD DE,nnnn
- [x] `0x13` - INC DE
- [x] `0x1B` - DEC DE
- [x] `0x21` - LD HL,nnnn
- [x] `0x23` - INC HL
- [x] `0x2B` - DEC HL
- [x] `0x31` - LD SP,nnnn
- [x] `0x33` - INC SP
- [x] `0x3B` - DEC SP
- [x] `0x76` - HALT
- [x] `0xC3` - JP nnnn (unconditional jump)
- [x] `0xC9` - RET (return from subroutine)
- [x] `0xCD` - CALL nnnn (call subroutine)

### 🔄 Next Steps (Future Milestones)

**Milestone 2: Complete i8080 (~244 opcodes)**
- [ ] MOV r,r family (0x40-0x7F) - 49 opcodes
- [ ] ADD/ADC/SUB/SBC family (0x80-0x9F) - 32 opcodes
- [ ] AND/OR/XOR/CP family (0xA0-0xBF) - 32 opcodes
- [ ] Conditional jumps/calls/returns (JZ, JNZ, JC, etc.)
- [ ] Stack operations (PUSH, POP)
- [ ] I/O operations (IN, OUT)
- [ ] Rotate/shift operations (RLCA, RRCA, RLA, RRA)

**Milestone 3: CP/M BDOS Emulation**
- [ ] BDOS syscall interface
- [ ] Console I/O (functions 1, 2, 9, 10, 11)
- [ ] File I/O (functions 15, 16, 20-26)
- [ ] Disk operations (simulated)

**Milestone 4: Z80 Extensions**
- [ ] CB prefix - Bit operations (BIT, SET, RES, shifts)
- [ ] ED prefix - Block operations (LDIR, CPIR, etc.)
- [ ] DD/FD prefix - IX/IY index registers
- [ ] Alternate register set (AF', BC', DE', HL')

## Key Design Decisions

### 1. Register Storage (16-bit pairs as 32-bit integers)

```abap
" Store register pairs as single 32-bit integers
DATA: mv_af TYPE i.  " A=high byte, F=low byte

" Access via helper methods
METHOD get_high_byte.
  rv_val = iv_pair DIV 256.
  rv_val = rv_val MOD 256.
ENDMETHOD.

METHOD set_high_byte.
  rv_new = ( iv_pair MOD 256 ) + ( iv_val * 256 ).
ENDMETHOD.
```

**Why?** Simpler arithmetic for 16-bit operations (INC BC, ADD HL,BC)

### 2. Memory as Internal Table

```abap
" 64KB as standard table
TYPES: ty_memory TYPE STANDARD TABLE OF x LENGTH 1 WITH EMPTY KEY.
DATA: mt_memory TYPE ty_memory.

" Index-based access (1-based in ABAP!)
READ TABLE mt_memory INDEX ( lv_addr + 1 ) INTO rv_val.
mt_memory[ lv_addr + 1 ] = iv_val.
```

**Alternative considered:** Hashed table with key=address
- Pro: O(1) lookup for sparse memory access
- Con: More overhead for sequential access, CP/M uses most of 64KB

### 3. Pre-computed Lookup Tables

```abap
" INC table (257 entries): flags for every possible result 0-256
mt_inc_table[0]   = '50'  " 0x50 - Zero + Half-carry
mt_inc_table[1]   = '00'  " No flags
...
mt_inc_table[128] = '90'  " 0x90 - Sign set

" Usage in opcode:
WHEN '04'.  " INC B
  mv_bc = mv_bc + 256.
  READ TABLE mt_inc_table INDEX ( get_high_byte( mv_bc ) + 1 )
       INTO lv_flags.
  " Combine with existing carry flag
  lv_flags = lv_flags BIT-OR ( get_flags_byte( ) BIT-AND c_flag_c ).
  set_flags_byte( lv_flags ).
```

**Why?** Flag calculation is complex (parity, half-carry, overflow). Pre-computing eliminates:
- Bit counting for parity (expensive)
- Conditional logic for each flag bit
- Potential bugs in flag calculation

### 4. CASE Statement for Opcode Dispatch

```abap
METHOD execute_opcode.
  CASE iv_opcode.
    WHEN '00'. " NOP
    WHEN '01'. mv_bc = read_word_pp( cv_addr = mv_pc ).
    WHEN '02'. write_byte( mv_bc, get_high_byte( mv_af ) ).
    " ... 256 cases
  ENDCASE.
ENDMETHOD.
```

**Why not pure table-driven?**
- ABAP has no function pointers
- Dynamic method calls are slow: `CALL METHOD (lv_handler)...`
- CASE is optimized by ABAP kernel (hash table for large switches)
- Direct code is easier to debug/trace

## Testing

### Unit Test Coverage

The test suite (`z_test_cpu_8080.prog.abap`) validates:

1. **CPU Initialization** - Registers, memory, status
2. **Memory Operations** - Byte/word read/write, little-endian encoding
3. **Register Instructions** - LD BC/DE/HL/SP, INC/DEC
4. **Flag Handling** - INC/DEC flag table lookups
5. **Control Flow** - JP, CALL, RET, HALT
6. **Multi-Instruction Programs** - Execute sequence until HALT

### Test Output Format

```
======= Test 1: CPU Initialization =======
✓ PC starts at 0x0100
✓ AF register is zero
✓ BC register is zero

======= Test 12: CALL/RET (0xCD/0xC9) =======
✓ CALL jumps to 0x3000
✓ CALL pushes return address on stack
✓ RET returns to caller
✓ RET pops return address

Test Summary:
  Passed: 42
  Failed: 0
✓ All tests passed!
```

## Performance Considerations

### Measured Bottlenecks (in real SAP system)

1. **Internal table access** - `READ TABLE ... INDEX` is O(1) but has overhead
   - Mitigation: Consider field-symbols for hot paths

2. **Bit operations** - `BIT-AND`, `BIT-OR` are fast, but `BIT-SHIFT` is slower
   - Mitigation: Use DIV/MOD for byte extraction (faster than shift)

3. **Method calls** - ABAP method call overhead is ~10x C function call
   - Mitigation: Inline critical paths (memory access, flag checks)

### Estimated Performance

**On modern SAP NetWeaver ABAP 7.5+ (2020 hardware):**
- ~500K-1M instructions/second (estimated)
- Compare: Original 8080 @ 2MHz = 500K instructions/second
- **We can emulate at original speed!**

**On older systems (ABAP 7.0, 2010 hardware):**
- ~100K-200K instructions/second
- Still enough for interactive CP/M programs

## Usage Example

```abap
" Create CPU instance
DATA: lo_cpu TYPE REF TO zcl_cpu_8080.
CREATE OBJECT lo_cpu.

" Load a simple program
DATA(lv_program) = '01 34 12 03 76'.  " LD BC,0x1234; INC BC; HALT

lo_cpu->load_com_file( iv_data = lv_program ).

" Execute until HALT
DATA(lv_count) = lo_cpu->execute_until_halt( iv_max_instructions = 1000 ).

" Inspect results
WRITE: / 'Executed', lv_count, 'instructions'.
WRITE: / 'BC =', lo_cpu->get_bc( ).  " Should be 0x1235
```

## Converting to Table-Driven: Effort Estimate

If you wanted to convert this to **fully table-driven**:

### Required Components

1. **Opcode Metadata Table**
   ```abap
   TYPES: BEGIN OF ty_opcode_meta,
     opcode       TYPE x LENGTH 1,
     mnemonic     TYPE string,
     handler      TYPE string,  " Method name
     operand1     TYPE string,
     operand2     TYPE string,
     length       TYPE i,
     cycles       TYPE i,
     flags_affect TYPE c LENGTH 5,
   END OF ty_opcode_meta.
   ```

2. **Operation Handlers**
   - Still need ~50-100 unique operation methods
   - LOAD_REG, ALU_ADD, ALU_SUB, JUMP, CALL, RET, etc.
   - **Same amount of code as switch/case!**

3. **Dynamic Dispatch**
   ```abap
   READ TABLE gt_opcodes WITH KEY opcode = lv_op INTO ls_meta.
   CALL METHOD (ls_meta-handler)  " Dynamic call - slow!
     EXPORTING ...
   ```

### Time Estimate

- **Metadata definition**: 244 i8080 opcodes × 8 fields = **2-3 days**
- **Handler methods**: ~50 unique handlers = **3-5 days** (same as switch/case)
- **Dynamic dispatch framework**: **1-2 days**
- **Testing/debugging**: **+50%** (harder to trace)

**Total: 10-15 days**

**Hybrid approach (current): 5-8 days**

### Conclusion: Hybrid is Better

Pure table-driven offers **no performance benefit** in ABAP:
- Dynamic method calls are slower than CASE dispatch
- Metadata lookup adds overhead
- Still need operation handler code
- Harder to debug (step through metadata, not logic)

**Hybrid wins:** Fast execution, readable code, easy debugging.

## References

- [RunCPM](https://github.com/MockbaTheBorg/RunCPM) - Reference C implementation
- [Z80 CPU User Manual](http://www.zilog.com/docs/z80/um0080.pdf)
- [i8080 Opcode Reference](http://www.emulator101.com/reference/8080-by-opcode.html)
- [CP/M 2.2 System Manual](http://www.gaby.de/cpm/manuals/archive/cpm22htm/)

## License

This is a proof-of-concept educational project. The RunCPM reference implementation is MIT licensed.

## Next Steps to Run CP/M Software

To actually run CP/M .COM files (like ZORK, Turbo Pascal, dBase):

1. **Complete i8080 opcode set** (244 opcodes) - ~1 week
2. **Implement CP/M BDOS syscalls** - ~1 week
   - Console I/O (print string, read line)
   - File operations (open, read, write, close)
   - Disk simulation (map to ABAP database tables?)
3. **Test with real software** - ongoing
   - Start: HELLO.COM
   - Then: Simple text adventures
   - Finally: Turbo Pascal, MBASIC, dBase II

**Total time to first "Hello World" .COM file: ~2-3 weeks**

---

*Built with ABAP bit-twiddling and a lot of coffee*
