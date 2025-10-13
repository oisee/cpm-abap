# i8080/Z80 Emulator on ABAP - Architecture Brainstorm

## Overview
Building a table-driven CPU emulator in ABAP, based on RunCPM architecture, starting with i8080 subset and optionally extending to full Z80.

## RunCPM Architecture Analysis

### Key Files Structure
```
RunCPM/RunCPM/
├── cpu.h          (4429 lines) - Main CPU emulation with Z80 opcodes
├── cpm.h          - CP/M 2.2 BDOS/BIOS implementation
├── ram.h          - Memory management
├── disk.h         - Disk I/O for CP/M drives
├── console.h      - Console I/O abstraction
└── globals.h      - Type definitions and macros
```

### CPU Core Architecture (from cpu.h)

#### 1. Register Set
```c
int32 AF;   // Accumulator + Flags (A high byte, F low byte)
int32 BC;   // B + C register pair
int32 DE;   // D + E register pair
int32 HL;   // H + L register pair
int32 IX;   // Index register X (Z80)
int32 IY;   // Index register Y (Z80)
int32 PC;   // Program Counter
int32 SP;   // Stack Pointer
int32 AF1;  // Alternate AF (Z80)
int32 BC1;  // Alternate BC (Z80)
int32 DE1;  // Alternate DE (Z80)
int32 HL1;  // Alternate HL (Z80)
int32 IR;   // Interrupt/Refresh register (Z80)
int32 IFF;  // Interrupt Flip Flop (Z80)
```

**ABAP Translation:**
- Use TYPE I (32-bit integer) for all registers
- High/low byte access via BIT operations and DIV/MOD
- Or use TYPE X LENGTH 2 with casting

#### 2. Flag Register (F in AF)
```c
Bit 7: S (Sign)      - FLAG_S  = 128 (0x80)
Bit 6: Z (Zero)      - FLAG_Z  = 64  (0x40)
Bit 5: 0 (unused)
Bit 4: H (Half-carry) - FLAG_H = 16  (0x10)
Bit 3: 0 (unused)
Bit 2: P/V (Parity/Overflow) - FLAG_P = 4 (0x04)
Bit 1: N (Add/Subtract) - FLAG_N = 2 (0x02)
Bit 0: C (Carry)     - FLAG_C  = 1  (0x01)
```

**ABAP Macros:**
```abap
" Set/clear flags
SETFLAG(f,c): AF = c ? AF BIT-OR FLAG_x : AF BIT-AND (NOT FLAG_x)
TSTFLAG(f): (AF BIT-AND FLAG_x) NE 0
```

#### 3. Lookup Tables (Pre-computed)
RunCPM initializes tables in `initTables()` for fast flag calculation:

```c
uint8 parityTable[256];      // Parity flag calculation
uint8 incTable[257];         // INC instruction flags
uint8 decTable[256];         // DEC instruction flags
uint8 addTable[512];         // ADD flags (9-bit input)
uint8 subTable[256];         // SUB flags
uint8 andTable[256];         // AND flags + result
uint8 xororTable[256];       // XOR/OR flags + result
uint8 rotateShiftTable[256]; // Rotate/shift flags
uint8 cbitsTable[512];       // Carry bits (9-bit)
// ... more specialized tables
```

**ABAP Implementation:**
```abap
TYPES: ty_lookup_table TYPE STANDARD TABLE OF x LENGTH 1 WITH EMPTY KEY.

DATA: gt_parity_table TYPE ty_lookup_table,
      gt_inc_table    TYPE ty_lookup_table,
      gt_dec_table    TYPE ty_lookup_table,
      gt_cbits_table  TYPE ty_lookup_table.

METHOD init_tables.
  " Pre-compute all 256/512 entries at initialization
  DO 256 TIMES.
    DATA(idx) = sy-index - 1.
    " Calculate parity, flags, etc.
    APPEND ... TO gt_parity_table.
  ENDDO.
ENDMETHOD.
```

#### 4. Main Execution Loop

```c
switch (RAM_PP(PC)) {  // Fetch opcode and increment PC
    case 0x00: /* NOP */ break;
    case 0x01: /* LD BC,nnnn */ BC = GET_WORD(PC); PC += 2; break;
    case 0x02: /* LD (BC),A */ PUT_BYTE(BC, HIGH_REGISTER(AF)); break;
    // ... 256 opcodes
    case 0xCB: /* CB prefix - bit operations */ ...
    case 0xED: /* ED prefix - Z80 extended */ ...
    case 0xDD: /* DD prefix - IX operations */ ...
    case 0xFD: /* FD prefix - IY operations */ ...
}
```

**Key Pattern:** Table-driven decoding using bit masks:
```c
case 0xCB:
    op = GET_BYTE(PC++);

    // Decode target register from bits 0-2
    switch (op & 0x07) {
        case 0: reg = B; break;
        case 1: reg = C; break;
        // ...
        case 7: reg = A; break;
    }

    // Decode operation from bits 6-7
    switch (op & 0xC0) {
        case 0x00: /* Shift/rotate - decode bits 3-5 */ break;
        case 0x40: /* BIT n,r */ break;
        case 0x80: /* RES n,r */ break;
        case 0xC0: /* SET n,r */ break;
    }
```

## ABAP Implementation Strategy

### Phase 1: Minimal i8080 Core (Week 1-2)

**Goal:** Run a simple "Hello World" .COM file

#### Data Structures

```abap
CLASS lcl_cpu_8080 DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      reset,
      execute_instruction RETURNING VALUE(cycles) TYPE i,
      load_com_file IMPORTING iv_data TYPE xstring
                              iv_addr TYPE i DEFAULT 256.

  PRIVATE SECTION.
    " Registers (16-bit pairs stored as 32-bit for efficiency)
    DATA: mv_af TYPE i,  " A=high byte, F=low byte
          mv_bc TYPE i,
          mv_de TYPE i,
          mv_hl TYPE i,
          mv_sp TYPE i,
          mv_pc TYPE i.

    " Memory (64KB)
    DATA: mt_memory TYPE STANDARD TABLE OF x LENGTH 1 WITH EMPTY KEY.

    " Lookup tables
    DATA: mt_parity TYPE STANDARD TABLE OF x LENGTH 1 WITH EMPTY KEY,
          mt_inc    TYPE STANDARD TABLE OF x LENGTH 1 WITH EMPTY KEY,
          mt_dec    TYPE STANDARD TABLE OF x LENGTH 1 WITH EMPTY KEY.

    " Flag constants
    CONSTANTS: c_flag_c TYPE x LENGTH 1 VALUE '01',  " Carry
               c_flag_n TYPE x LENGTH 1 VALUE '02',  " Add/Sub
               c_flag_p TYPE x LENGTH 1 VALUE '04',  " Parity
               c_flag_h TYPE x LENGTH 1 VALUE '10',  " Half-carry
               c_flag_z TYPE x LENGTH 1 VALUE '40',  " Zero
               c_flag_s TYPE x LENGTH 1 VALUE '80'.  " Sign

    METHODS:
      init_tables,
      get_high_byte IMPORTING iv_pair TYPE i RETURNING VALUE(rv) TYPE i,
      get_low_byte  IMPORTING iv_pair TYPE i RETURNING VALUE(rv) TYPE i,
      set_high_byte IMPORTING iv_pair TYPE i iv_val TYPE i RETURNING VALUE(rv) TYPE i,
      set_low_byte  IMPORTING iv_pair TYPE i iv_val TYPE i RETURNING VALUE(rv) TYPE i,

      " Flag operations
      set_flag IMPORTING iv_flag TYPE x LENGTH 1 iv_cond TYPE abap_bool,
      test_flag IMPORTING iv_flag TYPE x LENGTH 1 RETURNING VALUE(rv) TYPE abap_bool,

      " Memory access
      read_byte  IMPORTING iv_addr TYPE i RETURNING VALUE(rv) TYPE x LENGTH 1,
      write_byte IMPORTING iv_addr TYPE i iv_val TYPE x LENGTH 1,
      read_word  IMPORTING iv_addr TYPE i RETURNING VALUE(rv) TYPE i,
      write_word IMPORTING iv_addr TYPE i iv_val TYPE i,

      " ALU operations
      alu_add IMPORTING iv_a TYPE i iv_b TYPE i iv_carry TYPE abap_bool DEFAULT abap_false
              EXPORTING ev_result TYPE i ev_flags TYPE x LENGTH 1,
      alu_sub IMPORTING iv_a TYPE i iv_b TYPE i iv_carry TYPE abap_bool DEFAULT abap_false
              EXPORTING ev_result TYPE i ev_flags TYPE x LENGTH 1,
      alu_and IMPORTING iv_a TYPE i iv_b TYPE i
              EXPORTING ev_result TYPE i ev_flags TYPE x LENGTH 1.
ENDCLASS.
```

#### Opcode Metadata Table

```abap
TYPES: BEGIN OF ty_opcode_meta,
         opcode       TYPE x LENGTH 1,   " Opcode byte
         mnemonic     TYPE string,       " Human-readable
         length       TYPE i,            " Instruction length in bytes
         cycles       TYPE i,            " Clock cycles
         operation    TYPE string,       " Operation type
         addressing   TYPE string,       " Addressing mode
         flags_affect TYPE c LENGTH 5,   " SZAPC - which flags change
       END OF ty_opcode_meta.

DATA: gt_opcodes TYPE HASHED TABLE OF ty_opcode_meta
                 WITH UNIQUE KEY opcode.

METHOD init_opcode_table.
  DATA: ls_op TYPE ty_opcode_meta.

  " NOP
  ls_op = VALUE #( opcode = '00' mnemonic = 'NOP' length = 1
                   operation = 'NOP' flags_affect = '-----' ).
  INSERT ls_op INTO TABLE gt_opcodes.

  " LD BC,nnnn
  ls_op = VALUE #( opcode = '01' mnemonic = 'LD BC,nnnn' length = 3
                   operation = 'LOAD_IMM16' addressing = 'BC' flags_affect = '-----' ).
  INSERT ls_op INTO TABLE gt_opcodes.

  " MOV r,r group (0x40-0x7F except 0x76 HALT)
  " These can be generated programmatically via bit patterns

  " ... populate all 244 i8080 opcodes
ENDMETHOD.
```

#### Bit Manipulation Helpers

```abap
METHOD get_high_byte.
  rv = iv_pair DIV 256.
  rv = rv MOD 256.  " Ensure 8-bit
ENDMETHOD.

METHOD get_low_byte.
  rv = iv_pair MOD 256.
ENDMETHOD.

METHOD set_high_byte.
  rv = ( iv_pair MOD 256 ) + ( iv_val * 256 ).
ENDMETHOD.

METHOD set_low_byte.
  rv = ( iv_pair DIV 256 ) * 256 + iv_val.
ENDMETHOD.

METHOD set_flag.
  IF iv_cond = abap_true.
    mv_af = set_low_byte( iv_pair = mv_af
                          iv_val = get_low_byte( mv_af ) BIT-OR iv_flag ).
  ELSE.
    DATA(lv_mask) = iv_flag BIT-XOR 'FF'.  " Invert mask
    mv_af = set_low_byte( iv_pair = mv_af
                          iv_val = get_low_byte( mv_af ) BIT-AND lv_mask ).
  ENDIF.
ENDMETHOD.

METHOD test_flag.
  DATA(lv_f) = get_low_byte( mv_af ).
  rv = COND #( WHEN ( lv_f BIT-AND iv_flag ) NE '00' THEN abap_true
               ELSE abap_false ).
ENDMETHOD.
```

#### Table-Driven Execution

```abap
METHOD execute_instruction.
  " Fetch opcode
  DATA(lv_opcode) = read_byte( mv_pc ).
  mv_pc = mv_pc + 1.

  " Lookup metadata
  READ TABLE gt_opcodes WITH TABLE KEY opcode = lv_opcode
       INTO DATA(ls_op).

  IF sy-subrc NE 0.
    " Undefined opcode
    RAISE EXCEPTION TYPE cx_invalid_opcode.
  ENDIF.

  " Dispatch by operation type
  CASE ls_op-operation.
    WHEN 'NOP'.
      " Do nothing

    WHEN 'LOAD_IMM16'.
      " LD r16,nnnn - load 16-bit immediate
      DATA(lv_word) = read_word( mv_pc ).
      mv_pc = mv_pc + 2.

      CASE ls_op-addressing.
        WHEN 'BC'. mv_bc = lv_word.
        WHEN 'DE'. mv_de = lv_word.
        WHEN 'HL'. mv_hl = lv_word.
        WHEN 'SP'. mv_sp = lv_word.
      ENDCASE.

    WHEN 'LOAD_IMM8'.
      " LD r,nn - load 8-bit immediate
      DATA(lv_byte) = read_byte( mv_pc ).
      mv_pc = mv_pc + 1.
      " ... set target register

    WHEN 'MOV'.
      " MOV r,r - register to register
      " Decode from opcode bits
      DATA(lv_dst) = ( lv_opcode BIT-AND '38' ) BIT-SHIFT RIGHT BY 3.
      DATA(lv_src) = lv_opcode BIT-AND '07'.

      DATA(lv_val) = get_register( lv_src ).
      set_register( lv_dst, lv_val ).

    WHEN 'ADD'.
      DATA(lv_reg_id) = lv_opcode BIT-AND '07'.
      DATA(lv_operand) = get_register( lv_reg_id ).
      DATA(lv_a) = get_high_byte( mv_af ).

      alu_add( EXPORTING iv_a = lv_a iv_b = lv_operand
               IMPORTING ev_result = DATA(lv_result)
                         ev_flags = DATA(lv_flags) ).

      mv_af = set_high_byte( mv_af, lv_result ).
      mv_af = set_low_byte( mv_af, lv_flags ).

    WHEN 'CALL'.
      " CALL nnnn
      DATA(lv_target) = read_word( mv_pc ).
      mv_pc = mv_pc + 2.

      " Push return address
      mv_sp = mv_sp - 2.
      write_word( mv_sp, mv_pc ).

      " Jump
      mv_pc = lv_target.

    " ... more operation types
  ENDCASE.

  cycles = ls_op-cycles.
ENDMETHOD.
```

### Phase 2: CP/M BDOS Emulation (Week 2-3)

**Goal:** Run CP/M .COM programs that use console I/O

#### BDOS Call Interface

CP/M programs call BDOS via:
```asm
LD C, function_number
LD DE, parameter  ; or other registers
CALL 0x0005       ; BDOS entry point
```

RunCPM intercepts via `IN/OUT 0xFF` port trick:
```c
uint32 cpu_in(const uint32 p) {
    if (p == 0xFF) {
        _Bdos();  // Handle BDOS call, return value in A
        return HIGH_REGISTER(AF);
    }
    return _HardwareIn(p);
}
```

**ABAP Implementation:**
```abap
CLASS lcl_bdos DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_call IMPORTING iv_function TYPE i
                           iv_param_de TYPE i
                 CHANGING cv_af TYPE i
                         cv_bc TYPE i
                         cv_de TYPE i
                         cv_hl TYPE i.

  PRIVATE SECTION.
    DATA: mv_dma_address TYPE i VALUE 128.  " Default DMA buffer

    METHODS:
      console_output IMPORTING iv_char TYPE c,
      console_input RETURNING VALUE(rv_char) TYPE c,
      read_string IMPORTING iv_buffer_addr TYPE i,
      print_string IMPORTING iv_string_addr TYPE i.
ENDCLASS.

METHOD handle_call.
  DATA(lv_func) = get_low_byte( cv_bc ).  " C register

  CASE lv_func.
    WHEN 2.  " C_WRITE - Console output
      DATA(lv_char) = CONV c( get_low_byte( cv_de ) ).
      console_output( lv_char ).

    WHEN 9.  " C_WRITESTR - Print string (terminated by '$')
      print_string( cv_de ).

    WHEN 10. " C_READSTR - Read console buffer
      read_string( cv_de ).

    WHEN 11. " C_STAT - Console status
      " Return 0xFF if char ready, 0x00 if not
      cv_af = set_high_byte( cv_af, 0 ).  " No input for now

    WHEN 0.  " P_TERMCPM - Program termination
      " Set status flag to exit

    " ... more BDOS functions (37+ total)
  ENDCASE.
ENDMETHOD.

METHOD print_string.
  DATA(lv_addr) = iv_string_addr.

  DO.
    DATA(lv_char) = read_byte( lv_addr ).
    IF lv_char = '24'.  " '$' terminator
      EXIT.
    ENDIF.

    WRITE lv_char.
    lv_addr = lv_addr + 1.
  ENDDO.
ENDMETHOD.
```

### Phase 3: Z80 Extensions (Week 4+) - Optional

**Goal:** Support Turbo Pascal, advanced CP/M software

#### Additional Structures

```abap
" Add to lcl_cpu_8080:
DATA: mv_ix TYPE i,    " Index register IX
      mv_iy TYPE i,    " Index register IY
      mv_af1 TYPE i,   " Alternate AF
      mv_bc1 TYPE i,   " Alternate BC
      mv_de1 TYPE i,   " Alternate DE
      mv_hl1 TYPE i,   " Alternate HL
      mv_ir TYPE i,    " I/R register
      mv_iff TYPE i.   " Interrupt flip-flop
```

#### Prefix Handling

```abap
METHOD execute_instruction.
  DATA(lv_opcode) = read_byte( mv_pc ).
  mv_pc = mv_pc + 1.

  CASE lv_opcode.
    WHEN 'CB'.  " Bit operations
      execute_cb_prefix( ).

    WHEN 'ED'.  " Extended instructions
      execute_ed_prefix( ).

    WHEN 'DD'.  " IX operations
      execute_dd_prefix( ).

    WHEN 'FD'.  " IY operations
      execute_fd_prefix( ).

    WHEN OTHERS.
      " Regular i8080/Z80 instruction
      execute_base_instruction( lv_opcode ).
  ENDCASE.
ENDMETHOD.

METHOD execute_cb_prefix.
  " Bit operations: BIT, SET, RES, shifts/rotates
  DATA(lv_op) = read_byte( mv_pc ).
  mv_pc = mv_pc + 1.

  " Decode target register (bits 0-2)
  DATA(lv_reg) = lv_op BIT-AND '07'.

  " Decode operation (bits 6-7)
  DATA(lv_operation) = ( lv_op BIT-AND 'C0' ) BIT-SHIFT RIGHT BY 6.

  CASE lv_operation.
    WHEN 0.  " Shift/rotate (bits 3-5 specify which)
      DATA(lv_shift_type) = ( lv_op BIT-AND '38' ) BIT-SHIFT RIGHT BY 3.
      " RLC, RRC, RL, RR, SLA, SRA, SLIA, SRL

    WHEN 1.  " BIT n,r
      DATA(lv_bit_num) = ( lv_op BIT-AND '38' ) BIT-SHIFT RIGHT BY 3.
      " Test bit lv_bit_num in register lv_reg

    WHEN 2.  " RES n,r
      " Reset (clear) bit

    WHEN 3.  " SET n,r
      " Set bit
  ENDCASE.
ENDMETHOD.

METHOD execute_ed_prefix.
  " Z80 extended instructions: block moves, I/O, etc.
  DATA(lv_op) = read_byte( mv_pc ).
  mv_pc = mv_pc + 1.

  CASE lv_op.
    WHEN 'B0'.  " LDIR - Block copy with repeat
      DO.
        write_byte( mv_de, read_byte( mv_hl ) ).
        mv_hl = mv_hl + 1.
        mv_de = mv_de + 1.
        mv_bc = mv_bc - 1.

        IF mv_bc = 0.
          EXIT.
        ENDIF.
      ENDDO.

    WHEN '44'.  " NEG - Negate accumulator
      " A = 0 - A

    WHEN '4D'.  " RETI - Return from interrupt
      " POP PC, restore IFF

    " ... more ED prefix instructions
  ENDCASE.
ENDMETHOD.
```

## Testing Strategy

### Level 1: Unit Tests
```abap
CLASS ltc_cpu_8080_test DEFINITION FOR TESTING.
  PRIVATE SECTION.
    DATA: mo_cpu TYPE REF TO lcl_cpu_8080.

    METHODS:
      setup,
      test_nop,
      test_ld_bc_imm,
      test_ld_a_bc,
      test_add_simple,
      test_add_with_carry,
      test_flags_zero,
      test_flags_sign,
      test_call_ret.
ENDCLASS.

METHOD test_add_simple.
  " Load A with 5
  mo_cpu->mv_af = mo_cpu->set_high_byte( mo_cpu->mv_af, 5 ).

  " Load B with 3
  mo_cpu->mv_bc = mo_cpu->set_high_byte( mo_cpu->mv_bc, 3 ).

  " Execute ADD B (0x80)
  mo_cpu->write_byte( iv_addr = 256 iv_val = '80' ).
  mo_cpu->mv_pc = 256.
  mo_cpu->execute_instruction( ).

  " Assert A = 8
  DATA(lv_a) = mo_cpu->get_high_byte( mo_cpu->mv_af ).
  cl_abap_unit_assert=>assert_equals( act = lv_a exp = 8 ).

  " Assert no flags set
  DATA(lv_f) = mo_cpu->get_low_byte( mo_cpu->mv_af ).
  cl_abap_unit_assert=>assert_equals( act = lv_f exp = 0 ).
ENDMETHOD.
```

### Level 2: Integration Tests
Test with small .COM files:
```abap
METHOD test_hello_world_com.
  " Load HELLO.COM bytes
  DATA(lv_com_data) = get_hello_world_binary( ).
  mo_cpu->load_com_file( lv_com_data ).

  " Execute until BDOS call 0 (exit)
  DO 10000 TIMES.
    mo_cpu->execute_instruction( ).

    IF mo_cpu->get_status( ) = 'HALTED'.
      EXIT.
    ENDIF.
  ENDDO.

  " Verify output contained "Hello, World!"
  cl_abap_unit_assert=>assert_true( output_contains( 'Hello, World!' ) ).
ENDMETHOD.
```

### Level 3: System Tests
- ZORK.COM gameplay
- Turbo Pascal compilation
- dBase II database operations

## Performance Considerations

### ABAP-Specific Optimizations

1. **Internal Table Access**
   ```abap
   " BAD: Linear search every memory access
   READ TABLE mt_memory INDEX lv_addr INTO lv_byte.

   " GOOD: Direct access or HASHED table
   DATA: mt_memory TYPE HASHED TABLE OF ty_mem_cell WITH UNIQUE KEY address.
   ```

2. **Field Symbols for Registers**
   ```abap
   " Fast register decoding
   FIELD-SYMBOLS: <fs_reg> TYPE i.

   CASE lv_reg_id.
     WHEN 0. ASSIGN mv_bc TO <fs_reg>.  " B
     WHEN 1. ASSIGN mv_bc TO <fs_reg>.  " C (with shift)
     " ...
   ENDCASE.

   <fs_reg> = lv_value.  " Direct write
   ```

3. **Batch Operations**
   ```abap
   " Pre-compute 1000 instructions, then execute
   " Reduces method call overhead
   ```

4. **ALU Lookup Tables**
   ```abap
   " Instead of calculating flags every time:
   DATA(lv_flags) = mt_add_flags[ lv_result ]-flags.
   ```

## File Organization

```
src/
├── zcl_cpu_core.clas.abap           - Main CPU emulation
├── zcl_cpu_i8080.clas.abap          - i8080 specific opcodes
├── zcl_cpu_z80.clas.abap            - Z80 extensions
├── zcl_bdos.clas.abap               - CP/M BDOS emulation
├── zcl_memory.clas.abap             - Memory manager
├── zcl_disassembler.clas.abap       - Debugging aid
├── zcl_opcode_table.clas.abap       - Opcode metadata
├── zif_cpu.intf.abap                - Interface definition
└── tests/
    ├── ztc_cpu_unit.clas.abap       - Unit tests
    └── ztc_cpu_integration.clas.abap - Integration tests

reports/
├── z_run_cpm.prog.abap              - Main runner program
└── z_cpm_debugger.prog.abap         - Interactive debugger
```

## Milestones

### Milestone 1: "Hello ABAP CPU" (Weekend)
- [x] Clone RunCPM for reference
- [ ] Create basic classes (CPU, Memory)
- [ ] Implement 20 core opcodes (NOP, LD, MOV, ADD, SUB, CALL, RET, JP, HALT)
- [ ] Execute hardcoded "Hello World" bytecode
- **Success Criteria:** WRITE statement outputs "Hello World"

### Milestone 2: "Run HELLO.COM" (Week 1-2)
- [ ] Complete all 244 i8080 opcodes
- [ ] Implement BDOS functions 0, 2, 9 (exit, write char, write string)
- [ ] Load .COM file from XSTRING
- [ ] Execute authentic CP/M HELLO.COM
- **Success Criteria:** Real .COM file runs successfully

### Milestone 3: "Interactive CP/M" (Week 2-3)
- [ ] Implement BDOS functions 1, 10, 11 (read char, read line, status)
- [ ] Add disk I/O (minimal - read only)
- [ ] Support file operations
- **Success Criteria:** Run simple text adventure game

### Milestone 4: "Z80 Complete" (Week 4+)
- [ ] Add Z80 registers and alternates
- [ ] Implement CB prefix (bit operations)
- [ ] Implement ED prefix (block operations)
- [ ] Implement DD/FD prefixes (IX/IY)
- **Success Criteria:** Turbo Pascal compiles a program

## SAP Integration Ideas (Future)

### Crazy Ideas:
1. **ZORK as SAP Transaction**
   ```abap
   CALL TRANSACTION 'ZORK' WITH AUTHORITY-CHECK.
   ```

2. **CP/M Programs in Background Jobs**
   ```abap
   SUBMIT z_run_cpm WITH p_com = 'TURBO.COM' VIA JOB.
   ```

3. **Chiptune Player with AY-8910**
   - Z80 emulator plays .PT3 files
   - Output via browser Audio API
   - SAP GUI ActiveX control?

4. **Retro IDE in SAP**
   - Write Z80 assembly in SAP editor
   - Assemble with internal assembler
   - Run in emulator
   - Save to database

5. **Multi-user BBS**
   - CP/M instances per user
   - Shared file system in HANA
   - Chat via BDOS extensions

## References

- **RunCPM Source:** `./RunCPM/` (cloned)
- **Z80 CPU User Manual:** http://www.zilog.com/docs/z80/um0080.pdf
- **CP/M 2.2 System Manual:** http://www.gaby.de/cpm/manuals/archive/cpm22htm/
- **ABAP Bit Operations:** SE80 → Documentation → ABAP Keywords → BIT-AND, BIT-OR, BIT-XOR, BIT-SHIFT
- **i8080 Opcode Reference:** http://www.emulator101.com/reference/8080-by-opcode.html
- **Z80 Opcode Map:** https://clrhome.org/table/

## Next Steps

1. **Study RunCPM cpu.h** - Understand execution flow
2. **Create POC** - Minimal 20-opcode executor in ABAP
3. **Validate approach** - Ensure bit operations work as expected
4. **Build incrementally** - Add opcodes in functional groups
5. **Test constantly** - Unit test each opcode group

---

**Decision:** Start with i8080, use table-driven approach from RunCPM, extend to Z80 if needed for specific software (Turbo Pascal, Z80 assemblers, late CP/M tools).

**Philosophy:** "Make it work, make it right, make it fast" - in that order!
