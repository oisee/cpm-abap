# Z-Machine ABAP Architecture

This document describes the architecture for implementing a Z-machine interpreter in ABAP, optimized for transpiler compatibility.

## Design Goals

1. **Transpiler Compatible:** Must work with abaplint transpiler for Node.js testing
2. **SAP Native:** Run natively in SAP systems without external dependencies
3. **Version 3 Focus:** Support Zork I, II, III and classic Infocom games
4. **Modular Design:** Separate concerns for maintainability
5. **Testable:** Comprehensive unit tests for all opcodes

## Class Structure

```
ZCL_ZMACHINE
├── ZCL_ZMACHINE_MEMORY      - Memory management
├── ZCL_ZMACHINE_DECODER     - Instruction decoding
├── ZCL_ZMACHINE_TEXT        - ZSCII text encoding/decoding
├── ZCL_ZMACHINE_OBJECTS     - Object tree operations
├── ZCL_ZMACHINE_IO          - Input/output handling
└── ZCL_ZMACHINE_STACK       - Call stack management
```

## Memory Representation

### Story File Storage

Following the pattern from `zcl_cpu_8080_v2`, memory is stored as STRING with 2 hex characters per byte:

```abap
CLASS zcl_zmachine_memory DEFINITION.
  PUBLIC SECTION.
    METHODS:
      load_story IMPORTING iv_story TYPE string,
      read_byte  IMPORTING iv_addr TYPE i RETURNING VALUE(rv_val) TYPE i,
      read_word  IMPORTING iv_addr TYPE i RETURNING VALUE(rv_val) TYPE i,
      write_byte IMPORTING iv_addr TYPE i iv_val TYPE i,
      write_word IMPORTING iv_addr TYPE i iv_val TYPE i.

  PRIVATE SECTION.
    DATA: mv_memory TYPE string,      " Story file (hex encoded)
          mv_dynamic_end TYPE i,      " End of dynamic memory
          mv_static_end TYPE i,       " End of static memory
          mv_high_start TYPE i.       " Start of high memory
ENDCLASS.
```

### Memory Layout Constants

```abap
CONSTANTS:
  " Header addresses
  c_hdr_version     TYPE i VALUE 0,
  c_hdr_flags1      TYPE i VALUE 1,
  c_hdr_high_mem    TYPE i VALUE 4,
  c_hdr_init_pc     TYPE i VALUE 6,
  c_hdr_dictionary  TYPE i VALUE 8,
  c_hdr_objects     TYPE i VALUE 10,
  c_hdr_globals     TYPE i VALUE 12,
  c_hdr_static_mem  TYPE i VALUE 14,
  c_hdr_flags2      TYPE i VALUE 16,
  c_hdr_abbrev      TYPE i VALUE 24,
  c_hdr_file_len    TYPE i VALUE 26,
  c_hdr_checksum    TYPE i VALUE 28.
```

### Byte/Word Operations

```abap
METHOD read_byte.
  " Convert address to hex string offset
  DATA(lv_offset) = iv_addr * 2.

  " Extract 2 hex characters
  DATA(lv_hex) = mv_memory+lv_offset(2).

  " Convert to integer
  rv_val = hex_to_int( lv_hex ).
ENDMETHOD.

METHOD read_word.
  " Z-machine is big-endian
  DATA(lv_high) = read_byte( iv_addr ).
  DATA(lv_low) = read_byte( iv_addr + 1 ).
  rv_val = lv_high * 256 + lv_low.
ENDMETHOD.

METHOD write_byte.
  " Only allow writes to dynamic memory
  IF iv_addr >= mv_dynamic_end.
    " Error: write to static/high memory
    RETURN.
  ENDIF.

  DATA(lv_offset) = iv_addr * 2.
  DATA(lv_hex) = int_to_hex( iv_val MOD 256 ).

  " String replacement (transpiler compatible)
  DATA(lv_before) = mv_memory(lv_offset).
  DATA(lv_after_start) = lv_offset + 2.
  DATA(lv_after) = mv_memory+lv_after_start.
  mv_memory = lv_before && lv_hex && lv_after.
ENDMETHOD.
```

## Stack Architecture

### Call Stack Representation

Since ABAP internal tables don't transpile well, we use STRING-based storage:

```abap
CLASS zcl_zmachine_stack DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_frame,
        return_pc     TYPE i,       " Return address
        result_var    TYPE i,       " Where to store result (-1 = discard)
        num_locals    TYPE i,       " Number of local variables
        locals        TYPE string,  " 15 words as hex (30 chars)
        eval_stack    TYPE string,  " Evaluation stack as hex
        stack_ptr     TYPE i,       " Evaluation stack pointer
      END OF ty_frame.

    METHODS:
      push_frame IMPORTING is_frame TYPE ty_frame,
      pop_frame  RETURNING VALUE(rs_frame) TYPE ty_frame,
      get_local  IMPORTING iv_num TYPE i RETURNING VALUE(rv_val) TYPE i,
      set_local  IMPORTING iv_num TYPE i iv_val TYPE i,
      push_eval  IMPORTING iv_val TYPE i,
      pop_eval   RETURNING VALUE(rv_val) TYPE i.

  PRIVATE SECTION.
    DATA: mv_frames TYPE string,    " Serialized frames
          mv_frame_count TYPE i,
          ms_current TYPE ty_frame. " Current frame (hot)
ENDCLASS.
```

### Global Variables

240 global variables stored starting at address from header $0C:

```abap
METHOD get_global.
  " Globals are 16-bit words at globals_addr + 2*(var-16)
  DATA(lv_addr) = mv_globals_addr + ( iv_var - 16 ) * 2.
  rv_val = mo_memory->read_word( lv_addr ).
ENDMETHOD.

METHOD set_global.
  DATA(lv_addr) = mv_globals_addr + ( iv_var - 16 ) * 2.
  mo_memory->write_word( iv_addr = lv_addr iv_val = iv_val ).
ENDMETHOD.
```

## Instruction Decoder

### Decoding State Machine

```abap
CLASS zcl_zmachine_decoder DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_instruction,
        opcode       TYPE i,        " Opcode number
        form         TYPE i,        " 0=short, 1=long, 2=var, 3=ext
        op_count     TYPE i,        " 0OP, 1OP, 2OP, VAR
        operands     TYPE string,   " Up to 8 operand values (hex)
        operand_cnt  TYPE i,        " Actual operand count
        has_store    TYPE abap_bool,
        store_var    TYPE i,
        has_branch   TYPE abap_bool,
        branch_on    TYPE abap_bool, " Branch on true or false
        branch_off   TYPE i,        " Branch offset
        text_literal TYPE string,   " For print/print_ret
      END OF ty_instruction.

    METHODS:
      decode IMPORTING iv_pc TYPE i
             EXPORTING ev_next_pc TYPE i
             RETURNING VALUE(rs_instr) TYPE ty_instruction.

  PRIVATE SECTION.
    DATA: mo_memory TYPE REF TO zcl_zmachine_memory.

    METHODS:
      decode_operand_types IMPORTING iv_byte TYPE i
                           RETURNING VALUE(rv_types) TYPE string,
      decode_branch IMPORTING iv_pc TYPE i
                    EXPORTING ev_on_true TYPE abap_bool
                              ev_offset TYPE i
                              ev_bytes TYPE i.
ENDCLASS.
```

### Form Detection

```abap
METHOD decode.
  DATA(lv_byte) = mo_memory->read_byte( iv_pc ).
  DATA(lv_pc) = iv_pc + 1.

  " Determine instruction form
  DATA(lv_top2) = lv_byte DIV 64.  " Top 2 bits

  CASE lv_top2.
    WHEN 3.  " 11xxxxxx = Variable form
      rs_instr-form = 2.
      IF lv_byte MOD 64 DIV 32 = 0.  " Bit 5 = 0
        rs_instr-op_count = 2.  " 2OP
      ELSE.
        rs_instr-op_count = 3.  " VAR
      ENDIF.
      rs_instr-opcode = lv_byte MOD 32.

    WHEN 2.  " 10xxxxxx = Short form
      rs_instr-form = 0.
      DATA(lv_type) = lv_byte MOD 64 DIV 16.  " Bits 4-5
      IF lv_type = 3.
        rs_instr-op_count = 0.  " 0OP
      ELSE.
        rs_instr-op_count = 1.  " 1OP
      ENDIF.
      rs_instr-opcode = lv_byte MOD 16.

    WHEN OTHERS.  " 00xxxxxx or 01xxxxxx = Long form
      rs_instr-form = 1.
      rs_instr-op_count = 2.  " Always 2OP
      rs_instr-opcode = lv_byte MOD 32.
  ENDCASE.

  " Continue decoding operands, store, branch...
ENDMETHOD.
```

## Text Encoding/Decoding

### ZSCII Decoder

```abap
CLASS zcl_zmachine_text DEFINITION.
  PUBLIC SECTION.
    METHODS:
      decode_string IMPORTING iv_addr TYPE i
                    EXPORTING ev_end_addr TYPE i
                    RETURNING VALUE(rv_text) TYPE string,
      encode_word   IMPORTING iv_text TYPE string
                    RETURNING VALUE(rv_encoded) TYPE string,
      get_abbreviation IMPORTING iv_index TYPE i
                       RETURNING VALUE(rv_text) TYPE string.

  PRIVATE SECTION.
    CONSTANTS:
      " Alphabet tables (Z-chars 6-31)
      c_a0 TYPE string VALUE 'abcdefghijklmnopqrstuvwxyz',
      c_a1 TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
      c_a2 TYPE string VALUE ' ^0123456789.,!?_#''"\/-:()'.

    DATA: mo_memory TYPE REF TO zcl_zmachine_memory,
          mv_abbrev_addr TYPE i.

    METHODS:
      decode_zchar IMPORTING iv_zchar TYPE i
                             iv_alphabet TYPE i
                   CHANGING  cv_text TYPE string
                             cv_alphabet TYPE i
                             cv_state TYPE i
                             cv_zscii TYPE i.
ENDCLASS.
```

### String Decoding Algorithm

```abap
METHOD decode_string.
  DATA: lv_addr TYPE i VALUE iv_addr,
        lv_alphabet TYPE i VALUE 0,
        lv_state TYPE i VALUE 0,       " 0=normal, 1=abbrev, 2=zscii1, 3=zscii2
        lv_zscii TYPE i VALUE 0,
        lv_abbrev_z TYPE i.

  rv_text = ''.

  DO.
    " Read 2-byte word
    DATA(lv_word) = mo_memory->read_word( lv_addr ).
    lv_addr = lv_addr + 2.

    " Extract 3 Z-characters (5 bits each)
    DATA(lv_zc1) = lv_word DIV 1024 MOD 32.   " Bits 14-10
    DATA(lv_zc2) = lv_word DIV 32 MOD 32.     " Bits 9-5
    DATA(lv_zc3) = lv_word MOD 32.            " Bits 4-0

    " Process each Z-character
    decode_zchar( EXPORTING iv_zchar = lv_zc1 iv_alphabet = lv_alphabet
                  CHANGING cv_text = rv_text cv_alphabet = lv_alphabet
                           cv_state = lv_state cv_zscii = lv_zscii ).
    decode_zchar( EXPORTING iv_zchar = lv_zc2 iv_alphabet = lv_alphabet
                  CHANGING cv_text = rv_text cv_alphabet = lv_alphabet
                           cv_state = lv_state cv_zscii = lv_zscii ).
    decode_zchar( EXPORTING iv_zchar = lv_zc3 iv_alphabet = lv_alphabet
                  CHANGING cv_text = rv_text cv_alphabet = lv_alphabet
                           cv_state = lv_state cv_zscii = lv_zscii ).

    " Check end bit (bit 15)
    IF lv_word >= 32768.
      EXIT.
    ENDIF.
  ENDDO.

  ev_end_addr = lv_addr.
ENDMETHOD.
```

## Object Operations

### Object Access

```abap
CLASS zcl_zmachine_objects DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_parent    IMPORTING iv_obj TYPE i RETURNING VALUE(rv_parent) TYPE i,
      get_sibling   IMPORTING iv_obj TYPE i RETURNING VALUE(rv_sibling) TYPE i,
      get_child     IMPORTING iv_obj TYPE i RETURNING VALUE(rv_child) TYPE i,
      set_parent    IMPORTING iv_obj TYPE i iv_parent TYPE i,
      set_sibling   IMPORTING iv_obj TYPE i iv_sibling TYPE i,
      set_child     IMPORTING iv_obj TYPE i iv_child TYPE i,
      test_attr     IMPORTING iv_obj TYPE i iv_attr TYPE i RETURNING VALUE(rv_set) TYPE abap_bool,
      set_attr      IMPORTING iv_obj TYPE i iv_attr TYPE i,
      clear_attr    IMPORTING iv_obj TYPE i iv_attr TYPE i,
      get_prop      IMPORTING iv_obj TYPE i iv_prop TYPE i RETURNING VALUE(rv_val) TYPE i,
      put_prop      IMPORTING iv_obj TYPE i iv_prop TYPE i iv_val TYPE i,
      get_prop_addr IMPORTING iv_obj TYPE i iv_prop TYPE i RETURNING VALUE(rv_addr) TYPE i,
      get_prop_len  IMPORTING iv_addr TYPE i RETURNING VALUE(rv_len) TYPE i,
      get_next_prop IMPORTING iv_obj TYPE i iv_prop TYPE i RETURNING VALUE(rv_next) TYPE i,
      get_short_name IMPORTING iv_obj TYPE i RETURNING VALUE(rv_name) TYPE string,
      insert_obj    IMPORTING iv_obj TYPE i iv_dest TYPE i,
      remove_obj    IMPORTING iv_obj TYPE i.

  PRIVATE SECTION.
    " Version 3: 9 bytes per object, max 255
    CONSTANTS: c_obj_size TYPE i VALUE 9,
               c_obj_max TYPE i VALUE 255,
               c_attr_bytes TYPE i VALUE 4,
               c_prop_defaults TYPE i VALUE 31.

    DATA: mo_memory TYPE REF TO zcl_zmachine_memory,
          mv_obj_table TYPE i.

    METHODS:
      get_obj_addr IMPORTING iv_obj TYPE i RETURNING VALUE(rv_addr) TYPE i,
      get_prop_table IMPORTING iv_obj TYPE i RETURNING VALUE(rv_addr) TYPE i.
ENDCLASS.
```

### Object Address Calculation (v3)

```abap
METHOD get_obj_addr.
  " Object table starts with 31 default property words (62 bytes)
  " Then objects are numbered 1-255, 9 bytes each
  rv_addr = mv_obj_table + c_prop_defaults * 2 + ( iv_obj - 1 ) * c_obj_size.
ENDMETHOD.

METHOD test_attr.
  " Attributes 0-31 stored in 4 bytes, MSB first
  " Attribute n is bit (31-n) in the 32-bit value
  DATA(lv_addr) = get_obj_addr( iv_obj ).
  DATA(lv_byte_num) = iv_attr DIV 8.
  DATA(lv_bit_num) = 7 - ( iv_attr MOD 8 ).

  DATA(lv_byte) = mo_memory->read_byte( lv_addr + lv_byte_num ).

  " Check bit using arithmetic (no BIT operations)
  DATA(lv_mask) = 1.
  DO lv_bit_num TIMES.
    lv_mask = lv_mask * 2.
  ENDDO.

  IF lv_byte MOD ( lv_mask * 2 ) >= lv_mask.
    rv_set = abap_true.
  ELSE.
    rv_set = abap_false.
  ENDIF.
ENDMETHOD.
```

## I/O Handling

### Abstract I/O Interface

```abap
INTERFACE zif_zmachine_io.
  METHODS:
    print_char IMPORTING iv_zscii TYPE i,
    print_text IMPORTING iv_text TYPE string,
    new_line,
    read_line  IMPORTING iv_max_len TYPE i
               RETURNING VALUE(rv_input) TYPE string,
    read_char  RETURNING VALUE(rv_zscii) TYPE i,
    show_status IMPORTING iv_location TYPE string
                          iv_score TYPE i
                          iv_moves TYPE i
                          iv_hours TYPE i
                          iv_minutes TYPE i
                          iv_is_time TYPE abap_bool,
    split_window IMPORTING iv_lines TYPE i,
    set_window   IMPORTING iv_window TYPE i,
    erase_window IMPORTING iv_window TYPE i,
    set_cursor   IMPORTING iv_line TYPE i iv_column TYPE i,
    set_style    IMPORTING iv_style TYPE i,
    buffer_mode  IMPORTING iv_mode TYPE i.
ENDINTERFACE.

CLASS zcl_zmachine_io_console DEFINITION.
  " Console implementation for testing
  PUBLIC SECTION.
    INTERFACES: zif_zmachine_io.
    DATA: mv_output TYPE string,     " Captured output
          mv_input_queue TYPE string. " Simulated input
ENDCLASS.
```

## Main Interpreter Class

```abap
CLASS zcl_zmachine DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      load_story IMPORTING iv_story TYPE string,
      set_io     IMPORTING io_io TYPE REF TO zif_zmachine_io,
      run,
      step       RETURNING VALUE(rv_running) TYPE abap_bool,
      reset,

      " Debugging
      get_pc     RETURNING VALUE(rv_pc) TYPE i,
      get_sp     RETURNING VALUE(rv_sp) TYPE i,
      dump_state RETURNING VALUE(rv_state) TYPE string.

  PRIVATE SECTION.
    DATA: mo_memory  TYPE REF TO zcl_zmachine_memory,
          mo_decoder TYPE REF TO zcl_zmachine_decoder,
          mo_text    TYPE REF TO zcl_zmachine_text,
          mo_objects TYPE REF TO zcl_zmachine_objects,
          mo_stack   TYPE REF TO zcl_zmachine_stack,
          mo_io      TYPE REF TO zif_zmachine_io,

          mv_pc      TYPE i,           " Program counter
          mv_version TYPE i,           " Story file version
          mv_running TYPE abap_bool.

    METHODS:
      execute_instruction IMPORTING is_instr TYPE zcl_zmachine_decoder=>ty_instruction,

      " Opcode implementations
      execute_0op IMPORTING is_instr TYPE zcl_zmachine_decoder=>ty_instruction,
      execute_1op IMPORTING is_instr TYPE zcl_zmachine_decoder=>ty_instruction,
      execute_2op IMPORTING is_instr TYPE zcl_zmachine_decoder=>ty_instruction,
      execute_var IMPORTING is_instr TYPE zcl_zmachine_decoder=>ty_instruction,

      " Variable access
      get_variable IMPORTING iv_var TYPE i RETURNING VALUE(rv_val) TYPE i,
      set_variable IMPORTING iv_var TYPE i iv_val TYPE i,

      " Branching
      do_branch IMPORTING iv_condition TYPE abap_bool
                          is_instr TYPE zcl_zmachine_decoder=>ty_instruction,
      do_store  IMPORTING iv_val TYPE i
                          is_instr TYPE zcl_zmachine_decoder=>ty_instruction,

      " Routine calls
      call_routine IMPORTING iv_addr TYPE i
                             iv_args TYPE string
                             iv_store TYPE i,
      return_routine IMPORTING iv_value TYPE i.
ENDCLASS.
```

## Execution Loop

```abap
METHOD run.
  mv_running = abap_true.

  WHILE mv_running = abap_true.
    step( ).
  ENDWHILE.
ENDMETHOD.

METHOD step.
  " Decode instruction at PC
  DATA(ls_instr) = mo_decoder->decode(
    EXPORTING iv_pc = mv_pc
    IMPORTING ev_next_pc = mv_pc ).

  " Execute instruction
  execute_instruction( ls_instr ).

  rv_running = mv_running.
ENDMETHOD.

METHOD execute_instruction.
  CASE is_instr-op_count.
    WHEN 0.
      execute_0op( is_instr ).
    WHEN 1.
      execute_1op( is_instr ).
    WHEN 2.
      execute_2op( is_instr ).
    WHEN 3.  " VAR
      execute_var( is_instr ).
  ENDCASE.
ENDMETHOD.
```

## Testing Strategy

### Unit Test Structure

```abap
CLASS ltcl_zmachine_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_vm TYPE REF TO zcl_zmachine,
          mo_io TYPE REF TO zcl_zmachine_io_console.

    METHODS:
      setup,

      " Memory tests
      test_read_byte FOR TESTING,
      test_read_word FOR TESTING,
      test_write_byte FOR TESTING,

      " Text tests
      test_decode_simple FOR TESTING,
      test_decode_shift FOR TESTING,
      test_decode_abbreviation FOR TESTING,

      " Opcode tests (grouped)
      test_je FOR TESTING,
      test_jl FOR TESTING,
      test_jg FOR TESTING,
      test_add FOR TESTING,
      test_sub FOR TESTING,
      test_mul FOR TESTING,
      test_div FOR TESTING,
      test_call FOR TESTING,
      test_ret FOR TESTING,

      " Object tests
      test_get_parent FOR TESTING,
      test_test_attr FOR TESTING,
      test_insert_obj FOR TESTING,

      " Integration
      test_hello_world FOR TESTING,
      test_simple_game FOR TESTING.
ENDCLASS.
```

### Test Helper Methods

```abap
METHOD create_test_story.
  " Create minimal valid v3 story file
  DATA(lv_story) = ''.

  " Header (64 bytes)
  lv_story = lv_story && '03'.        " $00: Version 3
  lv_story = lv_story && '00'.        " $01: Flags 1
  " ... fill remaining header ...

  " Add test code at initial PC
  " ...

  rv_story = lv_story.
ENDMETHOD.
```

## File Organization

```
src/
├── zcl_zmachine.clas.abap                 " Main interpreter
├── zcl_zmachine.clas.testclasses.abap     " Unit tests
├── zcl_zmachine_memory.clas.abap          " Memory management
├── zcl_zmachine_decoder.clas.abap         " Instruction decoder
├── zcl_zmachine_text.clas.abap            " ZSCII encoding
├── zcl_zmachine_objects.clas.abap         " Object operations
├── zcl_zmachine_stack.clas.abap           " Call stack
├── zif_zmachine_io.intf.abap              " I/O interface
├── zcl_zmachine_io_console.clas.abap      " Console I/O
└── zcl_zmachine_io_sap.clas.abap          " SAP GUI I/O (future)
```

## Transpiler Considerations

### Patterns That Work

```abap
" STRING operations for memory
mv_memory = lv_before && lv_hex && lv_after.
lv_substr = mv_memory+lv_offset(2).

" Arithmetic for bit operations
lv_high = lv_val DIV 256.
lv_low = lv_val MOD 256.
lv_bit = lv_val MOD ( lv_mask * 2 ) DIV lv_mask.

" DO loops
DO 256 TIMES.
  lv_i = sy-index - 1.
ENDDO.

" CASE statements
CASE lv_opcode.
  WHEN 0. " rtrue
  WHEN 1. " rfalse
ENDCASE.
```

### Patterns to Avoid

```abap
" NO internal tables
APPEND lv_val TO lt_stack.  " Bad

" NO BIT operations
lv_result = lv_val BIT-AND 255.  " Bad

" NO XSTRING
DATA: mv_mem TYPE xstring.  " Bad

" NO dynamic method calls
CALL METHOD (lv_method_name).  " Bad
```

## Performance Considerations

1. **String operations:** Minimize concatenation in hot paths
2. **Method calls:** Inline critical operations
3. **Memory access:** Cache frequently-read values
4. **Stack operations:** Keep current frame in instance variables

## Future Enhancements

1. **Version 4-5 support:** Extended opcodes, more objects
2. **Save/restore:** Quetzal format support
3. **Sound:** Basic beep support
4. **Graphics:** Version 6 picture support (SAP GUI only)
5. **Debugging:** Step-by-step execution, breakpoints

---

*Document created for ABAP Z-Machine Interpreter project*
*Version: 1.0*
*Date: 2025-12-07*
