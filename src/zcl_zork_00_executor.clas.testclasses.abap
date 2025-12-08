*&---------------------------------------------------------------------*
*& Unit Tests for ZCL_ZORK_00_EXECUTOR
*& Tests the Z-machine instruction execution engine
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Mock I/O class for testing
*----------------------------------------------------------------------*
CLASS lcl_test_io DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_zork_00_io.

    DATA: mv_output TYPE string,
          mv_lines  TYPE i.

    METHODS:
      get_output RETURNING VALUE(rv_output) TYPE string,
      clear_output.

ENDCLASS.

CLASS lcl_test_io IMPLEMENTATION.

  METHOD zif_zork_00_io~print_char.
    " Convert ZSCII to character and append
    IF iv_zscii >= 32 AND iv_zscii <= 126.
      DATA(lv_char) = cl_abap_conv_codepage=>create_out( )->convert( VALUE #( ( iv_zscii ) ) ).
      mv_output = mv_output && lv_char.
    ENDIF.
  ENDMETHOD.

  METHOD zif_zork_00_io~print_text.
    mv_output = mv_output && iv_text.
  ENDMETHOD.

  METHOD zif_zork_00_io~new_line.
    mv_output = mv_output && cl_abap_char_utilities=>newline.
    mv_lines = mv_lines + 1.
  ENDMETHOD.

  METHOD zif_zork_00_io~print_num.
    mv_output = mv_output && |{ iv_num }|.
  ENDMETHOD.

  METHOD zif_zork_00_io~buffer_mode.
    " Ignore for testing
  ENDMETHOD.

  METHOD zif_zork_00_io~read_line.
    " Return empty for testing
    rv_text = ''.
  ENDMETHOD.

  METHOD zif_zork_00_io~read_char.
    " Return newline for testing
    rv_zscii = 13.
  ENDMETHOD.

  METHOD zif_zork_00_io~show_status.
    mv_output = mv_output && |[Status: { iv_location } Score:{ iv_score } Moves:{ iv_moves }]|.
  ENDMETHOD.

  METHOD zif_zork_00_io~split_window.
    " Ignore for testing
  ENDMETHOD.

  METHOD zif_zork_00_io~set_window.
    " Ignore for testing
  ENDMETHOD.

  METHOD zif_zork_00_io~erase_window.
    " Ignore for testing
  ENDMETHOD.

  METHOD zif_zork_00_io~set_cursor.
    " Ignore for testing
  ENDMETHOD.

  METHOD zif_zork_00_io~set_style.
    " Ignore for testing
  ENDMETHOD.

  METHOD zif_zork_00_io~sound_effect.
    " Ignore for testing
  ENDMETHOD.

  METHOD get_output.
    rv_output = mv_output.
  ENDMETHOD.

  METHOD clear_output.
    mv_output = ''.
    mv_lines = 0.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* Test class for executor
*----------------------------------------------------------------------*
CLASS ltcl_executor_test DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_memory   TYPE REF TO zcl_zork_00_memory,
      mo_stack    TYPE REF TO zcl_zork_00_stack,
      mo_decoder  TYPE REF TO zcl_zork_00_decoder,
      mo_executor TYPE REF TO zcl_zork_00_executor,
      mo_io       TYPE REF TO lcl_test_io.

    METHODS:
      setup,
      create_test_story
        RETURNING VALUE(rv_story) TYPE string,

      " Test methods
      test_init FOR TESTING,
      test_arithmetic_add FOR TESTING,
      test_arithmetic_sub FOR TESTING,
      test_arithmetic_mul FOR TESTING,
      test_arithmetic_div FOR TESTING,
      test_jump_equal FOR TESTING,
      test_jump_zero FOR TESTING,
      test_jump_less FOR TESTING,
      test_jump_greater FOR TESTING,
      test_bitwise_and FOR TESTING,
      test_bitwise_or FOR TESTING,
      test_inc_dec FOR TESTING,
      test_load_store FOR TESTING,
      test_stack_push_pull FOR TESTING.

ENDCLASS.


CLASS ltcl_executor_test IMPLEMENTATION.

  METHOD setup.
    " Create components
    mo_memory = NEW zcl_zork_00_memory( ).
    mo_stack = NEW zcl_zork_00_stack( mo_memory ).
    mo_decoder = NEW zcl_zork_00_decoder( mo_memory ).
    mo_io = NEW lcl_test_io( ).

    " Load test story
    DATA(lv_story) = create_test_story( ).
    mo_memory->load_story( lv_story ).

    " Create executor
    mo_executor = NEW zcl_zork_00_executor(
      io_memory  = mo_memory
      io_stack   = mo_stack
      io_decoder = mo_decoder
      io_io      = mo_io ).
  ENDMETHOD.


  METHOD create_test_story.
    " Create minimal v3 story file for testing
    " Header (64 bytes) + some code space
    DATA: lv_hex TYPE string.

    " Header at 0x00-0x3F (64 bytes)
    lv_hex = ''.

    " 0x00: Version = 3
    lv_hex = lv_hex && '03'.

    " 0x01: Flags 1 = 0
    lv_hex = lv_hex && '00'.

    " 0x02-0x03: Release number = 1
    lv_hex = lv_hex && '0001'.

    " 0x04-0x05: High memory base = 0x0100 (256)
    lv_hex = lv_hex && '0100'.

    " 0x06-0x07: Initial PC = 0x0100 (256) - start of code
    lv_hex = lv_hex && '0100'.

    " 0x08-0x09: Dictionary = 0x0200 (512)
    lv_hex = lv_hex && '0200'.

    " 0x0A-0x0B: Object table = 0x0180 (384)
    lv_hex = lv_hex && '0180'.

    " 0x0C-0x0D: Globals table = 0x0040 (64) - right after header
    lv_hex = lv_hex && '0040'.

    " 0x0E-0x0F: Static memory base = 0x0100 (256)
    lv_hex = lv_hex && '0100'.

    " 0x10: Flags 2 = 0
    lv_hex = lv_hex && '00'.

    " 0x11-0x17: Reserved (7 bytes)
    lv_hex = lv_hex && '00000000000000'.

    " 0x18-0x19: Abbreviations table = 0x0000
    lv_hex = lv_hex && '0000'.

    " 0x1A-0x1B: File length / 2 = 0x0100
    lv_hex = lv_hex && '0100'.

    " 0x1C-0x1D: Checksum = 0x0000
    lv_hex = lv_hex && '0000'.

    " 0x1E-0x3F: Rest of header (34 bytes) - zeros
    lv_hex = lv_hex && '0000000000000000'.  " 8 bytes
    lv_hex = lv_hex && '0000000000000000'.  " 8 bytes
    lv_hex = lv_hex && '0000000000000000'.  " 8 bytes
    lv_hex = lv_hex && '0000000000000000'.  " 8 bytes
    lv_hex = lv_hex && '0000'.              " 2 bytes

    " 0x40-0xFF: Globals area (192 bytes = 96 words) - initialized to zeros
    DATA(lv_i) = 0.
    WHILE lv_i < 96.
      lv_hex = lv_hex && '0000'.
      lv_i = lv_i + 1.
    ENDWHILE.

    " 0x100+: Code area - will be written by individual tests
    " Add 256 bytes of zeros as initial code area
    lv_i = 0.
    WHILE lv_i < 256.
      lv_hex = lv_hex && '00'.
      lv_i = lv_i + 1.
    ENDWHILE.

    rv_story = lv_hex.
  ENDMETHOD.


  METHOD test_init.
    " Test executor initialization
    cl_abap_unit_assert=>assert_equals(
      act = mo_executor->get_pc( )
      exp = 256  " 0x0100
      msg = 'Initial PC should be 0x0100' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_executor->get_status( )
      exp = zcl_zork_00_executor=>c_status_running
      msg = 'Initial status should be RUNNING' ).
  ENDMETHOD.


  METHOD test_arithmetic_add.
    " Test 2OP:20 add
    " VAR form: E0 14 = call to var opcode 20 (add)
    " But simpler: use long form add
    " Actually, let's directly test using the stack and decode

    " Write test code at PC (0x100):
    " Long form 2OP add: 0x14 (opcode 20) with operand types in bits 6-5
    " Using variable form: 0xD4 = 11010100 = var, 2OP, opcode 20
    " Operand types: 01 01 11 11 = small small omit omit
    " Then store variable

    " Simpler approach: Set up globals and use ADD instruction
    " Global 0 (var 16) at addr 0x40
    mo_memory->write_word( iv_addr = 64 iv_val = 100 ).   " G0 = 100
    mo_memory->write_word( iv_addr = 66 iv_val = 50 ).    " G1 = 50

    " Write: ADD G0, G1 -> G2
    " Variable form add: D4 = 11010100 (var, 2op, op20)
    " Types: 10 10 11 11 = var var omit omit = 0xAF
    " Operands: 10 (G0), 11 (G1)
    " Store: 12 (G2)
    mo_memory->write_byte( iv_addr = 256 iv_val = 212 ).  " D4 = var add
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = types
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).   " -> G2
    mo_memory->write_byte( iv_addr = 261 iv_val = 186 ).  " BA = quit

    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).

    " Check result in G2 (addr 68)
    DATA(lv_result) = mo_memory->read_word( 68 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 150
      msg = '100 + 50 should = 150' ).
  ENDMETHOD.


  METHOD test_arithmetic_sub.
    " Test 2OP:21 sub
    mo_memory->write_word( iv_addr = 64 iv_val = 200 ).   " G0 = 200
    mo_memory->write_word( iv_addr = 66 iv_val = 75 ).    " G1 = 75

    " Write: SUB G0, G1 -> G2
    " Variable form sub: D5 = 11010101 (var, 2op, op21)
    mo_memory->write_byte( iv_addr = 256 iv_val = 213 ).  " D5 = var sub
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = types
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).   " -> G2
    mo_memory->write_byte( iv_addr = 261 iv_val = 186 ).  " BA = quit

    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).

    DATA(lv_result) = mo_memory->read_word( 68 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 125
      msg = '200 - 75 should = 125' ).
  ENDMETHOD.


  METHOD test_arithmetic_mul.
    " Test 2OP:22 mul
    mo_memory->write_word( iv_addr = 64 iv_val = 12 ).    " G0 = 12
    mo_memory->write_word( iv_addr = 66 iv_val = 7 ).     " G1 = 7

    " Variable form mul: D6 = 11010110 (var, 2op, op22)
    mo_memory->write_byte( iv_addr = 256 iv_val = 214 ).  " D6 = var mul
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = types
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).   " -> G2
    mo_memory->write_byte( iv_addr = 261 iv_val = 186 ).  " BA = quit

    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).

    DATA(lv_result) = mo_memory->read_word( 68 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 84
      msg = '12 * 7 should = 84' ).
  ENDMETHOD.


  METHOD test_arithmetic_div.
    " Test 2OP:23 div
    mo_memory->write_word( iv_addr = 64 iv_val = 100 ).   " G0 = 100
    mo_memory->write_word( iv_addr = 66 iv_val = 7 ).     " G1 = 7

    " Variable form div: D7 = 11010111 (var, 2op, op23)
    mo_memory->write_byte( iv_addr = 256 iv_val = 215 ).  " D7 = var div
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = types
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).   " -> G2
    mo_memory->write_byte( iv_addr = 261 iv_val = 186 ).  " BA = quit

    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).

    DATA(lv_result) = mo_memory->read_word( 68 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 14
      msg = '100 / 7 should = 14' ).
  ENDMETHOD.


  METHOD test_jump_equal.
    " Test 2OP:1 je (jump if equal)
    mo_memory->write_word( iv_addr = 64 iv_val = 42 ).    " G0 = 42
    mo_memory->write_word( iv_addr = 66 iv_val = 42 ).    " G1 = 42 (same)

    " Long form je: 01 = je with small operands
    " 0x41 = long form with var,small operands, opcode 1
    " Actually use: 0xC1 = var form, 2op, opcode 1
    mo_memory->write_byte( iv_addr = 256 iv_val = 193 ).  " C1 = var je
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = var var omit omit
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1
    mo_memory->write_byte( iv_addr = 260 iv_val = 197 ).  " Branch: C5 = on true, offset 5
    " If branch taken, PC goes to 260+5-2+1 = 264

    mo_executor->set_pc( 256 ).
    DATA(lv_next_pc) = mo_executor->execute_step( ).

    " Branch should be taken (values equal)
    cl_abap_unit_assert=>assert_equals(
      act = lv_next_pc
      exp = 264
      msg = 'JE should branch when values equal' ).
  ENDMETHOD.


  METHOD test_jump_zero.
    " Test 1OP:0 jz (jump if zero)
    mo_memory->write_word( iv_addr = 64 iv_val = 0 ).     " G0 = 0

    " Short form jz: 0xA0 = 10 10 0000 = short, var operand, opcode 0
    mo_memory->write_byte( iv_addr = 256 iv_val = 160 ).  " A0 = short jz with var
    mo_memory->write_byte( iv_addr = 257 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 258 iv_val = 197 ).  " Branch: C5 = on true, offset 5

    mo_executor->set_pc( 256 ).
    DATA(lv_next_pc) = mo_executor->execute_step( ).

    " Branch should be taken (value is zero)
    cl_abap_unit_assert=>assert_equals(
      act = lv_next_pc
      exp = 262
      msg = 'JZ should branch when value is zero' ).
  ENDMETHOD.


  METHOD test_jump_less.
    " Test 2OP:2 jl (jump if less)
    mo_memory->write_word( iv_addr = 64 iv_val = 10 ).    " G0 = 10
    mo_memory->write_word( iv_addr = 66 iv_val = 20 ).    " G1 = 20

    " Variable form jl: C2 = var, 2op, opcode 2
    mo_memory->write_byte( iv_addr = 256 iv_val = 194 ).  " C2 = var jl
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = var var omit omit
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1
    mo_memory->write_byte( iv_addr = 260 iv_val = 197 ).  " Branch: C5 = on true, offset 5

    mo_executor->set_pc( 256 ).
    DATA(lv_next_pc) = mo_executor->execute_step( ).

    " Branch should be taken (10 < 20)
    cl_abap_unit_assert=>assert_equals(
      act = lv_next_pc
      exp = 264
      msg = 'JL should branch when first < second' ).
  ENDMETHOD.


  METHOD test_jump_greater.
    " Test 2OP:3 jg (jump if greater)
    mo_memory->write_word( iv_addr = 64 iv_val = 30 ).    " G0 = 30
    mo_memory->write_word( iv_addr = 66 iv_val = 20 ).    " G1 = 20

    " Variable form jg: C3 = var, 2op, opcode 3
    mo_memory->write_byte( iv_addr = 256 iv_val = 195 ).  " C3 = var jg
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = var var omit omit
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1
    mo_memory->write_byte( iv_addr = 260 iv_val = 197 ).  " Branch: C5 = on true, offset 5

    mo_executor->set_pc( 256 ).
    DATA(lv_next_pc) = mo_executor->execute_step( ).

    " Branch should be taken (30 > 20)
    cl_abap_unit_assert=>assert_equals(
      act = lv_next_pc
      exp = 264
      msg = 'JG should branch when first > second' ).
  ENDMETHOD.


  METHOD test_bitwise_and.
    " Test 2OP:9 and
    mo_memory->write_word( iv_addr = 64 iv_val = 255 ).   " G0 = 0xFF
    mo_memory->write_word( iv_addr = 66 iv_val = 15 ).    " G1 = 0x0F

    " Variable form and: C9 = var, 2op, opcode 9
    mo_memory->write_byte( iv_addr = 256 iv_val = 201 ).  " C9 = var and
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = var var omit omit
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).   " -> G2

    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).

    DATA(lv_result) = mo_memory->read_word( 68 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 15  " 0xFF AND 0x0F = 0x0F
      msg = '0xFF AND 0x0F should = 0x0F' ).
  ENDMETHOD.


  METHOD test_bitwise_or.
    " Test 2OP:8 or
    mo_memory->write_word( iv_addr = 64 iv_val = 240 ).   " G0 = 0xF0
    mo_memory->write_word( iv_addr = 66 iv_val = 15 ).    " G1 = 0x0F

    " Variable form or: C8 = var, 2op, opcode 8
    mo_memory->write_byte( iv_addr = 256 iv_val = 200 ).  " C8 = var or
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = var var omit omit
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).   " -> G2

    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).

    DATA(lv_result) = mo_memory->read_word( 68 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 255  " 0xF0 OR 0x0F = 0xFF
      msg = '0xF0 OR 0x0F should = 0xFF' ).
  ENDMETHOD.


  METHOD test_inc_dec.
    " Test 1OP:5 inc and 1OP:6 dec
    mo_memory->write_word( iv_addr = 64 iv_val = 10 ).    " G0 = 10

    " Short form inc: 0xA5 = short, var operand, opcode 5
    mo_memory->write_byte( iv_addr = 256 iv_val = 165 ).  " A5 = inc with var
    mo_memory->write_byte( iv_addr = 257 iv_val = 16 ).   " variable 16 (G0)

    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).

    DATA(lv_result) = mo_memory->read_word( 64 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 11
      msg = 'INC G0 should make 10 become 11' ).

    " Now test dec
    mo_memory->write_byte( iv_addr = 258 iv_val = 166 ).  " A6 = dec with var
    mo_memory->write_byte( iv_addr = 259 iv_val = 16 ).   " variable 16 (G0)

    mo_executor->execute_step( ).

    lv_result = mo_memory->read_word( 64 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 10
      msg = 'DEC G0 should make 11 become 10' ).
  ENDMETHOD.


  METHOD test_load_store.
    " Test 2OP:15 loadw and VAR:1 storew
    " First write a value to memory
    mo_memory->write_word( iv_addr = 300 iv_val = 12345 ).

    " Test loadw: load word from array
    " We'll load from address 300 (base 300, index 0)
    mo_memory->write_word( iv_addr = 64 iv_val = 300 ).   " G0 = 300 (base)
    mo_memory->write_word( iv_addr = 66 iv_val = 0 ).     " G1 = 0 (index)

    " Variable form loadw: CF = var, 2op, opcode 15
    mo_memory->write_byte( iv_addr = 256 iv_val = 207 ).  " CF = var loadw
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).  " AF = var var omit omit
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0 (base)
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).   " G1 (index)
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).   " -> G2

    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).

    DATA(lv_result) = mo_memory->read_word( 68 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 12345
      msg = 'LOADW should load 12345 from memory' ).
  ENDMETHOD.


  METHOD test_stack_push_pull.
    " Test VAR:8 push and VAR:9 pull
    mo_memory->write_word( iv_addr = 64 iv_val = 999 ).   " G0 = 999

    " Push G0 onto stack
    " Variable form push: E8 = var, var opcode 8
    mo_memory->write_byte( iv_addr = 256 iv_val = 232 ).  " E8 = push
    mo_memory->write_byte( iv_addr = 257 iv_val = 191 ).  " BF = var omit omit omit
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).   " G0

    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).

    " Stack should have value
    DATA(lv_stack_val) = mo_stack->peek_stack( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_stack_val
      exp = 999
      msg = 'Stack should have 999 after push' ).

    " Now pull to G1
    " Variable form pull: E9 = var, var opcode 9
    mo_memory->write_byte( iv_addr = 259 iv_val = 233 ).  " E9 = pull
    mo_memory->write_byte( iv_addr = 260 iv_val = 95 ).   " 5F = small omit omit omit
    mo_memory->write_byte( iv_addr = 261 iv_val = 17 ).   " G1

    mo_executor->execute_step( ).

    DATA(lv_result) = mo_memory->read_word( 66 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 999
      msg = 'G1 should have 999 after pull' ).
  ENDMETHOD.

ENDCLASS.
