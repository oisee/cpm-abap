*&---------------------------------------------------------------------*
*& Z-Machine Decoder Tests
*&---------------------------------------------------------------------*
CLASS ltcl_decoder_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_memory  TYPE REF TO zcl_zork_00_memory,
          mo_decoder TYPE REF TO zcl_zork_00_decoder.

    METHODS:
      setup,

      " Short form tests
      test_short_1op_small FOR TESTING,
      test_short_1op_large FOR TESTING,
      test_short_1op_var FOR TESTING,
      test_short_0op FOR TESTING,

      " Long form tests
      test_long_2op FOR TESTING,

      " Variable form tests
      test_var_call FOR TESTING,
      test_var_storew FOR TESTING,

      " Branch tests
      test_branch_short FOR TESTING,
      test_branch_long FOR TESTING,

      " Store tests
      test_store_add FOR TESTING,

      " Helper to create test memory with code
      create_test_memory
        IMPORTING iv_code       TYPE string
        RETURNING VALUE(ro_mem) TYPE REF TO zcl_zork_00_memory.

ENDCLASS.


CLASS ltcl_decoder_test IMPLEMENTATION.

  METHOD setup.
    " Setup is done per-test via create_test_memory
  ENDMETHOD.


  METHOD create_test_memory.
    " Create memory with minimal header + code at PC=$40
    DATA: lv_story TYPE string.

    " Header (64 bytes = 128 hex chars)
    " Version 3, static_mem at $80 (128) so we have dynamic memory for code
    lv_story = ''.
    lv_story = lv_story && '03'.        " $00: Version 3
    lv_story = lv_story && '00'.        " $01: Flags
    lv_story = lv_story && '0001'.      " $02-03: Release
    lv_story = lv_story && '0100'.      " $04-05: High memory = $0100
    lv_story = lv_story && '0040'.      " $06-07: Init PC = $0040
    lv_story = lv_story && '0200'.      " $08-09: Dictionary
    lv_story = lv_story && '0100'.      " $0A-0B: Objects
    lv_story = lv_story && '00C0'.      " $0C-0D: Globals = $00C0
    lv_story = lv_story && '0080'.      " $0E-0F: Static memory = $0080 (128)
    lv_story = lv_story && '00'.        " $10: Flags2
    lv_story = lv_story && '00000000000000'.  " $11-17
    lv_story = lv_story && '0060'.      " $18-19: Abbreviations
    lv_story = lv_story && '0100'.      " $1A-1B: File length/2
    lv_story = lv_story && '0000'.      " $1C-1D: Checksum
    lv_story = lv_story && '0000'.      " $1E-1F: Interpreter

    " Pad header to 64 bytes (need 32 more bytes = 64 hex chars)
    lv_story = lv_story && '00000000000000000000000000000000'.
    lv_story = lv_story && '00000000000000000000000000000000'.

    " Now at $40, add the test code
    lv_story = lv_story && iv_code.

    " Pad to 256 bytes total for valid story
    WHILE strlen( lv_story ) < 512.
      lv_story = lv_story && '00'.
    ENDWHILE.

    CREATE OBJECT ro_mem.
    ro_mem->load_story( lv_story ).
  ENDMETHOD.


  METHOD test_short_1op_small.
    " Test short form with 1 small constant operand
    " Example: jz 42 (opcode $A0 + operand type 01 in bits 5-4)
    " $A0 = 10 100 000 = short form, type 01 (small), opcode 0 (jz)
    " Byte: $90 = 10 01 0000 = short, small const, opcode 0
    " Actually: jz with small constant = $80 | (01 << 4) | 0 = $90

    " jz with small operand: form=short, optype=01, opcode=0
    " Encoding: 10 01 0000 = $90, then operand byte, then branch
    " Let's use: $90 $2A $C0 (jz 42, branch true +0 = return true)
    mo_memory = create_test_memory( '902AC0' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-form
      exp = zif_zork_00_types=>c_form_short
      msg = 'Form should be short' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-op_count
      exp = zif_zork_00_types=>c_opcount_1op
      msg = 'Op count should be 1OP' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-opcode
      exp = 0
      msg = 'Opcode should be 0 (jz)' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-operand_cnt
      exp = 1
      msg = 'Should have 1 operand' ).

    DATA(lv_op) = mo_decoder->get_operand( is_instr = ls_instr iv_index = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_op
      exp = 42
      msg = 'Operand should be 42' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-has_branch
      exp = abap_true
      msg = 'jz should have branch' ).
  ENDMETHOD.


  METHOD test_short_1op_large.
    " Test short form with 1 large constant operand
    " inc with large constant: 10 00 0101 = $85, then 2-byte operand
    " $85 $01 $00 = inc with large constant $0100 (256)
    " Actually inc is opcode 5 (1OP), but it doesn't take large const...
    " Let's use print_addr (1OP:7) which takes a large constant
    " 10 00 0111 = $87, operand $1234
    mo_memory = create_test_memory( '871234' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-opcode
      exp = 7
      msg = 'Opcode should be 7 (print_addr)' ).

    DATA(lv_op) = mo_decoder->get_operand( is_instr = ls_instr iv_index = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_op
      exp = 4660  " 0x1234
      msg = 'Operand should be 0x1234' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_next
      exp = 67
      msg = 'Next PC should be 67 (64+3)' ).
  ENDMETHOD.


  METHOD test_short_1op_var.
    " Test short form with variable operand
    " ret with variable: 10 10 1011 = $AB, then variable number
    " $AB $10 = ret with global variable 16
    mo_memory = create_test_memory( 'AB10' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-opcode
      exp = 11
      msg = 'Opcode should be 11 (ret)' ).

    DATA(lv_type) = mo_decoder->get_operand_type( is_instr = ls_instr iv_index = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = zif_zork_00_types=>c_optype_var
      msg = 'Operand type should be variable' ).

    DATA(lv_op) = mo_decoder->get_operand( is_instr = ls_instr iv_index = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_op
      exp = 16
      msg = 'Variable number should be 16' ).
  ENDMETHOD.


  METHOD test_short_0op.
    " Test short form with 0 operands (0OP)
    " rtrue: 10 11 0000 = $B0
    mo_memory = create_test_memory( 'B0' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-form
      exp = zif_zork_00_types=>c_form_short
      msg = 'Form should be short' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-op_count
      exp = zif_zork_00_types=>c_opcount_0op
      msg = 'Op count should be 0OP' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-opcode
      exp = 0
      msg = 'Opcode should be 0 (rtrue)' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-operand_cnt
      exp = 0
      msg = 'Should have 0 operands' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_next
      exp = 65
      msg = 'Next PC should be 65' ).
  ENDMETHOD.


  METHOD test_long_2op.
    " Test long form with 2 operands
    " add with two small constants: 00 00 0100 = $14
    " But add is 2OP:20 which is $14 in long form!
    " Long: bit 6 = type1, bit 5 = type2 (0=small, 1=var)
    " $14 = 0001 0100 = long form, both small, opcode 20 (add)
    " $14 $05 $03 $00 = add 5 + 3, store to var 0
    mo_memory = create_test_memory( '14050300' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-form
      exp = zif_zork_00_types=>c_form_long
      msg = 'Form should be long' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-op_count
      exp = zif_zork_00_types=>c_opcount_2op
      msg = 'Op count should be 2OP' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-opcode
      exp = 20
      msg = 'Opcode should be 20 (add)' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-operand_cnt
      exp = 2
      msg = 'Should have 2 operands' ).

    DATA(lv_op1) = mo_decoder->get_operand( is_instr = ls_instr iv_index = 0 ).
    DATA(lv_op2) = mo_decoder->get_operand( is_instr = ls_instr iv_index = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_op1
      exp = 5
      msg = 'First operand should be 5' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_op2
      exp = 3
      msg = 'Second operand should be 3' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-has_store
      exp = abap_true
      msg = 'add should have store' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-store_var
      exp = 0
      msg = 'Store variable should be 0 (stack)' ).
  ENDMETHOD.


  METHOD test_var_call.
    " Test variable form call instruction
    " call: 11 0 00000 = $E0, then types byte, then operands, then store
    " $E0 $3F $1234 $00 = call packed_addr $1234, store to var 0
    " Types $3F = 00 11 11 11 = large, omit, omit, omit
    mo_memory = create_test_memory( 'E03F123400' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-form
      exp = zif_zork_00_types=>c_form_variable
      msg = 'Form should be variable' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-op_count
      exp = zif_zork_00_types=>c_opcount_var
      msg = 'Op count should be VAR' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-opcode
      exp = 0
      msg = 'Opcode should be 0 (call)' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-operand_cnt
      exp = 1
      msg = 'Should have 1 operand' ).

    DATA(lv_op) = mo_decoder->get_operand( is_instr = ls_instr iv_index = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_op
      exp = 4660  " 0x1234
      msg = 'Operand should be 0x1234' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-has_store
      exp = abap_true
      msg = 'call should have store' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-store_var
      exp = 0
      msg = 'Store to stack (var 0)' ).
  ENDMETHOD.


  METHOD test_var_storew.
    " Test variable form storew instruction
    " storew: 11 0 00001 = $E1, then types, then operands
    " storew array index value
    " $E1 $15 $1000 $05 $00 $FF = storew $1000, 5, var0(255)
    " Types $15 = 00 01 01 01 = large, small, small, omit
    " Wait, storew needs exactly 3 operands: array, word-index, value
    " Types: 00 01 01 11 = $17 (large, small, small, omit)
    mo_memory = create_test_memory( 'E117100005FF' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-opcode
      exp = 1
      msg = 'Opcode should be 1 (storew)' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-operand_cnt
      exp = 3
      msg = 'Should have 3 operands' ).

    DATA(lv_op1) = mo_decoder->get_operand( is_instr = ls_instr iv_index = 0 ).
    DATA(lv_op2) = mo_decoder->get_operand( is_instr = ls_instr iv_index = 1 ).
    DATA(lv_op3) = mo_decoder->get_operand( is_instr = ls_instr iv_index = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_op1
      exp = 4096  " $1000
      msg = 'First operand (array) should be $1000' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_op2
      exp = 5
      msg = 'Second operand (index) should be 5' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_op3
      exp = 255
      msg = 'Third operand (value) should be 255' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-has_store
      exp = abap_false
      msg = 'storew should not have store' ).
  ENDMETHOD.


  METHOD test_branch_short.
    " Test short branch (1 byte)
    " jz with short branch: $90 $00 $C0
    " $C0 = 11 000000 = branch on true, short offset 0 (return true)
    mo_memory = create_test_memory( '9000C0' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-has_branch
      exp = abap_true
      msg = 'Should have branch' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-branch_on
      exp = abap_true
      msg = 'Should branch on true' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-branch_off
      exp = 0
      msg = 'Branch offset should be 0 (return true)' ).
  ENDMETHOD.


  METHOD test_branch_long.
    " Test long branch (2 bytes)
    " jz with long branch: $90 $00 $00 $10
    " $00 $10 = 0 0 000000 00010000 = branch on false, offset 16
    mo_memory = create_test_memory( '90000010' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-branch_on
      exp = abap_false
      msg = 'Should branch on false' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-branch_off
      exp = 16
      msg = 'Branch offset should be 16' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_next
      exp = 68
      msg = 'Next PC should be 68 (64+4)' ).
  ENDMETHOD.


  METHOD test_store_add.
    " Test that add instruction correctly identifies store
    " add: long form $14, operands, store byte
    " $14 $0A $05 $10 = add 10, 5 -> store to var 16
    mo_memory = create_test_memory( '140A0510' ).
    CREATE OBJECT mo_decoder EXPORTING io_memory = mo_memory.

    DATA(ls_instr) = mo_decoder->decode(
      EXPORTING iv_pc = 64
      IMPORTING ev_next_pc = DATA(lv_next) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-has_store
      exp = abap_true
      msg = 'add should have store' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_instr-store_var
      exp = 16
      msg = 'Store to global var 16' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_next
      exp = 68
      msg = 'Next PC should be 68' ).
  ENDMETHOD.

ENDCLASS.
