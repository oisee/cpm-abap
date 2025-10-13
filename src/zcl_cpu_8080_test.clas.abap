*&---------------------------------------------------------------------*
*& Unit Test Class for Z80/i8080 CPU Emulator
*& Compatible with abaplint transpiler
*&---------------------------------------------------------------------*
CLASS zcl_cpu_8080_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    DATA: mo_cpu TYPE REF TO zcl_cpu_8080.

    METHODS:
      setup FOR TESTING,
      test_init FOR TESTING,
      test_memory FOR TESTING,
      test_nop FOR TESTING,
      test_ld_bc FOR TESTING,
      test_inc_bc FOR TESTING,
      test_halt FOR TESTING,
      test_jump FOR TESTING,
      test_call_ret FOR TESTING,
      test_program FOR TESTING.

ENDCLASS.


CLASS zcl_cpu_8080_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cpu.
  ENDMETHOD.


  METHOD test_init.
    " Test CPU initialization
    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_pc( )
      exp = 256
      msg = 'PC should start at 0x0100' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_af( )
      exp = 0
      msg = 'AF register should be zero' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_status( )
      exp = 0
      msg = 'CPU status should be RUNNING' ).
  ENDMETHOD.


  METHOD test_memory.
    " Test memory read/write operations
    mo_cpu->write_byte( iv_addr = 1000 iv_val = 'AB' ).

    DATA(lv_byte) = mo_cpu->read_memory( 1000 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_byte
      exp = 'AB'
      msg = 'Memory write/read should work' ).

    mo_cpu->write_word( iv_addr = 2000 iv_val = 12345 ).

    DATA(lv_word) = mo_cpu->read_word( 2000 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_word
      exp = 12345
      msg = 'Memory word operations should work (little-endian)' ).
  ENDMETHOD.


  METHOD test_nop.
    " Test NOP instruction
    mo_cpu->reset( ).
    mo_cpu->write_byte( iv_addr = 256 iv_val = '00' ).  " NOP
    mo_cpu->execute_instruction( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_pc( )
      exp = 257
      msg = 'NOP should increment PC' ).
  ENDMETHOD.


  METHOD test_ld_bc.
    " Test LD BC,nnnn instruction
    mo_cpu->reset( ).

    " Opcode: 01 34 12 (LD BC,0x1234)
    mo_cpu->write_byte( iv_addr = 256 iv_val = '01' ).
    mo_cpu->write_byte( iv_addr = 257 iv_val = '34' ).  " Low byte
    mo_cpu->write_byte( iv_addr = 258 iv_val = '12' ).  " High byte

    mo_cpu->execute_instruction( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_bc( )
      exp = 4660  " 0x1234
      msg = 'LD BC,0x1234 should load BC correctly' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_pc( )
      exp = 259
      msg = 'PC should advance by 3' ).
  ENDMETHOD.


  METHOD test_inc_bc.
    " Test INC BC instruction
    mo_cpu->reset( ).
    mo_cpu->mv_bc = 1000.

    mo_cpu->write_byte( iv_addr = 256 iv_val = '03' ).  " INC BC
    mo_cpu->execute_instruction( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_bc( )
      exp = 1001
      msg = 'INC BC should increment BC' ).
  ENDMETHOD.


  METHOD test_halt.
    " Test HALT instruction
    mo_cpu->reset( ).
    mo_cpu->write_byte( iv_addr = 256 iv_val = '76' ).  " HALT

    mo_cpu->execute_instruction( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_status( )
      exp = 1
      msg = 'HALT should set status to HALTED' ).
  ENDMETHOD.


  METHOD test_jump.
    " Test JP nnnn (unconditional jump)
    mo_cpu->reset( ).

    " JP 0x2000
    mo_cpu->write_byte( iv_addr = 256 iv_val = 'C3' ).
    mo_cpu->write_byte( iv_addr = 257 iv_val = '00' ).  " Low byte
    mo_cpu->write_byte( iv_addr = 258 iv_val = '20' ).  " High byte

    mo_cpu->execute_instruction( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_pc( )
      exp = 8192  " 0x2000
      msg = 'JP 0x2000 should jump to address' ).
  ENDMETHOD.


  METHOD test_call_ret.
    " Test CALL and RET instructions
    mo_cpu->reset( ).
    mo_cpu->mv_sp = 65535.  " Top of stack

    " CALL 0x3000
    mo_cpu->write_byte( iv_addr = 256 iv_val = 'CD' ).
    mo_cpu->write_byte( iv_addr = 257 iv_val = '00' ).
    mo_cpu->write_byte( iv_addr = 258 iv_val = '30' ).

    mo_cpu->execute_instruction( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_pc( )
      exp = 12288  " 0x3000
      msg = 'CALL should jump to target' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_sp( )
      exp = 65533
      msg = 'CALL should push return address on stack' ).

    " RET
    mo_cpu->write_byte( iv_addr = 12288 iv_val = 'C9' ).
    mo_cpu->execute_instruction( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_pc( )
      exp = 259
      msg = 'RET should return to caller' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_sp( )
      exp = 65535
      msg = 'RET should pop return address' ).
  ENDMETHOD.


  METHOD test_program.
    " Test multi-instruction program
    mo_cpu->reset( ).

    " Program:
    " LD BC,0x1234   ; 01 34 12
    " INC BC         ; 03
    " LD HL,0x5678   ; 21 78 56
    " HALT           ; 76

    mo_cpu->write_byte( iv_addr = 256 iv_val = '01' ).
    mo_cpu->write_byte( iv_addr = 257 iv_val = '34' ).
    mo_cpu->write_byte( iv_addr = 258 iv_val = '12' ).
    mo_cpu->write_byte( iv_addr = 259 iv_val = '03' ).
    mo_cpu->write_byte( iv_addr = 260 iv_val = '21' ).
    mo_cpu->write_byte( iv_addr = 261 iv_val = '78' ).
    mo_cpu->write_byte( iv_addr = 262 iv_val = '56' ).
    mo_cpu->write_byte( iv_addr = 263 iv_val = '76' ).

    " Execute program
    DATA(lv_instructions) = mo_cpu->execute_until_halt( iv_max_instructions = 100 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_instructions
      exp = 4
      msg = 'Program should execute 4 instructions' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_bc( )
      exp = 4661  " 0x1235 (0x1234 + 1)
      msg = 'BC should be incremented' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_hl( )
      exp = 22136  " 0x5678
      msg = 'HL should be loaded correctly' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_status( )
      exp = 1
      msg = 'Program should halt' ).
  ENDMETHOD.

ENDCLASS.
