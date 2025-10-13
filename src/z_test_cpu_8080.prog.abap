*&---------------------------------------------------------------------*
*& Test Program for Z80/i8080 CPU Emulator
*&---------------------------------------------------------------------*
REPORT z_test_cpu_8080.

" Include the CPU class
INCLUDE zcl_cpu_8080.

DATA: go_cpu TYPE REF TO zcl_cpu_8080.

" Test counter
DATA: gv_tests_passed TYPE i,
      gv_tests_failed TYPE i.

*&---------------------------------------------------------------------*
*& Test Helper Macros
*&---------------------------------------------------------------------*
DEFINE assert_equals.
  IF &1 = &2.
    gv_tests_passed = gv_tests_passed + 1.
    WRITE: / '✓', &3.
  ELSE.
    gv_tests_failed = gv_tests_failed + 1.
    WRITE: / '✗', &3.
    WRITE: / '  Expected:', &2, 'Got:', &1.
  ENDIF.
END-OF-DEFINITION.

DEFINE test_section.
  WRITE: / space.
  WRITE: / '======', &1, '======'.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& Main Test Suite
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  " Create CPU instance
  CREATE OBJECT go_cpu.

  WRITE: / '==========================================='.
  WRITE: / 'Z80/i8080 CPU Emulator - Test Suite'.
  WRITE: / '==========================================='.

  " ===== Test 1: Basic Initialization =====
  test_section 'Test 1: CPU Initialization'.

  assert_equals go_cpu->get_pc( ) 256 'PC starts at 0x0100'.
  assert_equals go_cpu->get_af( ) 0   'AF register is zero'.
  assert_equals go_cpu->get_bc( ) 0   'BC register is zero'.
  assert_equals go_cpu->get_status( ) 0 'CPU status is RUNNING'.


  " ===== Test 2: Memory Operations =====
  test_section 'Test 2: Memory Read/Write'.

  go_cpu->write_byte( iv_addr = 1000 iv_val = 'AB' ).
  DATA(lv_byte) = go_cpu->read_memory( 1000 ).
  assert_equals lv_byte 'AB' 'Memory write/read byte'.

  go_cpu->write_word( iv_addr = 2000 iv_val = 12345 ).
  DATA(lv_word) = go_cpu->read_word( 2000 ).
  assert_equals lv_word 12345 'Memory write/read word (little-endian)'.


  " ===== Test 3: NOP Instruction =====
  test_section 'Test 3: NOP (0x00)'.

  go_cpu->reset( ).
  go_cpu->write_byte( iv_addr = 256 iv_val = '00' ).  " NOP
  go_cpu->execute_instruction( ).

  assert_equals go_cpu->get_pc( ) 257 'NOP increments PC'.


  " ===== Test 4: LD BC,nnnn =====
  test_section 'Test 4: LD BC,nnnn (0x01)'.

  go_cpu->reset( ).
  " Opcode: 01 34 12 (LD BC,0x1234)
  go_cpu->write_byte( iv_addr = 256 iv_val = '01' ).
  go_cpu->write_byte( iv_addr = 257 iv_val = '34' ).  " Low byte
  go_cpu->write_byte( iv_addr = 258 iv_val = '12' ).  " High byte

  go_cpu->execute_instruction( ).

  assert_equals go_cpu->get_bc( ) 4660 'LD BC,0x1234 loads BC'.  " 0x1234 = 4660
  assert_equals go_cpu->get_pc( ) 259 'PC advances by 3'.


  " ===== Test 5: LD (BC),A =====
  test_section 'Test 5: LD (BC),A (0x02)'.

  go_cpu->reset( ).
  " Set A = 0x42, BC = 0x2000
  go_cpu->mv_af = go_cpu->set_high_byte( iv_pair = 0 iv_val = 66 ).  " 0x42 = 66
  go_cpu->mv_bc = 8192.  " 0x2000

  go_cpu->write_byte( iv_addr = 256 iv_val = '02' ).  " LD (BC),A
  go_cpu->execute_instruction( ).

  lv_byte = go_cpu->read_memory( 8192 ).
  assert_equals lv_byte '42' 'LD (BC),A stores A to memory'.


  " ===== Test 6: INC BC =====
  test_section 'Test 6: INC BC (0x03)'.

  go_cpu->reset( ).
  go_cpu->mv_bc = 1000.

  go_cpu->write_byte( iv_addr = 256 iv_val = '03' ).  " INC BC
  go_cpu->execute_instruction( ).

  assert_equals go_cpu->get_bc( ) 1001 'INC BC increments'.


  " ===== Test 7: INC B (with flags) =====
  test_section 'Test 7: INC B with flags (0x04)'.

  go_cpu->reset( ).
  go_cpu->mv_bc = 15.  " B=0, C=15

  go_cpu->write_byte( iv_addr = 256 iv_val = '04' ).  " INC B
  go_cpu->execute_instruction( ).

  assert_equals go_cpu->get_bc( ) 271 'INC B: B becomes 1'.  " 0x0100 + 15 = 271


  " ===== Test 8: DEC B (with flags) =====
  test_section 'Test 8: DEC B with flags (0x05)'.

  go_cpu->reset( ).
  go_cpu->mv_bc = 256.  " B=1, C=0

  go_cpu->write_byte( iv_addr = 256 iv_val = '05' ).  " DEC B
  go_cpu->execute_instruction( ).

  assert_equals go_cpu->get_bc( ) 0 'DEC B: B becomes 0'.


  " ===== Test 9: LD B,nn =====
  test_section 'Test 9: LD B,nn (0x06)'.

  go_cpu->reset( ).
  go_cpu->write_byte( iv_addr = 256 iv_val = '06' ).  " LD B,nn
  go_cpu->write_byte( iv_addr = 257 iv_val = '7F' ).  " 127

  go_cpu->execute_instruction( ).

  DATA(lv_b) = go_cpu->get_high_byte( go_cpu->get_bc( ) ).
  assert_equals lv_b 127 'LD B,0x7F loads B'.


  " ===== Test 10: HALT =====
  test_section 'Test 10: HALT (0x76)'.

  go_cpu->reset( ).
  go_cpu->write_byte( iv_addr = 256 iv_val = '76' ).  " HALT

  go_cpu->execute_instruction( ).

  assert_equals go_cpu->get_status( ) 1 'HALT sets status to HALTED'.


  " ===== Test 11: JP nnnn (Jump) =====
  test_section 'Test 11: JP nnnn (0xC3)'.

  go_cpu->reset( ).
  " JP 0x2000
  go_cpu->write_byte( iv_addr = 256 iv_val = 'C3' ).
  go_cpu->write_byte( iv_addr = 257 iv_val = '00' ).  " Low byte
  go_cpu->write_byte( iv_addr = 258 iv_val = '20' ).  " High byte

  go_cpu->execute_instruction( ).

  assert_equals go_cpu->get_pc( ) 8192 'JP 0x2000 jumps to address'.


  " ===== Test 12: CALL and RET =====
  test_section 'Test 12: CALL/RET (0xCD/0xC9)'.

  go_cpu->reset( ).
  go_cpu->mv_sp = 65535.  " Top of stack

  " CALL 0x3000
  go_cpu->write_byte( iv_addr = 256 iv_val = 'CD' ).
  go_cpu->write_byte( iv_addr = 257 iv_val = '00' ).
  go_cpu->write_byte( iv_addr = 258 iv_val = '30' ).

  go_cpu->execute_instruction( ).

  assert_equals go_cpu->get_pc( ) 12288 'CALL jumps to 0x3000'.  " 0x3000 = 12288
  assert_equals go_cpu->get_sp( ) 65533 'CALL pushes return address on stack'.

  " RET
  go_cpu->write_byte( iv_addr = 12288 iv_val = 'C9' ).
  go_cpu->execute_instruction( ).

  assert_equals go_cpu->get_pc( ) 259 'RET returns to caller'.
  assert_equals go_cpu->get_sp( ) 65535 'RET pops return address'.


  " ===== Test 13: Simple Program =====
  test_section 'Test 13: Simple Multi-Instruction Program'.

  go_cpu->reset( ).

  " Program:
  " LD BC,0x1234   ; 01 34 12
  " INC BC         ; 03
  " LD HL,0x5678   ; 21 78 56
  " HALT           ; 76

  go_cpu->write_byte( iv_addr = 256 iv_val = '01' ).
  go_cpu->write_byte( iv_addr = 257 iv_val = '34' ).
  go_cpu->write_byte( iv_addr = 258 iv_val = '12' ).
  go_cpu->write_byte( iv_addr = 259 iv_val = '03' ).
  go_cpu->write_byte( iv_addr = 260 iv_val = '21' ).
  go_cpu->write_byte( iv_addr = 261 iv_val = '78' ).
  go_cpu->write_byte( iv_addr = 262 iv_val = '56' ).
  go_cpu->write_byte( iv_addr = 263 iv_val = '76' ).

  " Execute program
  DATA(lv_instructions) = go_cpu->execute_until_halt( iv_max_instructions = 100 ).

  assert_equals lv_instructions 4 'Program executes 4 instructions'.
  assert_equals go_cpu->get_bc( ) 4661 'BC = 0x1235'.  " 0x1234 + 1
  assert_equals go_cpu->get_hl( ) 22136 'HL = 0x5678'.  " 0x5678 = 22136
  assert_equals go_cpu->get_status( ) 1 'Program halts'.


  " ===== Summary =====
  WRITE: / space.
  WRITE: / '==========================================='.
  WRITE: / 'Test Summary:'.
  WRITE: / '  Passed:', gv_tests_passed.
  WRITE: / '  Failed:', gv_tests_failed.

  IF gv_tests_failed = 0.
    WRITE: / '✓ All tests passed!'.
  ELSE.
    WRITE: / '✗ Some tests failed!'.
  ENDIF.

  WRITE: / '==========================================='.
