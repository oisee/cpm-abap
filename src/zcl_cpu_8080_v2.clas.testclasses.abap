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
    DATA: mo_cpu TYPE REF TO zcl_cpu_8080_v2.

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
      test_program FOR TESTING,
      test_ld_r_r FOR TESTING,
      test_alu_add FOR TESTING,
      test_alu_sub FOR TESTING,
      test_alu_and FOR TESTING,
      test_alu_or FOR TESTING,
      test_alu_xor FOR TESTING.

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
    mo_cpu->write_byte( iv_addr = 1000 iv_val = 171 ).  " 0xAB = 171

    DATA(lv_byte) = mo_cpu->read_memory( 1000 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_byte
      exp = 171  " 0xAB
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
    mo_cpu->write_byte( iv_addr = 256 iv_val = 0 ).  " NOP
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
    mo_cpu->write_byte( iv_addr = 256 iv_val = 1 ).   " 0x01
    mo_cpu->write_byte( iv_addr = 257 iv_val = 52 ).  " 0x34
    mo_cpu->write_byte( iv_addr = 258 iv_val = 18 ).  " 0x12

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
    " Cannot set mv_bc directly in v2, use write/read instead
    " Write program: LD BC,1000; INC BC
    mo_cpu->write_byte( iv_addr = 256 iv_val = 1 ).  " LD BC,nnnn
    mo_cpu->write_byte( iv_addr = 257 iv_val = 232 ).  " Low byte of 1000
    mo_cpu->write_byte( iv_addr = 258 iv_val = 3 ).   " High byte of 1000
    mo_cpu->write_byte( iv_addr = 259 iv_val = 3 ).  " INC BC

    mo_cpu->execute_instruction( ).  " LD BC,1000
    mo_cpu->execute_instruction( ).  " INC BC

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_bc( )
      exp = 1001
      msg = 'INC BC should increment BC' ).
  ENDMETHOD.


  METHOD test_halt.
    " Test HALT instruction
    mo_cpu->reset( ).
    mo_cpu->write_byte( iv_addr = 256 iv_val = 118 ).  " 0x76 = HALT

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
    mo_cpu->write_byte( iv_addr = 256 iv_val = 195 ).  " 0xC3 = JP
    mo_cpu->write_byte( iv_addr = 257 iv_val = 0 ).   " Low byte
    mo_cpu->write_byte( iv_addr = 258 iv_val = 32 ).  " 0x20 = High byte

    mo_cpu->execute_instruction( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_pc( )
      exp = 8192  " 0x2000
      msg = 'JP 0x2000 should jump to address' ).
  ENDMETHOD.


  METHOD test_call_ret.
    " Test CALL and RET instructions
    mo_cpu->reset( ).
    " Set SP using LD SP,nnnn first
    mo_cpu->write_byte( iv_addr = 256 iv_val = 49 ).   " 0x31 = LD SP,nnnn
    mo_cpu->write_byte( iv_addr = 257 iv_val = 255 ).  " Low byte of 65535
    mo_cpu->write_byte( iv_addr = 258 iv_val = 255 ).  " High byte of 65535
    mo_cpu->write_byte( iv_addr = 259 iv_val = 205 ).  " 0xCD = CALL
    mo_cpu->write_byte( iv_addr = 260 iv_val = 0 ).    " Low byte of 0x3000
    mo_cpu->write_byte( iv_addr = 261 iv_val = 48 ).   " 0x30 = High byte

    mo_cpu->execute_instruction( ).  " LD SP,65535

    mo_cpu->execute_instruction( ).  " CALL 0x3000

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_pc( )
      exp = 12288  " 0x3000
      msg = 'CALL should jump to target' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_sp( )
      exp = 65533
      msg = 'CALL should push return address on stack' ).

    " RET
    mo_cpu->write_byte( iv_addr = 12288 iv_val = 201 ).  " 0xC9 = RET
    mo_cpu->execute_instruction( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cpu->get_pc( )
      exp = 262  " Return address after CALL
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

    mo_cpu->write_byte( iv_addr = 256 iv_val = 1 ).
    mo_cpu->write_byte( iv_addr = 257 iv_val = 52 ).   " 0x34
    mo_cpu->write_byte( iv_addr = 258 iv_val = 18 ).   " 0x12
    mo_cpu->write_byte( iv_addr = 259 iv_val = 3 ).
    mo_cpu->write_byte( iv_addr = 260 iv_val = 33 ).   " 0x21
    mo_cpu->write_byte( iv_addr = 261 iv_val = 120 ).  " 0x78
    mo_cpu->write_byte( iv_addr = 262 iv_val = 86 ).   " 0x56
    mo_cpu->write_byte( iv_addr = 263 iv_val = 118 ).  " 0x76

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


  METHOD test_ld_r_r.
    " Test LD r,r (MOV r,r) family - register to register moves
    mo_cpu->reset( ).

    " Program:
    " LD BC,0x1234  ; 01 34 12
    " LD A,B        ; 78 (opcode 120 = LD A,B)
    " LD C,A        ; 4F (opcode 79 = LD C,A)
    " LD D,C        ; 51 (opcode 81 = LD D,C)
    " HALT          ; 76

    mo_cpu->write_byte( iv_addr = 256 iv_val = 1 ).    " LD BC,0x1234
    mo_cpu->write_byte( iv_addr = 257 iv_val = 52 ).   " 0x34
    mo_cpu->write_byte( iv_addr = 258 iv_val = 18 ).   " 0x12
    mo_cpu->write_byte( iv_addr = 259 iv_val = 120 ).  " LD A,B (0x78)
    mo_cpu->write_byte( iv_addr = 260 iv_val = 79 ).   " LD C,A (0x4F)
    mo_cpu->write_byte( iv_addr = 261 iv_val = 81 ).   " LD D,C (0x51)
    mo_cpu->write_byte( iv_addr = 262 iv_val = 118 ).  " HALT

    mo_cpu->execute_instruction( ).  " LD BC,0x1234
    " BC = 0x1234, B = 0x12, C = 0x34

    mo_cpu->execute_instruction( ).  " LD A,B
    " A should now be 0x12
    DATA(lv_af) = mo_cpu->get_af( ).
    DATA(lv_a) = lv_af DIV 256.
    cl_abap_unit_assert=>assert_equals(
      act = lv_a
      exp = 18  " 0x12
      msg = 'LD A,B should copy B to A' ).

    mo_cpu->execute_instruction( ).  " LD C,A
    " C should now be 0x12
    DATA(lv_bc) = mo_cpu->get_bc( ).
    DATA(lv_c) = lv_bc MOD 256.
    cl_abap_unit_assert=>assert_equals(
      act = lv_c
      exp = 18  " 0x12
      msg = 'LD C,A should copy A to C' ).

    mo_cpu->execute_instruction( ).  " LD D,C
    " D should now be 0x12
    DATA(lv_de) = mo_cpu->get_de( ).
    DATA(lv_d) = lv_de DIV 256.
    cl_abap_unit_assert=>assert_equals(
      act = lv_d
      exp = 18  " 0x12
      msg = 'LD D,C should copy C to D' ).
  ENDMETHOD.


  METHOD test_alu_add.
    " Test ADD A,r instruction
    mo_cpu->reset( ).

    " Program:
    " LD A,10       ; 3E 0A
    " LD B,5        ; 06 05
    " ADD A,B       ; 80 (opcode 128)
    " HALT          ; 76

    mo_cpu->write_byte( iv_addr = 256 iv_val = 62 ).   " LD A,n (0x3E)
    mo_cpu->write_byte( iv_addr = 257 iv_val = 10 ).
    mo_cpu->write_byte( iv_addr = 258 iv_val = 6 ).    " LD B,n (0x06)
    mo_cpu->write_byte( iv_addr = 259 iv_val = 5 ).
    mo_cpu->write_byte( iv_addr = 260 iv_val = 128 ).  " ADD A,B (0x80)
    mo_cpu->write_byte( iv_addr = 261 iv_val = 118 ).  " HALT

    mo_cpu->execute_instruction( ).  " LD A,10
    mo_cpu->execute_instruction( ).  " LD B,5
    mo_cpu->execute_instruction( ).  " ADD A,B

    DATA(lv_af) = mo_cpu->get_af( ).
    DATA(lv_a) = lv_af DIV 256.

    cl_abap_unit_assert=>assert_equals(
      act = lv_a
      exp = 15  " 10 + 5
      msg = 'ADD A,B should compute 10+5=15' ).
  ENDMETHOD.


  METHOD test_alu_sub.
    " Test SUB A,r instruction
    mo_cpu->reset( ).

    " Program:
    " LD A,20       ; 3E 14
    " LD C,7        ; 0E 07
    " SUB A,C       ; 91 (opcode 145 = SUB A,C)
    " HALT          ; 76

    mo_cpu->write_byte( iv_addr = 256 iv_val = 62 ).   " LD A,n
    mo_cpu->write_byte( iv_addr = 257 iv_val = 20 ).
    mo_cpu->write_byte( iv_addr = 258 iv_val = 14 ).   " LD C,n (0x0E)
    mo_cpu->write_byte( iv_addr = 259 iv_val = 7 ).
    mo_cpu->write_byte( iv_addr = 260 iv_val = 145 ).  " SUB A,C (0x91)
    mo_cpu->write_byte( iv_addr = 261 iv_val = 118 ).  " HALT

    mo_cpu->execute_instruction( ).  " LD A,20
    mo_cpu->execute_instruction( ).  " LD C,7
    mo_cpu->execute_instruction( ).  " SUB A,C

    DATA(lv_af) = mo_cpu->get_af( ).
    DATA(lv_a) = lv_af DIV 256.

    cl_abap_unit_assert=>assert_equals(
      act = lv_a
      exp = 13  " 20 - 7
      msg = 'SUB A,C should compute 20-7=13' ).
  ENDMETHOD.


  METHOD test_alu_and.
    " Test AND A,r instruction
    mo_cpu->reset( ).

    " Program:
    " LD A,0xFF     ; 3E FF
    " LD D,0x0F     ; 16 0F
    " AND A,D       ; A2 (opcode 162 = AND A,D)
    " HALT          ; 76

    mo_cpu->write_byte( iv_addr = 256 iv_val = 62 ).   " LD A,n
    mo_cpu->write_byte( iv_addr = 257 iv_val = 255 ).
    mo_cpu->write_byte( iv_addr = 258 iv_val = 22 ).   " LD D,n (0x16)
    mo_cpu->write_byte( iv_addr = 259 iv_val = 15 ).
    mo_cpu->write_byte( iv_addr = 260 iv_val = 162 ).  " AND A,D (0xA2)
    mo_cpu->write_byte( iv_addr = 261 iv_val = 118 ).  " HALT

    mo_cpu->execute_instruction( ).  " LD A,0xFF
    mo_cpu->execute_instruction( ).  " LD D,0x0F
    mo_cpu->execute_instruction( ).  " AND A,D

    DATA(lv_af) = mo_cpu->get_af( ).
    DATA(lv_a) = lv_af DIV 256.

    cl_abap_unit_assert=>assert_equals(
      act = lv_a
      exp = 15  " 0xFF AND 0x0F = 0x0F
      msg = 'AND A,D should compute 0xFF AND 0x0F = 0x0F' ).
  ENDMETHOD.


  METHOD test_alu_or.
    " Test OR A,r instruction
    mo_cpu->reset( ).

    " Program:
    " LD A,0x0F     ; 3E 0F
    " LD E,0xF0     ; 1E F0
    " OR A,E        ; B3 (opcode 179 = OR A,E)
    " HALT          ; 76

    mo_cpu->write_byte( iv_addr = 256 iv_val = 62 ).   " LD A,n
    mo_cpu->write_byte( iv_addr = 257 iv_val = 15 ).
    mo_cpu->write_byte( iv_addr = 258 iv_val = 30 ).   " LD E,n (0x1E)
    mo_cpu->write_byte( iv_addr = 259 iv_val = 240 ).
    mo_cpu->write_byte( iv_addr = 260 iv_val = 179 ).  " OR A,E (0xB3)
    mo_cpu->write_byte( iv_addr = 261 iv_val = 118 ).  " HALT

    mo_cpu->execute_instruction( ).  " LD A,0x0F
    mo_cpu->execute_instruction( ).  " LD E,0xF0
    mo_cpu->execute_instruction( ).  " OR A,E

    DATA(lv_af) = mo_cpu->get_af( ).
    DATA(lv_a) = lv_af DIV 256.

    cl_abap_unit_assert=>assert_equals(
      act = lv_a
      exp = 255  " 0x0F OR 0xF0 = 0xFF
      msg = 'OR A,E should compute 0x0F OR 0xF0 = 0xFF' ).
  ENDMETHOD.


  METHOD test_alu_xor.
    " Test XOR A,r instruction
    mo_cpu->reset( ).

    " Program:
    " LD A,0xAA     ; 3E AA
    " LD H,0x55     ; 26 55
    " XOR A,H       ; AC (opcode 172 = XOR A,H)
    " HALT          ; 76

    mo_cpu->write_byte( iv_addr = 256 iv_val = 62 ).   " LD A,n
    mo_cpu->write_byte( iv_addr = 257 iv_val = 170 ).
    mo_cpu->write_byte( iv_addr = 258 iv_val = 38 ).   " LD H,n (0x26)
    mo_cpu->write_byte( iv_addr = 259 iv_val = 85 ).
    mo_cpu->write_byte( iv_addr = 260 iv_val = 172 ).  " XOR A,H (0xAC)
    mo_cpu->write_byte( iv_addr = 261 iv_val = 118 ).  " HALT

    mo_cpu->execute_instruction( ).  " LD A,0xAA
    mo_cpu->execute_instruction( ).  " LD H,0x55
    mo_cpu->execute_instruction( ).  " XOR A,H

    DATA(lv_af) = mo_cpu->get_af( ).
    DATA(lv_a) = lv_af DIV 256.

    cl_abap_unit_assert=>assert_equals(
      act = lv_a
      exp = 255  " 0xAA XOR 0x55 = 0xFF
      msg = 'XOR A,H should compute 0xAA XOR 0x55 = 0xFF' ).
  ENDMETHOD.

ENDCLASS.
