CLASS ltcl_z80_cpu DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES tt_bytes TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    DATA mo_bus TYPE REF TO zcl_cpu_z80_bus_simple.
    DATA mo_cpu TYPE REF TO zcl_cpu_z80.

    METHODS setup.

    " Basic tests
    METHODS test_initial_state FOR TESTING.
    METHODS test_nop FOR TESTING.
    METHODS test_halt FOR TESTING.

    " 8-bit load tests
    METHODS test_ld_b_n FOR TESTING.
    METHODS test_ld_c_n FOR TESTING.
    METHODS test_ld_a_n FOR TESTING.
    METHODS test_ld_b_c FOR TESTING.
    METHODS test_ld_a_hl FOR TESTING.
    METHODS test_ld_hl_a FOR TESTING.

    " 16-bit load tests
    METHODS test_ld_bc_nn FOR TESTING.
    METHODS test_ld_de_nn FOR TESTING.
    METHODS test_ld_hl_nn FOR TESTING.
    METHODS test_ld_sp_nn FOR TESTING.

    " Increment/Decrement tests
    METHODS test_inc_b FOR TESTING.
    METHODS test_dec_b FOR TESTING.
    METHODS test_inc_bc FOR TESTING.
    METHODS test_dec_bc FOR TESTING.
    METHODS test_inc_hl FOR TESTING.

    " Arithmetic tests
    METHODS test_add_a_b FOR TESTING.
    METHODS test_add_a_n FOR TESTING.
    METHODS test_sub_b FOR TESTING.
    METHODS test_and_b FOR TESTING.
    METHODS test_or_b FOR TESTING.
    METHODS test_xor_a FOR TESTING.
    METHODS test_cp_b FOR TESTING.

    " Jump tests
    METHODS test_jp_nn FOR TESTING.
    METHODS test_jp_z FOR TESTING.
    METHODS test_jp_nz FOR TESTING.
    METHODS test_jr_n FOR TESTING.

    " Stack tests
    METHODS test_push_bc FOR TESTING.
    METHODS test_pop_bc FOR TESTING.
    METHODS test_push_pop_af FOR TESTING.

    " Call/Return tests
    METHODS test_call_ret FOR TESTING.

    " CB prefix tests
    METHODS test_rlc_b FOR TESTING.
    METHODS test_rrc_b FOR TESTING.
    METHODS test_bit_0_b FOR TESTING.
    METHODS test_set_0_b FOR TESTING.
    METHODS test_res_0_b FOR TESTING.

    " Helper methods
    METHODS load_program IMPORTING it_bytes TYPE tt_bytes.
    METHODS get_a RETURNING VALUE(rv_val) TYPE i.
    METHODS get_b RETURNING VALUE(rv_val) TYPE i.
    METHODS get_c RETURNING VALUE(rv_val) TYPE i.
    METHODS get_f RETURNING VALUE(rv_val) TYPE i.
ENDCLASS.


CLASS ltcl_z80_cpu IMPLEMENTATION.
  METHOD setup.
    mo_bus = NEW zcl_cpu_z80_bus_simple( ).
    mo_cpu = NEW zcl_cpu_z80( mo_bus ).
  ENDMETHOD.

  METHOD load_program.
    DATA lv_addr TYPE i VALUE 0.
    LOOP AT it_bytes INTO DATA(lv_byte).
      mo_bus->zif_cpu_z80_bus~write_mem( iv_addr = lv_addr iv_val = lv_byte ).
      lv_addr = lv_addr + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_a.
    rv_val = mo_cpu->get_af( ) DIV 256.
  ENDMETHOD.

  METHOD get_b.
    rv_val = mo_cpu->get_bc( ) DIV 256.
  ENDMETHOD.

  METHOD get_c.
    rv_val = mo_cpu->get_bc( ) MOD 256.
  ENDMETHOD.

  METHOD get_f.
    rv_val = mo_cpu->get_af( ) MOD 256.
  ENDMETHOD.

  METHOD test_initial_state.
    " CPU should start with PC=0, not halted
    cl_abap_unit_assert=>assert_equals( exp = 0 act = mo_cpu->get_pc( ) msg = 'PC should be 0' ).
    cl_abap_unit_assert=>assert_false( act = mo_cpu->is_halted( ) msg = 'CPU should not be halted' ).
  ENDMETHOD.

  METHOD test_nop.
    " NOP (0x00) should just increment PC
    DATA lt_prog TYPE tt_bytes.
    APPEND 0 TO lt_prog.  " NOP
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = mo_cpu->get_pc( ) msg = 'PC should be 1 after NOP' ).
  ENDMETHOD.

  METHOD test_halt.
    " HALT (0x76) should set halted flag
    DATA lt_prog TYPE tt_bytes.
    APPEND 118 TO lt_prog.  " HALT = 0x76
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_true( act = mo_cpu->is_halted( ) msg = 'CPU should be halted' ).
  ENDMETHOD.

  METHOD test_ld_b_n.
    " LD B,n (0x06, n)
    DATA lt_prog TYPE tt_bytes.
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 66 TO lt_prog.  " n = 0x42
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 66 act = get_b( ) msg = 'B should be 0x42' ).
  ENDMETHOD.

  METHOD test_ld_c_n.
    " LD C,n (0x0E, n)
    DATA lt_prog TYPE tt_bytes.
    APPEND 14 TO lt_prog.  " LD C,n = 0x0E
    APPEND 55 TO lt_prog.  " n = 0x37
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 55 act = get_c( ) msg = 'C should be 0x37' ).
  ENDMETHOD.

  METHOD test_ld_a_n.
    " LD A,n (0x3E, n)
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n = 0x3E
    APPEND 255 TO lt_prog. " n = 0xFF
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 255 act = get_a( ) msg = 'A should be 0xFF' ).
  ENDMETHOD.

  METHOD test_ld_b_c.
    " LD B,C (0x41) - first set C, then copy to B
    DATA lt_prog TYPE tt_bytes.
    APPEND 14 TO lt_prog.  " LD C,n
    APPEND 99 TO lt_prog.  " n = 99
    APPEND 65 TO lt_prog.  " LD B,C = 0x41
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD C,99
    mo_cpu->step( ).  " LD B,C

    cl_abap_unit_assert=>assert_equals( exp = 99 act = get_b( ) msg = 'B should be 99' ).
    cl_abap_unit_assert=>assert_equals( exp = 99 act = get_c( ) msg = 'C should still be 99' ).
  ENDMETHOD.

  METHOD test_ld_a_hl.
    " LD A,(HL) (0x7E) - load A from memory address in HL
    DATA lt_prog TYPE tt_bytes.
    APPEND 33 TO lt_prog.  " LD HL,nn = 0x21
    APPEND 0 TO lt_prog.   " low byte of address
    APPEND 16 TO lt_prog.  " high byte = 0x1000
    APPEND 126 TO lt_prog. " LD A,(HL) = 0x7E
    load_program( lt_prog ).

    " Put test value at address 0x1000
    mo_bus->zif_cpu_z80_bus~write_mem( iv_addr = 4096 iv_val = 123 ).

    mo_cpu->step( ).  " LD HL,0x1000
    mo_cpu->step( ).  " LD A,(HL)

    cl_abap_unit_assert=>assert_equals( exp = 123 act = get_a( ) msg = 'A should be 123' ).
  ENDMETHOD.

  METHOD test_ld_hl_a.
    " LD (HL),A (0x77) - store A to memory address in HL
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 200 TO lt_prog. " A = 200
    APPEND 33 TO lt_prog.  " LD HL,nn
    APPEND 0 TO lt_prog.   " low byte
    APPEND 32 TO lt_prog.  " high byte = 0x2000
    APPEND 119 TO lt_prog. " LD (HL),A = 0x77
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD A,200
    mo_cpu->step( ).  " LD HL,0x2000
    mo_cpu->step( ).  " LD (HL),A

    DATA lv_val TYPE i.
    lv_val = mo_bus->zif_cpu_z80_bus~read_mem( 8192 ).  " 0x2000
    cl_abap_unit_assert=>assert_equals( exp = 200 act = lv_val msg = 'Memory at 0x2000 should be 200' ).
  ENDMETHOD.

  METHOD test_ld_bc_nn.
    " LD BC,nn (0x01, low, high)
    DATA lt_prog TYPE tt_bytes.
    APPEND 1 TO lt_prog.   " LD BC,nn
    APPEND 52 TO lt_prog.  " low = 0x34
    APPEND 18 TO lt_prog.  " high = 0x12 -> BC = 0x1234
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 4660 act = mo_cpu->get_bc( ) msg = 'BC should be 0x1234' ).
  ENDMETHOD.

  METHOD test_ld_de_nn.
    " LD DE,nn (0x11, low, high)
    DATA lt_prog TYPE tt_bytes.
    APPEND 17 TO lt_prog.  " LD DE,nn = 0x11
    APPEND 205 TO lt_prog. " low = 0xCD
    APPEND 171 TO lt_prog. " high = 0xAB -> DE = 0xABCD
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 43981 act = mo_cpu->get_de( ) msg = 'DE should be 0xABCD' ).
  ENDMETHOD.

  METHOD test_ld_hl_nn.
    " LD HL,nn (0x21, low, high)
    DATA lt_prog TYPE tt_bytes.
    APPEND 33 TO lt_prog.  " LD HL,nn = 0x21
    APPEND 0 TO lt_prog.   " low = 0x00
    APPEND 128 TO lt_prog. " high = 0x80 -> HL = 0x8000
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 32768 act = mo_cpu->get_hl( ) msg = 'HL should be 0x8000' ).
  ENDMETHOD.

  METHOD test_ld_sp_nn.
    " LD SP,nn (0x31, low, high)
    DATA lt_prog TYPE tt_bytes.
    APPEND 49 TO lt_prog.  " LD SP,nn = 0x31
    APPEND 255 TO lt_prog. " low = 0xFF
    APPEND 255 TO lt_prog. " high = 0xFF -> SP = 0xFFFF
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 65535 act = mo_cpu->get_sp( ) msg = 'SP should be 0xFFFF' ).
  ENDMETHOD.

  METHOD test_inc_b.
    " INC B (0x04)
    DATA lt_prog TYPE tt_bytes.
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 41 TO lt_prog.  " B = 41
    APPEND 4 TO lt_prog.   " INC B
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD B,41
    mo_cpu->step( ).  " INC B

    cl_abap_unit_assert=>assert_equals( exp = 42 act = get_b( ) msg = 'B should be 42' ).
  ENDMETHOD.

  METHOD test_dec_b.
    " DEC B (0x05)
    DATA lt_prog TYPE tt_bytes.
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 10 TO lt_prog.  " B = 10
    APPEND 5 TO lt_prog.   " DEC B
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD B,10
    mo_cpu->step( ).  " DEC B

    cl_abap_unit_assert=>assert_equals( exp = 9 act = get_b( ) msg = 'B should be 9' ).
  ENDMETHOD.

  METHOD test_inc_bc.
    " INC BC (0x03)
    DATA lt_prog TYPE tt_bytes.
    APPEND 1 TO lt_prog.   " LD BC,nn
    APPEND 255 TO lt_prog. " low = 0xFF
    APPEND 0 TO lt_prog.   " high = 0x00 -> BC = 0x00FF
    APPEND 3 TO lt_prog.   " INC BC
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD BC,0x00FF
    mo_cpu->step( ).  " INC BC

    cl_abap_unit_assert=>assert_equals( exp = 256 act = mo_cpu->get_bc( ) msg = 'BC should be 0x0100' ).
  ENDMETHOD.

  METHOD test_dec_bc.
    " DEC BC (0x0B)
    DATA lt_prog TYPE tt_bytes.
    APPEND 1 TO lt_prog.   " LD BC,nn
    APPEND 0 TO lt_prog.   " low = 0x00
    APPEND 1 TO lt_prog.   " high = 0x01 -> BC = 0x0100
    APPEND 11 TO lt_prog.  " DEC BC = 0x0B
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD BC,0x0100
    mo_cpu->step( ).  " DEC BC

    cl_abap_unit_assert=>assert_equals( exp = 255 act = mo_cpu->get_bc( ) msg = 'BC should be 0x00FF' ).
  ENDMETHOD.

  METHOD test_inc_hl.
    " INC HL (0x23)
    DATA lt_prog TYPE tt_bytes.
    APPEND 33 TO lt_prog.  " LD HL,nn
    APPEND 255 TO lt_prog. " low
    APPEND 255 TO lt_prog. " high -> HL = 0xFFFF
    APPEND 35 TO lt_prog.  " INC HL = 0x23
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD HL,0xFFFF
    mo_cpu->step( ).  " INC HL -> wraps to 0x0000

    cl_abap_unit_assert=>assert_equals( exp = 0 act = mo_cpu->get_hl( ) msg = 'HL should wrap to 0x0000' ).
  ENDMETHOD.

  METHOD test_add_a_b.
    " ADD A,B (0x80)
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 10 TO lt_prog.  " A = 10
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 20 TO lt_prog.  " B = 20
    APPEND 128 TO lt_prog. " ADD A,B = 0x80
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD A,10
    mo_cpu->step( ).  " LD B,20
    mo_cpu->step( ).  " ADD A,B

    cl_abap_unit_assert=>assert_equals( exp = 30 act = get_a( ) msg = 'A should be 30' ).
  ENDMETHOD.

  METHOD test_add_a_n.
    " ADD A,n (0xC6, n)
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 100 TO lt_prog. " A = 100
    APPEND 198 TO lt_prog. " ADD A,n = 0xC6
    APPEND 55 TO lt_prog.  " n = 55
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD A,100
    mo_cpu->step( ).  " ADD A,55

    cl_abap_unit_assert=>assert_equals( exp = 155 act = get_a( ) msg = 'A should be 155' ).
  ENDMETHOD.

  METHOD test_sub_b.
    " SUB B (0x90)
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 50 TO lt_prog.  " A = 50
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 30 TO lt_prog.  " B = 30
    APPEND 144 TO lt_prog. " SUB B = 0x90
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD A,50
    mo_cpu->step( ).  " LD B,30
    mo_cpu->step( ).  " SUB B

    cl_abap_unit_assert=>assert_equals( exp = 20 act = get_a( ) msg = 'A should be 20' ).
  ENDMETHOD.

  METHOD test_and_b.
    " AND B (0xA0)
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 255 TO lt_prog. " A = 0xFF
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 15 TO lt_prog.  " B = 0x0F
    APPEND 160 TO lt_prog. " AND B = 0xA0
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD A,0xFF
    mo_cpu->step( ).  " LD B,0x0F
    mo_cpu->step( ).  " AND B

    cl_abap_unit_assert=>assert_equals( exp = 15 act = get_a( ) msg = 'A should be 0x0F' ).
  ENDMETHOD.

  METHOD test_or_b.
    " OR B (0xB0)
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 240 TO lt_prog. " A = 0xF0
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 15 TO lt_prog.  " B = 0x0F
    APPEND 176 TO lt_prog. " OR B = 0xB0
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD A,0xF0
    mo_cpu->step( ).  " LD B,0x0F
    mo_cpu->step( ).  " OR B

    cl_abap_unit_assert=>assert_equals( exp = 255 act = get_a( ) msg = 'A should be 0xFF' ).
  ENDMETHOD.

  METHOD test_xor_a.
    " XOR A (0xAF) - common way to zero A
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 123 TO lt_prog. " A = 123
    APPEND 175 TO lt_prog. " XOR A = 0xAF
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD A,123
    mo_cpu->step( ).  " XOR A

    cl_abap_unit_assert=>assert_equals( exp = 0 act = get_a( ) msg = 'A should be 0' ).
  ENDMETHOD.

  METHOD test_cp_b.
    " CP B (0xB8) - compare, should set Z flag if equal
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 42 TO lt_prog.  " A = 42
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 42 TO lt_prog.  " B = 42
    APPEND 184 TO lt_prog. " CP B = 0xB8
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD A,42
    mo_cpu->step( ).  " LD B,42
    mo_cpu->step( ).  " CP B

    " A should be unchanged
    cl_abap_unit_assert=>assert_equals( exp = 42 act = get_a( ) msg = 'A should still be 42' ).
    " Z flag should be set (bit 6)
    DATA lv_z TYPE i.
    lv_z = get_f( ) DIV 64 MOD 2.  " Extract bit 6
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lv_z msg = 'Z flag should be set' ).
  ENDMETHOD.

  METHOD test_jp_nn.
    " JP nn (0xC3, low, high)
    DATA lt_prog TYPE tt_bytes.
    APPEND 195 TO lt_prog. " JP nn = 0xC3
    APPEND 0 TO lt_prog.   " low byte
    APPEND 16 TO lt_prog.  " high byte = 0x1000
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 4096 act = mo_cpu->get_pc( ) msg = 'PC should be 0x1000' ).
  ENDMETHOD.

  METHOD test_jp_z.
    " JP Z,nn (0xCA) - jump if Z flag set
    DATA lt_prog TYPE tt_bytes.
    APPEND 175 TO lt_prog. " XOR A -> sets Z flag
    APPEND 202 TO lt_prog. " JP Z,nn = 0xCA
    APPEND 0 TO lt_prog.   " low byte
    APPEND 32 TO lt_prog.  " high byte = 0x2000
    load_program( lt_prog ).

    mo_cpu->step( ).  " XOR A
    mo_cpu->step( ).  " JP Z,0x2000

    cl_abap_unit_assert=>assert_equals( exp = 8192 act = mo_cpu->get_pc( ) msg = 'PC should be 0x2000' ).
  ENDMETHOD.

  METHOD test_jp_nz.
    " JP NZ,nn (0xC2) - jump if Z flag not set
    DATA lt_prog TYPE tt_bytes.
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 1 TO lt_prog.   " A = 1 (non-zero, Z flag clear)
    APPEND 183 TO lt_prog. " OR A = 0xB7 - sets flags based on A
    APPEND 194 TO lt_prog. " JP NZ,nn = 0xC2
    APPEND 0 TO lt_prog.   " low byte
    APPEND 48 TO lt_prog.  " high byte = 0x3000
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD A,1
    mo_cpu->step( ).  " OR A
    mo_cpu->step( ).  " JP NZ,0x3000

    cl_abap_unit_assert=>assert_equals( exp = 12288 act = mo_cpu->get_pc( ) msg = 'PC should be 0x3000' ).
  ENDMETHOD.

  METHOD test_jr_n.
    " JR n (0x18) - relative jump
    DATA lt_prog TYPE tt_bytes.
    APPEND 24 TO lt_prog.  " JR n = 0x18
    APPEND 5 TO lt_prog.   " offset = +5 (PC will be at 2, so jumps to 7)
    load_program( lt_prog ).

    mo_cpu->step( ).

    cl_abap_unit_assert=>assert_equals( exp = 7 act = mo_cpu->get_pc( ) msg = 'PC should be 7' ).
  ENDMETHOD.

  METHOD test_push_bc.
    " PUSH BC (0xC5)
    DATA lt_prog TYPE tt_bytes.
    APPEND 49 TO lt_prog.  " LD SP,nn
    APPEND 0 TO lt_prog.   " low
    APPEND 255 TO lt_prog. " high = 0xFF00
    APPEND 1 TO lt_prog.   " LD BC,nn
    APPEND 52 TO lt_prog.  " low = 0x34
    APPEND 18 TO lt_prog.  " high = 0x12 -> BC = 0x1234
    APPEND 197 TO lt_prog. " PUSH BC = 0xC5
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD SP,0xFF00
    mo_cpu->step( ).  " LD BC,0x1234
    mo_cpu->step( ).  " PUSH BC

    " SP should be decremented by 2
    cl_abap_unit_assert=>assert_equals( exp = 65278 act = mo_cpu->get_sp( ) msg = 'SP should be 0xFEFE' ).
    " Check values on stack
    DATA lv_hi TYPE i.
    DATA lv_lo TYPE i.
    lv_lo = mo_bus->zif_cpu_z80_bus~read_mem( 65278 ).  " 0xFEFE
    lv_hi = mo_bus->zif_cpu_z80_bus~read_mem( 65279 ).  " 0xFEFF
    cl_abap_unit_assert=>assert_equals( exp = 52 act = lv_lo msg = 'Low byte on stack should be 0x34' ).
    cl_abap_unit_assert=>assert_equals( exp = 18 act = lv_hi msg = 'High byte on stack should be 0x12' ).
  ENDMETHOD.

  METHOD test_pop_bc.
    " POP BC (0xC1)
    DATA lt_prog TYPE tt_bytes.
    APPEND 49 TO lt_prog.  " LD SP,nn
    APPEND 254 TO lt_prog. " low = 0xFE
    APPEND 254 TO lt_prog. " high = 0xFE -> SP = 0xFEFE
    APPEND 193 TO lt_prog. " POP BC = 0xC1
    load_program( lt_prog ).

    " Pre-set stack values
    mo_bus->zif_cpu_z80_bus~write_mem( iv_addr = 65278 iv_val = 171 ).  " 0xAB
    mo_bus->zif_cpu_z80_bus~write_mem( iv_addr = 65279 iv_val = 205 ).  " 0xCD

    mo_cpu->step( ).  " LD SP,0xFEFE
    mo_cpu->step( ).  " POP BC

    cl_abap_unit_assert=>assert_equals( exp = 52651 act = mo_cpu->get_bc( ) msg = 'BC should be 0xCDAB' ).
    cl_abap_unit_assert=>assert_equals( exp = 65280 act = mo_cpu->get_sp( ) msg = 'SP should be 0xFF00' ).
  ENDMETHOD.

  METHOD test_push_pop_af.
    " PUSH AF / POP AF (0xF5 / 0xF1)
    DATA lt_prog TYPE tt_bytes.
    APPEND 49 TO lt_prog.  " LD SP,nn
    APPEND 0 TO lt_prog.   " low
    APPEND 255 TO lt_prog. " high = 0xFF00
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 170 TO lt_prog. " A = 0xAA
    APPEND 245 TO lt_prog. " PUSH AF = 0xF5
    APPEND 62 TO lt_prog.  " LD A,n
    APPEND 0 TO lt_prog.   " A = 0
    APPEND 241 TO lt_prog. " POP AF = 0xF1
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD SP,0xFF00
    mo_cpu->step( ).  " LD A,0xAA
    mo_cpu->step( ).  " PUSH AF
    mo_cpu->step( ).  " LD A,0
    mo_cpu->step( ).  " POP AF

    cl_abap_unit_assert=>assert_equals( exp = 170 act = get_a( ) msg = 'A should be restored to 0xAA' ).
  ENDMETHOD.

  METHOD test_call_ret.
    " CALL nn (0xCD) and RET (0xC9)
    DATA lt_prog TYPE tt_bytes.
    " Program at 0x0000:
    APPEND 49 TO lt_prog.  " LD SP,nn
    APPEND 0 TO lt_prog.   " low
    APPEND 255 TO lt_prog. " high = 0xFF00
    APPEND 205 TO lt_prog. " CALL nn = 0xCD
    APPEND 16 TO lt_prog.  " low = 0x10
    APPEND 0 TO lt_prog.   " high = 0x00 -> CALL 0x0010
    APPEND 118 TO lt_prog. " HALT (return point = 0x0006)
    load_program( lt_prog ).

    " Subroutine at 0x0010:
    mo_bus->zif_cpu_z80_bus~write_mem( iv_addr = 16 iv_val = 62 ).   " LD A,n
    mo_bus->zif_cpu_z80_bus~write_mem( iv_addr = 17 iv_val = 77 ).   " A = 77
    mo_bus->zif_cpu_z80_bus~write_mem( iv_addr = 18 iv_val = 201 ).  " RET = 0xC9

    mo_cpu->step( ).  " LD SP,0xFF00
    mo_cpu->step( ).  " CALL 0x0010

    cl_abap_unit_assert=>assert_equals( exp = 16 act = mo_cpu->get_pc( ) msg = 'PC should be at subroutine' ).

    mo_cpu->step( ).  " LD A,77
    mo_cpu->step( ).  " RET

    cl_abap_unit_assert=>assert_equals( exp = 6 act = mo_cpu->get_pc( ) msg = 'PC should be back at 0x0006' ).
    cl_abap_unit_assert=>assert_equals( exp = 77 act = get_a( ) msg = 'A should be 77 from subroutine' ).
  ENDMETHOD.

  METHOD test_rlc_b.
    " RLC B (CB 00) - rotate left circular
    DATA lt_prog TYPE tt_bytes.
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 129 TO lt_prog. " B = 0x81 (1000_0001)
    APPEND 203 TO lt_prog. " CB prefix
    APPEND 0 TO lt_prog.   " RLC B
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD B,0x81
    mo_cpu->step( ).  " RLC B -> 0x03 (0000_0011), C=1

    cl_abap_unit_assert=>assert_equals( exp = 3 act = get_b( ) msg = 'B should be 0x03' ).
  ENDMETHOD.

  METHOD test_rrc_b.
    " RRC B (CB 08) - rotate right circular
    DATA lt_prog TYPE tt_bytes.
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 129 TO lt_prog. " B = 0x81 (1000_0001)
    APPEND 203 TO lt_prog. " CB prefix
    APPEND 8 TO lt_prog.   " RRC B
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD B,0x81
    mo_cpu->step( ).  " RRC B -> 0xC0 (1100_0000), C=1

    cl_abap_unit_assert=>assert_equals( exp = 192 act = get_b( ) msg = 'B should be 0xC0' ).
  ENDMETHOD.

  METHOD test_bit_0_b.
    " BIT 0,B (CB 40) - test bit 0
    DATA lt_prog TYPE tt_bytes.
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 1 TO lt_prog.   " B = 0x01 (bit 0 set)
    APPEND 203 TO lt_prog. " CB prefix
    APPEND 64 TO lt_prog.  " BIT 0,B = 0x40
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD B,0x01
    mo_cpu->step( ).  " BIT 0,B

    " Z flag should be clear (bit is set)
    DATA lv_z TYPE i.
    lv_z = get_f( ) DIV 64 MOD 2.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lv_z msg = 'Z flag should be clear' ).
  ENDMETHOD.

  METHOD test_set_0_b.
    " SET 0,B (CB C0) - set bit 0
    DATA lt_prog TYPE tt_bytes.
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 0 TO lt_prog.   " B = 0
    APPEND 203 TO lt_prog. " CB prefix
    APPEND 192 TO lt_prog. " SET 0,B = 0xC0
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD B,0
    mo_cpu->step( ).  " SET 0,B

    cl_abap_unit_assert=>assert_equals( exp = 1 act = get_b( ) msg = 'B should be 1' ).
  ENDMETHOD.

  METHOD test_res_0_b.
    " RES 0,B (CB 80) - reset bit 0
    DATA lt_prog TYPE tt_bytes.
    APPEND 6 TO lt_prog.   " LD B,n
    APPEND 255 TO lt_prog. " B = 0xFF
    APPEND 203 TO lt_prog. " CB prefix
    APPEND 128 TO lt_prog. " RES 0,B = 0x80
    load_program( lt_prog ).

    mo_cpu->step( ).  " LD B,0xFF
    mo_cpu->step( ).  " RES 0,B

    cl_abap_unit_assert=>assert_equals( exp = 254 act = get_b( ) msg = 'B should be 0xFE' ).
  ENDMETHOD.
ENDCLASS.