CLASS zcl_cpu_z80_prefix_fd DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_cpu_z80_prefix.

    METHODS constructor
      IMPORTING
        io_cpu TYPE REF TO zif_cpu_z80_core.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_cpu TYPE REF TO zif_cpu_z80_core.

    " Pre-computed flag tables
    DATA mt_szp_flags TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA mt_sz_flags TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    METHODS init_flag_tables.

    METHODS get_index_reg
      RETURNING VALUE(rv_val) TYPE i.

    METHODS set_index_reg
      IMPORTING iv_val TYPE i.

    METHODS get_reg8
      IMPORTING iv_idx TYPE i
                iv_disp TYPE i DEFAULT 0
      RETURNING VALUE(rv_val) TYPE i.

    METHODS set_reg8
      IMPORTING iv_idx TYPE i
                iv_val TYPE i
                iv_disp TYPE i DEFAULT 0.

    METHODS exec_fdcb
      RETURNING VALUE(rv_cycles) TYPE i.

ENDCLASS.


CLASS zcl_cpu_z80_prefix_fd IMPLEMENTATION.

  METHOD constructor.
    mo_cpu = io_cpu.
    init_flag_tables( ).
  ENDMETHOD.


  METHOD init_flag_tables.
    DATA: lv_i      TYPE i,
          lv_flags  TYPE i,
          lv_parity TYPE i,
          lv_temp   TYPE i,
          lv_bit    TYPE i.

    DO 256 TIMES.
      lv_i = sy-index - 1.

      " SZ flags (Sign + Zero)
      lv_flags = 0.
      IF lv_i >= 128.
        lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s.
      ENDIF.
      IF lv_i = 0.
        lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z.
      ENDIF.
      APPEND lv_flags TO mt_sz_flags.

      " SZP flags (Sign + Zero + Parity)
      lv_parity = 0.
      lv_temp = lv_i.
      DO 8 TIMES.
        lv_bit = lv_temp MOD 2.
        lv_parity = lv_parity + lv_bit.
        lv_temp = lv_temp DIV 2.
      ENDDO.
      IF lv_parity MOD 2 = 0.
        lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv.
      ENDIF.
      APPEND lv_flags TO mt_szp_flags.
    ENDDO.
  ENDMETHOD.


  METHOD get_index_reg.
    rv_val = mo_cpu->get_iy( ).
  ENDMETHOD.


  METHOD set_index_reg.
    mo_cpu->set_iy( iv_val ).
  ENDMETHOD.


  METHOD get_reg8.
    " For FD prefix: register 4=IYH, 5=IYL, 6=(IY+d)
    CASE iv_idx.
      WHEN 0. rv_val = mo_cpu->get_b( ).
      WHEN 1. rv_val = mo_cpu->get_c( ).
      WHEN 2. rv_val = mo_cpu->get_d( ).
      WHEN 3. rv_val = mo_cpu->get_e( ).
      WHEN 4. rv_val = get_index_reg( ) DIV 256.  " IYH
      WHEN 5. rv_val = get_index_reg( ) MOD 256.  " IYL
      WHEN 6.
        DATA(lv_addr) = get_index_reg( ) + iv_disp.
        IF lv_addr < 0. lv_addr = lv_addr + 65536. ENDIF.
        lv_addr = lv_addr MOD 65536.
        rv_val = mo_cpu->read_mem( lv_addr ).
      WHEN 7. rv_val = mo_cpu->get_a( ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_reg8.
    DATA lv_iy TYPE i.
    CASE iv_idx.
      WHEN 0. mo_cpu->set_b( iv_val ).
      WHEN 1. mo_cpu->set_c( iv_val ).
      WHEN 2. mo_cpu->set_d( iv_val ).
      WHEN 3. mo_cpu->set_e( iv_val ).
      WHEN 4.
        lv_iy = get_index_reg( ).
        set_index_reg( iv_val * 256 + ( lv_iy MOD 256 ) ).
      WHEN 5.
        lv_iy = get_index_reg( ).
        set_index_reg( ( lv_iy DIV 256 ) * 256 + iv_val ).
      WHEN 6.
        DATA(lv_addr) = get_index_reg( ) + iv_disp.
        IF lv_addr < 0. lv_addr = lv_addr + 65536. ENDIF.
        lv_addr = lv_addr MOD 65536.
        mo_cpu->write_mem( iv_addr = lv_addr iv_val = iv_val ).
      WHEN 7. mo_cpu->set_a( iv_val ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_cpu_z80_prefix~execute.
    DATA: lv_op    TYPE i,
          lv_disp  TYPE i,
          lv_addr  TYPE i,
          lv_val   TYPE i,
          lv_result TYPE i,
          lv_iy    TYPE i.

    lv_op = mo_cpu->fetch_byte( ).
    lv_iy = get_index_reg( ).

    CASE lv_op.
      " ADD IY,rr
      WHEN 9.   " ADD IY,BC
        lv_result = lv_iy + mo_cpu->get_bc( ).
        DATA(lv_carry) = 0.
        IF lv_result >= 65536. lv_carry = 1. lv_result = lv_result MOD 65536. ENDIF.
        DATA(lv_half) = 0.
        IF ( lv_iy MOD 4096 ) + ( mo_cpu->get_bc( ) MOD 4096 ) >= 4096. lv_half = 1. ENDIF.
        DATA(lv_f) = mo_cpu->get_f( ).
        lv_f = lv_f MOD 64.  " Clear C, N, H
        IF lv_half = 1. lv_f = lv_f + zif_cpu_z80_core=>c_flag_h. ENDIF.
        IF lv_carry = 1. lv_f = lv_f + zif_cpu_z80_core=>c_flag_c. ENDIF.
        mo_cpu->set_f( lv_f ).
        set_index_reg( lv_result ).
        rv_cycles = 15.

      WHEN 25.  " ADD IY,DE
        lv_result = lv_iy + mo_cpu->get_de( ).
        lv_carry = 0.
        IF lv_result >= 65536. lv_carry = 1. lv_result = lv_result MOD 65536. ENDIF.
        lv_half = 0.
        IF ( lv_iy MOD 4096 ) + ( mo_cpu->get_de( ) MOD 4096 ) >= 4096. lv_half = 1. ENDIF.
        lv_f = mo_cpu->get_f( ).
        lv_f = lv_f MOD 64.
        IF lv_half = 1. lv_f = lv_f + zif_cpu_z80_core=>c_flag_h. ENDIF.
        IF lv_carry = 1. lv_f = lv_f + zif_cpu_z80_core=>c_flag_c. ENDIF.
        mo_cpu->set_f( lv_f ).
        set_index_reg( lv_result ).
        rv_cycles = 15.

      WHEN 33.  " LD IY,nn
        set_index_reg( mo_cpu->fetch_word( ) ).
        rv_cycles = 14.

      WHEN 34.  " LD (nn),IY
        lv_addr = mo_cpu->fetch_word( ).
        mo_cpu->write_mem( iv_addr = lv_addr iv_val = lv_iy MOD 256 ).
        mo_cpu->write_mem( iv_addr = ( lv_addr + 1 ) MOD 65536 iv_val = lv_iy DIV 256 ).
        rv_cycles = 20.

      WHEN 35.  " INC IY
        set_index_reg( ( lv_iy + 1 ) MOD 65536 ).
        rv_cycles = 10.

      WHEN 41.  " ADD IY,IY
        lv_result = lv_iy + lv_iy.
        lv_carry = 0.
        IF lv_result >= 65536. lv_carry = 1. lv_result = lv_result MOD 65536. ENDIF.
        lv_half = 0.
        IF ( lv_iy MOD 4096 ) * 2 >= 4096. lv_half = 1. ENDIF.
        lv_f = mo_cpu->get_f( ).
        lv_f = lv_f MOD 64.
        IF lv_half = 1. lv_f = lv_f + zif_cpu_z80_core=>c_flag_h. ENDIF.
        IF lv_carry = 1. lv_f = lv_f + zif_cpu_z80_core=>c_flag_c. ENDIF.
        mo_cpu->set_f( lv_f ).
        set_index_reg( lv_result ).
        rv_cycles = 15.

      WHEN 42.  " LD IY,(nn)
        lv_addr = mo_cpu->fetch_word( ).
        lv_val = mo_cpu->read_mem( lv_addr ) + mo_cpu->read_mem( ( lv_addr + 1 ) MOD 65536 ) * 256.
        set_index_reg( lv_val ).
        rv_cycles = 20.

      WHEN 43.  " DEC IY
        set_index_reg( ( lv_iy + 65535 ) MOD 65536 ).
        rv_cycles = 10.

      WHEN 52.  " INC (IY+d)
        lv_disp = mo_cpu->fetch_byte( ).
        IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF.
        lv_val = get_reg8( iv_idx = 6 iv_disp = lv_disp ).
        lv_result = mo_cpu->alu_inc8( lv_val ).
        set_reg8( iv_idx = 6 iv_val = lv_result iv_disp = lv_disp ).
        rv_cycles = 23.

      WHEN 53.  " DEC (IY+d)
        lv_disp = mo_cpu->fetch_byte( ).
        IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF.
        lv_val = get_reg8( iv_idx = 6 iv_disp = lv_disp ).
        lv_result = mo_cpu->alu_dec8( lv_val ).
        set_reg8( iv_idx = 6 iv_val = lv_result iv_disp = lv_disp ).
        rv_cycles = 23.

      WHEN 54.  " LD (IY+d),n
        lv_disp = mo_cpu->fetch_byte( ).
        IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF.
        lv_val = mo_cpu->fetch_byte( ).
        set_reg8( iv_idx = 6 iv_val = lv_val iv_disp = lv_disp ).
        rv_cycles = 19.

      WHEN 57.  " ADD IY,SP
        lv_result = lv_iy + mo_cpu->get_sp( ).
        lv_carry = 0.
        IF lv_result >= 65536. lv_carry = 1. lv_result = lv_result MOD 65536. ENDIF.
        lv_half = 0.
        IF ( lv_iy MOD 4096 ) + ( mo_cpu->get_sp( ) MOD 4096 ) >= 4096. lv_half = 1. ENDIF.
        lv_f = mo_cpu->get_f( ).
        lv_f = lv_f MOD 64.
        IF lv_half = 1. lv_f = lv_f + zif_cpu_z80_core=>c_flag_h. ENDIF.
        IF lv_carry = 1. lv_f = lv_f + zif_cpu_z80_core=>c_flag_c. ENDIF.
        mo_cpu->set_f( lv_f ).
        set_index_reg( lv_result ).
        rv_cycles = 15.

      " LD r,(IY+d) and LD (IY+d),r
      WHEN 70. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->set_b( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 78. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->set_c( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 86. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->set_d( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 94. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->set_e( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 102. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->set_h( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 110. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->set_l( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 126. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->set_a( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.

      WHEN 112. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. set_reg8( iv_idx = 6 iv_val = mo_cpu->get_b( ) iv_disp = lv_disp ). rv_cycles = 19.
      WHEN 113. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. set_reg8( iv_idx = 6 iv_val = mo_cpu->get_c( ) iv_disp = lv_disp ). rv_cycles = 19.
      WHEN 114. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. set_reg8( iv_idx = 6 iv_val = mo_cpu->get_d( ) iv_disp = lv_disp ). rv_cycles = 19.
      WHEN 115. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. set_reg8( iv_idx = 6 iv_val = mo_cpu->get_e( ) iv_disp = lv_disp ). rv_cycles = 19.
      WHEN 116. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. set_reg8( iv_idx = 6 iv_val = mo_cpu->get_h( ) iv_disp = lv_disp ). rv_cycles = 19.
      WHEN 117. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. set_reg8( iv_idx = 6 iv_val = mo_cpu->get_l( ) iv_disp = lv_disp ). rv_cycles = 19.
      WHEN 119. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. set_reg8( iv_idx = 6 iv_val = mo_cpu->get_a( ) iv_disp = lv_disp ). rv_cycles = 19.

      " ALU operations with (IY+d)
      WHEN 134. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->alu_add8( iv_val = get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 142. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->alu_add8( iv_val = get_reg8( iv_idx = 6 iv_disp = lv_disp ) iv_carry = 1 ). rv_cycles = 19.
      WHEN 150. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->alu_sub8( iv_val = get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 158. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->alu_sub8( iv_val = get_reg8( iv_idx = 6 iv_disp = lv_disp ) iv_carry = 1 ). rv_cycles = 19.
      WHEN 166. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->alu_and8( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 174. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->alu_xor8( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 182. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->alu_or8( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.
      WHEN 190. lv_disp = mo_cpu->fetch_byte( ). IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF. mo_cpu->alu_cp8( get_reg8( iv_idx = 6 iv_disp = lv_disp ) ). rv_cycles = 19.

      " FDCB prefix (bit operations on IY+d)
      WHEN 203.
        rv_cycles = exec_fdcb( ).

      " POP IY
      WHEN 225.
        set_index_reg( mo_cpu->pop( ) ).
        rv_cycles = 14.

      " EX (SP),IY
      WHEN 227.
        DATA(lv_sp) = mo_cpu->get_sp( ).
        lv_val = mo_cpu->read_mem( lv_sp ) + mo_cpu->read_mem( ( lv_sp + 1 ) MOD 65536 ) * 256.
        mo_cpu->write_mem( iv_addr = lv_sp iv_val = lv_iy MOD 256 ).
        mo_cpu->write_mem( iv_addr = ( lv_sp + 1 ) MOD 65536 iv_val = lv_iy DIV 256 ).
        set_index_reg( lv_val ).
        rv_cycles = 23.

      " PUSH IY
      WHEN 229.
        mo_cpu->push( lv_iy ).
        rv_cycles = 15.

      " JP (IY)
      WHEN 233.
        mo_cpu->set_pc( lv_iy ).
        rv_cycles = 8.

      " LD SP,IY
      WHEN 249.
        mo_cpu->set_sp( lv_iy ).
        rv_cycles = 10.

      WHEN OTHERS.
        " Unhandled FD opcode - treat as NOP
        rv_cycles = 4.
    ENDCASE.
  ENDMETHOD.


  METHOD exec_fdcb.
    DATA: lv_disp   TYPE i,
          lv_op     TYPE i,
          lv_addr   TYPE i,
          lv_val    TYPE i,
          lv_result TYPE i,
          lv_flags  TYPE i,
          lv_bit    TYPE i,
          lv_mask   TYPE i.

    " FDCB instructions: FD CB disp op
    lv_disp = mo_cpu->fetch_byte( ).
    IF lv_disp >= 128. lv_disp = lv_disp - 256. ENDIF.
    lv_op = mo_cpu->fetch_byte( ).

    lv_addr = get_index_reg( ) + lv_disp.
    IF lv_addr < 0. lv_addr = lv_addr + 65536. ENDIF.
    lv_addr = lv_addr MOD 65536.
    lv_val = mo_cpu->read_mem( lv_addr ).

    DATA(lv_group) = lv_op DIV 64.
    DATA(lv_subop) = ( lv_op DIV 8 ) MOD 8.

    CASE lv_group.
      WHEN 0.  " Rotate/shift
        CASE lv_subop.
          WHEN 0. lv_result = mo_cpu->alu_rlc( lv_val ).
          WHEN 1. lv_result = mo_cpu->alu_rrc( lv_val ).
          WHEN 2. lv_result = mo_cpu->alu_rl( lv_val ).
          WHEN 3. lv_result = mo_cpu->alu_rr( lv_val ).
          WHEN 4. lv_result = mo_cpu->alu_sla( lv_val ).
          WHEN 5. lv_result = mo_cpu->alu_sra( lv_val ).
          WHEN 6.  " SLL (undocumented)
            DATA(lv_bit7) = lv_val DIV 128.
            lv_result = ( ( lv_val * 2 ) MOD 256 ) + 1.
            READ TABLE mt_szp_flags INDEX lv_result + 1 INTO lv_flags.
            IF lv_bit7 = 1. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_c. ENDIF.
            mo_cpu->set_f( lv_flags ).
          WHEN 7. lv_result = mo_cpu->alu_srl( lv_val ).
        ENDCASE.
        mo_cpu->write_mem( iv_addr = lv_addr iv_val = lv_result ).
        rv_cycles = 23.

      WHEN 1.  " BIT
        lv_bit = lv_subop.
        lv_mask = 1.
        DO lv_bit TIMES.
          lv_mask = lv_mask * 2.
        ENDDO.
        lv_flags = mo_cpu->get_f( ).
        IF lv_flags MOD 128 >= 64. lv_flags = lv_flags - 64. ENDIF.
        IF lv_flags MOD 32 < 16. lv_flags = lv_flags + 16. ENDIF.
        IF lv_flags MOD 4 >= 2. lv_flags = lv_flags - 2. ENDIF.
        IF lv_val MOD ( lv_mask * 2 ) < lv_mask.
          lv_flags = lv_flags + 64.
        ENDIF.
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 20.

      WHEN 2.  " RES
        lv_bit = lv_subop.
        lv_mask = 1.
        DO lv_bit TIMES.
          lv_mask = lv_mask * 2.
        ENDDO.
        IF lv_val MOD ( lv_mask * 2 ) >= lv_mask.
          lv_result = lv_val - lv_mask.
        ELSE.
          lv_result = lv_val.
        ENDIF.
        mo_cpu->write_mem( iv_addr = lv_addr iv_val = lv_result ).
        rv_cycles = 23.

      WHEN 3.  " SET
        lv_bit = lv_subop.
        lv_mask = 1.
        DO lv_bit TIMES.
          lv_mask = lv_mask * 2.
        ENDDO.
        IF lv_val MOD ( lv_mask * 2 ) < lv_mask.
          lv_result = lv_val + lv_mask.
        ELSE.
          lv_result = lv_val.
        ENDIF.
        mo_cpu->write_mem( iv_addr = lv_addr iv_val = lv_result ).
        rv_cycles = 23.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
