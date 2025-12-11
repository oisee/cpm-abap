CLASS zcl_cpu_z80_prefix_cb DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_cpu_z80_prefix.

    METHODS constructor
      IMPORTING
        io_cpu TYPE REF TO zif_cpu_z80_core.

  PRIVATE SECTION.
    DATA mo_cpu TYPE REF TO zif_cpu_z80_core.

    " Pre-computed flag tables (SZP flags for each byte value)
    DATA mt_szp_flags TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    METHODS get_reg8
      IMPORTING iv_idx TYPE i
      RETURNING VALUE(rv_val) TYPE i.

    METHODS set_reg8
      IMPORTING iv_idx TYPE i
                iv_val TYPE i.

    METHODS init_flag_tables.

ENDCLASS.


CLASS zcl_cpu_z80_prefix_cb IMPLEMENTATION.

  METHOD constructor.
    mo_cpu = io_cpu.
    init_flag_tables( ).
  ENDMETHOD.


  METHOD init_flag_tables.
    DATA: lv_i     TYPE i,
          lv_flags TYPE i,
          lv_parity TYPE i,
          lv_temp  TYPE i,
          lv_bit   TYPE i.

    DO 256 TIMES.
      lv_i = sy-index - 1.
      lv_flags = 0.
      IF lv_i >= 128.
        lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s.
      ENDIF.
      IF lv_i = 0.
        lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z.
      ENDIF.
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


  METHOD get_reg8.
    CASE iv_idx.
      WHEN 0. rv_val = mo_cpu->get_b( ).
      WHEN 1. rv_val = mo_cpu->get_c( ).
      WHEN 2. rv_val = mo_cpu->get_d( ).
      WHEN 3. rv_val = mo_cpu->get_e( ).
      WHEN 4. rv_val = mo_cpu->get_h( ).
      WHEN 5. rv_val = mo_cpu->get_l( ).
      WHEN 6. rv_val = mo_cpu->read_mem( mo_cpu->get_hl( ) ).
      WHEN 7. rv_val = mo_cpu->get_a( ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_reg8.
    CASE iv_idx.
      WHEN 0. mo_cpu->set_b( iv_val ).
      WHEN 1. mo_cpu->set_c( iv_val ).
      WHEN 2. mo_cpu->set_d( iv_val ).
      WHEN 3. mo_cpu->set_e( iv_val ).
      WHEN 4. mo_cpu->set_h( iv_val ).
      WHEN 5. mo_cpu->set_l( iv_val ).
      WHEN 6. mo_cpu->write_mem( iv_addr = mo_cpu->get_hl( ) iv_val = iv_val ).
      WHEN 7. mo_cpu->set_a( iv_val ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_cpu_z80_prefix~execute.
    DATA: lv_op     TYPE i,
          lv_reg    TYPE i,
          lv_bit    TYPE i,
          lv_val    TYPE i,
          lv_result TYPE i,
          lv_flags  TYPE i,
          lv_mask   TYPE i.

    lv_op = mo_cpu->fetch_byte( ).
    lv_reg = lv_op MOD 8.
    lv_val = get_reg8( lv_reg ).

    DATA(lv_group) = lv_op DIV 64.
    DATA(lv_subop) = ( lv_op DIV 8 ) MOD 8.

    CASE lv_group.
      WHEN 0.
        CASE lv_subop.
          WHEN 0. lv_result = mo_cpu->alu_rlc( lv_val ).
          WHEN 1. lv_result = mo_cpu->alu_rrc( lv_val ).
          WHEN 2. lv_result = mo_cpu->alu_rl( lv_val ).
          WHEN 3. lv_result = mo_cpu->alu_rr( lv_val ).
          WHEN 4. lv_result = mo_cpu->alu_sla( lv_val ).
          WHEN 5. lv_result = mo_cpu->alu_sra( lv_val ).
          WHEN 6.
            DATA(lv_bit7) = lv_val DIV 128.
            lv_result = ( ( lv_val * 2 ) MOD 256 ) + 1.
            READ TABLE mt_szp_flags INDEX lv_result + 1 INTO lv_flags.
            IF lv_bit7 = 1. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_c. ENDIF.
            mo_cpu->set_f( lv_flags ).
          WHEN 7. lv_result = mo_cpu->alu_srl( lv_val ).
        ENDCASE.
        set_reg8( iv_idx = lv_reg iv_val = lv_result ).

      WHEN 1.
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

      WHEN 2.
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
        set_reg8( iv_idx = lv_reg iv_val = lv_result ).

      WHEN 3.
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
        set_reg8( iv_idx = lv_reg iv_val = lv_result ).
    ENDCASE.

    IF lv_reg = 6. rv_cycles = 15. ELSE. rv_cycles = 8. ENDIF.
  ENDMETHOD.

ENDCLASS.
