CLASS zcl_cpu_z80 DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* Z80 CPU Emulator
* Implements the Zilog Z80 processor with main opcodes + CB prefix
* Uses bus interface for memory-mapped I/O and port I/O
* Design based on proven 6502 emulator architecture (hybrid approach)
************************************************************************

  PUBLIC SECTION.
    INTERFACES zif_cpu_z80_core.
    TYPES: BEGIN OF ts_status,
             af      TYPE i,
             bc      TYPE i,
             de      TYPE i,
             hl      TYPE i,
             af_alt  TYPE i,
             bc_alt  TYPE i,
             de_alt  TYPE i,
             hl_alt  TYPE i,
             ix      TYPE i,
             iy      TYPE i,
             sp      TYPE i,
             pc      TYPE i,
             i       TYPE i,
             r       TYPE i,
             iff1    TYPE abap_bool,
             iff2    TYPE abap_bool,
             im      TYPE i,
             cycles  TYPE i,
             running TYPE abap_bool,
             halted  TYPE abap_bool,
           END OF ts_status.

    " Flag bit positions in F register
    CONSTANTS: c_flag_c  TYPE i VALUE 1,    " Carry
               c_flag_n  TYPE i VALUE 2,    " Add/Subtract
               c_flag_pv TYPE i VALUE 4,    " Parity/Overflow
               c_flag_f3 TYPE i VALUE 8,    " Undocumented (bit 3)
               c_flag_h  TYPE i VALUE 16,   " Half-carry
               c_flag_f5 TYPE i VALUE 32,   " Undocumented (bit 5)
               c_flag_z  TYPE i VALUE 64,   " Zero
               c_flag_s  TYPE i VALUE 128.  " Sign

    METHODS constructor
      IMPORTING io_bus TYPE REF TO zif_cpu_z80_bus.

    METHODS reset.

    METHODS step
      RETURNING VALUE(rv_cycles) TYPE i.

    METHODS run
      IMPORTING iv_max_cycles TYPE i DEFAULT 1000000.

    METHODS nmi.

    METHODS irq
      IMPORTING iv_data TYPE i DEFAULT 255.

    METHODS get_status
      RETURNING VALUE(rs_status) TYPE ts_status.

    METHODS is_halted
      RETURNING VALUE(rv_halted) TYPE abap_bool.

    METHODS provide_input
      IMPORTING iv_text TYPE string.

    " Debug helpers
    METHODS get_pc RETURNING VALUE(rv_pc) TYPE i.
    METHODS get_sp RETURNING VALUE(rv_sp) TYPE i.
    METHODS get_af RETURNING VALUE(rv_af) TYPE i.
    METHODS get_bc RETURNING VALUE(rv_bc) TYPE i.
    METHODS get_de RETURNING VALUE(rv_de) TYPE i.
    METHODS get_hl RETURNING VALUE(rv_hl) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_bus TYPE REF TO zif_cpu_z80_bus.
    DATA mo_cb_handler TYPE REF TO zif_cpu_z80_prefix.
    DATA mo_dd_handler TYPE REF TO zif_cpu_z80_prefix.
    DATA mo_ed_handler TYPE REF TO zif_cpu_z80_prefix.
    DATA mo_fd_handler TYPE REF TO zif_cpu_z80_prefix.

    " Main registers (16-bit pairs stored as integers: high*256+low)
    DATA: mv_af TYPE i,   " A (high) + F flags (low)
          mv_bc TYPE i,   " B (high) + C (low)
          mv_de TYPE i,   " D (high) + E (low)
          mv_hl TYPE i.   " H (high) + L (low)

    " Alternate register set
    DATA: mv_af_alt TYPE i,
          mv_bc_alt TYPE i,
          mv_de_alt TYPE i,
          mv_hl_alt TYPE i.

    " Index registers
    DATA: mv_ix TYPE i,
          mv_iy TYPE i.

    " Other registers
    DATA: mv_sp TYPE i,   " Stack pointer
          mv_pc TYPE i,   " Program counter
          mv_i  TYPE i,   " Interrupt vector
          mv_r  TYPE i.   " Memory refresh

    " Interrupt state
    DATA: mv_iff1 TYPE abap_bool,
          mv_iff2 TYPE abap_bool,
          mv_im   TYPE i.           " Interrupt mode (0, 1, 2)

    " Execution state
    DATA: mv_cycles  TYPE i,
          mv_running TYPE abap_bool,
          mv_halted  TYPE abap_bool.

    " Lookup tables for flag calculation (performance optimization)
    DATA: mt_sz_flags   TYPE STANDARD TABLE OF i WITH EMPTY KEY,  " Sign+Zero flags
          mt_szp_flags  TYPE STANDARD TABLE OF i WITH EMPTY KEY,  " Sign+Zero+Parity flags
          mt_parity     TYPE STANDARD TABLE OF i WITH EMPTY KEY.  " Parity lookup

    " Memory access
    METHODS read8
      IMPORTING iv_addr       TYPE i
      RETURNING VALUE(rv_val) TYPE i.

    METHODS write8
      IMPORTING iv_addr TYPE i
                iv_val  TYPE i.

    METHODS read16
      IMPORTING iv_addr       TYPE i
      RETURNING VALUE(rv_val) TYPE i.

    METHODS write16
      IMPORTING iv_addr TYPE i
                iv_val  TYPE i.

    " Fetch with PC increment
    METHODS fetch8
      RETURNING VALUE(rv_val) TYPE i.

    METHODS fetch16
      RETURNING VALUE(rv_val) TYPE i.

    " Stack operations
    METHODS push16
      IMPORTING iv_val TYPE i.

    METHODS pop16
      RETURNING VALUE(rv_val) TYPE i.

    " Register helpers
    METHODS get_high
      IMPORTING iv_pair       TYPE i
      RETURNING VALUE(rv_val) TYPE i.

    METHODS get_low
      IMPORTING iv_pair       TYPE i
      RETURNING VALUE(rv_val) TYPE i.

    METHODS set_high
      IMPORTING iv_pair       TYPE i
                iv_val        TYPE i
      RETURNING VALUE(rv_new) TYPE i.

    METHODS set_low
      IMPORTING iv_pair       TYPE i
                iv_val        TYPE i
      RETURNING VALUE(rv_new) TYPE i.

    " Flag helpers
    METHODS get_flag
      IMPORTING iv_flag       TYPE i
      RETURNING VALUE(rv_set) TYPE abap_bool.

    METHODS set_flag
      IMPORTING iv_flag TYPE i
                iv_set  TYPE abap_bool.

    " 8-bit register access by index (for opcode decoding)
    METHODS get_reg8
      IMPORTING iv_idx        TYPE i
      RETURNING VALUE(rv_val) TYPE i.

    METHODS set_reg8
      IMPORTING iv_idx TYPE i
                iv_val TYPE i.

    " 16-bit register pair access by index
    METHODS get_reg16
      IMPORTING iv_idx        TYPE i
      RETURNING VALUE(rv_val) TYPE i.

    METHODS set_reg16
      IMPORTING iv_idx TYPE i
                iv_val TYPE i.

    " ALU operations
    METHODS alu_add8
      IMPORTING iv_val       TYPE i
                iv_with_carry TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_result) TYPE i.

    METHODS alu_sub8
      IMPORTING iv_val       TYPE i
                iv_with_carry TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_result) TYPE i.

    METHODS alu_and8
      IMPORTING iv_val TYPE i.

    METHODS alu_or8
      IMPORTING iv_val TYPE i.

    METHODS alu_xor8
      IMPORTING iv_val TYPE i.

    METHODS alu_cp8
      IMPORTING iv_val TYPE i.

    METHODS alu_inc8
      IMPORTING iv_val           TYPE i
      RETURNING VALUE(rv_result) TYPE i.

    METHODS alu_dec8
      IMPORTING iv_val           TYPE i
      RETURNING VALUE(rv_result) TYPE i.

    " Rotate/shift operations
    METHODS alu_rlc
      IMPORTING iv_val           TYPE i
      RETURNING VALUE(rv_result) TYPE i.

    METHODS alu_rrc
      IMPORTING iv_val           TYPE i
      RETURNING VALUE(rv_result) TYPE i.

    METHODS alu_rl
      IMPORTING iv_val           TYPE i
      RETURNING VALUE(rv_result) TYPE i.

    METHODS alu_rr
      IMPORTING iv_val           TYPE i
      RETURNING VALUE(rv_result) TYPE i.

    METHODS alu_sla
      IMPORTING iv_val           TYPE i
      RETURNING VALUE(rv_result) TYPE i.

    METHODS alu_sra
      IMPORTING iv_val           TYPE i
      RETURNING VALUE(rv_result) TYPE i.

    METHODS alu_srl
      IMPORTING iv_val           TYPE i
      RETURNING VALUE(rv_result) TYPE i.

    " Lookup table initialization
    METHODS init_tables.

    " Instruction execution
    METHODS exec_main
      IMPORTING iv_opcode        TYPE i
      RETURNING VALUE(rv_cycles) TYPE i.


    " Condition checking for conditional ops
    METHODS check_condition
      IMPORTING iv_cond          TYPE i
      RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.



CLASS zcl_cpu_z80 IMPLEMENTATION.

  METHOD constructor.
    mo_bus = io_bus.
    init_tables( ).
    mo_cb_handler = NEW zcl_cpu_z80_prefix_cb( me ).
    mo_dd_handler = NEW zcl_cpu_z80_prefix_dd( me ).
    mo_ed_handler = NEW zcl_cpu_z80_prefix_ed( me ).
    mo_fd_handler = NEW zcl_cpu_z80_prefix_fd( me ).
    reset( ).
  ENDMETHOD.


  METHOD init_tables.
    DATA: lv_i      TYPE i,
          lv_parity TYPE i,
          lv_bits   TYPE i,
          lv_flags  TYPE i,
          lv_val    TYPE i.

    " Pre-compute Sign+Zero flags for values 0-255
    DO 256 TIMES.
      lv_i = sy-index - 1.
      lv_flags = 0.
      IF lv_i = 0.
        lv_flags = c_flag_z.
      ENDIF.
      IF lv_i >= 128.
        lv_flags = lv_flags + c_flag_s.
      ENDIF.
      APPEND lv_flags TO mt_sz_flags.
    ENDDO.

    " Pre-compute parity for values 0-255
    DO 256 TIMES.
      lv_i = sy-index - 1.
      lv_val = lv_i.
      lv_bits = 0.
      DO 8 TIMES.
        IF lv_val MOD 2 = 1.
          lv_bits = lv_bits + 1.
        ENDIF.
        lv_val = lv_val DIV 2.
      ENDDO.
      IF lv_bits MOD 2 = 0.
        lv_parity = c_flag_pv.
      ELSE.
        lv_parity = 0.
      ENDIF.
      APPEND lv_parity TO mt_parity.
    ENDDO.

    " Pre-compute Sign+Zero+Parity flags for values 0-255
    DO 256 TIMES.
      lv_i = sy-index - 1.
      READ TABLE mt_sz_flags INDEX lv_i + 1 INTO lv_flags.
      READ TABLE mt_parity INDEX lv_i + 1 INTO lv_parity.
      lv_flags = lv_flags + lv_parity.
      APPEND lv_flags TO mt_szp_flags.
    ENDDO.
  ENDMETHOD.


  METHOD reset.
    mv_af = 65535.
    mv_bc = 0.
    mv_de = 0.
    mv_hl = 0.
    mv_af_alt = 0.
    mv_bc_alt = 0.
    mv_de_alt = 0.
    mv_hl_alt = 0.
    mv_ix = 0.
    mv_iy = 0.
    mv_sp = 65535.
    mv_pc = 0.
    mv_i = 0.
    mv_r = 0.
    mv_iff1 = abap_false.
    mv_iff2 = abap_false.
    mv_im = 0.
    mv_cycles = 0.
    mv_running = abap_true.
    mv_halted = abap_false.
  ENDMETHOD.


  METHOD read8.
    rv_val = mo_bus->read_mem( iv_addr MOD 65536 ).
  ENDMETHOD.


  METHOD write8.
    mo_bus->write_mem( iv_addr = iv_addr MOD 65536 iv_val = iv_val MOD 256 ).
  ENDMETHOD.


  METHOD read16.
    DATA(lv_lo) = read8( iv_addr ).
    DATA(lv_hi) = read8( iv_addr + 1 ).
    rv_val = lv_lo + lv_hi * 256.
  ENDMETHOD.


  METHOD write16.
    write8( iv_addr = iv_addr iv_val = iv_val MOD 256 ).
    write8( iv_addr = iv_addr + 1 iv_val = iv_val DIV 256 ).
  ENDMETHOD.


  METHOD fetch8.
    rv_val = read8( mv_pc ).
    mv_pc = ( mv_pc + 1 ) MOD 65536.
  ENDMETHOD.


  METHOD fetch16.
    rv_val = read16( mv_pc ).
    mv_pc = ( mv_pc + 2 ) MOD 65536.
  ENDMETHOD.


  METHOD push16.
    mv_sp = ( mv_sp - 2 ) MOD 65536.
    IF mv_sp < 0.
      mv_sp = mv_sp + 65536.
    ENDIF.
    write16( iv_addr = mv_sp iv_val = iv_val ).
  ENDMETHOD.


  METHOD pop16.
    rv_val = read16( mv_sp ).
    mv_sp = ( mv_sp + 2 ) MOD 65536.
  ENDMETHOD.


  METHOD get_high.
    rv_val = ( iv_pair DIV 256 ) MOD 256.
  ENDMETHOD.


  METHOD get_low.
    rv_val = iv_pair MOD 256.
  ENDMETHOD.


  METHOD set_high.
    rv_new = ( iv_pair MOD 256 ) + ( iv_val MOD 256 ) * 256.
  ENDMETHOD.


  METHOD set_low.
    rv_new = ( iv_pair DIV 256 ) * 256 + ( iv_val MOD 256 ).
  ENDMETHOD.


  METHOD get_flag.
    rv_set = xsdbool( ( mv_af MOD 256 ) MOD ( iv_flag * 2 ) >= iv_flag ).
  ENDMETHOD.


  METHOD set_flag.
    DATA(lv_f) = mv_af MOD 256.
    DATA(lv_has) = xsdbool( lv_f MOD ( iv_flag * 2 ) >= iv_flag ).
    IF iv_set = abap_true AND lv_has = abap_false.
      lv_f = lv_f + iv_flag.
    ELSEIF iv_set = abap_false AND lv_has = abap_true.
      lv_f = lv_f - iv_flag.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
  ENDMETHOD.


  METHOD get_reg8.
    CASE iv_idx.
      WHEN 0. rv_val = get_high( mv_bc ).
      WHEN 1. rv_val = get_low( mv_bc ).
      WHEN 2. rv_val = get_high( mv_de ).
      WHEN 3. rv_val = get_low( mv_de ).
      WHEN 4. rv_val = get_high( mv_hl ).
      WHEN 5. rv_val = get_low( mv_hl ).
      WHEN 6. rv_val = read8( mv_hl ).
      WHEN 7. rv_val = get_high( mv_af ).
      WHEN OTHERS. rv_val = 0.
    ENDCASE.
  ENDMETHOD.


  METHOD set_reg8.
    DATA(lv_val) = iv_val MOD 256.
    CASE iv_idx.
      WHEN 0. mv_bc = set_high( iv_pair = mv_bc iv_val = lv_val ).
      WHEN 1. mv_bc = set_low( iv_pair = mv_bc iv_val = lv_val ).
      WHEN 2. mv_de = set_high( iv_pair = mv_de iv_val = lv_val ).
      WHEN 3. mv_de = set_low( iv_pair = mv_de iv_val = lv_val ).
      WHEN 4. mv_hl = set_high( iv_pair = mv_hl iv_val = lv_val ).
      WHEN 5. mv_hl = set_low( iv_pair = mv_hl iv_val = lv_val ).
      WHEN 6. write8( iv_addr = mv_hl iv_val = lv_val ).
      WHEN 7. mv_af = set_high( iv_pair = mv_af iv_val = lv_val ).
    ENDCASE.
  ENDMETHOD.


  METHOD get_reg16.
    CASE iv_idx.
      WHEN 0. rv_val = mv_bc.
      WHEN 1. rv_val = mv_de.
      WHEN 2. rv_val = mv_hl.
      WHEN 3. rv_val = mv_sp.
      WHEN OTHERS. rv_val = 0.
    ENDCASE.
  ENDMETHOD.


  METHOD set_reg16.
    DATA(lv_val) = iv_val MOD 65536.
    CASE iv_idx.
      WHEN 0. mv_bc = lv_val.
      WHEN 1. mv_de = lv_val.
      WHEN 2. mv_hl = lv_val.
      WHEN 3. mv_sp = lv_val.
    ENDCASE.
  ENDMETHOD.


  METHOD check_condition.
    CASE iv_cond.
      WHEN 0. rv_result = xsdbool( NOT get_flag( c_flag_z ) ).
      WHEN 1. rv_result = get_flag( c_flag_z ).
      WHEN 2. rv_result = xsdbool( NOT get_flag( c_flag_c ) ).
      WHEN 3. rv_result = get_flag( c_flag_c ).
      WHEN 4. rv_result = xsdbool( NOT get_flag( c_flag_pv ) ).
      WHEN 5. rv_result = get_flag( c_flag_pv ).
      WHEN 6. rv_result = xsdbool( NOT get_flag( c_flag_s ) ).
      WHEN 7. rv_result = get_flag( c_flag_s ).
      WHEN OTHERS. rv_result = abap_false.
    ENDCASE.
  ENDMETHOD.


  METHOD alu_add8.
    DATA: lv_a      TYPE i,
          lv_result TYPE i,
          lv_carry  TYPE i,
          lv_flags  TYPE i,
          lv_half   TYPE i.

    lv_a = get_high( mv_af ).
    IF iv_with_carry = abap_true AND get_flag( c_flag_c ) = abap_true.
      lv_carry = 1.
    ELSE.
      lv_carry = 0.
    ENDIF.

    lv_result = lv_a + iv_val + lv_carry.
    READ TABLE mt_sz_flags INDEX ( lv_result MOD 256 ) + 1 INTO lv_flags.

    IF lv_result > 255.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.

    lv_half = ( lv_a MOD 16 ) + ( iv_val MOD 16 ) + lv_carry.
    IF lv_half > 15.
      lv_flags = lv_flags + c_flag_h.
    ENDIF.

    DATA(lv_a_sign) = lv_a DIV 128.
    DATA(lv_v_sign) = iv_val DIV 128.
    DATA(lv_r_sign) = ( lv_result MOD 256 ) DIV 128.
    IF lv_a_sign = lv_v_sign AND lv_a_sign <> lv_r_sign.
      lv_flags = lv_flags + c_flag_pv.
    ENDIF.

    mv_af = ( lv_result MOD 256 ) * 256 + lv_flags.
    rv_result = lv_result MOD 256.
  ENDMETHOD.


  METHOD alu_sub8.
    DATA: lv_a      TYPE i,
          lv_result TYPE i,
          lv_carry  TYPE i,
          lv_flags  TYPE i,
          lv_half   TYPE i.

    lv_a = get_high( mv_af ).
    IF iv_with_carry = abap_true AND get_flag( c_flag_c ) = abap_true.
      lv_carry = 1.
    ELSE.
      lv_carry = 0.
    ENDIF.

    lv_result = lv_a - iv_val - lv_carry.
    DATA(lv_result_byte) = lv_result MOD 256.
    IF lv_result_byte < 0.
      lv_result_byte = lv_result_byte + 256.
    ENDIF.
    READ TABLE mt_sz_flags INDEX lv_result_byte + 1 INTO lv_flags.

    lv_flags = lv_flags + c_flag_n.

    IF lv_result < 0.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.

    lv_half = ( lv_a MOD 16 ) - ( iv_val MOD 16 ) - lv_carry.
    IF lv_half < 0.
      lv_flags = lv_flags + c_flag_h.
    ENDIF.

    DATA(lv_a_sign) = lv_a DIV 128.
    DATA(lv_v_sign) = iv_val DIV 128.
    DATA(lv_r_sign) = lv_result_byte DIV 128.
    IF lv_a_sign <> lv_v_sign AND lv_a_sign <> lv_r_sign.
      lv_flags = lv_flags + c_flag_pv.
    ENDIF.

    mv_af = lv_result_byte * 256 + lv_flags.
    rv_result = lv_result_byte.
  ENDMETHOD.


  METHOD alu_and8.
    DATA: lv_a      TYPE i,
          lv_result TYPE i,
          lv_flags  TYPE i,
          lv_mask   TYPE i.

    lv_a = get_high( mv_af ).
    lv_result = 0.
    lv_mask = 1.
    DO 8 TIMES.
      IF ( lv_a DIV lv_mask ) MOD 2 = 1 AND ( iv_val DIV lv_mask ) MOD 2 = 1.
        lv_result = lv_result + lv_mask.
      ENDIF.
      lv_mask = lv_mask * 2.
    ENDDO.

    READ TABLE mt_szp_flags INDEX lv_result + 1 INTO lv_flags.
    lv_flags = lv_flags + c_flag_h.
    mv_af = lv_result * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_or8.
    DATA: lv_a      TYPE i,
          lv_result TYPE i,
          lv_flags  TYPE i,
          lv_mask   TYPE i.

    lv_a = get_high( mv_af ).
    lv_result = 0.
    lv_mask = 1.
    DO 8 TIMES.
      IF ( lv_a DIV lv_mask ) MOD 2 = 1 OR ( iv_val DIV lv_mask ) MOD 2 = 1.
        lv_result = lv_result + lv_mask.
      ENDIF.
      lv_mask = lv_mask * 2.
    ENDDO.

    READ TABLE mt_szp_flags INDEX lv_result + 1 INTO lv_flags.
    mv_af = lv_result * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_xor8.
    DATA: lv_a      TYPE i,
          lv_result TYPE i,
          lv_flags  TYPE i,
          lv_mask   TYPE i.

    lv_a = get_high( mv_af ).
    lv_result = 0.
    lv_mask = 1.
    DO 8 TIMES.
      IF ( ( lv_a DIV lv_mask ) MOD 2 + ( iv_val DIV lv_mask ) MOD 2 ) = 1.
        lv_result = lv_result + lv_mask.
      ENDIF.
      lv_mask = lv_mask * 2.
    ENDDO.

    READ TABLE mt_szp_flags INDEX lv_result + 1 INTO lv_flags.
    mv_af = lv_result * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_cp8.
    DATA: lv_a      TYPE i,
          lv_result TYPE i,
          lv_flags  TYPE i,
          lv_half   TYPE i.

    lv_a = get_high( mv_af ).
    lv_result = lv_a - iv_val.

    DATA(lv_result_byte) = lv_result MOD 256.
    IF lv_result_byte < 0.
      lv_result_byte = lv_result_byte + 256.
    ENDIF.
    READ TABLE mt_sz_flags INDEX lv_result_byte + 1 INTO lv_flags.

    lv_flags = lv_flags + c_flag_n.

    IF lv_result < 0.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.

    lv_half = ( lv_a MOD 16 ) - ( iv_val MOD 16 ).
    IF lv_half < 0.
      lv_flags = lv_flags + c_flag_h.
    ENDIF.

    DATA(lv_a_sign) = lv_a DIV 128.
    DATA(lv_v_sign) = iv_val DIV 128.
    DATA(lv_r_sign) = lv_result_byte DIV 128.
    IF lv_a_sign <> lv_v_sign AND lv_a_sign <> lv_r_sign.
      lv_flags = lv_flags + c_flag_pv.
    ENDIF.

    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_inc8.
    DATA lv_flags TYPE i.
    rv_result = ( iv_val + 1 ) MOD 256.
    DATA(lv_carry) = mv_af MOD 256 MOD 2.
    READ TABLE mt_sz_flags INDEX rv_result + 1 INTO lv_flags.
    lv_flags = lv_flags + lv_carry.
    IF ( iv_val MOD 16 ) = 15.
      lv_flags = lv_flags + c_flag_h.
    ENDIF.
    IF iv_val = 127.
      lv_flags = lv_flags + c_flag_pv.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_dec8.
    DATA lv_flags TYPE i.
    rv_result = iv_val - 1.
    IF rv_result < 0.
      rv_result = rv_result + 256.
    ENDIF.
    DATA(lv_carry) = mv_af MOD 256 MOD 2.
    READ TABLE mt_sz_flags INDEX rv_result + 1 INTO lv_flags.
    lv_flags = lv_flags + lv_carry + c_flag_n.
    IF ( iv_val MOD 16 ) = 0.
      lv_flags = lv_flags + c_flag_h.
    ENDIF.
    IF iv_val = 128.
      lv_flags = lv_flags + c_flag_pv.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_rlc.
    DATA(lv_bit7) = iv_val DIV 128.
    rv_result = ( ( iv_val * 2 ) MOD 256 ) + lv_bit7.
    DATA lv_flags TYPE i.
    READ TABLE mt_szp_flags INDEX rv_result + 1 INTO lv_flags.
    IF lv_bit7 = 1.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_rrc.
    DATA(lv_bit0) = iv_val MOD 2.
    rv_result = ( iv_val DIV 2 ) + lv_bit0 * 128.
    DATA lv_flags TYPE i.
    READ TABLE mt_szp_flags INDEX rv_result + 1 INTO lv_flags.
    IF lv_bit0 = 1.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_rl.
    DATA(lv_old_carry) = mv_af MOD 256 MOD 2.
    DATA(lv_bit7) = iv_val DIV 128.
    rv_result = ( ( iv_val * 2 ) MOD 256 ) + lv_old_carry.
    DATA lv_flags TYPE i.
    READ TABLE mt_szp_flags INDEX rv_result + 1 INTO lv_flags.
    IF lv_bit7 = 1.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_rr.
    DATA(lv_old_carry) = mv_af MOD 256 MOD 2.
    DATA(lv_bit0) = iv_val MOD 2.
    rv_result = ( iv_val DIV 2 ) + lv_old_carry * 128.
    DATA lv_flags TYPE i.
    READ TABLE mt_szp_flags INDEX rv_result + 1 INTO lv_flags.
    IF lv_bit0 = 1.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_sla.
    DATA(lv_bit7) = iv_val DIV 128.
    rv_result = ( iv_val * 2 ) MOD 256.
    DATA lv_flags TYPE i.
    READ TABLE mt_szp_flags INDEX rv_result + 1 INTO lv_flags.
    IF lv_bit7 = 1.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_sra.
    DATA(lv_bit7) = iv_val DIV 128.
    DATA(lv_bit0) = iv_val MOD 2.
    rv_result = ( iv_val DIV 2 ) + lv_bit7 * 128.
    DATA lv_flags TYPE i.
    READ TABLE mt_szp_flags INDEX rv_result + 1 INTO lv_flags.
    IF lv_bit0 = 1.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD alu_srl.
    DATA(lv_bit0) = iv_val MOD 2.
    rv_result = iv_val DIV 2.
    DATA lv_flags TYPE i.
    READ TABLE mt_szp_flags INDEX rv_result + 1 INTO lv_flags.
    IF lv_bit0 = 1.
      lv_flags = lv_flags + c_flag_c.
    ENDIF.
    mv_af = ( mv_af DIV 256 ) * 256 + lv_flags.
  ENDMETHOD.


  METHOD step.
    DATA lv_opcode TYPE i.

    IF mv_running = abap_false.
      rv_cycles = 0.
      RETURN.
    ENDIF.

    IF mv_halted = abap_true.
      rv_cycles = 4.
      mv_cycles = mv_cycles + rv_cycles.
      RETURN.
    ENDIF.

    lv_opcode = fetch8( ).
    mv_r = ( mv_r MOD 128 + 1 ) MOD 128 + ( mv_r DIV 128 ) * 128.
    rv_cycles = exec_main( lv_opcode ).
    mv_cycles = mv_cycles + rv_cycles.
  ENDMETHOD.


  METHOD exec_main.
    DATA: lv_val    TYPE i,
          lv_addr   TYPE i,
          lv_dst    TYPE i,
          lv_src    TYPE i,
          lv_offset TYPE i,
          lv_temp   TYPE i,
          lv_f      TYPE i,
          lv_a      TYPE i,
          lv_flags  TYPE i,
          lv_old_c  TYPE i.

    CASE iv_opcode.
      WHEN 0. rv_cycles = 4.  " NOP

      WHEN 1. mv_bc = fetch16( ). rv_cycles = 10.  " LD BC,nn

      WHEN 2. write8( iv_addr = mv_bc iv_val = get_high( mv_af ) ). rv_cycles = 7.  " LD (BC),A

      WHEN 3. mv_bc = ( mv_bc + 1 ) MOD 65536. rv_cycles = 6.  " INC BC

      WHEN 4. lv_val = alu_inc8( get_high( mv_bc ) ). mv_bc = set_high( iv_pair = mv_bc iv_val = lv_val ). rv_cycles = 4.  " INC B

      WHEN 5. lv_val = alu_dec8( get_high( mv_bc ) ). mv_bc = set_high( iv_pair = mv_bc iv_val = lv_val ). rv_cycles = 4.  " DEC B

      WHEN 6. mv_bc = set_high( iv_pair = mv_bc iv_val = fetch8( ) ). rv_cycles = 7.  " LD B,n

      WHEN 7.  " RLCA
        lv_a = get_high( mv_af ).
        lv_val = lv_a DIV 128.
        lv_a = ( ( lv_a * 2 ) MOD 256 ) + lv_val.
        mv_af = set_high( iv_pair = mv_af iv_val = lv_a ).
        lv_f = mv_af MOD 256.
        lv_f = lv_f - ( lv_f MOD 2 ) + lv_val.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 4.

      WHEN 8.  " EX AF,AF'
        lv_temp = mv_af. mv_af = mv_af_alt. mv_af_alt = lv_temp.
        rv_cycles = 4.

      WHEN 9.  " ADD HL,BC
        lv_val = mv_hl + mv_bc.
        lv_f = mv_af MOD 256.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        lv_f = lv_f - ( lv_f MOD 2 ).
        IF lv_val > 65535. lv_f = lv_f + c_flag_c. ENDIF.
        IF ( mv_hl MOD 4096 ) + ( mv_bc MOD 4096 ) > 4095. lv_f = lv_f + c_flag_h. ENDIF.
        mv_hl = lv_val MOD 65536.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 11.

      WHEN 10. mv_af = set_high( iv_pair = mv_af iv_val = read8( mv_bc ) ). rv_cycles = 7.  " LD A,(BC)

      WHEN 11. mv_bc = mv_bc - 1. IF mv_bc < 0. mv_bc = mv_bc + 65536. ENDIF. rv_cycles = 6.  " DEC BC

      WHEN 12. lv_val = alu_inc8( get_low( mv_bc ) ). mv_bc = set_low( iv_pair = mv_bc iv_val = lv_val ). rv_cycles = 4.  " INC C

      WHEN 13. lv_val = alu_dec8( get_low( mv_bc ) ). mv_bc = set_low( iv_pair = mv_bc iv_val = lv_val ). rv_cycles = 4.  " DEC C

      WHEN 14. mv_bc = set_low( iv_pair = mv_bc iv_val = fetch8( ) ). rv_cycles = 7.  " LD C,n

      WHEN 15.  " RRCA
        lv_a = get_high( mv_af ).
        lv_val = lv_a MOD 2.
        lv_a = ( lv_a DIV 2 ) + lv_val * 128.
        mv_af = set_high( iv_pair = mv_af iv_val = lv_a ).
        lv_f = mv_af MOD 256.
        lv_f = lv_f - ( lv_f MOD 2 ) + lv_val.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 4.

      WHEN 16.  " DJNZ e
        lv_offset = fetch8( ).
        IF lv_offset >= 128. lv_offset = lv_offset - 256. ENDIF.
        lv_val = get_high( mv_bc ) - 1.
        IF lv_val < 0. lv_val = lv_val + 256. ENDIF.
        mv_bc = set_high( iv_pair = mv_bc iv_val = lv_val ).
        IF lv_val <> 0.
          mv_pc = ( mv_pc + lv_offset ) MOD 65536.
          IF mv_pc < 0. mv_pc = mv_pc + 65536. ENDIF.
          rv_cycles = 13.
        ELSE.
          rv_cycles = 8.
        ENDIF.

      WHEN 17. mv_de = fetch16( ). rv_cycles = 10.  " LD DE,nn

      WHEN 18. write8( iv_addr = mv_de iv_val = get_high( mv_af ) ). rv_cycles = 7.  " LD (DE),A

      WHEN 19. mv_de = ( mv_de + 1 ) MOD 65536. rv_cycles = 6.  " INC DE

      WHEN 20. lv_val = alu_inc8( get_high( mv_de ) ). mv_de = set_high( iv_pair = mv_de iv_val = lv_val ). rv_cycles = 4.  " INC D

      WHEN 21. lv_val = alu_dec8( get_high( mv_de ) ). mv_de = set_high( iv_pair = mv_de iv_val = lv_val ). rv_cycles = 4.  " DEC D

      WHEN 22. mv_de = set_high( iv_pair = mv_de iv_val = fetch8( ) ). rv_cycles = 7.  " LD D,n

      WHEN 23.  " RLA
        lv_a = get_high( mv_af ).
        lv_old_c = mv_af MOD 256 MOD 2.
        lv_val = lv_a DIV 128.
        lv_a = ( ( lv_a * 2 ) MOD 256 ) + lv_old_c.
        mv_af = set_high( iv_pair = mv_af iv_val = lv_a ).
        lv_f = mv_af MOD 256.
        lv_f = lv_f - ( lv_f MOD 2 ) + lv_val.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 4.

      WHEN 24.  " JR e
        lv_offset = fetch8( ).
        IF lv_offset >= 128. lv_offset = lv_offset - 256. ENDIF.
        mv_pc = ( mv_pc + lv_offset ) MOD 65536.
        IF mv_pc < 0. mv_pc = mv_pc + 65536. ENDIF.
        rv_cycles = 12.

      WHEN 25.  " ADD HL,DE
        lv_val = mv_hl + mv_de.
        lv_f = mv_af MOD 256.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        lv_f = lv_f - ( lv_f MOD 2 ).
        IF lv_val > 65535. lv_f = lv_f + c_flag_c. ENDIF.
        IF ( mv_hl MOD 4096 ) + ( mv_de MOD 4096 ) > 4095. lv_f = lv_f + c_flag_h. ENDIF.
        mv_hl = lv_val MOD 65536.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 11.

      WHEN 26. mv_af = set_high( iv_pair = mv_af iv_val = read8( mv_de ) ). rv_cycles = 7.  " LD A,(DE)

      WHEN 27. mv_de = mv_de - 1. IF mv_de < 0. mv_de = mv_de + 65536. ENDIF. rv_cycles = 6.  " DEC DE

      WHEN 28. lv_val = alu_inc8( get_low( mv_de ) ). mv_de = set_low( iv_pair = mv_de iv_val = lv_val ). rv_cycles = 4.  " INC E

      WHEN 29. lv_val = alu_dec8( get_low( mv_de ) ). mv_de = set_low( iv_pair = mv_de iv_val = lv_val ). rv_cycles = 4.  " DEC E

      WHEN 30. mv_de = set_low( iv_pair = mv_de iv_val = fetch8( ) ). rv_cycles = 7.  " LD E,n

      WHEN 31.  " RRA
        lv_a = get_high( mv_af ).
        lv_old_c = mv_af MOD 256 MOD 2.
        lv_val = lv_a MOD 2.
        lv_a = ( lv_a DIV 2 ) + lv_old_c * 128.
        mv_af = set_high( iv_pair = mv_af iv_val = lv_a ).
        lv_f = mv_af MOD 256.
        lv_f = lv_f - ( lv_f MOD 2 ) + lv_val.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 4.

      WHEN 32.  " JR NZ,e
        lv_offset = fetch8( ).
        IF lv_offset >= 128. lv_offset = lv_offset - 256. ENDIF.
        IF get_flag( c_flag_z ) = abap_false.
          mv_pc = ( mv_pc + lv_offset ) MOD 65536.
          IF mv_pc < 0. mv_pc = mv_pc + 65536. ENDIF.
          rv_cycles = 12.
        ELSE.
          rv_cycles = 7.
        ENDIF.

      WHEN 33. mv_hl = fetch16( ). rv_cycles = 10.  " LD HL,nn

      WHEN 34. lv_addr = fetch16( ). write16( iv_addr = lv_addr iv_val = mv_hl ). rv_cycles = 16.  " LD (nn),HL

      WHEN 35. mv_hl = ( mv_hl + 1 ) MOD 65536. rv_cycles = 6.  " INC HL

      WHEN 36. lv_val = alu_inc8( get_high( mv_hl ) ). mv_hl = set_high( iv_pair = mv_hl iv_val = lv_val ). rv_cycles = 4.  " INC H

      WHEN 37. lv_val = alu_dec8( get_high( mv_hl ) ). mv_hl = set_high( iv_pair = mv_hl iv_val = lv_val ). rv_cycles = 4.  " DEC H

      WHEN 38. mv_hl = set_high( iv_pair = mv_hl iv_val = fetch8( ) ). rv_cycles = 7.  " LD H,n

      WHEN 39.  " DAA
        lv_a = get_high( mv_af ).
        lv_f = mv_af MOD 256.
        DATA(lv_c_flag) = lv_f MOD 2.
        DATA(lv_n_flag) = ( lv_f DIV 2 ) MOD 2.
        DATA(lv_h_flag) = ( lv_f DIV 16 ) MOD 2.
        DATA lv_correction TYPE i VALUE 0.
        IF lv_h_flag = 1 OR ( lv_a MOD 16 ) > 9.
          lv_correction = lv_correction + 6.
        ENDIF.
        IF lv_c_flag = 1 OR lv_a > 153.
          lv_correction = lv_correction + 96.
          lv_c_flag = 1.
        ENDIF.
        IF lv_n_flag = 1.
          lv_a = lv_a - lv_correction.
        ELSE.
          lv_a = lv_a + lv_correction.
        ENDIF.
        lv_a = lv_a MOD 256.
        IF lv_a < 0. lv_a = lv_a + 256. ENDIF.
        READ TABLE mt_szp_flags INDEX lv_a + 1 INTO lv_flags.
        IF lv_c_flag = 1. lv_flags = lv_flags + c_flag_c. ENDIF.
        IF lv_n_flag = 1. lv_flags = lv_flags + c_flag_n. ENDIF.
        mv_af = lv_a * 256 + lv_flags.
        rv_cycles = 4.

      WHEN 40.  " JR Z,e
        lv_offset = fetch8( ).
        IF lv_offset >= 128. lv_offset = lv_offset - 256. ENDIF.
        IF get_flag( c_flag_z ) = abap_true.
          mv_pc = ( mv_pc + lv_offset ) MOD 65536.
          IF mv_pc < 0. mv_pc = mv_pc + 65536. ENDIF.
          rv_cycles = 12.
        ELSE.
          rv_cycles = 7.
        ENDIF.

      WHEN 41.  " ADD HL,HL
        lv_val = mv_hl + mv_hl.
        lv_f = mv_af MOD 256.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        lv_f = lv_f - ( lv_f MOD 2 ).
        IF lv_val > 65535. lv_f = lv_f + c_flag_c. ENDIF.
        IF ( mv_hl MOD 4096 ) * 2 > 4095. lv_f = lv_f + c_flag_h. ENDIF.
        mv_hl = lv_val MOD 65536.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 11.

      WHEN 42. lv_addr = fetch16( ). mv_hl = read16( lv_addr ). rv_cycles = 16.  " LD HL,(nn)

      WHEN 43. mv_hl = mv_hl - 1. IF mv_hl < 0. mv_hl = mv_hl + 65536. ENDIF. rv_cycles = 6.  " DEC HL

      WHEN 44. lv_val = alu_inc8( get_low( mv_hl ) ). mv_hl = set_low( iv_pair = mv_hl iv_val = lv_val ). rv_cycles = 4.  " INC L

      WHEN 45. lv_val = alu_dec8( get_low( mv_hl ) ). mv_hl = set_low( iv_pair = mv_hl iv_val = lv_val ). rv_cycles = 4.  " DEC L

      WHEN 46. mv_hl = set_low( iv_pair = mv_hl iv_val = fetch8( ) ). rv_cycles = 7.  " LD L,n

      WHEN 47.  " CPL
        lv_val = 255 - get_high( mv_af ).
        mv_af = set_high( iv_pair = mv_af iv_val = lv_val ).
        lv_f = mv_af MOD 256.
        IF lv_f MOD 32 < 16. lv_f = lv_f + 16. ENDIF.
        IF lv_f MOD 4 < 2. lv_f = lv_f + 2. ENDIF.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 4.

      WHEN 48.  " JR NC,e
        lv_offset = fetch8( ).
        IF lv_offset >= 128. lv_offset = lv_offset - 256. ENDIF.
        IF get_flag( c_flag_c ) = abap_false.
          mv_pc = ( mv_pc + lv_offset ) MOD 65536.
          IF mv_pc < 0. mv_pc = mv_pc + 65536. ENDIF.
          rv_cycles = 12.
        ELSE.
          rv_cycles = 7.
        ENDIF.

      WHEN 49. mv_sp = fetch16( ). rv_cycles = 10.  " LD SP,nn

      WHEN 50. lv_addr = fetch16( ). write8( iv_addr = lv_addr iv_val = get_high( mv_af ) ). rv_cycles = 13.  " LD (nn),A

      WHEN 51. mv_sp = ( mv_sp + 1 ) MOD 65536. rv_cycles = 6.  " INC SP

      WHEN 52. lv_val = alu_inc8( read8( mv_hl ) ). write8( iv_addr = mv_hl iv_val = lv_val ). rv_cycles = 11.  " INC (HL)

      WHEN 53. lv_val = alu_dec8( read8( mv_hl ) ). write8( iv_addr = mv_hl iv_val = lv_val ). rv_cycles = 11.  " DEC (HL)

      WHEN 54. write8( iv_addr = mv_hl iv_val = fetch8( ) ). rv_cycles = 10.  " LD (HL),n

      WHEN 55.  " SCF
        lv_f = mv_af MOD 256.
        IF lv_f MOD 2 = 0. lv_f = lv_f + 1. ENDIF.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 4.

      WHEN 56.  " JR C,e
        lv_offset = fetch8( ).
        IF lv_offset >= 128. lv_offset = lv_offset - 256. ENDIF.
        IF get_flag( c_flag_c ) = abap_true.
          mv_pc = ( mv_pc + lv_offset ) MOD 65536.
          IF mv_pc < 0. mv_pc = mv_pc + 65536. ENDIF.
          rv_cycles = 12.
        ELSE.
          rv_cycles = 7.
        ENDIF.

      WHEN 57.  " ADD HL,SP
        lv_val = mv_hl + mv_sp.
        lv_f = mv_af MOD 256.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        lv_f = lv_f - ( lv_f MOD 2 ).
        IF lv_val > 65535. lv_f = lv_f + c_flag_c. ENDIF.
        IF ( mv_hl MOD 4096 ) + ( mv_sp MOD 4096 ) > 4095. lv_f = lv_f + c_flag_h. ENDIF.
        mv_hl = lv_val MOD 65536.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 11.

      WHEN 58. lv_addr = fetch16( ). mv_af = set_high( iv_pair = mv_af iv_val = read8( lv_addr ) ). rv_cycles = 13.  " LD A,(nn)

      WHEN 59. mv_sp = mv_sp - 1. IF mv_sp < 0. mv_sp = mv_sp + 65536. ENDIF. rv_cycles = 6.  " DEC SP

      WHEN 60. lv_val = alu_inc8( get_high( mv_af ) ). mv_af = set_high( iv_pair = mv_af iv_val = lv_val ). rv_cycles = 4.  " INC A

      WHEN 61. lv_val = alu_dec8( get_high( mv_af ) ). mv_af = set_high( iv_pair = mv_af iv_val = lv_val ). rv_cycles = 4.  " DEC A

      WHEN 62. mv_af = set_high( iv_pair = mv_af iv_val = fetch8( ) ). rv_cycles = 7.  " LD A,n

      WHEN 63.  " CCF
        lv_f = mv_af MOD 256.
        lv_old_c = lv_f MOD 2.
        IF lv_old_c = 1. lv_f = lv_f - 1. ELSE. lv_f = lv_f + 1. ENDIF.
        IF lv_f MOD 4 >= 2. lv_f = lv_f - 2. ENDIF.
        IF lv_f MOD 32 >= 16. lv_f = lv_f - 16. ENDIF.
        IF lv_old_c = 1. lv_f = lv_f + 16. ENDIF.
        mv_af = ( mv_af DIV 256 ) * 256 + lv_f.
        rv_cycles = 4.

      " LD r,r' (64-127 except 118=HALT)
      WHEN 64 OR 65 OR 66 OR 67 OR 68 OR 69 OR 70 OR 71 OR
           72 OR 73 OR 74 OR 75 OR 76 OR 77 OR 78 OR 79 OR
           80 OR 81 OR 82 OR 83 OR 84 OR 85 OR 86 OR 87 OR
           88 OR 89 OR 90 OR 91 OR 92 OR 93 OR 94 OR 95 OR
           96 OR 97 OR 98 OR 99 OR 100 OR 101 OR 102 OR 103 OR
           104 OR 105 OR 106 OR 107 OR 108 OR 109 OR 110 OR 111 OR
           112 OR 113 OR 114 OR 115 OR 116 OR 117 OR 119 OR
           120 OR 121 OR 122 OR 123 OR 124 OR 125 OR 126 OR 127.
        lv_dst = ( iv_opcode - 64 ) DIV 8.
        lv_src = ( iv_opcode - 64 ) MOD 8.
        lv_val = get_reg8( lv_src ).
        set_reg8( iv_idx = lv_dst iv_val = lv_val ).
        IF lv_src = 6 OR lv_dst = 6. rv_cycles = 7. ELSE. rv_cycles = 4. ENDIF.

      WHEN 118. mv_halted = abap_true. rv_cycles = 4.  " HALT

      " ALU A,r (128-191)
      WHEN 128 OR 129 OR 130 OR 131 OR 132 OR 133 OR 134 OR 135.
        lv_src = iv_opcode - 128.
        alu_add8( get_reg8( lv_src ) ).
        IF lv_src = 6. rv_cycles = 7. ELSE. rv_cycles = 4. ENDIF.

      WHEN 136 OR 137 OR 138 OR 139 OR 140 OR 141 OR 142 OR 143.
        lv_src = iv_opcode - 136.
        alu_add8( iv_val = get_reg8( lv_src ) iv_with_carry = abap_true ).
        IF lv_src = 6. rv_cycles = 7. ELSE. rv_cycles = 4. ENDIF.

      WHEN 144 OR 145 OR 146 OR 147 OR 148 OR 149 OR 150 OR 151.
        lv_src = iv_opcode - 144.
        alu_sub8( get_reg8( lv_src ) ).
        IF lv_src = 6. rv_cycles = 7. ELSE. rv_cycles = 4. ENDIF.

      WHEN 152 OR 153 OR 154 OR 155 OR 156 OR 157 OR 158 OR 159.
        lv_src = iv_opcode - 152.
        alu_sub8( iv_val = get_reg8( lv_src ) iv_with_carry = abap_true ).
        IF lv_src = 6. rv_cycles = 7. ELSE. rv_cycles = 4. ENDIF.

      WHEN 160 OR 161 OR 162 OR 163 OR 164 OR 165 OR 166 OR 167.
        lv_src = iv_opcode - 160.
        alu_and8( get_reg8( lv_src ) ).
        IF lv_src = 6. rv_cycles = 7. ELSE. rv_cycles = 4. ENDIF.

      WHEN 168 OR 169 OR 170 OR 171 OR 172 OR 173 OR 174 OR 175.
        lv_src = iv_opcode - 168.
        alu_xor8( get_reg8( lv_src ) ).
        IF lv_src = 6. rv_cycles = 7. ELSE. rv_cycles = 4. ENDIF.

      WHEN 176 OR 177 OR 178 OR 179 OR 180 OR 181 OR 182 OR 183.
        lv_src = iv_opcode - 176.
        alu_or8( get_reg8( lv_src ) ).
        IF lv_src = 6. rv_cycles = 7. ELSE. rv_cycles = 4. ENDIF.

      WHEN 184 OR 185 OR 186 OR 187 OR 188 OR 189 OR 190 OR 191.
        lv_src = iv_opcode - 184.
        alu_cp8( get_reg8( lv_src ) ).
        IF lv_src = 6. rv_cycles = 7. ELSE. rv_cycles = 4. ENDIF.

      WHEN 192. IF check_condition( 0 ). mv_pc = pop16( ). rv_cycles = 11. ELSE. rv_cycles = 5. ENDIF.  " RET NZ
      WHEN 193. mv_bc = pop16( ). rv_cycles = 10.  " POP BC
      WHEN 194. lv_addr = fetch16( ). IF check_condition( 0 ). mv_pc = lv_addr. ENDIF. rv_cycles = 10.  " JP NZ,nn
      WHEN 195. mv_pc = fetch16( ). rv_cycles = 10.  " JP nn
      WHEN 196. lv_addr = fetch16( ). IF check_condition( 0 ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17. ELSE. rv_cycles = 10. ENDIF.  " CALL NZ,nn
      WHEN 197. push16( mv_bc ). rv_cycles = 11.  " PUSH BC
      WHEN 198. alu_add8( fetch8( ) ). rv_cycles = 7.  " ADD A,n
      WHEN 199. push16( mv_pc ). mv_pc = 0. rv_cycles = 11.  " RST 00
      WHEN 200. IF check_condition( 1 ). mv_pc = pop16( ). rv_cycles = 11. ELSE. rv_cycles = 5. ENDIF.  " RET Z
      WHEN 201. mv_pc = pop16( ). rv_cycles = 10.  " RET
      WHEN 202. lv_addr = fetch16( ). IF check_condition( 1 ). mv_pc = lv_addr. ENDIF. rv_cycles = 10.  " JP Z,nn
      WHEN 203. rv_cycles = mo_cb_handler->execute( ).  " CB prefix
      WHEN 204. lv_addr = fetch16( ). IF check_condition( 1 ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17. ELSE. rv_cycles = 10. ENDIF.  " CALL Z,nn
      WHEN 205. lv_addr = fetch16( ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17.  " CALL nn
      WHEN 206. alu_add8( iv_val = fetch8( ) iv_with_carry = abap_true ). rv_cycles = 7.  " ADC A,n
      WHEN 207. push16( mv_pc ). mv_pc = 8. rv_cycles = 11.  " RST 08
      WHEN 208. IF check_condition( 2 ). mv_pc = pop16( ). rv_cycles = 11. ELSE. rv_cycles = 5. ENDIF.  " RET NC
      WHEN 209. mv_de = pop16( ). rv_cycles = 10.  " POP DE
      WHEN 210. lv_addr = fetch16( ). IF check_condition( 2 ). mv_pc = lv_addr. ENDIF. rv_cycles = 10.  " JP NC,nn
      WHEN 211. lv_val = fetch8( ). mo_bus->write_io( iv_port = lv_val iv_val = get_high( mv_af ) ). rv_cycles = 11.  " OUT (n),A
      WHEN 212. lv_addr = fetch16( ). IF check_condition( 2 ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17. ELSE. rv_cycles = 10. ENDIF.  " CALL NC,nn
      WHEN 213. push16( mv_de ). rv_cycles = 11.  " PUSH DE
      WHEN 214. alu_sub8( fetch8( ) ). rv_cycles = 7.  " SUB n
      WHEN 215. push16( mv_pc ). mv_pc = 16. rv_cycles = 11.  " RST 10
      WHEN 216. IF check_condition( 3 ). mv_pc = pop16( ). rv_cycles = 11. ELSE. rv_cycles = 5. ENDIF.  " RET C
      WHEN 217. lv_temp = mv_bc. mv_bc = mv_bc_alt. mv_bc_alt = lv_temp. lv_temp = mv_de. mv_de = mv_de_alt. mv_de_alt = lv_temp. lv_temp = mv_hl. mv_hl = mv_hl_alt. mv_hl_alt = lv_temp. rv_cycles = 4.  " EXX
      WHEN 218. lv_addr = fetch16( ). IF check_condition( 3 ). mv_pc = lv_addr. ENDIF. rv_cycles = 10.  " JP C,nn
      WHEN 219. lv_val = fetch8( ). mv_af = set_high( iv_pair = mv_af iv_val = mo_bus->read_io( lv_val ) ). rv_cycles = 11.  " IN A,(n)
      WHEN 220. lv_addr = fetch16( ). IF check_condition( 3 ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17. ELSE. rv_cycles = 10. ENDIF.  " CALL C,nn
      WHEN 221. rv_cycles = mo_dd_handler->execute( ).  " DD prefix (IX)
      WHEN 222. alu_sub8( iv_val = fetch8( ) iv_with_carry = abap_true ). rv_cycles = 7.  " SBC A,n
      WHEN 223. push16( mv_pc ). mv_pc = 24. rv_cycles = 11.  " RST 18
      WHEN 224. IF check_condition( 4 ). mv_pc = pop16( ). rv_cycles = 11. ELSE. rv_cycles = 5. ENDIF.  " RET PO
      WHEN 225. mv_hl = pop16( ). rv_cycles = 10.  " POP HL
      WHEN 226. lv_addr = fetch16( ). IF check_condition( 4 ). mv_pc = lv_addr. ENDIF. rv_cycles = 10.  " JP PO,nn
      WHEN 227. lv_temp = read16( mv_sp ). write16( iv_addr = mv_sp iv_val = mv_hl ). mv_hl = lv_temp. rv_cycles = 19.  " EX (SP),HL
      WHEN 228. lv_addr = fetch16( ). IF check_condition( 4 ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17. ELSE. rv_cycles = 10. ENDIF.  " CALL PO,nn
      WHEN 229. push16( mv_hl ). rv_cycles = 11.  " PUSH HL
      WHEN 230. alu_and8( fetch8( ) ). rv_cycles = 7.  " AND n
      WHEN 231. push16( mv_pc ). mv_pc = 32. rv_cycles = 11.  " RST 20
      WHEN 232. IF check_condition( 5 ). mv_pc = pop16( ). rv_cycles = 11. ELSE. rv_cycles = 5. ENDIF.  " RET PE
      WHEN 233. mv_pc = mv_hl. rv_cycles = 4.  " JP (HL)
      WHEN 234. lv_addr = fetch16( ). IF check_condition( 5 ). mv_pc = lv_addr. ENDIF. rv_cycles = 10.  " JP PE,nn
      WHEN 235. lv_temp = mv_de. mv_de = mv_hl. mv_hl = lv_temp. rv_cycles = 4.  " EX DE,HL
      WHEN 236. lv_addr = fetch16( ). IF check_condition( 5 ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17. ELSE. rv_cycles = 10. ENDIF.  " CALL PE,nn
      WHEN 237. rv_cycles = mo_ed_handler->execute( ).  " ED prefix (extended)
      WHEN 238. alu_xor8( fetch8( ) ). rv_cycles = 7.  " XOR n
      WHEN 239. push16( mv_pc ). mv_pc = 40. rv_cycles = 11.  " RST 28
      WHEN 240. IF check_condition( 6 ). mv_pc = pop16( ). rv_cycles = 11. ELSE. rv_cycles = 5. ENDIF.  " RET P
      WHEN 241. mv_af = pop16( ). rv_cycles = 10.  " POP AF
      WHEN 242. lv_addr = fetch16( ). IF check_condition( 6 ). mv_pc = lv_addr. ENDIF. rv_cycles = 10.  " JP P,nn
      WHEN 243. mv_iff1 = abap_false. mv_iff2 = abap_false. rv_cycles = 4.  " DI
      WHEN 244. lv_addr = fetch16( ). IF check_condition( 6 ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17. ELSE. rv_cycles = 10. ENDIF.  " CALL P,nn
      WHEN 245. push16( mv_af ). rv_cycles = 11.  " PUSH AF
      WHEN 246. alu_or8( fetch8( ) ). rv_cycles = 7.  " OR n
      WHEN 247. push16( mv_pc ). mv_pc = 48. rv_cycles = 11.  " RST 30
      WHEN 248. IF check_condition( 7 ). mv_pc = pop16( ). rv_cycles = 11. ELSE. rv_cycles = 5. ENDIF.  " RET M
      WHEN 249. mv_sp = mv_hl. rv_cycles = 6.  " LD SP,HL
      WHEN 250. lv_addr = fetch16( ). IF check_condition( 7 ). mv_pc = lv_addr. ENDIF. rv_cycles = 10.  " JP M,nn
      WHEN 251. mv_iff1 = abap_true. mv_iff2 = abap_true. rv_cycles = 4.  " EI
      WHEN 252. lv_addr = fetch16( ). IF check_condition( 7 ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17. ELSE. rv_cycles = 10. ENDIF.  " CALL M,nn
      WHEN 253. rv_cycles = mo_fd_handler->execute( ).  " FD prefix (IY)
      WHEN 254. alu_cp8( fetch8( ) ). rv_cycles = 7.  " CP n
      WHEN 255. push16( mv_pc ). mv_pc = 56. rv_cycles = 11.  " RST 38

      WHEN OTHERS. rv_cycles = 4.
    ENDCASE.
  ENDMETHOD.


  METHOD run.
    DATA lv_cycles TYPE i.
    lv_cycles = 0.
    WHILE mv_running = abap_true AND lv_cycles < iv_max_cycles.
      lv_cycles = lv_cycles + step( ).
    ENDWHILE.
  ENDMETHOD.


  METHOD nmi.
    mv_halted = abap_false.
    mv_iff2 = mv_iff1.
    mv_iff1 = abap_false.
    push16( mv_pc ).
    mv_pc = 102.
    mv_cycles = mv_cycles + 11.
  ENDMETHOD.


  METHOD irq.
    IF mv_iff1 = abap_false.
      RETURN.
    ENDIF.
    mv_halted = abap_false.
    mv_iff1 = abap_false.
    mv_iff2 = abap_false.
    CASE mv_im.
      WHEN 0.
        push16( mv_pc ).
        mv_pc = ( iv_data MOD 64 ) * 8.
        mv_cycles = mv_cycles + 13.
      WHEN 1.
        push16( mv_pc ).
        mv_pc = 56.
        mv_cycles = mv_cycles + 13.
      WHEN 2.
        DATA(lv_vector) = mv_i * 256 + iv_data.
        push16( mv_pc ).
        mv_pc = read16( lv_vector ).
        mv_cycles = mv_cycles + 19.
    ENDCASE.
  ENDMETHOD.


  METHOD get_status.
    rs_status-af = mv_af.
    rs_status-bc = mv_bc.
    rs_status-de = mv_de.
    rs_status-hl = mv_hl.
    rs_status-af_alt = mv_af_alt.
    rs_status-bc_alt = mv_bc_alt.
    rs_status-de_alt = mv_de_alt.
    rs_status-hl_alt = mv_hl_alt.
    rs_status-ix = mv_ix.
    rs_status-iy = mv_iy.
    rs_status-sp = mv_sp.
    rs_status-pc = mv_pc.
    rs_status-i = mv_i.
    rs_status-r = mv_r.
    rs_status-iff1 = mv_iff1.
    rs_status-iff2 = mv_iff2.
    rs_status-im = mv_im.
    rs_status-cycles = mv_cycles.
    rs_status-running = mv_running.
    rs_status-halted = mv_halted.
  ENDMETHOD.


  METHOD is_halted.
    rv_halted = mv_halted.
  ENDMETHOD.


  METHOD provide_input.
    mo_bus->provide_input( iv_text ).
    mv_halted = abap_false.
  ENDMETHOD.


  METHOD get_pc.
    rv_pc = mv_pc.
  ENDMETHOD.


  METHOD get_sp.
    rv_sp = mv_sp.
  ENDMETHOD.


  METHOD get_af.
    rv_af = mv_af.
  ENDMETHOD.


  METHOD get_bc.
    rv_bc = mv_bc.
  ENDMETHOD.


  METHOD get_de.
    rv_de = mv_de.
  ENDMETHOD.


  METHOD get_hl.
    rv_hl = mv_hl.
  ENDMETHOD.


  METHOD zif_cpu_z80_core~read_mem.
    rv_val = read8( iv_addr ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~write_mem.
    write8( iv_addr = iv_addr iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~read_io.
    rv_val = mo_bus->read_io( iv_port ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~write_io.
    mo_bus->write_io( iv_port = iv_port iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~fetch_byte.
    rv_byte = fetch8( ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~fetch_word.
    rv_word = fetch16( ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_af.
    rv_val = mv_af.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_af.
    mv_af = iv_val MOD 65536.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_bc.
    rv_val = mv_bc.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_bc.
    mv_bc = iv_val MOD 65536.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_de.
    rv_val = mv_de.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_de.
    mv_de = iv_val MOD 65536.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_hl.
    rv_val = mv_hl.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_hl.
    mv_hl = iv_val MOD 65536.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_ix.
    rv_val = mv_ix.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_ix.
    mv_ix = iv_val MOD 65536.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_iy.
    rv_val = mv_iy.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_iy.
    mv_iy = iv_val MOD 65536.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_sp.
    rv_val = mv_sp.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_sp.
    mv_sp = iv_val MOD 65536.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_pc.
    rv_val = mv_pc.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_pc.
    mv_pc = iv_val MOD 65536.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_i.
    rv_val = mv_i.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_i.
    mv_i = iv_val MOD 256.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_r.
    rv_val = mv_r.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_r.
    mv_r = iv_val MOD 256.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_iff1.
    rv_val = mv_iff1.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_iff1.
    mv_iff1 = iv_val.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_iff2.
    rv_val = mv_iff2.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_iff2.
    mv_iff2 = iv_val.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_im.
    rv_val = mv_im.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_im.
    mv_im = iv_val MOD 3.
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_a.
    rv_val = get_high( mv_af ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_a.
    mv_af = set_high( iv_pair = mv_af iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_f.
    rv_val = get_low( mv_af ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_f.
    mv_af = set_low( iv_pair = mv_af iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_b.
    rv_val = get_high( mv_bc ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_b.
    mv_bc = set_high( iv_pair = mv_bc iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_c.
    rv_val = get_low( mv_bc ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_c.
    mv_bc = set_low( iv_pair = mv_bc iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_d.
    rv_val = get_high( mv_de ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_d.
    mv_de = set_high( iv_pair = mv_de iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_e.
    rv_val = get_low( mv_de ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_e.
    mv_de = set_low( iv_pair = mv_de iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_h.
    rv_val = get_high( mv_hl ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_h.
    mv_hl = set_high( iv_pair = mv_hl iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_l.
    rv_val = get_low( mv_hl ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_l.
    mv_hl = set_low( iv_pair = mv_hl iv_val = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~push.
    push16( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~pop.
    rv_val = pop16( ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~set_flag.
    set_flag( iv_flag = iv_flag iv_set = iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~get_flag.
    rv_val = get_flag( iv_flag ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_add8.
    rv_result = alu_add8( iv_val = iv_val iv_with_carry = COND #( WHEN iv_carry > 0 THEN abap_true ELSE abap_false ) ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_sub8.
    rv_result = alu_sub8( iv_val = iv_val iv_with_carry = COND #( WHEN iv_carry > 0 THEN abap_true ELSE abap_false ) ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_and8.
    alu_and8( iv_val ).
    rv_result = get_high( mv_af ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_or8.
    alu_or8( iv_val ).
    rv_result = get_high( mv_af ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_xor8.
    alu_xor8( iv_val ).
    rv_result = get_high( mv_af ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_cp8.
    alu_cp8( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_inc8.
    rv_result = alu_inc8( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_dec8.
    rv_result = alu_dec8( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_rlc.
    rv_result = alu_rlc( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_rrc.
    rv_result = alu_rrc( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_rl.
    rv_result = alu_rl( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_rr.
    rv_result = alu_rr( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_sla.
    rv_result = alu_sla( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_sra.
    rv_result = alu_sra( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~alu_srl.
    rv_result = alu_srl( iv_val ).
  ENDMETHOD.

  METHOD zif_cpu_z80_core~add_cycles.
    mv_cycles = mv_cycles + iv_cycles.
  ENDMETHOD.

ENDCLASS.
