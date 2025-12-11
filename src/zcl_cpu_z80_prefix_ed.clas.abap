CLASS zcl_cpu_z80_prefix_ed DEFINITION PUBLIC FINAL CREATE PUBLIC.

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

    METHODS init_flag_tables.

ENDCLASS.


CLASS zcl_cpu_z80_prefix_ed IMPLEMENTATION.

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

      " SZP flags (Sign + Zero + Parity)
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


  METHOD zif_cpu_z80_prefix~execute.
    DATA: lv_op     TYPE i,
          lv_addr   TYPE i,
          lv_val    TYPE i,
          lv_result TYPE i,
          lv_flags  TYPE i,
          lv_bc     TYPE i,
          lv_de     TYPE i,
          lv_hl     TYPE i,
          lv_a      TYPE i,
          lv_f      TYPE i.

    lv_op = mo_cpu->fetch_byte( ).

    CASE lv_op.
      " IN r,(C) - Input from port C to register
      WHEN 64.  " IN B,(C)
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->set_b( lv_val ).
        READ TABLE mt_szp_flags INDEX lv_val + 1 INTO lv_flags.
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f MOD 2 ) + lv_flags.  " Preserve C flag
        mo_cpu->set_f( lv_f ).
        rv_cycles = 12.

      WHEN 72.  " IN C,(C)
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->set_c( lv_val ).
        READ TABLE mt_szp_flags INDEX lv_val + 1 INTO lv_flags.
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f MOD 2 ) + lv_flags.
        mo_cpu->set_f( lv_f ).
        rv_cycles = 12.

      WHEN 80.  " IN D,(C)
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->set_d( lv_val ).
        READ TABLE mt_szp_flags INDEX lv_val + 1 INTO lv_flags.
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f MOD 2 ) + lv_flags.
        mo_cpu->set_f( lv_f ).
        rv_cycles = 12.

      WHEN 88.  " IN E,(C)
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->set_e( lv_val ).
        READ TABLE mt_szp_flags INDEX lv_val + 1 INTO lv_flags.
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f MOD 2 ) + lv_flags.
        mo_cpu->set_f( lv_f ).
        rv_cycles = 12.

      WHEN 96.  " IN H,(C)
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->set_h( lv_val ).
        READ TABLE mt_szp_flags INDEX lv_val + 1 INTO lv_flags.
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f MOD 2 ) + lv_flags.
        mo_cpu->set_f( lv_f ).
        rv_cycles = 12.

      WHEN 104.  " IN L,(C)
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->set_l( lv_val ).
        READ TABLE mt_szp_flags INDEX lv_val + 1 INTO lv_flags.
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f MOD 2 ) + lv_flags.
        mo_cpu->set_f( lv_f ).
        rv_cycles = 12.

      WHEN 120.  " IN A,(C)
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->set_a( lv_val ).
        READ TABLE mt_szp_flags INDEX lv_val + 1 INTO lv_flags.
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f MOD 2 ) + lv_flags.
        mo_cpu->set_f( lv_f ).
        rv_cycles = 12.

      " OUT (C),r - Output register to port C
      WHEN 65. mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = mo_cpu->get_b( ) ). rv_cycles = 12.
      WHEN 73. mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = mo_cpu->get_c( ) ). rv_cycles = 12.
      WHEN 81. mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = mo_cpu->get_d( ) ). rv_cycles = 12.
      WHEN 89. mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = mo_cpu->get_e( ) ). rv_cycles = 12.
      WHEN 97. mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = mo_cpu->get_h( ) ). rv_cycles = 12.
      WHEN 105. mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = mo_cpu->get_l( ) ). rv_cycles = 12.
      WHEN 121. mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = mo_cpu->get_a( ) ). rv_cycles = 12.

      " SBC HL,rr - 16-bit subtract with carry
      WHEN 66.  " SBC HL,BC
        lv_hl = mo_cpu->get_hl( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_f = mo_cpu->get_f( ).
        DATA(lv_carry_in) = lv_f MOD 2.
        lv_result = lv_hl - lv_bc - lv_carry_in.
        DATA(lv_carry) = 0.
        IF lv_result < 0. lv_carry = 1. lv_result = lv_result + 65536. ENDIF.
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_result = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        IF lv_result >= 32768. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s. ENDIF.
        IF lv_carry = 1. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_c. ENDIF.
        DATA(lv_half) = ( lv_hl MOD 4096 ) - ( lv_bc MOD 4096 ) - lv_carry_in.
        IF lv_half < 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_h. ENDIF.
        DATA(lv_ov) = 0.
        IF lv_hl >= 32768 AND lv_bc < 32768 AND lv_result < 32768. lv_ov = 1. ENDIF.
        IF lv_hl < 32768 AND lv_bc >= 32768 AND lv_result >= 32768. lv_ov = 1. ENDIF.
        IF lv_ov = 1. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        mo_cpu->set_hl( lv_result ).
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 15.

      WHEN 74.  " ADC HL,BC
        lv_hl = mo_cpu->get_hl( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_f = mo_cpu->get_f( ).
        lv_carry_in = lv_f MOD 2.
        lv_result = lv_hl + lv_bc + lv_carry_in.
        lv_carry = 0.
        IF lv_result >= 65536. lv_carry = 1. lv_result = lv_result MOD 65536. ENDIF.
        lv_flags = 0.
        IF lv_result = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        IF lv_result >= 32768. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s. ENDIF.
        IF lv_carry = 1. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_c. ENDIF.
        lv_half = ( lv_hl MOD 4096 ) + ( lv_bc MOD 4096 ) + lv_carry_in.
        IF lv_half >= 4096. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_h. ENDIF.
        lv_ov = 0.
        IF lv_hl < 32768 AND lv_bc < 32768 AND lv_result >= 32768. lv_ov = 1. ENDIF.
        IF lv_hl >= 32768 AND lv_bc >= 32768 AND lv_result < 32768. lv_ov = 1. ENDIF.
        IF lv_ov = 1. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        mo_cpu->set_hl( lv_result ).
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 15.

      " LD (nn),rr and LD rr,(nn)
      WHEN 67.  " LD (nn),BC
        lv_addr = mo_cpu->fetch_word( ).
        lv_bc = mo_cpu->get_bc( ).
        mo_cpu->write_mem( iv_addr = lv_addr iv_val = lv_bc MOD 256 ).
        mo_cpu->write_mem( iv_addr = ( lv_addr + 1 ) MOD 65536 iv_val = lv_bc DIV 256 ).
        rv_cycles = 20.

      WHEN 75.  " LD BC,(nn)
        lv_addr = mo_cpu->fetch_word( ).
        lv_val = mo_cpu->read_mem( lv_addr ) + mo_cpu->read_mem( ( lv_addr + 1 ) MOD 65536 ) * 256.
        mo_cpu->set_bc( lv_val ).
        rv_cycles = 20.

      WHEN 83.  " LD (nn),DE
        lv_addr = mo_cpu->fetch_word( ).
        lv_de = mo_cpu->get_de( ).
        mo_cpu->write_mem( iv_addr = lv_addr iv_val = lv_de MOD 256 ).
        mo_cpu->write_mem( iv_addr = ( lv_addr + 1 ) MOD 65536 iv_val = lv_de DIV 256 ).
        rv_cycles = 20.

      WHEN 91.  " LD DE,(nn)
        lv_addr = mo_cpu->fetch_word( ).
        lv_val = mo_cpu->read_mem( lv_addr ) + mo_cpu->read_mem( ( lv_addr + 1 ) MOD 65536 ) * 256.
        mo_cpu->set_de( lv_val ).
        rv_cycles = 20.

      WHEN 99.  " LD (nn),HL (ED version)
        lv_addr = mo_cpu->fetch_word( ).
        lv_hl = mo_cpu->get_hl( ).
        mo_cpu->write_mem( iv_addr = lv_addr iv_val = lv_hl MOD 256 ).
        mo_cpu->write_mem( iv_addr = ( lv_addr + 1 ) MOD 65536 iv_val = lv_hl DIV 256 ).
        rv_cycles = 20.

      WHEN 107.  " LD HL,(nn) (ED version)
        lv_addr = mo_cpu->fetch_word( ).
        lv_val = mo_cpu->read_mem( lv_addr ) + mo_cpu->read_mem( ( lv_addr + 1 ) MOD 65536 ) * 256.
        mo_cpu->set_hl( lv_val ).
        rv_cycles = 20.

      WHEN 115.  " LD (nn),SP
        lv_addr = mo_cpu->fetch_word( ).
        DATA(lv_sp) = mo_cpu->get_sp( ).
        mo_cpu->write_mem( iv_addr = lv_addr iv_val = lv_sp MOD 256 ).
        mo_cpu->write_mem( iv_addr = ( lv_addr + 1 ) MOD 65536 iv_val = lv_sp DIV 256 ).
        rv_cycles = 20.

      WHEN 123.  " LD SP,(nn)
        lv_addr = mo_cpu->fetch_word( ).
        lv_val = mo_cpu->read_mem( lv_addr ) + mo_cpu->read_mem( ( lv_addr + 1 ) MOD 65536 ) * 256.
        mo_cpu->set_sp( lv_val ).
        rv_cycles = 20.

      " NEG - Negate A
      WHEN 68 OR 76 OR 84 OR 92 OR 100 OR 108 OR 116 OR 124.
        lv_a = mo_cpu->get_a( ).
        lv_result = ( 256 - lv_a ) MOD 256.
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_result = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        IF lv_result >= 128. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s. ENDIF.
        IF lv_a <> 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_c. ENDIF.
        IF lv_a = 128. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        IF ( lv_a MOD 16 ) <> 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_h. ENDIF.
        mo_cpu->set_a( lv_result ).
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 8.

      " RETN/RETI
      WHEN 69 OR 85 OR 93 OR 101 OR 109 OR 117 OR 125.  " RETN
        mo_cpu->set_pc( mo_cpu->pop( ) ).
        mo_cpu->set_iff1( mo_cpu->get_iff2( ) ).
        rv_cycles = 14.

      WHEN 77.  " RETI
        mo_cpu->set_pc( mo_cpu->pop( ) ).
        rv_cycles = 14.

      " IM 0/1/2 - Set interrupt mode
      WHEN 70 OR 78 OR 102 OR 110.  " IM 0
        mo_cpu->set_im( 0 ).
        rv_cycles = 8.

      WHEN 86 OR 118.  " IM 1
        mo_cpu->set_im( 1 ).
        rv_cycles = 8.

      WHEN 94 OR 126.  " IM 2
        mo_cpu->set_im( 2 ).
        rv_cycles = 8.

      " LD I,A and LD R,A
      WHEN 71.  " LD I,A
        mo_cpu->set_i( mo_cpu->get_a( ) ).
        rv_cycles = 9.

      WHEN 79.  " LD R,A
        mo_cpu->set_r( mo_cpu->get_a( ) ).
        rv_cycles = 9.

      " LD A,I and LD A,R
      WHEN 87.  " LD A,I
        lv_val = mo_cpu->get_i( ).
        mo_cpu->set_a( lv_val ).
        lv_flags = 0.
        IF lv_val = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        IF lv_val >= 128. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s. ENDIF.
        IF mo_cpu->get_iff2( ) = abap_true. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        lv_f = mo_cpu->get_f( ).
        lv_flags = lv_flags + ( lv_f MOD 2 ).  " Preserve C
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 9.

      WHEN 95.  " LD A,R
        lv_val = mo_cpu->get_r( ).
        mo_cpu->set_a( lv_val ).
        lv_flags = 0.
        IF lv_val = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        IF lv_val >= 128. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s. ENDIF.
        IF mo_cpu->get_iff2( ) = abap_true. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        lv_f = mo_cpu->get_f( ).
        lv_flags = lv_flags + ( lv_f MOD 2 ).  " Preserve C
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 9.

      " RRD and RLD - Rotate decimal
      WHEN 103.  " RRD
        lv_a = mo_cpu->get_a( ).
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        lv_result = ( lv_a MOD 16 ) * 16 + ( lv_val DIV 16 ).
        mo_cpu->write_mem( iv_addr = lv_hl iv_val = lv_result ).
        lv_a = ( lv_a DIV 16 ) * 16 + ( lv_val MOD 16 ).
        mo_cpu->set_a( lv_a ).
        READ TABLE mt_szp_flags INDEX lv_a + 1 INTO lv_flags.
        lv_f = mo_cpu->get_f( ).
        lv_flags = lv_flags + ( lv_f MOD 2 ).  " Preserve C
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 18.

      WHEN 111.  " RLD
        lv_a = mo_cpu->get_a( ).
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        lv_result = ( lv_val MOD 16 ) * 16 + ( lv_a MOD 16 ).
        mo_cpu->write_mem( iv_addr = lv_hl iv_val = lv_result ).
        lv_a = ( lv_a DIV 16 ) * 16 + ( lv_val DIV 16 ).
        mo_cpu->set_a( lv_a ).
        READ TABLE mt_szp_flags INDEX lv_a + 1 INTO lv_flags.
        lv_f = mo_cpu->get_f( ).
        lv_flags = lv_flags + ( lv_f MOD 2 ).  " Preserve C
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 18.

      " Block transfer instructions
      WHEN 160.  " LDI
        lv_hl = mo_cpu->get_hl( ).
        lv_de = mo_cpu->get_de( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        mo_cpu->write_mem( iv_addr = lv_de iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 1 ) MOD 65536 ).
        mo_cpu->set_de( ( lv_de + 1 ) MOD 65536 ).
        lv_bc = ( lv_bc + 65535 ) MOD 65536.
        mo_cpu->set_bc( lv_bc ).
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f DIV 64 ) * 64 + ( lv_f MOD 2 ).  " Clear H, P/V, N; keep S, Z, C
        IF lv_bc <> 0. lv_f = lv_f + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        mo_cpu->set_f( lv_f ).
        rv_cycles = 16.

      WHEN 176.  " LDIR
        lv_hl = mo_cpu->get_hl( ).
        lv_de = mo_cpu->get_de( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        mo_cpu->write_mem( iv_addr = lv_de iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 1 ) MOD 65536 ).
        mo_cpu->set_de( ( lv_de + 1 ) MOD 65536 ).
        lv_bc = ( lv_bc + 65535 ) MOD 65536.
        mo_cpu->set_bc( lv_bc ).
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f DIV 64 ) * 64 + ( lv_f MOD 2 ).
        mo_cpu->set_f( lv_f ).
        IF lv_bc <> 0.
          mo_cpu->set_pc( ( mo_cpu->get_pc( ) + 65534 ) MOD 65536 ).  " PC -= 2
          rv_cycles = 21.
        ELSE.
          rv_cycles = 16.
        ENDIF.

      WHEN 168.  " LDD
        lv_hl = mo_cpu->get_hl( ).
        lv_de = mo_cpu->get_de( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        mo_cpu->write_mem( iv_addr = lv_de iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 65535 ) MOD 65536 ).
        mo_cpu->set_de( ( lv_de + 65535 ) MOD 65536 ).
        lv_bc = ( lv_bc + 65535 ) MOD 65536.
        mo_cpu->set_bc( lv_bc ).
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f DIV 64 ) * 64 + ( lv_f MOD 2 ).
        IF lv_bc <> 0. lv_f = lv_f + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        mo_cpu->set_f( lv_f ).
        rv_cycles = 16.

      WHEN 184.  " LDDR
        lv_hl = mo_cpu->get_hl( ).
        lv_de = mo_cpu->get_de( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        mo_cpu->write_mem( iv_addr = lv_de iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 65535 ) MOD 65536 ).
        mo_cpu->set_de( ( lv_de + 65535 ) MOD 65536 ).
        lv_bc = ( lv_bc + 65535 ) MOD 65536.
        mo_cpu->set_bc( lv_bc ).
        lv_f = mo_cpu->get_f( ).
        lv_f = ( lv_f DIV 64 ) * 64 + ( lv_f MOD 2 ).
        mo_cpu->set_f( lv_f ).
        IF lv_bc <> 0.
          mo_cpu->set_pc( ( mo_cpu->get_pc( ) + 65534 ) MOD 65536 ).
          rv_cycles = 21.
        ELSE.
          rv_cycles = 16.
        ENDIF.

      " Block compare instructions
      WHEN 161.  " CPI
        lv_a = mo_cpu->get_a( ).
        lv_hl = mo_cpu->get_hl( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        lv_result = ( lv_a - lv_val + 256 ) MOD 256.
        mo_cpu->set_hl( ( lv_hl + 1 ) MOD 65536 ).
        lv_bc = ( lv_bc + 65535 ) MOD 65536.
        mo_cpu->set_bc( lv_bc ).
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_result = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        IF lv_result >= 128. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s. ENDIF.
        IF lv_bc <> 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        IF ( lv_a MOD 16 ) < ( lv_val MOD 16 ). lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_h. ENDIF.
        lv_f = mo_cpu->get_f( ).
        lv_flags = lv_flags + ( lv_f MOD 2 ).  " Preserve C
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 16.

      WHEN 177.  " CPIR
        lv_a = mo_cpu->get_a( ).
        lv_hl = mo_cpu->get_hl( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        lv_result = ( lv_a - lv_val + 256 ) MOD 256.
        mo_cpu->set_hl( ( lv_hl + 1 ) MOD 65536 ).
        lv_bc = ( lv_bc + 65535 ) MOD 65536.
        mo_cpu->set_bc( lv_bc ).
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_result = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        IF lv_result >= 128. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s. ENDIF.
        IF lv_bc <> 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        IF ( lv_a MOD 16 ) < ( lv_val MOD 16 ). lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_h. ENDIF.
        lv_f = mo_cpu->get_f( ).
        lv_flags = lv_flags + ( lv_f MOD 2 ).
        mo_cpu->set_f( lv_flags ).
        IF lv_bc <> 0 AND lv_result <> 0.
          mo_cpu->set_pc( ( mo_cpu->get_pc( ) + 65534 ) MOD 65536 ).
          rv_cycles = 21.
        ELSE.
          rv_cycles = 16.
        ENDIF.

      WHEN 169.  " CPD
        lv_a = mo_cpu->get_a( ).
        lv_hl = mo_cpu->get_hl( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        lv_result = ( lv_a - lv_val + 256 ) MOD 256.
        mo_cpu->set_hl( ( lv_hl + 65535 ) MOD 65536 ).
        lv_bc = ( lv_bc + 65535 ) MOD 65536.
        mo_cpu->set_bc( lv_bc ).
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_result = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        IF lv_result >= 128. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s. ENDIF.
        IF lv_bc <> 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        IF ( lv_a MOD 16 ) < ( lv_val MOD 16 ). lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_h. ENDIF.
        lv_f = mo_cpu->get_f( ).
        lv_flags = lv_flags + ( lv_f MOD 2 ).
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 16.

      WHEN 185.  " CPDR
        lv_a = mo_cpu->get_a( ).
        lv_hl = mo_cpu->get_hl( ).
        lv_bc = mo_cpu->get_bc( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        lv_result = ( lv_a - lv_val + 256 ) MOD 256.
        mo_cpu->set_hl( ( lv_hl + 65535 ) MOD 65536 ).
        lv_bc = ( lv_bc + 65535 ) MOD 65536.
        mo_cpu->set_bc( lv_bc ).
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_result = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        IF lv_result >= 128. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_s. ENDIF.
        IF lv_bc <> 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_pv. ENDIF.
        IF ( lv_a MOD 16 ) < ( lv_val MOD 16 ). lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_h. ENDIF.
        lv_f = mo_cpu->get_f( ).
        lv_flags = lv_flags + ( lv_f MOD 2 ).
        mo_cpu->set_f( lv_flags ).
        IF lv_bc <> 0 AND lv_result <> 0.
          mo_cpu->set_pc( ( mo_cpu->get_pc( ) + 65534 ) MOD 65536 ).
          rv_cycles = 21.
        ELSE.
          rv_cycles = 16.
        ENDIF.

      " Block I/O instructions
      WHEN 162.  " INI
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->write_mem( iv_addr = lv_hl iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 1 ) MOD 65536 ).
        DATA(lv_b) = mo_cpu->get_b( ).
        lv_b = ( lv_b + 255 ) MOD 256.
        mo_cpu->set_b( lv_b ).
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_b = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 16.

      WHEN 178.  " INIR
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->write_mem( iv_addr = lv_hl iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 1 ) MOD 65536 ).
        lv_b = mo_cpu->get_b( ).
        lv_b = ( lv_b + 255 ) MOD 256.
        mo_cpu->set_b( lv_b ).
        lv_flags = zif_cpu_z80_core=>c_flag_n + zif_cpu_z80_core=>c_flag_z.
        mo_cpu->set_f( lv_flags ).
        IF lv_b <> 0.
          mo_cpu->set_pc( ( mo_cpu->get_pc( ) + 65534 ) MOD 65536 ).
          rv_cycles = 21.
        ELSE.
          rv_cycles = 16.
        ENDIF.

      WHEN 170.  " IND
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->write_mem( iv_addr = lv_hl iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 65535 ) MOD 65536 ).
        lv_b = mo_cpu->get_b( ).
        lv_b = ( lv_b + 255 ) MOD 256.
        mo_cpu->set_b( lv_b ).
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_b = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 16.

      WHEN 186.  " INDR
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_io( mo_cpu->get_c( ) ).
        mo_cpu->write_mem( iv_addr = lv_hl iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 65535 ) MOD 65536 ).
        lv_b = mo_cpu->get_b( ).
        lv_b = ( lv_b + 255 ) MOD 256.
        mo_cpu->set_b( lv_b ).
        lv_flags = zif_cpu_z80_core=>c_flag_n + zif_cpu_z80_core=>c_flag_z.
        mo_cpu->set_f( lv_flags ).
        IF lv_b <> 0.
          mo_cpu->set_pc( ( mo_cpu->get_pc( ) + 65534 ) MOD 65536 ).
          rv_cycles = 21.
        ELSE.
          rv_cycles = 16.
        ENDIF.

      WHEN 163.  " OUTI
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 1 ) MOD 65536 ).
        lv_b = mo_cpu->get_b( ).
        lv_b = ( lv_b + 255 ) MOD 256.
        mo_cpu->set_b( lv_b ).
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_b = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 16.

      WHEN 179.  " OTIR
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 1 ) MOD 65536 ).
        lv_b = mo_cpu->get_b( ).
        lv_b = ( lv_b + 255 ) MOD 256.
        mo_cpu->set_b( lv_b ).
        lv_flags = zif_cpu_z80_core=>c_flag_n + zif_cpu_z80_core=>c_flag_z.
        mo_cpu->set_f( lv_flags ).
        IF lv_b <> 0.
          mo_cpu->set_pc( ( mo_cpu->get_pc( ) + 65534 ) MOD 65536 ).
          rv_cycles = 21.
        ELSE.
          rv_cycles = 16.
        ENDIF.

      WHEN 171.  " OUTD
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 65535 ) MOD 65536 ).
        lv_b = mo_cpu->get_b( ).
        lv_b = ( lv_b + 255 ) MOD 256.
        mo_cpu->set_b( lv_b ).
        lv_flags = zif_cpu_z80_core=>c_flag_n.
        IF lv_b = 0. lv_flags = lv_flags + zif_cpu_z80_core=>c_flag_z. ENDIF.
        mo_cpu->set_f( lv_flags ).
        rv_cycles = 16.

      WHEN 187.  " OTDR
        lv_hl = mo_cpu->get_hl( ).
        lv_val = mo_cpu->read_mem( lv_hl ).
        mo_cpu->write_io( iv_port = mo_cpu->get_c( ) iv_val = lv_val ).
        mo_cpu->set_hl( ( lv_hl + 65535 ) MOD 65536 ).
        lv_b = mo_cpu->get_b( ).
        lv_b = ( lv_b + 255 ) MOD 256.
        mo_cpu->set_b( lv_b ).
        lv_flags = zif_cpu_z80_core=>c_flag_n + zif_cpu_z80_core=>c_flag_z.
        mo_cpu->set_f( lv_flags ).
        IF lv_b <> 0.
          mo_cpu->set_pc( ( mo_cpu->get_pc( ) + 65534 ) MOD 65536 ).
          rv_cycles = 21.
        ELSE.
          rv_cycles = 16.
        ENDIF.

      WHEN OTHERS.
        " Unhandled ED opcode - treat as NOP
        rv_cycles = 8.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
