*&---------------------------------------------------------------------*
*& Z80/i8080 CPU Emulator - Transpiler-Compatible Version
*& Based on RunCPM architecture with hybrid switch/case + lookup tables
*& Memory as XSTRING, BIT operations on proper hex types
*&---------------------------------------------------------------------*
CLASS zcl_cpu_8080_v2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " CPU Status constants
    CONSTANTS:
      c_status_running TYPE i VALUE 0,
      c_status_halted  TYPE i VALUE 1,
      c_status_exit    TYPE i VALUE 2.

    " Flag bit constants (matching Z80 flag register layout)
    CONSTANTS:
      c_flag_c TYPE i VALUE 1,   " Carry
      c_flag_n TYPE i VALUE 2,   " Add/Subtract (BCD)
      c_flag_p TYPE i VALUE 4,   " Parity/Overflow
      c_flag_h TYPE i VALUE 16,  " Half-carry (bit 3->4)
      c_flag_z TYPE i VALUE 64,  " Zero
      c_flag_s TYPE i VALUE 128. " Sign (bit 7)

    METHODS:
      constructor,

      " Main execution
      reset,
      execute_instruction
        RETURNING VALUE(rv_cycles) TYPE i,
      execute_until_halt
        IMPORTING iv_max_instructions TYPE i DEFAULT 100000
        RETURNING VALUE(rv_executed)   TYPE i,

      " Loading programs
      load_com_file
        IMPORTING iv_data TYPE xstring
                  iv_addr TYPE i DEFAULT 256,  " 0x0100 - CP/M .COM load address

      " Debugging/inspection
      get_status RETURNING VALUE(rv_status) TYPE i,
      get_pc     RETURNING VALUE(rv_pc)     TYPE i,
      get_af     RETURNING VALUE(rv_af)     TYPE i,
      get_bc     RETURNING VALUE(rv_bc)     TYPE i,
      get_de     RETURNING VALUE(rv_de)     TYPE i,
      get_hl     RETURNING VALUE(rv_hl)     TYPE i,
      get_sp     RETURNING VALUE(rv_sp)     TYPE i,

      " Memory inspection
      read_memory
        IMPORTING iv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      " For testing - expose methods
      read_byte
        IMPORTING iv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      write_byte
        IMPORTING iv_addr TYPE i
                  iv_val  TYPE i,

      read_word
        IMPORTING iv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      write_word
        IMPORTING iv_addr TYPE i
                  iv_val  TYPE i,

      get_high_byte
        IMPORTING iv_pair       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      get_low_byte
        IMPORTING iv_pair       TYPE i
        RETURNING VALUE(rv_val) TYPE i.

  PRIVATE SECTION.

    " === CPU Registers (16-bit pairs stored as 32-bit integers) ===
    DATA:
      mv_af TYPE i,    " Accumulator (A=high byte) + Flags (F=low byte)
      mv_bc TYPE i,    " BC register pair
      mv_de TYPE i,    " DE register pair
      mv_hl TYPE i,    " HL register pair
      mv_pc TYPE i,    " Program Counter
      mv_sp TYPE i,    " Stack Pointer
      mv_status TYPE i VALUE 0.  " CPU status

    " === Memory (64KB as STRING - transpiler-compatible) ===
    " Each byte = 2 hex chars, so 131072 chars total
    DATA: mv_memory TYPE string.

    " === Pre-computed Lookup Tables (as STRING) ===
    DATA:
      mv_parity_table TYPE string,  " Parity flag for 0..255 (512 hex chars)
      mv_inc_table    TYPE string,  " INC instruction flags (514 hex chars for 257 entries)
      mv_dec_table    TYPE string,  " DEC instruction flags (512 hex chars)
      mv_cbits_table  TYPE string.  " Carry bits (1024 hex chars for 512 entries)

    " === Helper Methods ===
    METHODS:
      " Bit manipulation
      set_high_byte
        IMPORTING iv_pair       TYPE i
                  iv_val        TYPE i
        RETURNING VALUE(rv_new) TYPE i,

      set_low_byte
        IMPORTING iv_pair       TYPE i
                  iv_val        TYPE i
        RETURNING VALUE(rv_new) TYPE i,

      " Flag operations
      set_flags_byte
        IMPORTING iv_flags TYPE i,

      get_flags_byte
        RETURNING VALUE(rv_flags) TYPE i,

      " Memory access helpers
      read_byte_pp
        CHANGING cv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      read_word_pp
        CHANGING cv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      " Lookup table initialization
      init_tables,

      calc_parity
        IMPORTING iv_val        TYPE i
        RETURNING VALUE(rv_par) TYPE i,

      byte_to_hex
        IMPORTING iv_byte        TYPE i
        RETURNING VALUE(rv_hex)  TYPE string,

      hex_to_byte
        IMPORTING iv_hex         TYPE string
        RETURNING VALUE(rv_byte) TYPE i,

      " Opcode execution
      execute_opcode
        IMPORTING iv_opcode TYPE i
        RETURNING VALUE(rv_cycles) TYPE i,

      " Helper methods for opcode implementation
      get_register
        IMPORTING iv_reg_id     TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      set_register
        IMPORTING iv_reg_id TYPE i
                  iv_val    TYPE i,

      alu_add
        IMPORTING iv_a          TYPE i
                  iv_b          TYPE i
                  iv_carry      TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(rv_result) TYPE i,

      alu_sub
        IMPORTING iv_a          TYPE i
                  iv_b          TYPE i
                  iv_borrow     TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(rv_result) TYPE i,

      alu_and
        IMPORTING iv_a          TYPE i
                  iv_b          TYPE i
        RETURNING VALUE(rv_result) TYPE i,

      alu_or
        IMPORTING iv_a          TYPE i
                  iv_b          TYPE i
        RETURNING VALUE(rv_result) TYPE i,

      alu_xor
        IMPORTING iv_a          TYPE i
                  iv_b          TYPE i
        RETURNING VALUE(rv_result) TYPE i,

      alu_cp
        IMPORTING iv_a TYPE i
                  iv_b TYPE i.

ENDCLASS.


CLASS zcl_cpu_8080_v2 IMPLEMENTATION.

  METHOD constructor.
    " Initialize memory (64KB = 131072 hex chars, all zeros)
    " Use repeated self-concatenation: 2^16 = 65536 bytes
    mv_memory = '00'.
    DO 16 TIMES.
      mv_memory = mv_memory && mv_memory.  " Double size each iteration
    ENDDO.

    " Initialize lookup tables
    init_tables( ).

    " Reset CPU state
    reset( ).
  ENDMETHOD.


  METHOD reset.
    " Initialize registers
    mv_af = 0.
    mv_bc = 0.
    mv_de = 0.
    mv_hl = 0.
    mv_pc = 256.  " CP/M programs start at 0x0100
    mv_sp = 0.    " Will be set by BDOS
    mv_status = c_status_running.
  ENDMETHOD.


  METHOD init_tables.
    DATA: lv_val   TYPE i,
          lv_flags TYPE i,
          lv_par   TYPE i,
          lv_temp  TYPE i,
          lv_hex   TYPE string,
          lv_masked TYPE i.

    " === Parity Table (256 entries) ===
    mv_parity_table = ''.
    DO 256 TIMES.
      lv_val = sy-index - 1.
      lv_par = calc_parity( lv_val ).
      IF lv_par MOD 2 = 0.  " Even parity
        lv_flags = 4.
      ELSE.
        lv_flags = 0.
      ENDIF.
      lv_hex = byte_to_hex( lv_flags ).
      mv_parity_table = mv_parity_table && lv_hex.
    ENDDO.

    " === INC Table (257 entries) ===
    mv_inc_table = ''.
    DO 257 TIMES.
      lv_val = sy-index - 1.
      lv_flags = 0.

      " Bits 7,5,3 of result
      lv_temp = lv_val MOD 256.
      lv_masked = lv_temp - ( lv_temp MOD 8 ).  " Keep bits 7,5,3
      IF lv_masked >= 128.
        lv_flags = lv_flags + 128.
        lv_masked = lv_masked - 128.
      ENDIF.
      IF lv_masked >= 32.
        lv_flags = lv_flags + 32.
        lv_masked = lv_masked - 32.
      ENDIF.
      IF lv_masked >= 8.
        lv_flags = lv_flags + 8.
      ENDIF.

      " Zero flag
      IF lv_val MOD 256 = 0.
        lv_flags = lv_flags + 64.
      ENDIF.

      " Half-carry flag
      IF lv_val MOD 16 = 0.
        lv_flags = lv_flags + 16.
      ENDIF.

      lv_hex = byte_to_hex( lv_flags ).
      mv_inc_table = mv_inc_table && lv_hex.
    ENDDO.

    " === DEC Table (256 entries) ===
    mv_dec_table = ''.
    DO 256 TIMES.
      lv_val = sy-index - 1.
      lv_flags = 0.

      " Bits 7,5,3 of result
      lv_temp = lv_val.
      lv_masked = lv_temp - ( lv_temp MOD 8 ).
      IF lv_masked >= 128.
        lv_flags = lv_flags + 128.
        lv_masked = lv_masked - 128.
      ENDIF.
      IF lv_masked >= 32.
        lv_flags = lv_flags + 32.
        lv_masked = lv_masked - 32.
      ENDIF.
      IF lv_masked >= 8.
        lv_flags = lv_flags + 8.
      ENDIF.

      " Zero flag
      IF lv_val = 0.
        lv_flags = lv_flags + 64.
      ENDIF.

      " Half-carry flag
      IF lv_val MOD 16 = 15.
        lv_flags = lv_flags + 16.
      ENDIF.

      " N flag (subtract)
      lv_flags = lv_flags + 2.

      lv_hex = byte_to_hex( lv_flags ).
      mv_dec_table = mv_dec_table && lv_hex.
    ENDDO.

    " === Carry Bits Table (512 entries) ===
    mv_cbits_table = ''.
    DO 512 TIMES.
      lv_val = sy-index - 1.
      lv_flags = 0.

      " Half-carry from bit 4
      IF lv_val MOD 32 >= 16.
        lv_flags = lv_flags + 16.
      ENDIF.

      " Carry from bit 8
      IF lv_val >= 256.
        lv_flags = lv_flags + 1.
      ENDIF.

      lv_hex = byte_to_hex( lv_flags ).
      mv_cbits_table = mv_cbits_table && lv_hex.
    ENDDO.
  ENDMETHOD.


  METHOD calc_parity.
    " Count number of 1 bits in byte value
    DATA: lv_count TYPE i,
          lv_tmp   TYPE i.

    lv_tmp = iv_val.
    lv_count = 0.

    DO 8 TIMES.
      IF lv_tmp MOD 2 = 1.
        lv_count = lv_count + 1.
      ENDIF.
      lv_tmp = lv_tmp DIV 2.
    ENDDO.

    rv_par = lv_count.
  ENDMETHOD.


  METHOD byte_to_hex.
    " Convert byte (0-255) to 2-character hex string
    DATA: lv_high TYPE i,
          lv_low  TYPE i.

    CONSTANTS: lc_hex TYPE string VALUE '0123456789ABCDEF'.

    lv_high = iv_byte DIV 16.
    lv_low = iv_byte MOD 16.

    rv_hex = lc_hex+lv_high(1) && lc_hex+lv_low(1).
  ENDMETHOD.


  METHOD hex_to_byte.
    " Convert 2-character hex string to byte (0-255)
    DATA: lv_high TYPE i,
          lv_low  TYPE i,
          lv_char TYPE c LENGTH 1.

    CONSTANTS: lc_hex TYPE string VALUE '0123456789ABCDEF'.

    lv_char = iv_hex+0(1).
    FIND lv_char IN lc_hex MATCH OFFSET lv_high.

    lv_char = iv_hex+1(1).
    FIND lv_char IN lc_hex MATCH OFFSET lv_low.

    rv_byte = lv_high * 16 + lv_low.
  ENDMETHOD.


  "=== Bit Manipulation Methods ===

  METHOD get_high_byte.
    rv_val = iv_pair DIV 256.
    rv_val = rv_val MOD 256.  " Ensure 8-bit
  ENDMETHOD.


  METHOD get_low_byte.
    rv_val = iv_pair MOD 256.
  ENDMETHOD.


  METHOD set_high_byte.
    rv_new = ( iv_pair MOD 256 ) + ( iv_val * 256 ).
  ENDMETHOD.


  METHOD set_low_byte.
    rv_new = ( iv_pair DIV 256 ) * 256 + iv_val.
  ENDMETHOD.


  METHOD set_flags_byte.
    mv_af = set_low_byte( iv_pair = mv_af iv_val = iv_flags ).
  ENDMETHOD.


  METHOD get_flags_byte.
    rv_flags = get_low_byte( mv_af ).
  ENDMETHOD.


  "=== Memory Access Methods ===

  METHOD read_byte.
    " Read byte from memory (XSTRING)
    DATA: lv_offset TYPE i,
          lv_hex    TYPE string,
          lv_addr   TYPE i.

    lv_addr = iv_addr MOD 65536.  " Wrap to 16-bit
    lv_offset = lv_addr * 2.  " 2 hex chars per byte

    lv_hex = mv_memory+lv_offset(2).
    rv_val = hex_to_byte( lv_hex ).
  ENDMETHOD.


  METHOD write_byte.
    " Write byte to memory (XSTRING)
    DATA: lv_offset TYPE i,
          lv_hex    TYPE string,
          lv_before TYPE string,
          lv_after  TYPE string,
          lv_addr   TYPE i,
          lv_byte   TYPE i,
          lv_after_offset TYPE i,
          lv_remaining TYPE i.

    lv_addr = iv_addr MOD 65536.
    lv_byte = iv_val MOD 256.
    lv_offset = lv_addr * 2.

    lv_hex = byte_to_hex( lv_byte ).

    " Replace 2 chars at offset
    IF lv_offset > 0.
      lv_before = mv_memory+0(lv_offset).
    ELSE.
      lv_before = ''.
    ENDIF.

    lv_after_offset = lv_offset + 2.
    lv_remaining = 131072 - lv_after_offset.
    IF lv_remaining > 0.
      lv_after = mv_memory+lv_after_offset(lv_remaining).
    ELSE.
      lv_after = ''.
    ENDIF.

    mv_memory = lv_before && lv_hex && lv_after.
  ENDMETHOD.


  METHOD read_word.
    " Little-endian: low byte first, then high byte
    DATA: lv_low TYPE i,
          lv_high TYPE i.

    lv_low = read_byte( iv_addr ).
    lv_high = read_byte( iv_addr + 1 ).
    rv_val = lv_low + ( lv_high * 256 ).
  ENDMETHOD.


  METHOD write_word.
    " Little-endian: low byte first, then high byte
    write_byte( iv_addr = iv_addr
                iv_val  = iv_val MOD 256 ).
    write_byte( iv_addr = iv_addr + 1
                iv_val  = ( iv_val DIV 256 ) MOD 256 ).
  ENDMETHOD.


  METHOD read_byte_pp.
    " Read byte and post-increment address
    rv_val = read_byte( cv_addr ).
    cv_addr = cv_addr + 1.
  ENDMETHOD.


  METHOD read_word_pp.
    " Read word and post-increment address by 2
    rv_val = read_word( cv_addr ).
    cv_addr = cv_addr + 2.
  ENDMETHOD.


  "=== Public Interface Methods ===

  METHOD load_com_file.
    " Load CP/M .COM file into memory
    DATA: lv_offset TYPE i,
          lv_byte   TYPE i,
          lv_len    TYPE i,
          lv_hex    TYPE x LENGTH 1.

    lv_len = xstrlen( iv_data ).
    lv_offset = 0.

    DO lv_len TIMES.
      lv_hex = iv_data+lv_offset(1).
      lv_byte = lv_hex.  " Implicit conversion
      write_byte( iv_addr = iv_addr + lv_offset
                  iv_val  = lv_byte ).
      lv_offset = lv_offset + 1.
    ENDDO.

    " Set PC to load address
    mv_pc = iv_addr.
  ENDMETHOD.


  METHOD execute_instruction.
    " Fetch opcode (and increment PC)
    DATA: lv_opcode TYPE i.

    lv_opcode = read_byte_pp( CHANGING cv_addr = mv_pc ).

    " Execute opcode
    rv_cycles = execute_opcode( lv_opcode ).
  ENDMETHOD.


  METHOD execute_until_halt.
    DATA: lv_count TYPE i VALUE 0.

    DO iv_max_instructions TIMES.
      IF mv_status NE c_status_running.
        EXIT.
      ENDIF.

      execute_instruction( ).
      lv_count = lv_count + 1.
    ENDDO.

    rv_executed = lv_count.
  ENDMETHOD.


  METHOD get_status.
    rv_status = mv_status.
  ENDMETHOD.

  METHOD get_pc.
    rv_pc = mv_pc.
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

  METHOD get_sp.
    rv_sp = mv_sp.
  ENDMETHOD.

  METHOD read_memory.
    rv_val = read_byte( iv_addr ).
  ENDMETHOD.


  "=== OPCODE EXECUTION - THE HEART OF THE EMULATOR ===

  METHOD execute_opcode.
    DATA: lv_temp  TYPE i,
          lv_addr  TYPE i,
          lv_val   TYPE i,
          lv_flags TYPE i,
          lv_hex   TYPE string,
          lv_a     TYPE i,
          lv_result TYPE i,
          lv_bit7   TYPE i,
          lv_bit0   TYPE i,
          lv_carry_flag TYPE i,
          lv_low_nibble TYPE i,
          lv_high_nibble TYPE i,
          lv_adjust TYPE i,
          lv_old_carry TYPE i,
          lv_offset TYPE i,
          lv_inc_flags TYPE i,
          lv_dec_flags TYPE i,
          lv_dst TYPE i,
          lv_src TYPE i,
          lv_op TYPE i,
          lv_reg TYPE i,
          lv_operand TYPE i.

    " Default cycle count
    rv_cycles = 4.

    " ====== SWITCH/CASE DISPATCH ======

    CASE iv_opcode.

      " === 0x00-0x0F ===
      WHEN 0.  " NOP
        " Do nothing

      WHEN 1.  " LD BC,nnnn
        mv_bc = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

      WHEN 2.  " LD (BC),A
        write_byte( iv_addr = mv_bc
                    iv_val  = get_high_byte( mv_af ) ).
        rv_cycles = 7.

      WHEN 3.  " INC BC
        mv_bc = ( mv_bc + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 4.  " INC B
        mv_bc = mv_bc + 256.  " Increment high byte
        lv_temp = get_high_byte( mv_bc ).
        " Use pre-computed table for flags
        lv_offset = lv_temp * 2.
        lv_hex = mv_inc_table+lv_offset(2).
        lv_inc_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.  " Keep carry only
        lv_flags = lv_flags + lv_inc_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 5.  " DEC B
        mv_bc = mv_bc - 256.  " Decrement high byte
        lv_temp = get_high_byte( mv_bc ).
        lv_offset = lv_temp * 2.
        lv_hex = mv_dec_table+lv_offset(2).
        lv_dec_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.  " Keep carry only
        lv_flags = lv_flags + lv_dec_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 6.  " LD B,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        mv_bc = set_high_byte( iv_pair = mv_bc iv_val = lv_val ).
        rv_cycles = 7.

      WHEN 10.  " LD A,(BC)
        lv_val = read_byte( mv_bc ).
        mv_af = set_high_byte( iv_pair = mv_af iv_val = lv_val ).
        rv_cycles = 7.

      WHEN 11.  " DEC BC
        mv_bc = ( mv_bc - 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 12.  " INC C
        lv_temp = ( get_low_byte( mv_bc ) + 1 ) MOD 256.
        mv_bc = set_low_byte( iv_pair = mv_bc iv_val = lv_temp ).
        lv_offset = lv_temp * 2.
        lv_hex = mv_inc_table+lv_offset(2).
        lv_inc_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.
        lv_flags = lv_flags + lv_inc_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 13.  " DEC C
        lv_temp = ( get_low_byte( mv_bc ) - 1 ) MOD 256.
        mv_bc = set_low_byte( iv_pair = mv_bc iv_val = lv_temp ).
        lv_offset = lv_temp * 2.
        lv_hex = mv_dec_table+lv_offset(2).
        lv_dec_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.
        lv_flags = lv_flags + lv_dec_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 7.  " RLC (rotate left circular)
        lv_a = get_high_byte( mv_af ).
        lv_bit7 = lv_a DIV 128.
        lv_result = ( lv_a * 2 + lv_bit7 ) MOD 256.
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        " Set carry flag to bit 7
        lv_flags = get_flags_byte( ) - ( get_flags_byte( ) MOD 2 ).  " Clear C
        lv_flags = lv_flags + lv_bit7.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 14.  " LD C,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        mv_bc = set_low_byte( iv_pair = mv_bc iv_val = lv_val ).
        rv_cycles = 7.

      WHEN 15.  " RRC (rotate right circular)
        lv_a = get_high_byte( mv_af ).
        lv_bit0 = lv_a MOD 2.
        lv_result = ( lv_a DIV 2 + lv_bit0 * 128 ) MOD 256.
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        " Set carry flag to bit 0
        lv_flags = get_flags_byte( ) - ( get_flags_byte( ) MOD 2 ).
        lv_flags = lv_flags + lv_bit0.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      " === 0x10-0x1F ===
      WHEN 17.  " LD DE,nnnn
        mv_de = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

      WHEN 18.  " LD (DE),A
        write_byte( iv_addr = mv_de iv_val = get_high_byte( mv_af ) ).
        rv_cycles = 7.

      WHEN 20.  " INC D
        lv_temp = ( get_high_byte( mv_de ) + 1 ) MOD 256.
        mv_de = set_high_byte( iv_pair = mv_de iv_val = lv_temp ).
        lv_offset = lv_temp * 2.
        lv_hex = mv_inc_table+lv_offset(2).
        lv_inc_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.
        lv_flags = lv_flags + lv_inc_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 21.  " DEC D
        lv_temp = ( get_high_byte( mv_de ) - 1 ) MOD 256.
        mv_de = set_high_byte( iv_pair = mv_de iv_val = lv_temp ).
        lv_offset = lv_temp * 2.
        lv_hex = mv_dec_table+lv_offset(2).
        lv_dec_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.
        lv_flags = lv_flags + lv_dec_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 22.  " MVI D,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        mv_de = set_high_byte( iv_pair = mv_de iv_val = lv_val ).
        rv_cycles = 7.

      WHEN 23.  " RAL (rotate left through carry)
        lv_a = get_high_byte( mv_af ).
        lv_carry_flag = get_flags_byte( ) MOD 2.
        lv_bit7 = lv_a DIV 128.
        lv_result = ( lv_a * 2 + lv_carry_flag ) MOD 256.
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        lv_flags = get_flags_byte( ) - ( get_flags_byte( ) MOD 2 ).
        lv_flags = lv_flags + lv_bit7.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 19.  " INC DE
        mv_de = ( mv_de + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 26.  " LD A,(DE)
        lv_val = read_byte( mv_de ).
        set_register( iv_reg_id = 7 iv_val = lv_val ).
        rv_cycles = 7.

      WHEN 27.  " DEC DE
        mv_de = ( mv_de - 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 30.  " LD E,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        mv_de = set_low_byte( iv_pair = mv_de iv_val = lv_val ).
        rv_cycles = 7.

      WHEN 31.  " RAR (rotate right through carry)
        lv_a = get_high_byte( mv_af ).
        lv_carry_flag = get_flags_byte( ) MOD 2.
        lv_bit0 = lv_a MOD 2.
        lv_result = ( lv_a DIV 2 + lv_carry_flag * 128 ) MOD 256.
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        lv_flags = get_flags_byte( ) - ( get_flags_byte( ) MOD 2 ).
        lv_flags = lv_flags + lv_bit0.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      " === 0x20-0x2F ===
      WHEN 33.  " LD HL,nnnn
        mv_hl = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

      WHEN 34.  " SHLD nnnn (store HL direct)
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        write_word( iv_addr = lv_addr iv_val = mv_hl ).
        rv_cycles = 16.

      WHEN 36.  " INC H
        lv_temp = ( get_high_byte( mv_hl ) + 1 ) MOD 256.
        mv_hl = set_high_byte( iv_pair = mv_hl iv_val = lv_temp ).
        lv_offset = lv_temp * 2.
        lv_hex = mv_inc_table+lv_offset(2).
        lv_inc_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.
        lv_flags = lv_flags + lv_inc_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 37.  " DEC H
        lv_temp = ( get_high_byte( mv_hl ) - 1 ) MOD 256.
        mv_hl = set_high_byte( iv_pair = mv_hl iv_val = lv_temp ).
        lv_offset = lv_temp * 2.
        lv_hex = mv_dec_table+lv_offset(2).
        lv_dec_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.
        lv_flags = lv_flags + lv_dec_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 38.  " MVI H,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        mv_hl = set_high_byte( iv_pair = mv_hl iv_val = lv_val ).
        rv_cycles = 7.

      WHEN 39.  " DAA (decimal adjust accumulator)
        lv_a = get_high_byte( mv_af ).
        lv_flags = get_flags_byte( ).
        lv_low_nibble = lv_a MOD 16.
        lv_high_nibble = lv_a DIV 16.
        lv_adjust = 0.

        " Check low nibble
        IF lv_low_nibble > 9 OR ( lv_flags DIV 16 MOD 2 = 1 ).  " H flag
          lv_adjust = lv_adjust + 6.
        ENDIF.

        " Check high nibble
        IF lv_high_nibble > 9 OR ( lv_flags MOD 2 = 1 ).  " C flag
          lv_adjust = lv_adjust + 96.  " 0x60
        ENDIF.

        lv_result = ( lv_a + lv_adjust ) MOD 256.
        set_register( iv_reg_id = 7 iv_val = lv_result ).

        " Update flags
        IF ( lv_a + lv_adjust ) >= 256.
          lv_flags = lv_flags + 1 - ( lv_flags MOD 2 ).  " Set carry
        ENDIF.
        rv_cycles = 4.

      WHEN 42.  " LHLD nnnn (load HL direct)
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        mv_hl = read_word( lv_addr ).
        rv_cycles = 16.

      WHEN 46.  " LD L,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        mv_hl = set_low_byte( iv_pair = mv_hl iv_val = lv_val ).
        rv_cycles = 7.

      WHEN 47.  " CPL (complement accumulator)
        lv_a = get_high_byte( mv_af ).
        lv_result = 255 - lv_a.  " Bitwise NOT
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        rv_cycles = 4.

      WHEN 35.  " INC HL
        mv_hl = ( mv_hl + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 43.  " DEC HL
        mv_hl = ( mv_hl - 1 ) MOD 65536.
        rv_cycles = 6.

      " === 0x30-0x3F ===
      WHEN 49.  " LD SP,nnnn
        mv_sp = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

      WHEN 50.  " STA nnnn (store accumulator direct)
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        write_byte( iv_addr = lv_addr iv_val = get_high_byte( mv_af ) ).
        rv_cycles = 13.

      WHEN 52.  " INR M (increment memory at HL)
        lv_val = read_byte( mv_hl ).
        lv_temp = ( lv_val + 1 ) MOD 256.
        write_byte( iv_addr = mv_hl iv_val = lv_temp ).
        " Update flags
        lv_offset = lv_temp * 2.
        lv_hex = mv_inc_table+lv_offset(2).
        lv_inc_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.
        lv_flags = lv_flags + lv_inc_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 10.

      WHEN 53.  " DCR M (decrement memory at HL)
        lv_val = read_byte( mv_hl ).
        lv_temp = ( lv_val - 1 ) MOD 256.
        write_byte( iv_addr = mv_hl iv_val = lv_temp ).
        " Update flags
        lv_offset = lv_temp * 2.
        lv_hex = mv_dec_table+lv_offset(2).
        lv_dec_flags = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.
        lv_flags = lv_flags + lv_dec_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 10.

      WHEN 54.  " MVI M,nn (move immediate to memory at HL)
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        write_byte( iv_addr = mv_hl iv_val = lv_val ).
        rv_cycles = 10.

      WHEN 55.  " STC (set carry flag)
        lv_flags = get_flags_byte( ).
        lv_flags = lv_flags + 1 - ( lv_flags MOD 2 ).  " Set C flag
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 58.  " LDA nnnn (load accumulator direct)
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        lv_val = read_byte( lv_addr ).
        set_register( iv_reg_id = 7 iv_val = lv_val ).
        rv_cycles = 13.

      WHEN 62.  " MVI A,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        set_register( iv_reg_id = 7 iv_val = lv_val ).
        rv_cycles = 7.

      WHEN 51.  " INC SP
        mv_sp = ( mv_sp + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 59.  " DEC SP
        mv_sp = ( mv_sp - 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 63.  " CMC (complement carry flag)
        lv_flags = get_flags_byte( ).
        lv_old_carry = lv_flags MOD 2.
        IF lv_old_carry = 1.
          lv_flags = lv_flags - 1.
        ELSE.
          lv_flags = lv_flags + 1.
        ENDIF.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN OTHERS.
        " === 0x40-0x7F: MOV r,r family (64 opcodes) ===
        " === 0x80-0xBF: Arithmetic and Logical Operations ===
        IF iv_opcode >= 64 AND iv_opcode <= 127.
          " MOV dst,src - bits 5-3=dest, bits 2-0=src
          IF iv_opcode = 118.  " HALT (0x76) is exception
            mv_status = c_status_halted.
            rv_cycles = 4.
          ELSE.
            lv_dst = ( iv_opcode - 64 ) DIV 8.
            lv_src = ( iv_opcode - 64 ) MOD 8.
            lv_val = get_register( lv_src ).
            set_register( iv_reg_id = lv_dst iv_val = lv_val ).
            IF lv_dst = 6 OR lv_src = 6.  " Memory access
              rv_cycles = 7.
            ELSE.
              rv_cycles = 5.
            ENDIF.
          ENDIF.
        ELSEIF iv_opcode >= 128 AND iv_opcode <= 191.
          " ADD/ADC/SUB/SBB/AND/XOR/OR/CP with register
          lv_op = ( iv_opcode - 128 ) DIV 8.
          lv_reg = ( iv_opcode - 128 ) MOD 8.
          lv_operand = get_register( lv_reg ).
          lv_a = get_high_byte( mv_af ).
          lv_result = 0.

          CASE lv_op.
            WHEN 0.  " ADD A,r
              lv_result = alu_add( iv_a = lv_a iv_b = lv_operand ).
              set_register( iv_reg_id = 7 iv_val = lv_result ).
            WHEN 1.  " ADC A,r (add with carry)
              lv_carry_flag = get_flags_byte( ) MOD 2.
              IF lv_carry_flag = 1.
                lv_result = alu_add( iv_a = lv_a iv_b = lv_operand iv_carry = abap_true ).
              ELSE.
                lv_result = alu_add( iv_a = lv_a iv_b = lv_operand iv_carry = abap_false ).
              ENDIF.
              set_register( iv_reg_id = 7 iv_val = lv_result ).
            WHEN 2.  " SUB A,r
              lv_result = alu_sub( iv_a = lv_a iv_b = lv_operand ).
              set_register( iv_reg_id = 7 iv_val = lv_result ).
            WHEN 3.  " SBB A,r (subtract with borrow)
              lv_carry_flag = get_flags_byte( ) MOD 2.
              IF lv_carry_flag = 1.
                lv_result = alu_sub( iv_a = lv_a iv_b = lv_operand iv_borrow = abap_true ).
              ELSE.
                lv_result = alu_sub( iv_a = lv_a iv_b = lv_operand iv_borrow = abap_false ).
              ENDIF.
              set_register( iv_reg_id = 7 iv_val = lv_result ).
            WHEN 4.  " AND A,r
              lv_result = alu_and( iv_a = lv_a iv_b = lv_operand ).
              set_register( iv_reg_id = 7 iv_val = lv_result ).
            WHEN 5.  " XOR A,r
              lv_result = alu_xor( iv_a = lv_a iv_b = lv_operand ).
              set_register( iv_reg_id = 7 iv_val = lv_result ).
            WHEN 6.  " OR A,r
              lv_result = alu_or( iv_a = lv_a iv_b = lv_operand ).
              set_register( iv_reg_id = 7 iv_val = lv_result ).
            WHEN 7.  " CP A,r (compare)
              alu_cp( iv_a = lv_a iv_b = lv_operand ).
          ENDCASE.

          IF lv_reg = 6.  " Memory operand
            rv_cycles = 7.
          ELSE.
            rv_cycles = 4.
          ENDIF.
        ELSEIF iv_opcode < 192.
          " Unimplemented opcode in range 0-191
          mv_status = c_status_halted.
          rv_cycles = 4.
        ENDIF.

    ENDCASE.

    " === 0xC0-0xFF: Immediate ops, conditional jumps, stack ops ===
    " Handle these after the main CASE
    IF mv_status = c_status_running AND iv_opcode >= 192.
      CASE iv_opcode.
      WHEN 192.  " RET NZ
        IF get_flags_byte( ) DIV 64 MOD 2 = 0.  " Z flag clear
          mv_pc = read_word( mv_sp ).
          mv_sp = ( mv_sp + 2 ) MOD 65536.
          rv_cycles = 11.
        ELSE.
          rv_cycles = 5.
        ENDIF.

      WHEN 193.  " POP BC
        mv_bc = read_word( mv_sp ).
        mv_sp = ( mv_sp + 2 ) MOD 65536.
        rv_cycles = 10.

      WHEN 194.  " JP NZ,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 64 MOD 2 = 0.  " Z flag clear
          mv_pc = lv_addr.
        ENDIF.
        rv_cycles = 10.

      WHEN 195.  " JP nnnn (unconditional jump)
        mv_pc = read_word( mv_pc ).
        rv_cycles = 10.

      WHEN 196.  " CALL NZ,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 64 MOD 2 = 0.  " Z flag clear
          mv_sp = ( mv_sp - 2 ) MOD 65536.
          write_word( iv_addr = mv_sp iv_val = mv_pc ).
          mv_pc = lv_addr.
          rv_cycles = 17.
        ELSE.
          rv_cycles = 11.
        ENDIF.

      WHEN 197.  " PUSH BC
        mv_sp = ( mv_sp - 2 ) MOD 65536.
        write_word( iv_addr = mv_sp iv_val = mv_bc ).
        rv_cycles = 11.

      WHEN 198.  " ADI nn (add immediate)
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        lv_a = get_high_byte( mv_af ).
        lv_result = alu_add( iv_a = lv_a iv_b = lv_val ).
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        rv_cycles = 7.

      WHEN 200.  " RET Z
        IF get_flags_byte( ) DIV 64 MOD 2 = 1.  " Z flag set
          mv_pc = read_word( mv_sp ).
          mv_sp = ( mv_sp + 2 ) MOD 65536.
          rv_cycles = 11.
        ELSE.
          rv_cycles = 5.
        ENDIF.

      WHEN 201.  " RET (return from subroutine)
        mv_pc = read_word( mv_sp ).
        mv_sp = ( mv_sp + 2 ) MOD 65536.
        rv_cycles = 10.

      WHEN 202.  " JP Z,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 64 MOD 2 = 1.  " Z flag set
          mv_pc = lv_addr.
        ENDIF.
        rv_cycles = 10.

      WHEN 204.  " CALL Z,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 64 MOD 2 = 1.  " Z flag set
          mv_sp = ( mv_sp - 2 ) MOD 65536.
          write_word( iv_addr = mv_sp iv_val = mv_pc ).
          mv_pc = lv_addr.
          rv_cycles = 17.
        ELSE.
          rv_cycles = 11.
        ENDIF.

      WHEN 205.  " CALL nnnn (call subroutine)
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        " Push return address onto stack
        mv_sp = ( mv_sp - 2 ) MOD 65536.
        write_word( iv_addr = mv_sp iv_val = mv_pc ).
        " Jump to target
        mv_pc = lv_addr.
        rv_cycles = 17.

      WHEN 206.  " ACI nn (add immediate with carry)
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        lv_a = get_high_byte( mv_af ).
        lv_carry_flag = get_flags_byte( ) MOD 2.
        IF lv_carry_flag = 1.
          lv_result = alu_add( iv_a = lv_a iv_b = lv_val iv_carry = abap_true ).
        ELSE.
          lv_result = alu_add( iv_a = lv_a iv_b = lv_val iv_carry = abap_false ).
        ENDIF.
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        rv_cycles = 7.

      WHEN 209.  " POP DE
        mv_de = read_word( mv_sp ).
        mv_sp = ( mv_sp + 2 ) MOD 65536.
        rv_cycles = 10.

      WHEN 213.  " PUSH DE
        mv_sp = ( mv_sp - 2 ) MOD 65536.
        write_word( iv_addr = mv_sp iv_val = mv_de ).
        rv_cycles = 11.

      WHEN 214.  " SUI nn (subtract immediate)
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        lv_a = get_high_byte( mv_af ).
        lv_result = alu_sub( iv_a = lv_a iv_b = lv_val ).
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        rv_cycles = 7.

      WHEN 222.  " SBI nn (subtract immediate with borrow)
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        lv_a = get_high_byte( mv_af ).
        lv_carry_flag = get_flags_byte( ) MOD 2.
        IF lv_carry_flag = 1.
          lv_result = alu_sub( iv_a = lv_a iv_b = lv_val iv_borrow = abap_true ).
        ELSE.
          lv_result = alu_sub( iv_a = lv_a iv_b = lv_val iv_borrow = abap_false ).
        ENDIF.
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        rv_cycles = 7.

      WHEN 225.  " POP HL
        mv_hl = read_word( mv_sp ).
        mv_sp = ( mv_sp + 2 ) MOD 65536.
        rv_cycles = 10.

      WHEN 229.  " PUSH HL
        mv_sp = ( mv_sp - 2 ) MOD 65536.
        write_word( iv_addr = mv_sp iv_val = mv_hl ).
        rv_cycles = 11.

      WHEN 230.  " ANI nn (AND immediate)
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        lv_a = get_high_byte( mv_af ).
        lv_result = alu_and( iv_a = lv_a iv_b = lv_val ).
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        rv_cycles = 7.

      WHEN 238.  " XRI nn (XOR immediate)
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        lv_a = get_high_byte( mv_af ).
        lv_result = alu_xor( iv_a = lv_a iv_b = lv_val ).
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        rv_cycles = 7.

      WHEN 241.  " POP AF
        mv_af = read_word( mv_sp ).
        mv_sp = ( mv_sp + 2 ) MOD 65536.
        rv_cycles = 10.

      WHEN 245.  " PUSH AF
        mv_sp = ( mv_sp - 2 ) MOD 65536.
        write_word( iv_addr = mv_sp iv_val = mv_af ).
        rv_cycles = 11.

      WHEN 246.  " ORI nn (OR immediate)
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        lv_a = get_high_byte( mv_af ).
        lv_result = alu_or( iv_a = lv_a iv_b = lv_val ).
        set_register( iv_reg_id = 7 iv_val = lv_result ).
        rv_cycles = 7.

      WHEN 254.  " CPI nn (compare immediate)
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        lv_a = get_high_byte( mv_af ).
        alu_cp( iv_a = lv_a iv_b = lv_val ).
        rv_cycles = 7.

      " === Additional conditional operations ===
      WHEN 208.  " RET NC (return if no carry)
        IF get_flags_byte( ) MOD 2 = 0.  " C flag clear
          mv_pc = read_word( mv_sp ).
          mv_sp = ( mv_sp + 2 ) MOD 65536.
          rv_cycles = 11.
        ELSE.
          rv_cycles = 5.
        ENDIF.

      WHEN 210.  " JP NC,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) MOD 2 = 0.  " C flag clear
          mv_pc = lv_addr.
        ENDIF.
        rv_cycles = 10.

      WHEN 212.  " CALL NC,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) MOD 2 = 0.  " C flag clear
          mv_sp = ( mv_sp - 2 ) MOD 65536.
          write_word( iv_addr = mv_sp iv_val = mv_pc ).
          mv_pc = lv_addr.
          rv_cycles = 17.
        ELSE.
          rv_cycles = 11.
        ENDIF.

      WHEN 216.  " RET C (return if carry)
        IF get_flags_byte( ) MOD 2 = 1.  " C flag set
          mv_pc = read_word( mv_sp ).
          mv_sp = ( mv_sp + 2 ) MOD 65536.
          rv_cycles = 11.
        ELSE.
          rv_cycles = 5.
        ENDIF.

      WHEN 218.  " JP C,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) MOD 2 = 1.  " C flag set
          mv_pc = lv_addr.
        ENDIF.
        rv_cycles = 10.

      WHEN 220.  " CALL C,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) MOD 2 = 1.  " C flag set
          mv_sp = ( mv_sp - 2 ) MOD 65536.
          write_word( iv_addr = mv_sp iv_val = mv_pc ).
          mv_pc = lv_addr.
          rv_cycles = 17.
        ELSE.
          rv_cycles = 11.
        ENDIF.

      WHEN 224.  " RET PO (return if parity odd)
        IF get_flags_byte( ) DIV 4 MOD 2 = 0.  " P flag clear
          mv_pc = read_word( mv_sp ).
          mv_sp = ( mv_sp + 2 ) MOD 65536.
          rv_cycles = 11.
        ELSE.
          rv_cycles = 5.
        ENDIF.

      WHEN 226.  " JP PO,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 4 MOD 2 = 0.  " P flag clear
          mv_pc = lv_addr.
        ENDIF.
        rv_cycles = 10.

      WHEN 228.  " CALL PO,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 4 MOD 2 = 0.  " P flag clear
          mv_sp = ( mv_sp - 2 ) MOD 65536.
          write_word( iv_addr = mv_sp iv_val = mv_pc ).
          mv_pc = lv_addr.
          rv_cycles = 17.
        ELSE.
          rv_cycles = 11.
        ENDIF.

      WHEN 232.  " RET PE (return if parity even)
        IF get_flags_byte( ) DIV 4 MOD 2 = 1.  " P flag set
          mv_pc = read_word( mv_sp ).
          mv_sp = ( mv_sp + 2 ) MOD 65536.
          rv_cycles = 11.
        ELSE.
          rv_cycles = 5.
        ENDIF.

      WHEN 234.  " JP PE,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 4 MOD 2 = 1.  " P flag set
          mv_pc = lv_addr.
        ENDIF.
        rv_cycles = 10.

      WHEN 236.  " CALL PE,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 4 MOD 2 = 1.  " P flag set
          mv_sp = ( mv_sp - 2 ) MOD 65536.
          write_word( iv_addr = mv_sp iv_val = mv_pc ).
          mv_pc = lv_addr.
          rv_cycles = 17.
        ELSE.
          rv_cycles = 11.
        ENDIF.

      WHEN 240.  " RET P (return if positive)
        IF get_flags_byte( ) DIV 128 MOD 2 = 0.  " S flag clear
          mv_pc = read_word( mv_sp ).
          mv_sp = ( mv_sp + 2 ) MOD 65536.
          rv_cycles = 11.
        ELSE.
          rv_cycles = 5.
        ENDIF.

      WHEN 242.  " JP P,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 128 MOD 2 = 0.  " S flag clear
          mv_pc = lv_addr.
        ENDIF.
        rv_cycles = 10.

      WHEN 244.  " CALL P,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 128 MOD 2 = 0.  " S flag clear
          mv_sp = ( mv_sp - 2 ) MOD 65536.
          write_word( iv_addr = mv_sp iv_val = mv_pc ).
          mv_pc = lv_addr.
          rv_cycles = 17.
        ELSE.
          rv_cycles = 11.
        ENDIF.

      WHEN 248.  " RET M (return if minus)
        IF get_flags_byte( ) DIV 128 MOD 2 = 1.  " S flag set
          mv_pc = read_word( mv_sp ).
          mv_sp = ( mv_sp + 2 ) MOD 65536.
          rv_cycles = 11.
        ELSE.
          rv_cycles = 5.
        ENDIF.

      WHEN 250.  " JP M,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 128 MOD 2 = 1.  " S flag set
          mv_pc = lv_addr.
        ENDIF.
        rv_cycles = 10.

      WHEN 252.  " CALL M,nnnn
        lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
        IF get_flags_byte( ) DIV 128 MOD 2 = 1.  " S flag set
          mv_sp = ( mv_sp - 2 ) MOD 65536.
          write_word( iv_addr = mv_sp iv_val = mv_pc ).
          mv_pc = lv_addr.
          rv_cycles = 17.
        ELSE.
          rv_cycles = 11.
        ENDIF.

        WHEN OTHERS.
          " Unimplemented opcode in 0xC0+ range
          mv_status = c_status_halted.
          rv_cycles = 4.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  "=== HELPER METHODS FOR OPCODE IMPLEMENTATION ===

  METHOD get_register.
    " Map register ID (0-7) to actual register value
    " 0=B, 1=C, 2=D, 3=E, 4=H, 5=L, 6=M (memory at HL), 7=A
    CASE iv_reg_id.
      WHEN 0.  " B
        rv_val = get_high_byte( mv_bc ).
      WHEN 1.  " C
        rv_val = get_low_byte( mv_bc ).
      WHEN 2.  " D
        rv_val = get_high_byte( mv_de ).
      WHEN 3.  " E
        rv_val = get_low_byte( mv_de ).
      WHEN 4.  " H
        rv_val = get_high_byte( mv_hl ).
      WHEN 5.  " L
        rv_val = get_low_byte( mv_hl ).
      WHEN 6.  " M (memory at HL)
        rv_val = read_byte( mv_hl ).
      WHEN 7.  " A
        rv_val = get_high_byte( mv_af ).
      WHEN OTHERS.
        rv_val = 0.
    ENDCASE.
  ENDMETHOD.


  METHOD set_register.
    " Map register ID (0-7) to actual register
    DATA: lv_byte TYPE i.

    lv_byte = iv_val MOD 256.  " Ensure 8-bit

    CASE iv_reg_id.
      WHEN 0.  " B
        mv_bc = set_high_byte( iv_pair = mv_bc iv_val = lv_byte ).
      WHEN 1.  " C
        mv_bc = set_low_byte( iv_pair = mv_bc iv_val = lv_byte ).
      WHEN 2.  " D
        mv_de = set_high_byte( iv_pair = mv_de iv_val = lv_byte ).
      WHEN 3.  " E
        mv_de = set_low_byte( iv_pair = mv_de iv_val = lv_byte ).
      WHEN 4.  " H
        mv_hl = set_high_byte( iv_pair = mv_hl iv_val = lv_byte ).
      WHEN 5.  " L
        mv_hl = set_low_byte( iv_pair = mv_hl iv_val = lv_byte ).
      WHEN 6.  " M (memory at HL)
        write_byte( iv_addr = mv_hl iv_val = lv_byte ).
      WHEN 7.  " A
        mv_af = set_high_byte( iv_pair = mv_af iv_val = lv_byte ).
    ENDCASE.
  ENDMETHOD.


  METHOD alu_add.
    " Add with optional carry - sets all flags
    DATA: lv_sum     TYPE i,
          lv_flags   TYPE i,
          lv_offset  TYPE i,
          lv_hex     TYPE string,
          lv_carry   TYPE i,
          lv_a_byte  TYPE i,
          lv_b_byte  TYPE i.

    " Ensure 8-bit inputs
    lv_a_byte = iv_a MOD 256.
    lv_b_byte = iv_b MOD 256.

    IF iv_carry = abap_true.
      lv_carry = 1.
    ELSE.
      lv_carry = 0.
    ENDIF.

    lv_sum = lv_a_byte + lv_b_byte + lv_carry.
    rv_result = lv_sum MOD 256.

    " Calculate flags using lookup table
    lv_offset = lv_sum * 2.
    lv_hex = mv_cbits_table+lv_offset(2).
    lv_flags = hex_to_byte( lv_hex ).

    " Add parity flag
    lv_offset = rv_result * 2.
    lv_hex = mv_parity_table+lv_offset(2).
    lv_flags = lv_flags + hex_to_byte( lv_hex ).

    " Add zero flag
    IF rv_result = 0.
      lv_flags = lv_flags + 64.
    ENDIF.

    " Add sign flag
    IF rv_result >= 128.
      lv_flags = lv_flags + 128.
    ENDIF.

    set_flags_byte( lv_flags ).
  ENDMETHOD.


  METHOD alu_sub.
    " Subtract with optional borrow - sets all flags
    DATA: lv_diff    TYPE i,
          lv_flags   TYPE i,
          lv_offset  TYPE i,
          lv_hex     TYPE string,
          lv_borrow  TYPE i,
          lv_a_byte  TYPE i,
          lv_b_byte  TYPE i.

    lv_a_byte = iv_a MOD 256.
    lv_b_byte = iv_b MOD 256.

    IF iv_borrow = abap_true.
      lv_borrow = 1.
    ELSE.
      lv_borrow = 0.
    ENDIF.

    lv_diff = lv_a_byte - lv_b_byte - lv_borrow.
    rv_result = lv_diff MOD 256.

    " Calculate flags
    lv_flags = 2.  " N flag (subtract)

    " Carry flag (borrow)
    IF lv_diff < 0.
      lv_flags = lv_flags + 1.
    ENDIF.

    " Half-carry flag
    DATA: lv_low_diff TYPE i.
    lv_low_diff = ( lv_a_byte MOD 16 ) - ( lv_b_byte MOD 16 ) - lv_borrow.
    IF lv_low_diff < 0.
      lv_flags = lv_flags + 16.
    ENDIF.

    " Parity flag
    lv_offset = rv_result * 2.
    lv_hex = mv_parity_table+lv_offset(2).
    lv_flags = lv_flags + hex_to_byte( lv_hex ).

    " Zero flag
    IF rv_result = 0.
      lv_flags = lv_flags + 64.
    ENDIF.

    " Sign flag
    IF rv_result >= 128.
      lv_flags = lv_flags + 128.
    ENDIF.

    set_flags_byte( lv_flags ).
  ENDMETHOD.


  METHOD alu_and.
    " Logical AND - sets flags
    DATA: lv_flags  TYPE i,
          lv_offset TYPE i,
          lv_hex    TYPE string.

    rv_result = ( iv_a MOD 256 ) * ( iv_b MOD 256 ).  " Bitwise AND via multiplication trick
    " Actually, let's do proper bitwise AND
    DATA: lv_a TYPE i,
          lv_b TYPE i,
          lv_bit TYPE i,
          lv_mask TYPE i.

    lv_a = iv_a MOD 256.
    lv_b = iv_b MOD 256.
    rv_result = 0.
    lv_mask = 1.

    DO 8 TIMES.
      IF ( lv_a MOD 2 = 1 ) AND ( lv_b MOD 2 = 1 ).
        rv_result = rv_result + lv_mask.
      ENDIF.
      lv_a = lv_a DIV 2.
      lv_b = lv_b DIV 2.
      lv_mask = lv_mask * 2.
    ENDDO.

    " Flags: reset carry, set half-carry (per i8080 spec)
    lv_flags = 16.  " H flag set for AND

    " Parity
    lv_offset = rv_result * 2.
    lv_hex = mv_parity_table+lv_offset(2).
    lv_flags = lv_flags + hex_to_byte( lv_hex ).

    " Zero
    IF rv_result = 0.
      lv_flags = lv_flags + 64.
    ENDIF.

    " Sign
    IF rv_result >= 128.
      lv_flags = lv_flags + 128.
    ENDIF.

    set_flags_byte( lv_flags ).
  ENDMETHOD.


  METHOD alu_or.
    " Logical OR - sets flags
    DATA: lv_flags  TYPE i,
          lv_offset TYPE i,
          lv_hex    TYPE string,
          lv_a      TYPE i,
          lv_b      TYPE i,
          lv_mask   TYPE i.

    lv_a = iv_a MOD 256.
    lv_b = iv_b MOD 256.
    rv_result = 0.
    lv_mask = 1.

    DO 8 TIMES.
      IF ( lv_a MOD 2 = 1 ) OR ( lv_b MOD 2 = 1 ).
        rv_result = rv_result + lv_mask.
      ENDIF.
      lv_a = lv_a DIV 2.
      lv_b = lv_b DIV 2.
      lv_mask = lv_mask * 2.
    ENDDO.

    " Flags: reset carry and half-carry
    lv_flags = 0.

    " Parity
    lv_offset = rv_result * 2.
    lv_hex = mv_parity_table+lv_offset(2).
    lv_flags = lv_flags + hex_to_byte( lv_hex ).

    " Zero
    IF rv_result = 0.
      lv_flags = lv_flags + 64.
    ENDIF.

    " Sign
    IF rv_result >= 128.
      lv_flags = lv_flags + 128.
    ENDIF.

    set_flags_byte( lv_flags ).
  ENDMETHOD.


  METHOD alu_xor.
    " Logical XOR - sets flags
    DATA: lv_flags  TYPE i,
          lv_offset TYPE i,
          lv_hex    TYPE string,
          lv_a      TYPE i,
          lv_b      TYPE i,
          lv_mask   TYPE i.

    lv_a = iv_a MOD 256.
    lv_b = iv_b MOD 256.
    rv_result = 0.
    lv_mask = 1.

    DATA: lv_bit_a TYPE i,
          lv_bit_b TYPE i.

    DO 8 TIMES.
      lv_bit_a = lv_a MOD 2.
      lv_bit_b = lv_b MOD 2.
      IF ( lv_bit_a = 1 AND lv_bit_b = 0 ) OR ( lv_bit_a = 0 AND lv_bit_b = 1 ).
        rv_result = rv_result + lv_mask.
      ENDIF.
      lv_a = lv_a DIV 2.
      lv_b = lv_b DIV 2.
      lv_mask = lv_mask * 2.
    ENDDO.

    " Flags: reset carry and half-carry
    lv_flags = 0.

    " Parity
    lv_offset = rv_result * 2.
    lv_hex = mv_parity_table+lv_offset(2).
    lv_flags = lv_flags + hex_to_byte( lv_hex ).

    " Zero
    IF rv_result = 0.
      lv_flags = lv_flags + 64.
    ENDIF.

    " Sign
    IF rv_result >= 128.
      lv_flags = lv_flags + 128.
    ENDIF.

    set_flags_byte( lv_flags ).
  ENDMETHOD.


  METHOD alu_cp.
    " Compare (subtract without storing result) - sets flags only
    DATA: lv_result TYPE i.
    lv_result = alu_sub( iv_a = iv_a iv_b = iv_b ).
    " Result is discarded, flags are already set by alu_sub
  ENDMETHOD.

ENDCLASS.
