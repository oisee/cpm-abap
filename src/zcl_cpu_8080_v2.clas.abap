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

    " === Memory (64KB as XSTRING - transpiler-compatible) ===
    " Each byte = 2 hex chars, so 131072 chars total
    DATA: mv_memory TYPE xstring.

    " === Pre-computed Lookup Tables (as XSTRING) ===
    DATA:
      mv_parity_table TYPE xstring,  " Parity flag for 0..255 (512 hex chars)
      mv_inc_table    TYPE xstring,  " INC instruction flags (514 hex chars for 257 entries)
      mv_dec_table    TYPE xstring,  " DEC instruction flags (512 hex chars)
      mv_cbits_table  TYPE xstring.  " Carry bits (1024 hex chars for 512 entries)

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
        RETURNING VALUE(rv_cycles) TYPE i.

ENDCLASS.


CLASS zcl_cpu_8080_v2 IMPLEMENTATION.

  METHOD constructor.
    " Initialize memory (64KB = 131072 hex chars, all zeros)
    DATA: lv_zeros TYPE string VALUE '00'.
    mv_memory = ''.
    DO 65536 TIMES.
      mv_memory = mv_memory && lv_zeros.
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
          lv_hex   TYPE string.

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
      DATA(lv_masked) = lv_temp - ( lv_temp MOD 8 ).  " Keep bits 7,5,3
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
          lv_hex    TYPE string.

    DATA(lv_addr) = iv_addr MOD 65536.  " Wrap to 16-bit
    lv_offset = lv_addr * 2.  " 2 hex chars per byte

    lv_hex = mv_memory+lv_offset(2).
    rv_val = hex_to_byte( lv_hex ).
  ENDMETHOD.


  METHOD write_byte.
    " Write byte to memory (XSTRING)
    DATA: lv_offset TYPE i,
          lv_hex    TYPE string,
          lv_before TYPE string,
          lv_after  TYPE string.

    DATA(lv_addr) = iv_addr MOD 65536.
    DATA(lv_byte) = iv_val MOD 256.
    lv_offset = lv_addr * 2.

    lv_hex = byte_to_hex( lv_byte ).

    " Replace 2 chars at offset
    IF lv_offset > 0.
      lv_before = mv_memory+0(lv_offset).
    ELSE.
      lv_before = ''.
    ENDIF.

    DATA(lv_after_offset) = lv_offset + 2.
    DATA(lv_remaining) = 131072 - lv_after_offset.
    IF lv_remaining > 0.
      lv_after = mv_memory+lv_after_offset(lv_remaining).
    ELSE.
      lv_after = ''.
    ENDIF.

    mv_memory = lv_before && lv_hex && lv_after.
  ENDMETHOD.


  METHOD read_word.
    " Little-endian: low byte first, then high byte
    DATA(lv_low) = read_byte( iv_addr ).
    DATA(lv_high) = read_byte( iv_addr + 1 ).
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
    DATA(lv_opcode) = read_byte_pp( CHANGING cv_addr = mv_pc ).

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
          lv_hex   TYPE string.

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
        DATA(lv_offset) = lv_temp * 2.
        lv_hex = mv_inc_table+lv_offset(2).
        DATA(lv_inc_flags) = hex_to_byte( lv_hex ).
        lv_flags = get_flags_byte( ) MOD 2.  " Keep carry only
        lv_flags = lv_flags + lv_inc_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN 5.  " DEC B
        mv_bc = mv_bc - 256.  " Decrement high byte
        lv_temp = get_high_byte( mv_bc ).
        lv_offset = lv_temp * 2.
        lv_hex = mv_dec_table+lv_offset(2).
        DATA(lv_dec_flags) = hex_to_byte( lv_hex ).
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

      WHEN 14.  " LD C,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        mv_bc = set_low_byte( iv_pair = mv_bc iv_val = lv_val ).
        rv_cycles = 7.

      " === 0x10-0x1F ===
      WHEN 17.  " LD DE,nnnn
        mv_de = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

      WHEN 19.  " INC DE
        mv_de = ( mv_de + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 27.  " DEC DE
        mv_de = ( mv_de - 1 ) MOD 65536.
        rv_cycles = 6.

      " === 0x20-0x2F ===
      WHEN 33.  " LD HL,nnnn
        mv_hl = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

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

      WHEN 51.  " INC SP
        mv_sp = ( mv_sp + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 59.  " DEC SP
        mv_sp = ( mv_sp - 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN 118.  " HALT
        mv_status = c_status_halted.
        rv_cycles = 4.

      WHEN 195.  " JP nnnn (unconditional jump)
        mv_pc = read_word( mv_pc ).
        rv_cycles = 10.

      WHEN 201.  " RET (return from subroutine)
        mv_pc = read_word( mv_sp ).
        mv_sp = ( mv_sp + 2 ) MOD 65536.
        rv_cycles = 10.

      WHEN 205.  " CALL nnnn (call subroutine)
        lv_addr = read_word( mv_pc ).
        mv_pc = mv_pc + 2.
        " Push return address onto stack
        mv_sp = ( mv_sp - 2 ) MOD 65536.
        write_word( iv_addr = mv_sp iv_val = mv_pc ).
        " Jump to target
        mv_pc = lv_addr.
        rv_cycles = 17.

      WHEN OTHERS.
        " Unimplemented opcode - halt with error
        mv_status = c_status_halted.
        rv_cycles = 4.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
