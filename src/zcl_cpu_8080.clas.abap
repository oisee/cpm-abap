*&---------------------------------------------------------------------*
*& Z80/i8080 CPU Emulator - Core Class
*& Based on RunCPM architecture with hybrid switch/case + lookup tables
*&---------------------------------------------------------------------*
CLASS zcl_cpu_8080 DEFINITION
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
      c_flag_c TYPE x LENGTH 1 VALUE '01',  " Carry
      c_flag_n TYPE x LENGTH 1 VALUE '02',  " Add/Subtract (BCD)
      c_flag_p TYPE x LENGTH 1 VALUE '04',  " Parity/Overflow
      c_flag_h TYPE x LENGTH 1 VALUE '10',  " Half-carry (bit 3->4)
      c_flag_z TYPE x LENGTH 1 VALUE '40',  " Zero
      c_flag_s TYPE x LENGTH 1 VALUE '80'.  " Sign (bit 7)

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
        RETURNING VALUE(rv_val) TYPE x LENGTH 1.

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

    " === Memory (64KB) ===
    TYPES: ty_memory TYPE STANDARD TABLE OF x LENGTH 1 WITH EMPTY KEY.
    DATA: mt_memory TYPE ty_memory.

    " === Pre-computed Lookup Tables ===
    DATA:
      mt_parity_table TYPE ty_memory,  " Parity flag for 0..255
      mt_inc_table    TYPE ty_memory,  " INC instruction flags (0..256)
      mt_dec_table    TYPE ty_memory,  " DEC instruction flags (0..255)
      mt_cbits_table  TYPE ty_memory.  " Carry bits for ADD/SUB (0..511)

    " === Helper Methods - Bit Manipulation ===
    METHODS:
      " Register byte access
      get_high_byte
        IMPORTING iv_pair       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      get_low_byte
        IMPORTING iv_pair       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      set_high_byte
        IMPORTING iv_pair       TYPE i
                  iv_val        TYPE i
        RETURNING VALUE(rv_new) TYPE i,

      set_low_byte
        IMPORTING iv_pair       TYPE i
                  iv_val        TYPE i
        RETURNING VALUE(rv_new) TYPE i,

      " Flag operations
      set_flag
        IMPORTING iv_flag TYPE x LENGTH 1
                  iv_cond TYPE abap_bool,

      test_flag
        IMPORTING iv_flag       TYPE x LENGTH 1
        RETURNING VALUE(rv_set) TYPE abap_bool,

      set_flags_byte
        IMPORTING iv_flags TYPE x LENGTH 1,

      get_flags_byte
        RETURNING VALUE(rv_flags) TYPE x LENGTH 1.

    " === Memory Access ===
    METHODS:
      read_byte
        IMPORTING iv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE x LENGTH 1,

      write_byte
        IMPORTING iv_addr TYPE i
                  iv_val  TYPE x LENGTH 1,

      read_word
        IMPORTING iv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      write_word
        IMPORTING iv_addr TYPE i
                  iv_val  TYPE i,

      " Convenience methods (post-increment/decrement)
      read_byte_pp
        CHANGING cv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE x LENGTH 1,

      read_word_pp
        CHANGING cv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE i.

    " === Lookup Table Initialization ===
    METHODS:
      init_tables,

      calc_parity
        IMPORTING iv_val        TYPE i
        RETURNING VALUE(rv_par) TYPE i.

    " === Opcode Execution ===
    METHODS:
      execute_opcode
        IMPORTING iv_opcode TYPE x LENGTH 1
        RETURNING VALUE(rv_cycles) TYPE i.

ENDCLASS.


CLASS zcl_cpu_8080 IMPLEMENTATION.

  METHOD constructor.
    " Initialize memory (64KB of zeros)
    DO 65536 TIMES.
      APPEND '00' TO mt_memory.
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
          lv_par   TYPE i.

    " === Parity Table (256 entries) ===
    " Parity flag: 4 if even parity (even number of 1 bits), 0 if odd
    CLEAR mt_parity_table.
    DO 256 TIMES.
      lv_val = sy-index - 1.
      lv_par = calc_parity( lv_val ).
      IF lv_par MOD 2 = 0.  " Even parity
        APPEND '04' TO mt_parity_table.
      ELSE.
        APPEND '00' TO mt_parity_table.
      ENDIF.
    ENDDO.

    " === INC Table (257 entries - includes 256) ===
    " Flags: (val & 0xA8) | ((val == 0) << 6) | ((val & 0x0F) == 0) << 4)
    CLEAR mt_inc_table.
    DO 257 TIMES.
      lv_val = sy-index - 1.
      lv_flags = 0.

      " Bits 7,5,3 of result (undocumented flags)
      DATA(lv_temp) = lv_val.
      lv_temp = lv_temp BIT-AND 168.  " 0xA8
      lv_flags = lv_temp.

      " Zero flag (bit 6)
      IF lv_val MOD 256 = 0.
        lv_flags = lv_flags BIT-OR 64.  " 0x40
      ENDIF.

      " Half-carry flag (bit 4) - set if lower nibble is 0
      IF lv_val MOD 16 = 0.
        lv_flags = lv_flags BIT-OR 16.  " 0x10
      ENDIF.

      APPEND lv_flags TO mt_inc_table.
    ENDDO.

    " === DEC Table (256 entries) ===
    " Flags: (val & 0xA8) | ((val == 0) << 6) | ((val & 0x0F) == 0x0F) << 4) | 2
    CLEAR mt_dec_table.
    DO 256 TIMES.
      lv_val = sy-index - 1.
      lv_flags = 0.

      " Bits 7,5,3 of result
      lv_temp = lv_val BIT-AND 168.  " 0xA8
      lv_flags = lv_temp.

      " Zero flag
      IF lv_val = 0.
        lv_flags = lv_flags BIT-OR 64.
      ENDIF.

      " Half-carry flag - set if lower nibble is 0x0F
      IF lv_val MOD 16 = 15.
        lv_flags = lv_flags BIT-OR 16.
      ENDIF.

      " N flag (subtract) - always set for DEC
      lv_flags = lv_flags BIT-OR 2.

      APPEND lv_flags TO mt_dec_table.
    ENDDO.

    " === Carry Bits Table (512 entries) ===
    " Used for ADD HL,rr instructions
    " (i & 0x10) | ((i >> 8) & 1)
    CLEAR mt_cbits_table.
    DO 512 TIMES.
      lv_val = sy-index - 1.
      lv_flags = 0.

      " Half-carry from bit 4
      lv_temp = lv_val BIT-AND 16.
      lv_flags = lv_temp.

      " Carry from bit 8
      IF lv_val >= 256.
        lv_flags = lv_flags BIT-OR 1.
      ENDIF.

      APPEND lv_flags TO mt_cbits_table.
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


  METHOD set_flag.
    DATA: lv_f    TYPE i,
          lv_mask TYPE i.

    lv_f = get_low_byte( mv_af ).

    IF iv_cond = abap_true.
      " Set flag bit
      lv_f = lv_f BIT-OR iv_flag.
    ELSE.
      " Clear flag bit
      lv_mask = iv_flag BIT-XOR 255.  " Invert mask
      lv_f = lv_f BIT-AND lv_mask.
    ENDIF.

    mv_af = set_low_byte( iv_pair = mv_af iv_val = lv_f ).
  ENDMETHOD.


  METHOD test_flag.
    DATA(lv_f) = get_low_byte( mv_af ).
    DATA(lv_result) = lv_f BIT-AND iv_flag.

    rv_set = COND #( WHEN lv_result NE 0 THEN abap_true
                     ELSE abap_false ).
  ENDMETHOD.


  METHOD set_flags_byte.
    mv_af = set_low_byte( iv_pair = mv_af iv_val = iv_flags ).
  ENDMETHOD.


  METHOD get_flags_byte.
    rv_flags = get_low_byte( mv_af ).
  ENDMETHOD.


  "=== Memory Access Methods ===

  METHOD read_byte.
    DATA(lv_addr) = iv_addr MOD 65536.  " Wrap to 16-bit
    READ TABLE mt_memory INDEX ( lv_addr + 1 ) INTO rv_val.
  ENDMETHOD.


  METHOD write_byte.
    DATA(lv_addr) = iv_addr MOD 65536.
    mt_memory[ lv_addr + 1 ] = iv_val.
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
          lv_byte   TYPE x LENGTH 1,
          lv_len    TYPE i.

    lv_len = xstrlen( iv_data ).
    lv_offset = 0.

    DO lv_len TIMES.
      lv_byte = iv_data+lv_offset(1).
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
          lv_flags TYPE i.

    " Default cycle count
    rv_cycles = 4.

    " ====== SWITCH/CASE DISPATCH ======
    " Hybrid approach: Direct dispatch + lookup tables for flags

    CASE iv_opcode.

      " === 0x00-0x0F ===
      WHEN '00'.  " NOP
        " Do nothing

      WHEN '01'.  " LD BC,nnnn
        mv_bc = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

      WHEN '02'.  " LD (BC),A
        write_byte( iv_addr = mv_bc
                    iv_val  = get_high_byte( mv_af ) ).
        rv_cycles = 7.

      WHEN '03'.  " INC BC
        mv_bc = ( mv_bc + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN '04'.  " INC B
        mv_bc = mv_bc + 256.  " Increment high byte
        lv_temp = get_high_byte( mv_bc ).
        " Use pre-computed table for flags (preserving carry)
        READ TABLE mt_inc_table INDEX ( lv_temp + 1 ) INTO DATA(lv_inc_flags).
        lv_flags = get_low_byte( mv_af ) BIT-AND c_flag_c.  " Keep carry
        lv_flags = lv_flags BIT-OR lv_inc_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN '05'.  " DEC B
        mv_bc = mv_bc - 256.  " Decrement high byte
        lv_temp = get_high_byte( mv_bc ).
        " Use pre-computed table
        READ TABLE mt_dec_table INDEX ( lv_temp + 1 ) INTO DATA(lv_dec_flags).
        lv_flags = get_low_byte( mv_af ) BIT-AND c_flag_c.  " Keep carry
        lv_flags = lv_flags BIT-OR lv_dec_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN '06'.  " LD B,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        mv_bc = set_high_byte( iv_pair = mv_bc iv_val = lv_val ).
        rv_cycles = 7.

      WHEN '0A'.  " LD A,(BC)
        lv_val = read_byte( mv_bc ).
        mv_af = set_high_byte( iv_pair = mv_af iv_val = lv_val ).
        rv_cycles = 7.

      WHEN '0B'.  " DEC BC
        mv_bc = ( mv_bc - 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN '0C'.  " INC C
        lv_temp = ( get_low_byte( mv_bc ) + 1 ) MOD 256.
        mv_bc = set_low_byte( iv_pair = mv_bc iv_val = lv_temp ).
        " Use pre-computed table
        READ TABLE mt_inc_table INDEX ( lv_temp + 1 ) INTO lv_inc_flags.
        lv_flags = get_low_byte( mv_af ) BIT-AND c_flag_c.
        lv_flags = lv_flags BIT-OR lv_inc_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN '0D'.  " DEC C
        lv_temp = ( get_low_byte( mv_bc ) - 1 ) MOD 256.
        mv_bc = set_low_byte( iv_pair = mv_bc iv_val = lv_temp ).
        " Use pre-computed table
        READ TABLE mt_dec_table INDEX ( lv_temp + 1 ) INTO lv_dec_flags.
        lv_flags = get_low_byte( mv_af ) BIT-AND c_flag_c.
        lv_flags = lv_flags BIT-OR lv_dec_flags.
        set_flags_byte( lv_flags ).
        rv_cycles = 4.

      WHEN '0E'.  " LD C,nn
        lv_val = read_byte_pp( CHANGING cv_addr = mv_pc ).
        mv_bc = set_low_byte( iv_pair = mv_bc iv_val = lv_val ).
        rv_cycles = 7.

      " === 0x10-0x1F ===
      WHEN '11'.  " LD DE,nnnn
        mv_de = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

      WHEN '13'.  " INC DE
        mv_de = ( mv_de + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN '1B'.  " DEC DE
        mv_de = ( mv_de - 1 ) MOD 65536.
        rv_cycles = 6.

      " === 0x20-0x2F ===
      WHEN '21'.  " LD HL,nnnn
        mv_hl = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

      WHEN '23'.  " INC HL
        mv_hl = ( mv_hl + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN '2B'.  " DEC HL
        mv_hl = ( mv_hl - 1 ) MOD 65536.
        rv_cycles = 6.

      " === 0x30-0x3F ===
      WHEN '31'.  " LD SP,nnnn
        mv_sp = read_word_pp( CHANGING cv_addr = mv_pc ).
        rv_cycles = 10.

      WHEN '33'.  " INC SP
        mv_sp = ( mv_sp + 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN '3B'.  " DEC SP
        mv_sp = ( mv_sp - 1 ) MOD 65536.
        rv_cycles = 6.

      WHEN '76'.  " HALT
        mv_status = c_status_halted.
        rv_cycles = 4.

      WHEN 'C3'.  " JP nnnn (unconditional jump)
        mv_pc = read_word( mv_pc ).
        rv_cycles = 10.

      WHEN 'C9'.  " RET (return from subroutine)
        mv_pc = read_word( mv_sp ).
        mv_sp = ( mv_sp + 2 ) MOD 65536.
        rv_cycles = 10.

      WHEN 'CD'.  " CALL nnnn (call subroutine)
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
        " TODO: Add error reporting
        rv_cycles = 4.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
