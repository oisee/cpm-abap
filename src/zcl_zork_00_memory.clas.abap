*&---------------------------------------------------------------------*
*& Z-Machine Memory Management Class
*& Handles story file loading, read/write operations, header parsing
*& Transpiler-compatible: uses STRING with 2 hex chars per byte
*&---------------------------------------------------------------------*
CLASS zcl_zork_00_memory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "======================================================================
    " Public Methods
    "======================================================================
    METHODS:
      constructor,

      " Story file loading
      load_story
        IMPORTING iv_story TYPE string,  " Hex-encoded story file

      load_story_xstring
        IMPORTING iv_data TYPE xstring,  " Binary story file

      " Byte operations
      read_byte
        IMPORTING iv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      write_byte
        IMPORTING iv_addr TYPE i
                  iv_val  TYPE i,

      " Word operations (big-endian, Z-machine standard)
      read_word
        IMPORTING iv_addr       TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      write_word
        IMPORTING iv_addr TYPE i
                  iv_val  TYPE i,

      " Header access
      get_header
        RETURNING VALUE(rs_header) TYPE zif_zork_00_types=>ty_header,

      get_version
        RETURNING VALUE(rv_version) TYPE i,

      get_init_pc
        RETURNING VALUE(rv_pc) TYPE i,

      get_globals_addr
        RETURNING VALUE(rv_addr) TYPE i,

      get_objects_addr
        RETURNING VALUE(rv_addr) TYPE i,

      get_dictionary_addr
        RETURNING VALUE(rv_addr) TYPE i,

      get_abbrev_addr
        RETURNING VALUE(rv_addr) TYPE i,

      get_static_mem_base
        RETURNING VALUE(rv_addr) TYPE i,

      get_high_mem_base
        RETURNING VALUE(rv_addr) TYPE i,

      " Memory info
      get_memory_size
        RETURNING VALUE(rv_size) TYPE i,

      is_dynamic_memory
        IMPORTING iv_addr        TYPE i
        RETURNING VALUE(rv_dyn) TYPE abap_bool,

      " Packed address conversion (v3: multiply by 2)
      unpack_routine_addr
        IMPORTING iv_packed      TYPE i
        RETURNING VALUE(rv_addr) TYPE i,

      unpack_string_addr
        IMPORTING iv_packed      TYPE i
        RETURNING VALUE(rv_addr) TYPE i,

      " Debugging
      dump_memory
        IMPORTING iv_addr       TYPE i
                  iv_len        TYPE i DEFAULT 16
        RETURNING VALUE(rv_hex) TYPE string.

  PRIVATE SECTION.

    "======================================================================
    " Instance Data
    "======================================================================
    DATA:
      mv_memory      TYPE string,   " Story file (hex encoded, 2 chars/byte)
      mv_size        TYPE i,        " Actual memory size in bytes
      ms_header      TYPE zif_zork_00_types=>ty_header,
      mv_dynamic_end TYPE i,        " End of dynamic memory (= static_mem)
      mv_high_start  TYPE i.        " Start of high memory

    "======================================================================
    " Private Methods
    "======================================================================
    METHODS:
      parse_header,

      hex_to_byte
        IMPORTING iv_hex         TYPE string
        RETURNING VALUE(rv_byte) TYPE i,

      byte_to_hex
        IMPORTING iv_byte       TYPE i
        RETURNING VALUE(rv_hex) TYPE string,

      xstring_to_hex
        IMPORTING iv_xstr       TYPE xstring
        RETURNING VALUE(rv_hex) TYPE string.

ENDCLASS.


CLASS zcl_zork_00_memory IMPLEMENTATION.

  METHOD constructor.
    " Initialize empty memory
    mv_memory = ''.
    mv_size = 0.
    CLEAR ms_header.
  ENDMETHOD.


  METHOD load_story.
    " Load hex-encoded story file
    " Input: STRING with 2 uppercase hex chars per byte
    mv_memory = iv_story.
    mv_size = strlen( iv_story ) / 2.

    " Parse header information
    parse_header( ).
  ENDMETHOD.


  METHOD load_story_xstring.
    " Convert binary to hex string and load
    mv_memory = xstring_to_hex( iv_data ).
    mv_size = xstrlen( iv_data ).

    " Parse header information
    parse_header( ).
  ENDMETHOD.


  METHOD read_byte.
    " Read single byte from memory
    " Returns 0 if address out of bounds
    IF iv_addr < 0 OR iv_addr >= mv_size.
      rv_val = 0.
      RETURN.
    ENDIF.

    " Calculate hex string offset (2 chars per byte)
    DATA(lv_offset) = iv_addr * 2.

    " Extract 2 hex characters
    DATA(lv_hex) = mv_memory+lv_offset(2).

    " Convert to integer
    rv_val = hex_to_byte( lv_hex ).
  ENDMETHOD.


  METHOD write_byte.
    " Write single byte to memory
    " Only dynamic memory is writable (0 to static_mem - 1)
    DATA: lv_offset       TYPE i,
          lv_hex          TYPE string,
          lv_before       TYPE string,
          lv_after        TYPE string,
          lv_after_start  TYPE i,
          lv_remaining    TYPE i,
          lv_val          TYPE i,
          lv_mem_len      TYPE i.

    IF iv_addr < 0 OR iv_addr >= mv_dynamic_end.
      " Illegal write to static/high memory - silently ignore
      RETURN.
    ENDIF.

    " Ensure value is 0-255
    lv_val = iv_val MOD 256.
    IF lv_val < 0.
      lv_val = lv_val + 256.
    ENDIF.

    " Convert to hex
    lv_hex = byte_to_hex( lv_val ).

    " Calculate offset and memory length
    lv_offset = iv_addr * 2.
    lv_mem_len = strlen( mv_memory ).

    " String replacement (transpiler compatible - MUST use explicit lengths)
    IF lv_offset > 0.
      lv_before = mv_memory+0(lv_offset).
    ELSE.
      lv_before = ''.
    ENDIF.

    lv_after_start = lv_offset + 2.
    lv_remaining = lv_mem_len - lv_after_start.
    IF lv_remaining > 0.
      lv_after = mv_memory+lv_after_start(lv_remaining).
    ELSE.
      lv_after = ''.
    ENDIF.

    mv_memory = lv_before && lv_hex && lv_after.
  ENDMETHOD.


  METHOD read_word.
    " Read 16-bit word (big-endian, Z-machine standard)
    DATA(lv_high) = read_byte( iv_addr ).
    DATA(lv_low) = read_byte( iv_addr + 1 ).
    rv_val = lv_high * 256 + lv_low.
  ENDMETHOD.


  METHOD write_word.
    " Write 16-bit word (big-endian)
    DATA(lv_high) = iv_val DIV 256.
    DATA(lv_low) = iv_val MOD 256.

    " Handle signed values
    IF lv_high < 0.
      lv_high = lv_high + 256.
    ENDIF.
    IF lv_low < 0.
      lv_low = lv_low + 256.
    ENDIF.

    write_byte( iv_addr = iv_addr iv_val = lv_high ).
    write_byte( iv_addr = iv_addr + 1 iv_val = lv_low ).
  ENDMETHOD.


  METHOD parse_header.
    " Extract header information from loaded story file
    " Z-machine header is 64 bytes at address 0

    IF mv_size < 64.
      " Invalid story file - too short
      CLEAR ms_header.
      RETURN.
    ENDIF.

    ms_header-version    = read_byte( zif_zork_00_types=>c_hdr_version ).
    ms_header-flags1     = read_byte( zif_zork_00_types=>c_hdr_flags1 ).
    ms_header-high_mem   = read_word( zif_zork_00_types=>c_hdr_high_mem ).
    ms_header-init_pc    = read_word( zif_zork_00_types=>c_hdr_init_pc ).
    ms_header-dictionary = read_word( zif_zork_00_types=>c_hdr_dictionary ).
    ms_header-objects    = read_word( zif_zork_00_types=>c_hdr_objects ).
    ms_header-globals    = read_word( zif_zork_00_types=>c_hdr_globals ).
    ms_header-static_mem = read_word( zif_zork_00_types=>c_hdr_static_mem ).
    ms_header-flags2     = read_byte( zif_zork_00_types=>c_hdr_flags2 ).
    ms_header-abbrev     = read_word( zif_zork_00_types=>c_hdr_abbrev ).

    " File length calculation (version dependent)
    DATA(lv_len_raw) = read_word( zif_zork_00_types=>c_hdr_file_len ).
    CASE ms_header-version.
      WHEN 1 OR 2 OR 3.
        ms_header-file_len = lv_len_raw * 2.
      WHEN 4 OR 5.
        ms_header-file_len = lv_len_raw * 4.
      WHEN 6 OR 7 OR 8.
        ms_header-file_len = lv_len_raw * 8.
      WHEN OTHERS.
        ms_header-file_len = lv_len_raw * 2.
    ENDCASE.

    ms_header-checksum = read_word( zif_zork_00_types=>c_hdr_checksum ).

    " Set memory region boundaries
    mv_dynamic_end = ms_header-static_mem.
    mv_high_start = ms_header-high_mem.
  ENDMETHOD.


  METHOD get_header.
    rs_header = ms_header.
  ENDMETHOD.


  METHOD get_version.
    rv_version = ms_header-version.
  ENDMETHOD.


  METHOD get_init_pc.
    rv_pc = ms_header-init_pc.
  ENDMETHOD.


  METHOD get_globals_addr.
    rv_addr = ms_header-globals.
  ENDMETHOD.


  METHOD get_objects_addr.
    rv_addr = ms_header-objects.
  ENDMETHOD.


  METHOD get_dictionary_addr.
    rv_addr = ms_header-dictionary.
  ENDMETHOD.


  METHOD get_abbrev_addr.
    rv_addr = ms_header-abbrev.
  ENDMETHOD.


  METHOD get_static_mem_base.
    rv_addr = ms_header-static_mem.
  ENDMETHOD.


  METHOD get_high_mem_base.
    rv_addr = ms_header-high_mem.
  ENDMETHOD.


  METHOD get_memory_size.
    rv_size = mv_size.
  ENDMETHOD.


  METHOD is_dynamic_memory.
    IF iv_addr >= 0 AND iv_addr < mv_dynamic_end.
      rv_dyn = abap_true.
    ELSE.
      rv_dyn = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD unpack_routine_addr.
    " Convert packed routine address to byte address
    " v1-3: multiply by 2
    " v4-5: multiply by 4
    " v6-7: multiply by 4 and add routine offset
    " v8: multiply by 8
    CASE ms_header-version.
      WHEN 1 OR 2 OR 3.
        rv_addr = iv_packed * 2.
      WHEN 4 OR 5.
        rv_addr = iv_packed * 4.
      WHEN 6 OR 7.
        rv_addr = iv_packed * 4.  " + routine_offset (not implemented)
      WHEN 8.
        rv_addr = iv_packed * 8.
      WHEN OTHERS.
        rv_addr = iv_packed * 2.
    ENDCASE.
  ENDMETHOD.


  METHOD unpack_string_addr.
    " Convert packed string address to byte address
    " Same as routine address for v1-5
    " v6-7: multiply by 4 and add string offset
    CASE ms_header-version.
      WHEN 1 OR 2 OR 3.
        rv_addr = iv_packed * 2.
      WHEN 4 OR 5.
        rv_addr = iv_packed * 4.
      WHEN 6 OR 7.
        rv_addr = iv_packed * 4.  " + string_offset (not implemented)
      WHEN 8.
        rv_addr = iv_packed * 8.
      WHEN OTHERS.
        rv_addr = iv_packed * 2.
    ENDCASE.
  ENDMETHOD.


  METHOD dump_memory.
    " Return hex dump of memory region for debugging
    rv_hex = ''.

    DATA(lv_end) = iv_addr + iv_len.
    IF lv_end > mv_size.
      lv_end = mv_size.
    ENDIF.

    DATA(lv_addr) = iv_addr.
    WHILE lv_addr < lv_end.
      DATA(lv_byte) = read_byte( lv_addr ).
      rv_hex = rv_hex && byte_to_hex( lv_byte ).
      lv_addr = lv_addr + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD hex_to_byte.
    " Convert 2-char hex string to integer (0-255)
    " Example: 'FF' -> 255
    DATA: lv_high TYPE i,
          lv_low  TYPE i.

    DATA(lv_c1) = iv_hex(1).
    DATA(lv_c2) = iv_hex+1(1).

    " Convert first char
    CASE lv_c1.
      WHEN '0'. lv_high = 0.
      WHEN '1'. lv_high = 1.
      WHEN '2'. lv_high = 2.
      WHEN '3'. lv_high = 3.
      WHEN '4'. lv_high = 4.
      WHEN '5'. lv_high = 5.
      WHEN '6'. lv_high = 6.
      WHEN '7'. lv_high = 7.
      WHEN '8'. lv_high = 8.
      WHEN '9'. lv_high = 9.
      WHEN 'A' OR 'a'. lv_high = 10.
      WHEN 'B' OR 'b'. lv_high = 11.
      WHEN 'C' OR 'c'. lv_high = 12.
      WHEN 'D' OR 'd'. lv_high = 13.
      WHEN 'E' OR 'e'. lv_high = 14.
      WHEN 'F' OR 'f'. lv_high = 15.
      WHEN OTHERS. lv_high = 0.
    ENDCASE.

    " Convert second char
    CASE lv_c2.
      WHEN '0'. lv_low = 0.
      WHEN '1'. lv_low = 1.
      WHEN '2'. lv_low = 2.
      WHEN '3'. lv_low = 3.
      WHEN '4'. lv_low = 4.
      WHEN '5'. lv_low = 5.
      WHEN '6'. lv_low = 6.
      WHEN '7'. lv_low = 7.
      WHEN '8'. lv_low = 8.
      WHEN '9'. lv_low = 9.
      WHEN 'A' OR 'a'. lv_low = 10.
      WHEN 'B' OR 'b'. lv_low = 11.
      WHEN 'C' OR 'c'. lv_low = 12.
      WHEN 'D' OR 'd'. lv_low = 13.
      WHEN 'E' OR 'e'. lv_low = 14.
      WHEN 'F' OR 'f'. lv_low = 15.
      WHEN OTHERS. lv_low = 0.
    ENDCASE.

    rv_byte = lv_high * 16 + lv_low.
  ENDMETHOD.


  METHOD byte_to_hex.
    " Convert integer (0-255) to 2-char uppercase hex string
    DATA: lv_high TYPE i,
          lv_low  TYPE i,
          lv_c1   TYPE string,
          lv_c2   TYPE string.

    DATA(lv_val) = iv_byte MOD 256.
    IF lv_val < 0.
      lv_val = lv_val + 256.
    ENDIF.

    lv_high = lv_val DIV 16.
    lv_low = lv_val MOD 16.

    " Convert high nibble
    CASE lv_high.
      WHEN 0.  lv_c1 = '0'.
      WHEN 1.  lv_c1 = '1'.
      WHEN 2.  lv_c1 = '2'.
      WHEN 3.  lv_c1 = '3'.
      WHEN 4.  lv_c1 = '4'.
      WHEN 5.  lv_c1 = '5'.
      WHEN 6.  lv_c1 = '6'.
      WHEN 7.  lv_c1 = '7'.
      WHEN 8.  lv_c1 = '8'.
      WHEN 9.  lv_c1 = '9'.
      WHEN 10. lv_c1 = 'A'.
      WHEN 11. lv_c1 = 'B'.
      WHEN 12. lv_c1 = 'C'.
      WHEN 13. lv_c1 = 'D'.
      WHEN 14. lv_c1 = 'E'.
      WHEN 15. lv_c1 = 'F'.
      WHEN OTHERS. lv_c1 = '0'.
    ENDCASE.

    " Convert low nibble
    CASE lv_low.
      WHEN 0.  lv_c2 = '0'.
      WHEN 1.  lv_c2 = '1'.
      WHEN 2.  lv_c2 = '2'.
      WHEN 3.  lv_c2 = '3'.
      WHEN 4.  lv_c2 = '4'.
      WHEN 5.  lv_c2 = '5'.
      WHEN 6.  lv_c2 = '6'.
      WHEN 7.  lv_c2 = '7'.
      WHEN 8.  lv_c2 = '8'.
      WHEN 9.  lv_c2 = '9'.
      WHEN 10. lv_c2 = 'A'.
      WHEN 11. lv_c2 = 'B'.
      WHEN 12. lv_c2 = 'C'.
      WHEN 13. lv_c2 = 'D'.
      WHEN 14. lv_c2 = 'E'.
      WHEN 15. lv_c2 = 'F'.
      WHEN OTHERS. lv_c2 = '0'.
    ENDCASE.

    rv_hex = lv_c1 && lv_c2.
  ENDMETHOD.


  METHOD xstring_to_hex.
    " Convert XSTRING to hex STRING
    " This is needed for loading binary story files
    rv_hex = ''.

    DATA(lv_len) = xstrlen( iv_xstr ).
    DATA(lv_i) = 0.

    WHILE lv_i < lv_len.
      " Extract single byte from xstring
      DATA(lv_byte_x) = iv_xstr+lv_i(1).
      DATA(lv_byte_i) = 0.

      " Convert xstring byte to integer
      " This is transpiler-compatible using string conversion
      DATA(lv_hex_str) = |{ lv_byte_x }|.
      lv_byte_i = hex_to_byte( lv_hex_str ).

      rv_hex = rv_hex && byte_to_hex( lv_byte_i ).
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
