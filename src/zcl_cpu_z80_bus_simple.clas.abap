CLASS zcl_cpu_z80_bus_simple DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cpu_z80_bus.

    METHODS constructor.
    METHODS get_memory RETURNING VALUE(rv_mem) TYPE xstring.
    METHODS dump_memory IMPORTING iv_start TYPE i DEFAULT 0
                                  iv_length TYPE i DEFAULT 256
                        RETURNING VALUE(rv_hex) TYPE string.

  PRIVATE SECTION.
    " Memory: 64KB addressable (0x0000-0xFFFF)
    " Using string for transpiler compatibility
    DATA mv_memory TYPE string.

    " I/O ports: 256 ports (0x00-0xFF)
    DATA mv_io_ports TYPE string.

    " Output buffer for console
    DATA mv_output TYPE string.

    " Input buffer
    DATA mv_input TYPE string.
    DATA mv_input_pos TYPE i.

    CONSTANTS c_mem_size TYPE i VALUE 65536.  " 64KB
    CONSTANTS c_io_size TYPE i VALUE 256.     " 256 I/O ports

    METHODS init_memory.
    METHODS char_to_ascii IMPORTING iv_char TYPE c RETURNING VALUE(rv_code) TYPE i.
    METHODS ascii_to_char IMPORTING iv_code TYPE i RETURNING VALUE(rv_char) TYPE string.
ENDCLASS.


CLASS zcl_cpu_z80_bus_simple IMPLEMENTATION.
  METHOD constructor.
    init_memory( ).
  ENDMETHOD.

  METHOD init_memory.
    " Initialize memory as string of hex chars (2 chars per byte)
    " For 64KB = 131072 chars
    DATA lv_zeros TYPE string.
    DATA lv_i TYPE i.

    " Build in chunks of 1024 bytes (2048 chars) for efficiency
    lv_zeros = `00000000000000000000000000000000`.  " 16 bytes
    DO 6 TIMES.
      lv_zeros = lv_zeros && lv_zeros.  " 32, 64, 128, 256, 512, 1024 bytes
    ENDDO.

    " Now lv_zeros is 1024 bytes (2048 chars)
    " Need 64 of these for 64KB
    mv_memory = ``.
    DO 64 TIMES.
      mv_memory = mv_memory && lv_zeros.
    ENDDO.

    " Initialize I/O ports (256 bytes = 512 chars)
    mv_io_ports = ``.
    DO 16 TIMES.
      mv_io_ports = mv_io_ports && `00000000000000000000000000000000`.  " 16 bytes each
    ENDDO.

    mv_output = ``.
    mv_input = ``.
    mv_input_pos = 0.
  ENDMETHOD.

  METHOD zif_cpu_z80_bus~read_mem.
    DATA lv_addr TYPE i.
    DATA lv_pos TYPE i.
    DATA lv_hex TYPE string.

    " Wrap address to 16-bit range
    lv_addr = iv_addr MOD c_mem_size.
    IF lv_addr < 0.
      lv_addr = lv_addr + c_mem_size.
    ENDIF.

    " Each byte is 2 hex chars
    lv_pos = lv_addr * 2.
    lv_hex = substring( val = mv_memory off = lv_pos len = 2 ).

    " Convert hex to integer
    DATA lv_hi TYPE i.
    DATA lv_lo TYPE i.
    DATA lv_c TYPE c LENGTH 1.

    lv_c = substring( val = lv_hex off = 0 len = 1 ).
    CASE lv_c.
      WHEN '0'. lv_hi = 0.  WHEN '1'. lv_hi = 1.  WHEN '2'. lv_hi = 2.  WHEN '3'. lv_hi = 3.
      WHEN '4'. lv_hi = 4.  WHEN '5'. lv_hi = 5.  WHEN '6'. lv_hi = 6.  WHEN '7'. lv_hi = 7.
      WHEN '8'. lv_hi = 8.  WHEN '9'. lv_hi = 9.  WHEN 'A' OR 'a'. lv_hi = 10.
      WHEN 'B' OR 'b'. lv_hi = 11.  WHEN 'C' OR 'c'. lv_hi = 12.
      WHEN 'D' OR 'd'. lv_hi = 13.  WHEN 'E' OR 'e'. lv_hi = 14.  WHEN 'F' OR 'f'. lv_hi = 15.
      WHEN OTHERS. lv_hi = 0.
    ENDCASE.

    lv_c = substring( val = lv_hex off = 1 len = 1 ).
    CASE lv_c.
      WHEN '0'. lv_lo = 0.  WHEN '1'. lv_lo = 1.  WHEN '2'. lv_lo = 2.  WHEN '3'. lv_lo = 3.
      WHEN '4'. lv_lo = 4.  WHEN '5'. lv_lo = 5.  WHEN '6'. lv_lo = 6.  WHEN '7'. lv_lo = 7.
      WHEN '8'. lv_lo = 8.  WHEN '9'. lv_lo = 9.  WHEN 'A' OR 'a'. lv_lo = 10.
      WHEN 'B' OR 'b'. lv_lo = 11.  WHEN 'C' OR 'c'. lv_lo = 12.
      WHEN 'D' OR 'd'. lv_lo = 13.  WHEN 'E' OR 'e'. lv_lo = 14.  WHEN 'F' OR 'f'. lv_lo = 15.
      WHEN OTHERS. lv_lo = 0.
    ENDCASE.

    rv_val = lv_hi * 16 + lv_lo.
  ENDMETHOD.

  METHOD zif_cpu_z80_bus~write_mem.
    DATA lv_addr TYPE i.
    DATA lv_val TYPE i.
    DATA lv_pos TYPE i.
    DATA lv_hex TYPE string.
    DATA lv_hi TYPE i.
    DATA lv_lo TYPE i.
    DATA lv_chars TYPE string VALUE `0123456789ABCDEF`.

    " Wrap address to 16-bit range
    lv_addr = iv_addr MOD c_mem_size.
    IF lv_addr < 0.
      lv_addr = lv_addr + c_mem_size.
    ENDIF.

    " Wrap value to 8-bit range
    lv_val = iv_val MOD 256.
    IF lv_val < 0.
      lv_val = lv_val + 256.
    ENDIF.

    " Convert integer to hex
    lv_hi = lv_val DIV 16.
    lv_lo = lv_val MOD 16.
    lv_hex = substring( val = lv_chars off = lv_hi len = 1 ) &&
             substring( val = lv_chars off = lv_lo len = 1 ).

    " Each byte is 2 hex chars
    lv_pos = lv_addr * 2.

    " Replace in memory string
    IF lv_pos = 0.
      mv_memory = lv_hex && substring( val = mv_memory off = 2 ).
    ELSEIF lv_pos >= strlen( mv_memory ) - 2.
      mv_memory = substring( val = mv_memory off = 0 len = lv_pos ) && lv_hex.
    ELSE.
      mv_memory = substring( val = mv_memory off = 0 len = lv_pos ) &&
                  lv_hex &&
                  substring( val = mv_memory off = lv_pos + 2 ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_cpu_z80_bus~read_io.
    DATA lv_port TYPE i.
    DATA lv_pos TYPE i.
    DATA lv_hex TYPE string.

    " Wrap port to 8-bit range
    lv_port = iv_port MOD c_io_size.
    IF lv_port < 0.
      lv_port = lv_port + c_io_size.
    ENDIF.

    " Special handling for console input (port 0x01)
    IF lv_port = 1.
      " Return next character from input buffer, or 0 if empty
      IF mv_input_pos < strlen( mv_input ).
        DATA lv_char TYPE c LENGTH 1.
        lv_char = substring( val = mv_input off = mv_input_pos len = 1 ).
        mv_input_pos = mv_input_pos + 1.
        " Get ASCII code via helper method
        rv_val = char_to_ascii( lv_char ).
        RETURN.
      ELSE.
        rv_val = 0.
        RETURN.
      ENDIF.
    ENDIF.

    " Special handling for console status (port 0x00)
    IF lv_port = 0.
      " Return 0xFF if input ready, 0x00 otherwise
      IF mv_input_pos < strlen( mv_input ).
        rv_val = 255.
      ELSE.
        rv_val = 0.
      ENDIF.
      RETURN.
    ENDIF.

    " Read from I/O port string
    lv_pos = lv_port * 2.
    lv_hex = substring( val = mv_io_ports off = lv_pos len = 2 ).

    " Convert hex to integer (same as read_mem)
    DATA lv_hi TYPE i.
    DATA lv_lo TYPE i.
    DATA lv_c TYPE c LENGTH 1.

    lv_c = substring( val = lv_hex off = 0 len = 1 ).
    CASE lv_c.
      WHEN '0'. lv_hi = 0.  WHEN '1'. lv_hi = 1.  WHEN '2'. lv_hi = 2.  WHEN '3'. lv_hi = 3.
      WHEN '4'. lv_hi = 4.  WHEN '5'. lv_hi = 5.  WHEN '6'. lv_hi = 6.  WHEN '7'. lv_hi = 7.
      WHEN '8'. lv_hi = 8.  WHEN '9'. lv_hi = 9.  WHEN 'A' OR 'a'. lv_hi = 10.
      WHEN 'B' OR 'b'. lv_hi = 11.  WHEN 'C' OR 'c'. lv_hi = 12.
      WHEN 'D' OR 'd'. lv_hi = 13.  WHEN 'E' OR 'e'. lv_hi = 14.  WHEN 'F' OR 'f'. lv_hi = 15.
      WHEN OTHERS. lv_hi = 0.
    ENDCASE.

    lv_c = substring( val = lv_hex off = 1 len = 1 ).
    CASE lv_c.
      WHEN '0'. lv_lo = 0.  WHEN '1'. lv_lo = 1.  WHEN '2'. lv_lo = 2.  WHEN '3'. lv_lo = 3.
      WHEN '4'. lv_lo = 4.  WHEN '5'. lv_lo = 5.  WHEN '6'. lv_lo = 6.  WHEN '7'. lv_lo = 7.
      WHEN '8'. lv_lo = 8.  WHEN '9'. lv_lo = 9.  WHEN 'A' OR 'a'. lv_lo = 10.
      WHEN 'B' OR 'b'. lv_lo = 11.  WHEN 'C' OR 'c'. lv_lo = 12.
      WHEN 'D' OR 'd'. lv_lo = 13.  WHEN 'E' OR 'e'. lv_lo = 14.  WHEN 'F' OR 'f'. lv_lo = 15.
      WHEN OTHERS. lv_lo = 0.
    ENDCASE.

    rv_val = lv_hi * 16 + lv_lo.
  ENDMETHOD.

  METHOD zif_cpu_z80_bus~write_io.
    DATA lv_port TYPE i.
    DATA lv_val TYPE i.
    DATA lv_pos TYPE i.
    DATA lv_hex TYPE string.
    DATA lv_hi TYPE i.
    DATA lv_lo TYPE i.
    DATA lv_chars TYPE string VALUE `0123456789ABCDEF`.

    " Wrap port to 8-bit range
    lv_port = iv_port MOD c_io_size.
    IF lv_port < 0.
      lv_port = lv_port + c_io_size.
    ENDIF.

    " Wrap value to 8-bit range
    lv_val = iv_val MOD 256.
    IF lv_val < 0.
      lv_val = lv_val + 256.
    ENDIF.

    " Special handling for console output (port 0x01)
    IF lv_port = 1.
      " Append character to output via helper method
      DATA lv_out TYPE string.
      lv_out = ascii_to_char( lv_val ).
      mv_output = mv_output && lv_out.
      RETURN.
    ENDIF.

    " Convert integer to hex
    lv_hi = lv_val DIV 16.
    lv_lo = lv_val MOD 16.
    lv_hex = substring( val = lv_chars off = lv_hi len = 1 ) &&
             substring( val = lv_chars off = lv_lo len = 1 ).

    " Each byte is 2 hex chars
    lv_pos = lv_port * 2.

    " Replace in I/O ports string
    IF lv_pos = 0.
      mv_io_ports = lv_hex && substring( val = mv_io_ports off = 2 ).
    ELSEIF lv_pos >= strlen( mv_io_ports ) - 2.
      mv_io_ports = substring( val = mv_io_ports off = 0 len = lv_pos ) && lv_hex.
    ELSE.
      mv_io_ports = substring( val = mv_io_ports off = 0 len = lv_pos ) &&
                    lv_hex &&
                    substring( val = mv_io_ports off = lv_pos + 2 ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_cpu_z80_bus~load.
    " Load binary data into memory at specified address
    DATA lv_len TYPE i.
    DATA lv_i TYPE i.
    DATA lv_byte TYPE i.
    DATA lv_addr TYPE i.

    lv_len = xstrlen( iv_data ).
    lv_addr = iv_addr.

    DO lv_len TIMES.
      lv_i = sy-index - 1.
      lv_byte = iv_data+lv_i(1).
      zif_cpu_z80_bus~write_mem( iv_addr = lv_addr iv_val = lv_byte ).
      lv_addr = lv_addr + 1.
    ENDDO.
  ENDMETHOD.

  METHOD zif_cpu_z80_bus~is_input_ready.
    rv_ready = xsdbool( mv_input_pos < strlen( mv_input ) ).
  ENDMETHOD.

  METHOD zif_cpu_z80_bus~get_output.
    rv_output = mv_output.
  ENDMETHOD.

  METHOD zif_cpu_z80_bus~clear_output.
    mv_output = ``.
  ENDMETHOD.

  METHOD zif_cpu_z80_bus~provide_input.
    mv_input = mv_input && iv_text.
  ENDMETHOD.

  METHOD get_memory.
    " Convert hex string to xstring
    DATA lv_i TYPE i.
    DATA lv_hex TYPE string.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_hi TYPE i.
    DATA lv_lo TYPE i.
    DATA lv_c TYPE c LENGTH 1.

    rv_mem = ``.
    lv_i = 0.
    WHILE lv_i < strlen( mv_memory ).
      lv_hex = substring( val = mv_memory off = lv_i len = 2 ).

      lv_c = substring( val = lv_hex off = 0 len = 1 ).
      CASE lv_c.
        WHEN '0'. lv_hi = 0.  WHEN '1'. lv_hi = 1.  WHEN '2'. lv_hi = 2.  WHEN '3'. lv_hi = 3.
        WHEN '4'. lv_hi = 4.  WHEN '5'. lv_hi = 5.  WHEN '6'. lv_hi = 6.  WHEN '7'. lv_hi = 7.
        WHEN '8'. lv_hi = 8.  WHEN '9'. lv_hi = 9.  WHEN 'A' OR 'a'. lv_hi = 10.
        WHEN 'B' OR 'b'. lv_hi = 11.  WHEN 'C' OR 'c'. lv_hi = 12.
        WHEN 'D' OR 'd'. lv_hi = 13.  WHEN 'E' OR 'e'. lv_hi = 14.  WHEN 'F' OR 'f'. lv_hi = 15.
        WHEN OTHERS. lv_hi = 0.
      ENDCASE.

      lv_c = substring( val = lv_hex off = 1 len = 1 ).
      CASE lv_c.
        WHEN '0'. lv_lo = 0.  WHEN '1'. lv_lo = 1.  WHEN '2'. lv_lo = 2.  WHEN '3'. lv_lo = 3.
        WHEN '4'. lv_lo = 4.  WHEN '5'. lv_lo = 5.  WHEN '6'. lv_lo = 6.  WHEN '7'. lv_lo = 7.
        WHEN '8'. lv_lo = 8.  WHEN '9'. lv_lo = 9.  WHEN 'A' OR 'a'. lv_lo = 10.
        WHEN 'B' OR 'b'. lv_lo = 11.  WHEN 'C' OR 'c'. lv_lo = 12.
        WHEN 'D' OR 'd'. lv_lo = 13.  WHEN 'E' OR 'e'. lv_lo = 14.  WHEN 'F' OR 'f'. lv_lo = 15.
        WHEN OTHERS. lv_lo = 0.
      ENDCASE.

      lv_byte = lv_hi * 16 + lv_lo.
      rv_mem = rv_mem && lv_byte.
      lv_i = lv_i + 2.
    ENDWHILE.
  ENDMETHOD.

  METHOD dump_memory.
    " Dump memory as hex string for debugging
    DATA lv_addr TYPE i.
    DATA lv_pos TYPE i.
    DATA lv_line TYPE string.
    DATA lv_chars TYPE string VALUE `0123456789ABCDEF`.
    DATA lv_hi TYPE i.
    DATA lv_lo TYPE i.

    rv_hex = ``.
    lv_addr = iv_start.

    WHILE lv_addr < iv_start + iv_length AND lv_addr < c_mem_size.
      " Address
      lv_hi = lv_addr DIV 4096.
      lv_line = substring( val = lv_chars off = lv_hi len = 1 ).
      lv_hi = ( lv_addr MOD 4096 ) DIV 256.
      lv_line = lv_line && substring( val = lv_chars off = lv_hi len = 1 ).
      lv_hi = ( lv_addr MOD 256 ) DIV 16.
      lv_line = lv_line && substring( val = lv_chars off = lv_hi len = 1 ).
      lv_hi = lv_addr MOD 16.
      lv_line = lv_line && substring( val = lv_chars off = lv_hi len = 1 ).
      lv_line = lv_line && `: `.

      " 16 bytes per line
      DATA lv_i TYPE i.
      lv_i = 0.
      WHILE lv_i < 16 AND lv_addr + lv_i < c_mem_size.
        lv_pos = ( lv_addr + lv_i ) * 2.
        lv_line = lv_line && substring( val = mv_memory off = lv_pos len = 2 ) && ` `.
        lv_i = lv_i + 1.
      ENDWHILE.

      rv_hex = rv_hex && lv_line && cl_abap_char_utilities=>newline.
      lv_addr = lv_addr + 16.
    ENDWHILE.
  ENDMETHOD.

  METHOD char_to_ascii.
    " Convert character to ASCII code (32-126 printable range)
    CASE iv_char.
      WHEN ' '. rv_code = 32.  WHEN '!'. rv_code = 33.  WHEN '"'. rv_code = 34.
      WHEN '#'. rv_code = 35.  WHEN '$'. rv_code = 36.  WHEN '%'. rv_code = 37.
      WHEN '&'. rv_code = 38.  WHEN ''''. rv_code = 39. WHEN '('. rv_code = 40.
      WHEN ')'. rv_code = 41.  WHEN '*'. rv_code = 42.  WHEN '+'. rv_code = 43.
      WHEN ','. rv_code = 44.  WHEN '-'. rv_code = 45.  WHEN '.'. rv_code = 46.
      WHEN '/'. rv_code = 47.
      WHEN '0'. rv_code = 48.  WHEN '1'. rv_code = 49.  WHEN '2'. rv_code = 50.
      WHEN '3'. rv_code = 51.  WHEN '4'. rv_code = 52.  WHEN '5'. rv_code = 53.
      WHEN '6'. rv_code = 54.  WHEN '7'. rv_code = 55.  WHEN '8'. rv_code = 56.
      WHEN '9'. rv_code = 57.
      WHEN ':'. rv_code = 58.  WHEN ';'. rv_code = 59.  WHEN '<'. rv_code = 60.
      WHEN '='. rv_code = 61.  WHEN '>'. rv_code = 62.  WHEN '?'. rv_code = 63.
      WHEN '@'. rv_code = 64.
      WHEN 'A'. rv_code = 65.  WHEN 'B'. rv_code = 66.  WHEN 'C'. rv_code = 67.
      WHEN 'D'. rv_code = 68.  WHEN 'E'. rv_code = 69.  WHEN 'F'. rv_code = 70.
      WHEN 'G'. rv_code = 71.  WHEN 'H'. rv_code = 72.  WHEN 'I'. rv_code = 73.
      WHEN 'J'. rv_code = 74.  WHEN 'K'. rv_code = 75.  WHEN 'L'. rv_code = 76.
      WHEN 'M'. rv_code = 77.  WHEN 'N'. rv_code = 78.  WHEN 'O'. rv_code = 79.
      WHEN 'P'. rv_code = 80.  WHEN 'Q'. rv_code = 81.  WHEN 'R'. rv_code = 82.
      WHEN 'S'. rv_code = 83.  WHEN 'T'. rv_code = 84.  WHEN 'U'. rv_code = 85.
      WHEN 'V'. rv_code = 86.  WHEN 'W'. rv_code = 87.  WHEN 'X'. rv_code = 88.
      WHEN 'Y'. rv_code = 89.  WHEN 'Z'. rv_code = 90.
      WHEN '['. rv_code = 91.  WHEN '\'. rv_code = 92.  WHEN ']'. rv_code = 93.
      WHEN '^'. rv_code = 94.  WHEN '_'. rv_code = 95.
      WHEN 'a'. rv_code = 97.  WHEN 'b'. rv_code = 98.  WHEN 'c'. rv_code = 99.
      WHEN 'd'. rv_code = 100. WHEN 'e'. rv_code = 101. WHEN 'f'. rv_code = 102.
      WHEN 'g'. rv_code = 103. WHEN 'h'. rv_code = 104. WHEN 'i'. rv_code = 105.
      WHEN 'j'. rv_code = 106. WHEN 'k'. rv_code = 107. WHEN 'l'. rv_code = 108.
      WHEN 'm'. rv_code = 109. WHEN 'n'. rv_code = 110. WHEN 'o'. rv_code = 111.
      WHEN 'p'. rv_code = 112. WHEN 'q'. rv_code = 113. WHEN 'r'. rv_code = 114.
      WHEN 's'. rv_code = 115. WHEN 't'. rv_code = 116. WHEN 'u'. rv_code = 117.
      WHEN 'v'. rv_code = 118. WHEN 'w'. rv_code = 119. WHEN 'x'. rv_code = 120.
      WHEN 'y'. rv_code = 121. WHEN 'z'. rv_code = 122.
      WHEN '{'. rv_code = 123. WHEN '|'. rv_code = 124. WHEN '}'. rv_code = 125.
      WHEN '~'. rv_code = 126.
      WHEN OTHERS. rv_code = 63.  " '?' for unknown
    ENDCASE.
  ENDMETHOD.

  METHOD ascii_to_char.
    " Convert ASCII code to character (32-126 printable range)
    CASE iv_code.
      WHEN 32. rv_char = ` `.   WHEN 33. rv_char = `!`.  WHEN 34. rv_char = `"`.
      WHEN 35. rv_char = `#`.   WHEN 36. rv_char = `$`.  WHEN 37. rv_char = `%`.
      WHEN 38. rv_char = `&`.   WHEN 39. rv_char = `'`.  WHEN 40. rv_char = `(`.
      WHEN 41. rv_char = `)`.   WHEN 42. rv_char = `*`.  WHEN 43. rv_char = `+`.
      WHEN 44. rv_char = `,`.   WHEN 45. rv_char = `-`.  WHEN 46. rv_char = `.`.
      WHEN 47. rv_char = `/`.
      WHEN 48. rv_char = `0`.   WHEN 49. rv_char = `1`.  WHEN 50. rv_char = `2`.
      WHEN 51. rv_char = `3`.   WHEN 52. rv_char = `4`.  WHEN 53. rv_char = `5`.
      WHEN 54. rv_char = `6`.   WHEN 55. rv_char = `7`.  WHEN 56. rv_char = `8`.
      WHEN 57. rv_char = `9`.
      WHEN 58. rv_char = `:`.   WHEN 59. rv_char = `;`.  WHEN 60. rv_char = `<`.
      WHEN 61. rv_char = `=`.   WHEN 62. rv_char = `>`.  WHEN 63. rv_char = `?`.
      WHEN 64. rv_char = `@`.
      WHEN 65. rv_char = `A`.   WHEN 66. rv_char = `B`.  WHEN 67. rv_char = `C`.
      WHEN 68. rv_char = `D`.   WHEN 69. rv_char = `E`.  WHEN 70. rv_char = `F`.
      WHEN 71. rv_char = `G`.   WHEN 72. rv_char = `H`.  WHEN 73. rv_char = `I`.
      WHEN 74. rv_char = `J`.   WHEN 75. rv_char = `K`.  WHEN 76. rv_char = `L`.
      WHEN 77. rv_char = `M`.   WHEN 78. rv_char = `N`.  WHEN 79. rv_char = `O`.
      WHEN 80. rv_char = `P`.   WHEN 81. rv_char = `Q`.  WHEN 82. rv_char = `R`.
      WHEN 83. rv_char = `S`.   WHEN 84. rv_char = `T`.  WHEN 85. rv_char = `U`.
      WHEN 86. rv_char = `V`.   WHEN 87. rv_char = `W`.  WHEN 88. rv_char = `X`.
      WHEN 89. rv_char = `Y`.   WHEN 90. rv_char = `Z`.
      WHEN 91. rv_char = `[`.   WHEN 92. rv_char = `\`.  WHEN 93. rv_char = `]`.
      WHEN 94. rv_char = `^`.   WHEN 95. rv_char = `_`.  WHEN 96. rv_char = |`|.
      WHEN 97. rv_char = `a`.   WHEN 98. rv_char = `b`.  WHEN 99. rv_char = `c`.
      WHEN 100. rv_char = `d`.  WHEN 101. rv_char = `e`. WHEN 102. rv_char = `f`.
      WHEN 103. rv_char = `g`.  WHEN 104. rv_char = `h`. WHEN 105. rv_char = `i`.
      WHEN 106. rv_char = `j`.  WHEN 107. rv_char = `k`. WHEN 108. rv_char = `l`.
      WHEN 109. rv_char = `m`.  WHEN 110. rv_char = `n`. WHEN 111. rv_char = `o`.
      WHEN 112. rv_char = `p`.  WHEN 113. rv_char = `q`. WHEN 114. rv_char = `r`.
      WHEN 115. rv_char = `s`.  WHEN 116. rv_char = `t`. WHEN 117. rv_char = `u`.
      WHEN 118. rv_char = `v`.  WHEN 119. rv_char = `w`. WHEN 120. rv_char = `x`.
      WHEN 121. rv_char = `y`.  WHEN 122. rv_char = `z`.
      WHEN 123. rv_char = `{`.  WHEN 124. rv_char = `|`. WHEN 125. rv_char = `}`.
      WHEN 126. rv_char = `~`.
      WHEN 10 OR 13. rv_char = cl_abap_char_utilities=>newline.
      WHEN 9. rv_char = `    `.  " Tab as 4 spaces
      WHEN OTHERS. rv_char = ``.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
