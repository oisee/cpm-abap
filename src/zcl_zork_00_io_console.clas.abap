*&---------------------------------------------------------------------*
*& Z-Machine Console I/O Implementation
*& Simple console-based I/O for testing
*& Captures output and allows simulated input for unit tests
*&---------------------------------------------------------------------*
CLASS zcl_zork_00_io_console DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES: zif_zork_00_io.

    METHODS:
      constructor,

      " Get captured output (for testing)
      get_output
        RETURNING VALUE(rv_output) TYPE string,

      " Clear captured output
      clear_output,

      " Queue input for read operations (for testing)
      queue_input
        IMPORTING iv_input TYPE string,

      " Get status line info (for testing)
      get_status_location
        RETURNING VALUE(rv_location) TYPE string,

      get_status_score
        RETURNING VALUE(rv_score) TYPE i,

      get_status_moves
        RETURNING VALUE(rv_moves) TYPE i.

  PRIVATE SECTION.

    DATA:
      mv_output       TYPE string,    " Captured output
      mv_input_queue  TYPE string,    " Queued input lines (separated by newline)
      mv_buffer_mode  TYPE i,         " 0=unbuffered, 1=buffered
      mv_current_win  TYPE i,         " Current window (0=lower, 1=upper)
      mv_split_lines  TYPE i,         " Upper window size
      mv_cursor_line  TYPE i,         " Cursor line in upper window
      mv_cursor_col   TYPE i,         " Cursor column in upper window
      mv_text_style   TYPE i,         " Current text style
      mv_stat_loc     TYPE string,    " Status line location
      mv_stat_score   TYPE i,         " Status line score
      mv_stat_moves   TYPE i.         " Status line moves

ENDCLASS.


CLASS zcl_zork_00_io_console IMPLEMENTATION.

  METHOD constructor.
    mv_output = ''.
    mv_input_queue = ''.
    mv_buffer_mode = 1.
    mv_current_win = 0.
    mv_split_lines = 0.
    mv_cursor_line = 1.
    mv_cursor_col = 1.
    mv_text_style = 0.
    mv_stat_loc = ''.
    mv_stat_score = 0.
    mv_stat_moves = 0.
  ENDMETHOD.


  METHOD zif_zork_00_io~print_char.
    " Convert ZSCII code to character and append to output
    DATA: lv_char TYPE string.

    " Basic ZSCII to ASCII conversion (simplified)
    " ZSCII 32-126 = standard ASCII printable
    " ZSCII 13 = newline
    IF iv_zscii = 13.
      mv_output = mv_output && cl_abap_char_utilities=>newline.
    ELSEIF iv_zscii >= 32 AND iv_zscii <= 126.
      " Convert to character using code
      CASE iv_zscii.
        WHEN 32. lv_char = ' '.
        WHEN 33. lv_char = '!'.
        WHEN 34. lv_char = '"'.
        WHEN 35. lv_char = '#'.
        WHEN 36. lv_char = '$'.
        WHEN 37. lv_char = '%'.
        WHEN 38. lv_char = '&'.
        WHEN 39. lv_char = ''''.
        WHEN 40. lv_char = '('.
        WHEN 41. lv_char = ')'.
        WHEN 42. lv_char = '*'.
        WHEN 43. lv_char = '+'.
        WHEN 44. lv_char = ','.
        WHEN 45. lv_char = '-'.
        WHEN 46. lv_char = '.'.
        WHEN 47. lv_char = '/'.
        WHEN 48. lv_char = '0'.
        WHEN 49. lv_char = '1'.
        WHEN 50. lv_char = '2'.
        WHEN 51. lv_char = '3'.
        WHEN 52. lv_char = '4'.
        WHEN 53. lv_char = '5'.
        WHEN 54. lv_char = '6'.
        WHEN 55. lv_char = '7'.
        WHEN 56. lv_char = '8'.
        WHEN 57. lv_char = '9'.
        WHEN 58. lv_char = ':'.
        WHEN 59. lv_char = ';'.
        WHEN 60. lv_char = '<'.
        WHEN 61. lv_char = '='.
        WHEN 62. lv_char = '>'.
        WHEN 63. lv_char = '?'.
        WHEN 64. lv_char = '@'.
        WHEN 65. lv_char = 'A'.
        WHEN 66. lv_char = 'B'.
        WHEN 67. lv_char = 'C'.
        WHEN 68. lv_char = 'D'.
        WHEN 69. lv_char = 'E'.
        WHEN 70. lv_char = 'F'.
        WHEN 71. lv_char = 'G'.
        WHEN 72. lv_char = 'H'.
        WHEN 73. lv_char = 'I'.
        WHEN 74. lv_char = 'J'.
        WHEN 75. lv_char = 'K'.
        WHEN 76. lv_char = 'L'.
        WHEN 77. lv_char = 'M'.
        WHEN 78. lv_char = 'N'.
        WHEN 79. lv_char = 'O'.
        WHEN 80. lv_char = 'P'.
        WHEN 81. lv_char = 'Q'.
        WHEN 82. lv_char = 'R'.
        WHEN 83. lv_char = 'S'.
        WHEN 84. lv_char = 'T'.
        WHEN 85. lv_char = 'U'.
        WHEN 86. lv_char = 'V'.
        WHEN 87. lv_char = 'W'.
        WHEN 88. lv_char = 'X'.
        WHEN 89. lv_char = 'Y'.
        WHEN 90. lv_char = 'Z'.
        WHEN 91. lv_char = '['.
        WHEN 92. lv_char = '\'.
        WHEN 93. lv_char = ']'.
        WHEN 94. lv_char = '^'.
        WHEN 95. lv_char = '_'.
        WHEN 96. lv_char = '`'.
        WHEN 97. lv_char = 'a'.
        WHEN 98. lv_char = 'b'.
        WHEN 99. lv_char = 'c'.
        WHEN 100. lv_char = 'd'.
        WHEN 101. lv_char = 'e'.
        WHEN 102. lv_char = 'f'.
        WHEN 103. lv_char = 'g'.
        WHEN 104. lv_char = 'h'.
        WHEN 105. lv_char = 'i'.
        WHEN 106. lv_char = 'j'.
        WHEN 107. lv_char = 'k'.
        WHEN 108. lv_char = 'l'.
        WHEN 109. lv_char = 'm'.
        WHEN 110. lv_char = 'n'.
        WHEN 111. lv_char = 'o'.
        WHEN 112. lv_char = 'p'.
        WHEN 113. lv_char = 'q'.
        WHEN 114. lv_char = 'r'.
        WHEN 115. lv_char = 's'.
        WHEN 116. lv_char = 't'.
        WHEN 117. lv_char = 'u'.
        WHEN 118. lv_char = 'v'.
        WHEN 119. lv_char = 'w'.
        WHEN 120. lv_char = 'x'.
        WHEN 121. lv_char = 'y'.
        WHEN 122. lv_char = 'z'.
        WHEN 123. lv_char = '{'.
        WHEN 124. lv_char = '|'.
        WHEN 125. lv_char = '}'.
        WHEN 126. lv_char = '~'.
        WHEN OTHERS. lv_char = '?'.
      ENDCASE.
      mv_output = mv_output && lv_char.
    ENDIF.
  ENDMETHOD.


  METHOD zif_zork_00_io~print_text.
    mv_output = mv_output && iv_text.
  ENDMETHOD.


  METHOD zif_zork_00_io~new_line.
    mv_output = mv_output && cl_abap_char_utilities=>newline.
  ENDMETHOD.


  METHOD zif_zork_00_io~print_num.
    DATA: lv_num_str TYPE string.
    lv_num_str = |{ iv_num }|.
    mv_output = mv_output && lv_num_str.
  ENDMETHOD.


  METHOD zif_zork_00_io~buffer_mode.
    mv_buffer_mode = iv_mode.
  ENDMETHOD.


  METHOD zif_zork_00_io~read_line.
    " Get next line from input queue
    DATA: lv_newline_pos TYPE i,
          lv_len         TYPE i.

    IF mv_input_queue = ''.
      rv_text = ''.
      RETURN.
    ENDIF.

    " Find newline in queue
    lv_len = strlen( mv_input_queue ).
    lv_newline_pos = 0.

    " Search for newline character (simplified - look for first newline)
    DATA(lv_i) = 0.
    WHILE lv_i < lv_len.
      DATA(lv_char) = mv_input_queue+lv_i(1).
      IF lv_char = cl_abap_char_utilities=>newline.
        lv_newline_pos = lv_i.
        EXIT.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.

    IF lv_newline_pos > 0.
      " Found newline - extract line
      rv_text = mv_input_queue+0(lv_newline_pos).
      DATA(lv_rest_start) = lv_newline_pos + 1.
      IF lv_rest_start < lv_len.
        DATA(lv_rest_len) = lv_len - lv_rest_start.
        mv_input_queue = mv_input_queue+lv_rest_start(lv_rest_len).
      ELSE.
        mv_input_queue = ''.
      ENDIF.
    ELSE.
      " No newline - return entire queue
      rv_text = mv_input_queue.
      mv_input_queue = ''.
    ENDIF.

    " Limit to max_len if specified
    IF iv_max_len > 0 AND strlen( rv_text ) > iv_max_len.
      rv_text = rv_text+0(iv_max_len).
    ENDIF.
  ENDMETHOD.


  METHOD zif_zork_00_io~read_char.
    " Get single character from input queue
    IF mv_input_queue = ''.
      rv_zscii = 0.
      RETURN.
    ENDIF.

    DATA(lv_char) = mv_input_queue+0(1).
    DATA(lv_len) = strlen( mv_input_queue ).

    IF lv_len > 1.
      DATA(lv_rest_len) = lv_len - 1.
      mv_input_queue = mv_input_queue+1(lv_rest_len).
    ELSE.
      mv_input_queue = ''.
    ENDIF.

    " Convert to ZSCII (simplified)
    " For now, just return ASCII code
    CASE lv_char.
      WHEN ' '. rv_zscii = 32.
      WHEN 'a'. rv_zscii = 97.
      WHEN 'b'. rv_zscii = 98.
      WHEN 'c'. rv_zscii = 99.
      WHEN 'd'. rv_zscii = 100.
      WHEN 'e'. rv_zscii = 101.
      WHEN 'f'. rv_zscii = 102.
      WHEN 'g'. rv_zscii = 103.
      WHEN 'h'. rv_zscii = 104.
      WHEN 'i'. rv_zscii = 105.
      WHEN 'j'. rv_zscii = 106.
      WHEN 'k'. rv_zscii = 107.
      WHEN 'l'. rv_zscii = 108.
      WHEN 'm'. rv_zscii = 109.
      WHEN 'n'. rv_zscii = 110.
      WHEN 'o'. rv_zscii = 111.
      WHEN 'p'. rv_zscii = 112.
      WHEN 'q'. rv_zscii = 113.
      WHEN 'r'. rv_zscii = 114.
      WHEN 's'. rv_zscii = 115.
      WHEN 't'. rv_zscii = 116.
      WHEN 'u'. rv_zscii = 117.
      WHEN 'v'. rv_zscii = 118.
      WHEN 'w'. rv_zscii = 119.
      WHEN 'x'. rv_zscii = 120.
      WHEN 'y'. rv_zscii = 121.
      WHEN 'z'. rv_zscii = 122.
      WHEN OTHERS. rv_zscii = 0.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_zork_00_io~show_status.
    mv_stat_loc = iv_location.
    mv_stat_score = iv_score.
    mv_stat_moves = iv_moves.

    " For console output, format a status line
    IF iv_is_time = abap_true.
      mv_output = mv_output && |[{ iv_location } - { iv_hours }:{ iv_minutes }]| &&
                  cl_abap_char_utilities=>newline.
    ELSE.
      mv_output = mv_output && |[{ iv_location } - Score: { iv_score } Moves: { iv_moves }]| &&
                  cl_abap_char_utilities=>newline.
    ENDIF.
  ENDMETHOD.


  METHOD zif_zork_00_io~split_window.
    mv_split_lines = iv_lines.
  ENDMETHOD.


  METHOD zif_zork_00_io~set_window.
    mv_current_win = iv_window.
  ENDMETHOD.


  METHOD zif_zork_00_io~erase_window.
    " For console, just note the operation
    IF iv_window = -1 OR iv_window = -2.
      " Clear everything
      mv_output = ''.
    ENDIF.
  ENDMETHOD.


  METHOD zif_zork_00_io~set_cursor.
    mv_cursor_line = iv_line.
    mv_cursor_col = iv_column.
  ENDMETHOD.


  METHOD zif_zork_00_io~set_style.
    mv_text_style = iv_style.
  ENDMETHOD.


  METHOD zif_zork_00_io~sound_effect.
    " Console doesn't support sound - ignore
  ENDMETHOD.


  METHOD get_output.
    rv_output = mv_output.
  ENDMETHOD.


  METHOD clear_output.
    mv_output = ''.
  ENDMETHOD.


  METHOD queue_input.
    IF mv_input_queue = ''.
      mv_input_queue = iv_input.
    ELSE.
      mv_input_queue = mv_input_queue && cl_abap_char_utilities=>newline && iv_input.
    ENDIF.
  ENDMETHOD.


  METHOD get_status_location.
    rv_location = mv_stat_loc.
  ENDMETHOD.


  METHOD get_status_score.
    rv_score = mv_stat_score.
  ENDMETHOD.


  METHOD get_status_moves.
    rv_moves = mv_stat_moves.
  ENDMETHOD.

ENDCLASS.
