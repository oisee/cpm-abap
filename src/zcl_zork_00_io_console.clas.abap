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
    DATA: lv_char TYPE c LENGTH 1.

    " ZSCII 13 = newline
    IF iv_zscii = 13.
      mv_output = mv_output && cl_abap_char_utilities=>newline.
    ELSEIF iv_zscii >= 32 AND iv_zscii <= 126.
      " Use cl_abap_conv_in_ce to convert code point to character
      " uccpi expects TYPE i (integer), iv_zscii is already TYPE i
      lv_char = cl_abap_conv_in_ce=>uccpi( iv_zscii ).
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
    DATA: lv_char TYPE c LENGTH 1.

    IF mv_input_queue = ''.
      rv_zscii = 0.
      RETURN.
    ENDIF.

    lv_char = mv_input_queue+0(1).
    DATA(lv_len) = strlen( mv_input_queue ).

    IF lv_len > 1.
      DATA(lv_rest_len) = lv_len - 1.
      mv_input_queue = mv_input_queue+1(lv_rest_len).
    ELSE.
      mv_input_queue = ''.
    ENDIF.

    " Convert character to ZSCII code using cl_abap_conv_out_ce
    " uccp returns the Unicode code point as a numeric string
    rv_zscii = cl_abap_conv_out_ce=>uccp( lv_char ).
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
