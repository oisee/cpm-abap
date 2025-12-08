*&---------------------------------------------------------------------*
*& Z-Machine HTML I/O Implementation
*& Uses CL_GUI_HTML_VIEWER for output, popup dialog for input
*& For SAP GUI only - not transpiler-compatible
*&---------------------------------------------------------------------*
CLASS zcl_zork_00_io_html DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES: zif_zork_00_io.

    METHODS:
      constructor
        IMPORTING io_container TYPE REF TO cl_gui_container OPTIONAL,

      " Refresh the HTML display
      refresh_display,

      " Get the full output history
      get_output_html
        RETURNING VALUE(rv_html) TYPE string,

      " Set waiting for input flag
      set_waiting_input
        IMPORTING iv_waiting TYPE abap_bool,

      " Check if waiting for input
      is_waiting_input
        RETURNING VALUE(rv_waiting) TYPE abap_bool,

      " Queue input (called from popup)
      queue_input
        IMPORTING iv_input TYPE string,

      " Free resources
      free.

  PRIVATE SECTION.

    DATA:
      mo_html_viewer   TYPE REF TO cl_gui_html_viewer,
      mo_container     TYPE REF TO cl_gui_container,
      mv_output        TYPE string,        " Accumulated output text
      mv_input_queue   TYPE string,        " Queued input
      mv_waiting_input TYPE abap_bool,     " Waiting for input flag
      mv_status_loc    TYPE string,        " Status line location
      mv_status_score  TYPE i,             " Status line score
      mv_status_moves  TYPE i,             " Status line moves
      mv_text_style    TYPE i.             " Current text style

    METHODS:
      build_html
        RETURNING VALUE(rv_html) TYPE string,

      escape_html
        IMPORTING iv_text        TYPE string
        RETURNING VALUE(rv_text) TYPE string.

ENDCLASS.


CLASS zcl_zork_00_io_html IMPLEMENTATION.

  METHOD constructor.
    mv_output = ''.
    mv_input_queue = ''.
    mv_waiting_input = abap_false.
    mv_status_loc = ''.
    mv_status_score = 0.
    mv_status_moves = 0.
    mv_text_style = 0.

    IF io_container IS BOUND.
      mo_container = io_container.
    ELSE.
      " Create default container
      CREATE OBJECT mo_container
        TYPE cl_gui_custom_container
        EXPORTING
          container_name = 'HTML_CONTAINER'.
    ENDIF.

    " Create HTML viewer
    CREATE OBJECT mo_html_viewer
      EXPORTING
        parent = mo_container.
  ENDMETHOD.


  METHOD zif_zork_00_io~print_char.
    " Convert ZSCII code to character and append
    DATA: lv_char TYPE string.

    IF iv_zscii = 13.
      mv_output = mv_output && '<br>'.
    ELSEIF iv_zscii >= 32 AND iv_zscii <= 126.
      " Convert to character
      CASE iv_zscii.
        WHEN 32. lv_char = ' '.
        WHEN 34. lv_char = '&quot;'.
        WHEN 38. lv_char = '&amp;'.
        WHEN 60. lv_char = '&lt;'.
        WHEN 62. lv_char = '&gt;'.
        WHEN OTHERS.
          " Direct character conversion
          DATA(lv_code) = CONV xstring( iv_zscii ).
          lv_char = cl_abap_conv_codepage=>create_in( )->convert( lv_code ).
      ENDCASE.
      mv_output = mv_output && lv_char.
    ENDIF.
  ENDMETHOD.


  METHOD zif_zork_00_io~print_text.
    mv_output = mv_output && escape_html( iv_text ).
  ENDMETHOD.


  METHOD zif_zork_00_io~new_line.
    mv_output = mv_output && '<br>'.
  ENDMETHOD.


  METHOD zif_zork_00_io~print_num.
    mv_output = mv_output && |{ iv_num }|.
  ENDMETHOD.


  METHOD zif_zork_00_io~buffer_mode.
    " HTML viewer handles buffering automatically
  ENDMETHOD.


  METHOD zif_zork_00_io~read_line.
    " Return queued input if available
    IF mv_input_queue IS NOT INITIAL.
      rv_text = mv_input_queue.
      IF iv_max_len > 0 AND strlen( rv_text ) > iv_max_len.
        rv_text = rv_text+0(iv_max_len).
      ENDIF.
      mv_input_queue = ''.
      mv_waiting_input = abap_false.

      " Echo input to output
      mv_output = mv_output && '<span class="input">&gt; ' &&
                  escape_html( rv_text ) && '</span><br>'.
    ELSE.
      " Signal that we need input
      mv_waiting_input = abap_true.
      rv_text = ''.
    ENDIF.
  ENDMETHOD.


  METHOD zif_zork_00_io~read_char.
    " For single character input
    IF mv_input_queue IS NOT INITIAL.
      DATA(lv_char) = mv_input_queue+0(1).
      IF strlen( mv_input_queue ) > 1.
        mv_input_queue = mv_input_queue+1.
      ELSE.
        mv_input_queue = ''.
      ENDIF.
      mv_waiting_input = abap_false.

      " Return ASCII code
      rv_zscii = cl_abap_conv_codepage=>create_out( )->convert( lv_char )+0(1).
    ELSE.
      mv_waiting_input = abap_true.
      rv_zscii = 0.
    ENDIF.
  ENDMETHOD.


  METHOD zif_zork_00_io~show_status.
    mv_status_loc = iv_location.
    mv_status_score = iv_score.
    mv_status_moves = iv_moves.

    " Status is shown in HTML header
    refresh_display( ).
  ENDMETHOD.


  METHOD zif_zork_00_io~split_window.
    " Not implemented for HTML view
  ENDMETHOD.


  METHOD zif_zork_00_io~set_window.
    " Not implemented for HTML view
  ENDMETHOD.


  METHOD zif_zork_00_io~erase_window.
    IF iv_window = -1 OR iv_window = -2.
      mv_output = ''.
      refresh_display( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_zork_00_io~set_cursor.
    " Not implemented for HTML view
  ENDMETHOD.


  METHOD zif_zork_00_io~set_style.
    mv_text_style = iv_style.
  ENDMETHOD.


  METHOD zif_zork_00_io~sound_effect.
    " Not implemented
  ENDMETHOD.


  METHOD refresh_display.
    DATA: lt_html TYPE TABLE OF char1024,
          lv_html TYPE string.

    lv_html = build_html( ).

    " Split HTML into lines for HTML viewer
    WHILE strlen( lv_html ) > 0.
      IF strlen( lv_html ) > 1024.
        APPEND lv_html+0(1024) TO lt_html.
        lv_html = lv_html+1024.
      ELSE.
        APPEND lv_html TO lt_html.
        CLEAR lv_html.
      ENDIF.
    ENDWHILE.

    mo_html_viewer->load_data(
      IMPORTING
        assigned_url = DATA(lv_url)
      CHANGING
        data_table   = lt_html ).

    mo_html_viewer->show_url( url = lv_url ).
  ENDMETHOD.


  METHOD build_html.
    rv_html =
      |<html>| &&
      |<head>| &&
      |<style>| &&
      |body \{ | &&
      |  background-color: #1a1a2e; | &&
      |  color: #00ff00; | &&
      |  font-family: 'Courier New', monospace; | &&
      |  font-size: 14px; | &&
      |  padding: 10px; | &&
      |  margin: 0; | &&
      |\}| &&
      |.status \{ | &&
      |  background-color: #00ff00; | &&
      |  color: #1a1a2e; | &&
      |  padding: 5px 10px; | &&
      |  margin-bottom: 10px; | &&
      |  font-weight: bold; | &&
      |\}| &&
      |.output \{ | &&
      |  white-space: pre-wrap; | &&
      |  line-height: 1.4; | &&
      |\}| &&
      |.input \{ | &&
      |  color: #ffff00; | &&
      |\}| &&
      |.prompt \{ | &&
      |  color: #ff6600; | &&
      |  animation: blink 1s infinite; | &&
      |\}| &&
      |</style>| &&
      |</head>| &&
      |<body>|.

    " Status line
    IF mv_status_loc IS NOT INITIAL.
      rv_html = rv_html &&
        |<div class="status">| &&
        |{ escape_html( mv_status_loc ) } | &&
        |&nbsp;&nbsp;&nbsp;&nbsp;| &&
        |Score: { mv_status_score }&nbsp;&nbsp;| &&
        |Moves: { mv_status_moves }| &&
        |</div>|.
    ENDIF.

    " Output area
    rv_html = rv_html &&
      |<div class="output">| &&
      mv_output.

    " Show prompt if waiting for input
    IF mv_waiting_input = abap_true.
      rv_html = rv_html && |<span class="prompt">&gt; _</span>|.
    ENDIF.

    rv_html = rv_html &&
      |</div>| &&
      |</body>| &&
      |</html>|.
  ENDMETHOD.


  METHOD escape_html.
    rv_text = iv_text.
    REPLACE ALL OCCURRENCES OF '&' IN rv_text WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN rv_text WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN rv_text WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_text WITH '&quot;'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN rv_text WITH '<br>'.
  ENDMETHOD.


  METHOD get_output_html.
    rv_html = build_html( ).
  ENDMETHOD.


  METHOD set_waiting_input.
    mv_waiting_input = iv_waiting.
  ENDMETHOD.


  METHOD is_waiting_input.
    rv_waiting = mv_waiting_input.
  ENDMETHOD.


  METHOD queue_input.
    mv_input_queue = iv_input.
    mv_waiting_input = abap_false.
  ENDMETHOD.


  METHOD free.
    IF mo_html_viewer IS BOUND.
      mo_html_viewer->free( ).
      FREE mo_html_viewer.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
