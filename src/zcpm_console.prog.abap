*&---------------------------------------------------------------------*
*& Report ZCPM_CONSOLE
*& CP/M 2.2 Emulator - Interactive Console
*& HTML-based display with input field
*&---------------------------------------------------------------------*
REPORT zcpm_console.

CONSTANTS:
  gc_default_max_lines TYPE i VALUE 24,
  gc_font_size         TYPE i VALUE 14,
  gc_line_height       TYPE f VALUE '1.4'.

*----------------------------------------------------------------------*
* Local Class - HTML Display Helper
*----------------------------------------------------------------------*
CLASS lcl_html_display DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_container TYPE REF TO cl_gui_container
                  iv_max_lines TYPE i DEFAULT gc_default_max_lines,
      append_text
        IMPORTING iv_text TYPE string,
      append_to_last_line
        IMPORTING iv_text TYPE string,
      clear,
      refresh,
      free.

  PRIVATE SECTION.
    DATA: mo_html_viewer TYPE REF TO cl_gui_html_viewer,
          mt_lines       TYPE STANDARD TABLE OF string WITH EMPTY KEY,
          mv_max_lines   TYPE i.

    METHODS:
      build_html RETURNING VALUE(rv_html) TYPE string,
      escape_html
        IMPORTING iv_text        TYPE string
        RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
* Local Class - Console Controller
*----------------------------------------------------------------------*
CLASS lcl_console_controller DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_program      TYPE xstring
                  iv_program_name TYPE string OPTIONAL,
      initialize_screen
        IMPORTING io_container TYPE REF TO cl_gui_custom_container,
      handle_command
        IMPORTING iv_command TYPE sy-ucomm,
      cleanup.

  PRIVATE SECTION.
    DATA: mo_cpm        TYPE REF TO zcl_cpm_emulator,
          mo_display    TYPE REF TO lcl_html_display,
          mo_container  TYPE REF TO cl_gui_custom_container,
          mv_program    TYPE xstring,
          mv_prog_name  TYPE string,
          mv_running    TYPE abap_bool,
          mv_started    TYPE abap_bool.

    METHODS:
      start_emulator,
      execute_step,
      process_input,
      calculate_max_lines RETURNING VALUE(rv_lines) TYPE i,
      load_smw0_file
        IMPORTING iv_objid       TYPE csequence
        RETURNING VALUE(rv_data) TYPE xstring,
      setup_data_files.
ENDCLASS.


*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-010.
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    PARAMETERS: r_test RADIOBUTTON GROUP src DEFAULT 'X' USER-COMMAND src,
                r_smw0 RADIOBUTTON GROUP src,
                r_file RADIOBUTTON GROUP src.
  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: p_prog TYPE wwwdatatab-objid DEFAULT 'ZHELLO.COM'
                AS LISTBOX VISIBLE LENGTH 40 MODIF ID smw.
  SELECTION-SCREEN END OF BLOCK b2.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_path TYPE string LOWER CASE MODIF ID fil.
  SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b10.

*----------------------------------------------------------------------*
* Built-in Test Programs (no SMW0 needed)
*----------------------------------------------------------------------*
CONSTANTS:
  gc_hello_com TYPE xstring VALUE '0E09110801CD0500C948656C6C6F2C2043502F4D20576F726C64210D0A24'.


*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gv_input     TYPE string,
      gv_ok        TYPE sy-ucomm,
      gv_prog_name TYPE string.

DATA: go_controller TYPE REF TO lcl_console_controller,
      go_container  TYPE REF TO cl_gui_custom_container,
      gv_program    TYPE xstring.


*----------------------------------------------------------------------*
* Selection Screen Events
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  SELECT objid, text FROM wwwdata
    INTO TABLE @DATA(lt_www)
    WHERE relid = 'MI'
      AND objid LIKE '%.COM'.

  LOOP AT lt_www INTO DATA(ls_www).
    ls_value-key = ls_www-objid.
    ls_value-text = ls_www-objid.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  IF lt_values IS INITIAL.
    ls_value-key = ''.
    ls_value-text = '(No .COM files in SMW0)'.
    APPEND ls_value TO lt_values.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_PROG'
      values = lt_values.

  LOOP AT SCREEN.
    IF screen-group1 = 'SMW'.
      screen-active = COND #( WHEN r_smw0 = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 = 'FIL'.
      screen-active = COND #( WHEN r_file = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  DATA: lt_filetab TYPE filetable,
        lv_rc      TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title      = 'Select CP/M Program'
      file_filter       = 'CP/M Programs (*.com)|*.com|All Files (*.*)|*.*'
      default_extension = 'com'
    CHANGING
      file_table        = lt_filetab
      rc                = lv_rc ).

  IF lv_rc >= 1.
    READ TABLE lt_filetab INDEX 1 INTO DATA(ls_file).
    p_path = ls_file-filename.
  ENDIF.


*----------------------------------------------------------------------*
* Class Implementations
*----------------------------------------------------------------------*
CLASS lcl_html_display IMPLEMENTATION.

  METHOD constructor.
    mv_max_lines = iv_max_lines.
    CREATE OBJECT mo_html_viewer
      EXPORTING
        parent = io_container.
  ENDMETHOD.

  METHOD append_text.
    DATA: lt_new_lines         TYPE STANDARD TABLE OF string,
          lv_text              TYPE string,
          lv_ends_with_newline TYPE abap_bool.

    lv_text = iv_text.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_text
      WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1) IN lv_text
      WITH cl_abap_char_utilities=>newline.

    DATA(lv_len) = strlen( lv_text ).
    IF lv_len > 0.
      DATA(lv_last) = substring( val = lv_text off = lv_len - 1 len = 1 ).
      lv_ends_with_newline = xsdbool( lv_last = cl_abap_char_utilities=>newline ).
    ENDIF.

    SPLIT lv_text AT cl_abap_char_utilities=>newline INTO TABLE lt_new_lines.

    LOOP AT lt_new_lines INTO DATA(lv_line).
      APPEND lv_line TO mt_lines.
    ENDLOOP.

    IF lv_ends_with_newline = abap_true.
      APPEND '' TO mt_lines.
    ENDIF.
  ENDMETHOD.

  METHOD append_to_last_line.
    DATA(lv_count) = lines( mt_lines ).
    IF lv_count > 0.
      FIELD-SYMBOLS <fs_line> TYPE string.
      READ TABLE mt_lines INDEX lv_count ASSIGNING <fs_line>.
      IF sy-subrc = 0.
        <fs_line> = <fs_line> && iv_text.
      ENDIF.
    ELSE.
      APPEND iv_text TO mt_lines.
    ENDIF.
  ENDMETHOD.

  METHOD clear.
    CLEAR mt_lines.
  ENDMETHOD.

  METHOD refresh.
    DATA: lt_html TYPE TABLE OF char1024,
          lv_html TYPE string,
          lv_url  TYPE c LENGTH 250.

    lv_html = build_html( ).

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
        assigned_url = lv_url
      CHANGING
        data_table   = lt_html ).
    mo_html_viewer->show_url( url = lv_url ).
  ENDMETHOD.

  METHOD build_html.
    DATA: lv_content TYPE string,
          lv_start   TYPE i,
          lv_total   TYPE i.

    lv_total = lines( mt_lines ).
    IF lv_total > mv_max_lines.
      lv_start = lv_total - mv_max_lines + 1.
    ELSE.
      lv_start = 1.
    ENDIF.

    LOOP AT mt_lines INTO DATA(lv_line) FROM lv_start.
      lv_content = lv_content && escape_html( lv_line ) && |<br>|.
    ENDLOOP.

    rv_html =
      |<html><head><style>| &&
      |body \{ background-color: #000000; color: #FFB000; | &&
      |font-family: Consolas, Monaco, monospace; | &&
      |font-size: { gc_font_size }px; | &&
      |padding: 10px; margin: 0; \}| &&
      |pre \{ margin: 0; white-space: pre-wrap; word-wrap: break-word; | &&
      |font-family: inherit; | &&
      |line-height: { gc_line_height }; \}| &&
      |</style></head><body>| &&
      |<pre>{ lv_content }</pre>| &&
      |</body></html>|.
  ENDMETHOD.

  METHOD escape_html.
    rv_text = iv_text.
    REPLACE ALL OCCURRENCES OF '&' IN rv_text WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN rv_text WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN rv_text WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_text WITH '&quot;'.
    REPLACE ALL OCCURRENCES OF ` ` IN rv_text WITH '&nbsp;'.
  ENDMETHOD.

  METHOD free.
    IF mo_html_viewer IS BOUND.
      mo_html_viewer->free( ).
      FREE mo_html_viewer.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_console_controller IMPLEMENTATION.

  METHOD constructor.
    mv_program = iv_program.
    mv_prog_name = iv_program_name.
    mv_running = abap_false.
    mv_started = abap_false.
  ENDMETHOD.

  METHOD load_smw0_file.
    DATA: lt_mime TYPE w3mimetabtype,
          ls_key  TYPE wwwdatatab,
          lv_size TYPE i.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_objid.

    SELECT SINGLE value FROM wwwparams INTO @DATA(lv_filesize)
      WHERE relid = @ls_key-relid AND objid = @ls_key-objid AND name = 'filesize'.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_size = lv_filesize.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING key  = ls_key
      TABLES    mime = lt_mime
      EXCEPTIONS OTHERS = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING input_length = lv_size
      IMPORTING buffer       = rv_data
      TABLES    binary_tab   = lt_mime.
  ENDMETHOD.

  METHOD setup_data_files.
    CHECK mo_cpm IS BOUND.
    CHECK mv_prog_name IS NOT INITIAL.

    DATA lv_base_name TYPE string.
    DATA lv_data_file TYPE string.
    DATA lv_data TYPE xstring.

    lv_base_name = to_upper( mv_prog_name ).
    IF lv_base_name CS '.COM'.
      lv_base_name = substring_before( val = lv_base_name sub = '.COM' ).
    ENDIF.

    lv_data_file = lv_base_name && '.DAT'.
    lv_data = load_smw0_file( lv_data_file ).

    IF lv_data IS NOT INITIAL.
      mo_cpm->register_file( iv_filename = lv_data_file iv_data = lv_data ).
      mo_cpm->setup_fcb( iv_fcb_addr = 92 iv_filename = lv_data_file ).
    ENDIF.
  ENDMETHOD.

  METHOD initialize_screen.
    mo_container = io_container.
    DATA(lv_max_lines) = calculate_max_lines( ).

    mo_display = NEW lcl_html_display(
      io_container = io_container
      iv_max_lines = lv_max_lines ).

    start_emulator( ).
  ENDMETHOD.

  METHOD calculate_max_lines.
    IF sy-srows > 10.
      rv_lines = sy-srows - 7.
    ELSE.
      rv_lines = gc_default_max_lines.
    ENDIF.
    IF rv_lines < 10.
      rv_lines = 10.
    ENDIF.
  ENDMETHOD.

  METHOD handle_command.
    CASE iv_command.
      WHEN 'EXIT' OR 'BACK' OR 'CANC' OR '&F03' OR '&F15' OR '&F12'.
        cleanup( ).
        LEAVE PROGRAM.

      WHEN 'ENTER' OR '' OR 'ONLI'.
        process_input( ).

      WHEN '&RESET'.
        mo_display->clear( ).
        start_emulator( ).
    ENDCASE.
  ENDMETHOD.

  METHOD start_emulator.
    IF mv_program IS INITIAL.
      mo_display->append_text( |Error: No program file loaded| ).
      mo_display->refresh( ).
      RETURN.
    ENDIF.

    TRY.
        mo_cpm = NEW zcl_cpm_emulator( ).
        mo_cpm->reset( ).
        mo_cpm->load_program( mv_program ).

        " Load associated data files (e.g., ZORK1.DAT for ZORK1.COM)
        setup_data_files( ).

        mv_started = abap_true.
        mv_running = abap_true.
        mo_display->clear( ).

        mo_display->append_text( |CP/M 2.2 Emulator| ).
        mo_display->append_text( |Program: { xstrlen( mv_program ) } bytes| ).
        IF mv_prog_name IS NOT INITIAL.
          mo_display->append_text( |File: { mv_prog_name }| ).
        ENDIF.
        mo_display->append_text( |--------------------------------| ).

        execute_step( ).

      CATCH cx_root INTO DATA(lx_error).
        mo_display->append_text( |Error: { lx_error->get_text( ) }| ).
        mo_display->refresh( ).
    ENDTRY.
  ENDMETHOD.

  METHOD execute_step.
    CHECK mo_cpm IS BOUND.

    DATA(lv_output) = mo_cpm->run( iv_max_cycles = 10000000 ).

    IF lv_output IS NOT INITIAL.
      mo_display->append_text( lv_output ).
    ENDIF.

    mv_running = mo_cpm->is_running( ).

    " Check if program ended (not running and not waiting for input)
    IF mv_running = abap_false AND mo_cpm->is_waiting_input( ) = abap_false.
      mo_display->append_text( |[Program Ended]| ).
    ENDIF.

    mo_display->refresh( ).
  ENDMETHOD.

  METHOD process_input.
    DATA lv_cmd TYPE string.

    lv_cmd = gv_input.
    CLEAR gv_input.

    IF lv_cmd IS NOT INITIAL.
      DATA(lv_upper) = to_upper( lv_cmd ).
      CASE lv_upper.
        WHEN '/Q' OR '/QUIT' OR '/EXIT'.
          mo_display->append_text( |Goodbye!| ).
          mo_display->refresh( ).
          cleanup( ).
          LEAVE PROGRAM.

        WHEN '/HELP' OR '/?'.
          mo_display->append_text( |Commands: /quit /help /reset| ).
          mo_display->refresh( ).
          RETURN.

        WHEN '/RESET'.
          mo_display->clear( ).
          start_emulator( ).
          RETURN.
      ENDCASE.
    ENDIF.

    " Process input if running or waiting for input
    IF mo_cpm IS BOUND.
      DATA(lv_active) = xsdbool( mv_running = abap_true OR
                                  mo_cpm->is_waiting_input( ) = abap_true ).
      IF lv_active = abap_true.
        IF lv_cmd IS NOT INITIAL.
          mo_display->append_to_last_line( lv_cmd ).
        ENDIF.

        " Send input with CR only (not CRLF)
        DATA lv_cr TYPE c LENGTH 1.
        lv_cr = cl_abap_char_utilities=>cr_lf+0(1).
        mo_cpm->provide_input( lv_cmd && lv_cr ).
        execute_step( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD cleanup.
    IF mo_display IS BOUND.
      mo_display->free( ).
      FREE mo_display.
    ENDIF.
    FREE mo_cpm.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF r_test = abap_true.
    gv_program = gc_hello_com.
    gv_prog_name = 'HELLO.COM'.

  ELSEIF r_smw0 = abap_true.
    IF p_prog IS INITIAL.
      MESSAGE 'Please select a program from SMW0' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA: lt_mime   TYPE w3mimetabtype,
          ls_key    TYPE wwwdatatab,
          lv_size   TYPE i.

    ls_key-relid = 'MI'.
    ls_key-objid = p_prog.

    SELECT SINGLE value FROM wwwparams INTO @DATA(lv_filesize)
      WHERE relid = @ls_key-relid AND objid = @ls_key-objid AND name = 'filesize'.

    IF sy-subrc <> 0.
      MESSAGE 'Program not found in SMW0' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    lv_size = lv_filesize.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING key  = ls_key
      TABLES    mime = lt_mime
      EXCEPTIONS OTHERS = 1.

    IF sy-subrc <> 0.
      MESSAGE 'Failed to load program from SMW0' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING input_length = lv_size
      IMPORTING buffer       = gv_program
      TABLES    binary_tab   = lt_mime.

    gv_prog_name = p_prog.

  ELSE.
    IF p_path IS INITIAL.
      MESSAGE 'Please specify a program file path' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA: lt_data TYPE TABLE OF x255,
          lv_len  TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename   = p_path
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_len
      CHANGING
        data_tab   = lt_data
      EXCEPTIONS
        OTHERS     = 1 ).

    IF sy-subrc <> 0.
      MESSAGE 'Failed to load program file' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_data INTO DATA(ls_data).
      gv_program = gv_program && ls_data.
    ENDLOOP.
    gv_program = gv_program(lv_len).

    " Extract filename from path
    DATA lv_filename TYPE string.
    lv_filename = p_path.
    IF lv_filename CS '\'.
      lv_filename = substring_after( val = lv_filename sub = '\' occ = -1 ).
    ENDIF.
    IF lv_filename CS '/'.
      lv_filename = substring_after( val = lv_filename sub = '/' occ = -1 ).
    ENDIF.
    gv_prog_name = lv_filename.
  ENDIF.

  IF gv_program IS INITIAL.
    MESSAGE 'Failed to load program' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  go_controller = NEW lcl_console_controller(
    iv_program      = gv_program
    iv_program_name = gv_prog_name ).
  CALL SCREEN 100.


*----------------------------------------------------------------------*
* Screen 100 PBO
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE'.

  IF go_container IS NOT BOUND.
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'HTML_CONTAINER'
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc = 0.
      go_controller->initialize_screen( go_container ).
    ENDIF.
  ENDIF.
ENDMODULE.


*----------------------------------------------------------------------*
* Screen 100 PAI
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA lt_dynpfields TYPE STANDARD TABLE OF dynpread WITH EMPTY KEY.
  DATA ls_dynpfield TYPE dynpread.

  ls_dynpfield-fieldname = 'GV_INPUT'.
  APPEND ls_dynpfield TO lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynpfields
    EXCEPTIONS
      OTHERS     = 0.

  READ TABLE lt_dynpfields INDEX 1 INTO ls_dynpfield.
  IF sy-subrc = 0.
    gv_input = ls_dynpfield-fieldvalue.
  ENDIF.

  go_controller->handle_command( gv_ok ).

  CLEAR gv_ok.
  CLEAR gv_input.
ENDMODULE.