*&---------------------------------------------------------------------*
*& Report ZCPM_SPEEDRUN
*& CP/M Emulator - Automated Test Runner
*& Feed CP/M programs with scripts and verify output
*&---------------------------------------------------------------------*
REPORT zcpm_speedrun.

*----------------------------------------------------------------------*
* Selection Screen - Program Source
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-010.
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    PARAMETERS: r_psmw0 RADIOBUTTON GROUP prg DEFAULT 'X' USER-COMMAND prg,
                r_pfile RADIOBUTTON GROUP prg.
  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: p_prog TYPE wwwdatatab-objid AS LISTBOX VISIBLE LENGTH 50
                MODIF ID psm.
  SELECTION-SCREEN END OF BLOCK b2.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_ppath TYPE string LOWER CASE MODIF ID pfl.
  SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b10.

*----------------------------------------------------------------------*
* Selection Screen - Script Source
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b40 WITH FRAME TITLE TEXT-040.
  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
    PARAMETERS: r_ssmw0 RADIOBUTTON GROUP scr DEFAULT 'X' USER-COMMAND scr,
                r_sfile RADIOBUTTON GROUP scr,
                r_sbuil RADIOBUTTON GROUP scr.
  SELECTION-SCREEN END OF BLOCK b4.

  SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
    PARAMETERS: p_script TYPE wwwdatatab-objid AS LISTBOX VISIBLE LENGTH 50
                MODIF ID ssm.
  SELECTION-SCREEN END OF BLOCK b5.

  SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-006.
    PARAMETERS: p_spath TYPE string LOWER CASE MODIF ID sfl.
  SELECTION-SCREEN END OF BLOCK b6.
SELECTION-SCREEN END OF BLOCK b40.


*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  " Populate program dropdown (*.COM)
  SELECT objid FROM wwwdata
    INTO TABLE @DATA(lt_progs)
    WHERE relid = 'MI'
      AND objid LIKE '%.COM'.

  CLEAR lt_values.
  LOOP AT lt_progs INTO DATA(ls_prog).
    ls_value-key = ls_prog-objid.
    ls_value-text = ls_prog-objid.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  IF lt_values IS NOT INITIAL.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING id     = 'P_PROG'
                values = lt_values.
    p_prog = lt_values[ 1 ]-key.
  ENDIF.

  " Populate script dropdown (*.TXT)
  SELECT objid FROM wwwdata
    INTO TABLE @DATA(lt_scripts)
    WHERE relid = 'MI'
      AND objid LIKE '%.TXT'.

  CLEAR lt_values.
  LOOP AT lt_scripts INTO DATA(ls_script).
    ls_value-key = ls_script-objid.
    ls_value-text = ls_script-objid.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  IF lt_values IS NOT INITIAL.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING id     = 'P_SCRIPT'
                values = lt_values.
    p_script = lt_values[ 1 ]-key.
  ENDIF.


*----------------------------------------------------------------------*
* At Selection Screen Output
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Program SMW0 dropdown
    IF screen-group1 = 'PSM'.
      screen-active = COND #( WHEN r_psmw0 = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    " Program file path
    IF screen-group1 = 'PFL'.
      screen-active = COND #( WHEN r_pfile = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    " Script SMW0 dropdown
    IF screen-group1 = 'SSM'.
      screen-active = COND #( WHEN r_ssmw0 = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    " Script file path
    IF screen-group1 = 'SFL'.
      screen-active = COND #( WHEN r_sfile = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


*----------------------------------------------------------------------*
* F4 Help
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ppath.
  DATA: lt_filetab TYPE filetable,
        lv_rc      TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title      = 'Select CP/M Program'
      file_filter       = 'CP/M Programs (*.com)|*.com|All (*.*)|*.*'
      default_extension = 'com'
    CHANGING
      file_table        = lt_filetab
      rc                = lv_rc ).

  IF lv_rc >= 1.
    p_ppath = lt_filetab[ 1 ]-filename.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_spath.
  DATA: lt_filetab TYPE filetable,
        lv_rc      TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title      = 'Select Script File'
      file_filter       = 'Text Files (*.txt)|*.txt|All (*.*)|*.*'
      default_extension = 'txt'
    CHANGING
      file_table        = lt_filetab
      rc                = lv_rc ).

  IF lv_rc >= 1.
    p_spath = lt_filetab[ 1 ]-filename.
  ENDIF.


*----------------------------------------------------------------------*
* Main Processing
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lv_program  TYPE xstring,
        lt_commands TYPE zcl_cpm_speedrun=>tt_commands,
        ls_result   TYPE zcl_cpm_speedrun=>ts_result.

  WRITE: / |{ repeat( val = '=' occ = 60 ) }|.
  WRITE: / 'ZCPM_SPEEDRUN - CP/M Test Runner'.
  WRITE: / |{ repeat( val = '=' occ = 60 ) }|.
  SKIP.

  " Load program
  IF r_psmw0 = abap_true.
    IF p_prog IS INITIAL.
      WRITE: / 'Error: No program selected from SMW0' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    WRITE: / |Program: { p_prog } (SMW0)|.
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING key  = CONV wwwdatatab( VALUE #( relid = 'MI' objid = p_prog ) )
      IMPORTING mime = lv_program
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0.
      WRITE: / 'Failed to load program from SMW0' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
  ELSE.
    IF p_ppath IS INITIAL.
      WRITE: / 'Error: No program file specified' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    WRITE: / |Program: { p_ppath } (File)|.
    DATA: lt_data TYPE TABLE OF x255,
          lv_len  TYPE i.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING filename   = CONV #( p_ppath )
                filetype   = 'BIN'
      IMPORTING filelength = lv_len
      CHANGING  data_tab   = lt_data
      EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      WRITE: / 'Failed to load program file' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    LOOP AT lt_data INTO DATA(ls_data).
      lv_program = lv_program && ls_data.
    ENDLOOP.
    lv_program = lv_program(lv_len).
  ENDIF.

  IF lv_program IS INITIAL.
    WRITE: / 'Failed to load program' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  WRITE: / |Size: { xstrlen( lv_program ) } bytes|.

  " Load script
  IF r_sbuil = abap_true.
    " Built-in Hello World test
    WRITE: / |Script: Built-in Hello World Test|.
    " This test assumes a program that prints "Hello" and exits
    APPEND '%*Hello' TO lt_commands.

  ELSEIF r_ssmw0 = abap_true.
    IF p_script IS INITIAL.
      WRITE: / 'Error: No script selected from SMW0' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    WRITE: / |Script: { p_script } (SMW0)|.

    " Load script from SMW0
    DATA lv_script_data TYPE xstring.
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING key  = CONV wwwdatatab( VALUE #( relid = 'MI' objid = p_script ) )
      IMPORTING mime = lv_script_data
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0.
      WRITE: / 'Failed to load script from SMW0' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.

    " Convert to string and split into lines
    DATA lv_script_text TYPE string.
    lv_script_text = cl_abap_codepage=>convert_from( lv_script_data ).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_script_text
      WITH cl_abap_char_utilities=>newline.
    SPLIT lv_script_text AT cl_abap_char_utilities=>newline INTO TABLE lt_commands.

    " Remove comments and empty lines
    DELETE lt_commands WHERE table_line IS INITIAL.
    DELETE lt_commands WHERE table_line(1) = '#'.

  ELSE.
    IF p_spath IS INITIAL.
      WRITE: / 'Error: No script file specified' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    WRITE: / |Script: { p_spath } (File)|.

    " Load script from file
    DATA: lt_script_lines TYPE TABLE OF string.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING filename   = CONV #( p_spath )
                filetype   = 'ASC'
      CHANGING  data_tab   = lt_script_lines
      EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      WRITE: / 'Failed to load script file' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    lt_commands = lt_script_lines.
    DELETE lt_commands WHERE table_line IS INITIAL.
    DELETE lt_commands WHERE table_line(1) = '#'.
  ENDIF.

  IF lt_commands IS INITIAL.
    WRITE: / 'Script is empty' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  WRITE: / |Commands: { lines( lt_commands ) }|.
  SKIP.

  WRITE: / |{ repeat( val = '-' occ = 60 ) }|.
  WRITE: / 'EXECUTION LOG'.
  WRITE: / |{ repeat( val = '-' occ = 60 ) }|.
  SKIP.

  " Run speedrun
  TRY.
      DATA(lo_speedrun) = NEW zcl_cpm_speedrun(
        iv_program  = lv_program
        it_commands = lt_commands ).

      ls_result = lo_speedrun->run( ).

      " Print log
      lo_speedrun->print_log( ).

      " Summary
      SKIP.
      WRITE: / |{ repeat( val = '=' occ = 60 ) }|.
      WRITE: / 'SUMMARY'.
      WRITE: / |{ repeat( val = '-' occ = 60 ) }|.

      IF ls_result-success = abap_true.
        WRITE: / |Commands: { ls_result-commands_run }/{ ls_result-commands_total }|.

        IF ls_result-assertions_total > 0.
          WRITE: / |Assertions: { ls_result-assertions_pass }/{ ls_result-assertions_total } passed|.
          IF ls_result-assertions_fail = 0.
            WRITE: / '*** ALL ASSERTIONS PASSED ***' COLOR COL_POSITIVE.
          ELSE.
            WRITE: / |*** { ls_result-assertions_fail } ASSERTION(S) FAILED ***| COLOR COL_NEGATIVE.
          ENDIF.
        ENDIF.

        IF ls_result-program_ended = abap_true.
          WRITE: / 'Program ended during run' COLOR COL_TOTAL.
        ENDIF.
      ELSE.
        WRITE: / |Failed: { ls_result-error_message }| COLOR COL_NEGATIVE.
      ENDIF.

      WRITE: / |{ repeat( val = '=' occ = 60 ) }|.

    CATCH cx_root INTO DATA(lx_error).
      WRITE: / 'ERROR:' COLOR COL_NEGATIVE, lx_error->get_text( ).
  ENDTRY.