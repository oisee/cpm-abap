*&---------------------------------------------------------------------*
*& Report ZCPM_ZORK_TEST
*& Test ZORK1 running on CP/M Z80 emulator with file BDOS
*&---------------------------------------------------------------------*
REPORT zcpm_zork_test.

CLASS lcl_smw0_loader DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS load
      IMPORTING iv_objid       TYPE wwwdatatab-objid
      RETURNING VALUE(rv_data) TYPE xstring.
ENDCLASS.

CLASS lcl_smw0_loader IMPLEMENTATION.
  METHOD load.
    DATA: lt_mime TYPE w3mimetabtype,
          ls_key  TYPE wwwdatatab,
          lv_size TYPE i.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_objid.

    " Get file size
    SELECT SINGLE value FROM wwwparams INTO @DATA(lv_filesize)
      WHERE relid = @ls_key-relid AND objid = @ls_key-objid AND name = 'filesize'.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    lv_size = lv_filesize.

    " Import binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING key  = ls_key
      TABLES    mime = lt_mime
      EXCEPTIONS OTHERS = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Convert to xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING input_length = lv_size
      IMPORTING buffer       = rv_data
      TABLES    binary_tab   = lt_mime.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  DATA: lo_cpm       TYPE REF TO zcl_cpm_emulator,
        lv_com_data  TYPE xstring,
        lv_dat_data  TYPE xstring,
        lv_output    TYPE string,
        lv_instr     TYPE i VALUE 0,
        lv_max_instr TYPE i VALUE 5000000.

  " Load ZORK1.COM and ZORK1.DAT from SMW0
  WRITE: / 'Loading ZORK1.COM from SMW0...'.
  lv_com_data = lcl_smw0_loader=>load( 'ZORK1.COM' ).
  IF lv_com_data IS INITIAL.
    WRITE: / 'ERROR: Failed to load ZORK1.COM'.
    RETURN.
  ENDIF.
  WRITE: / |  Loaded { xstrlen( lv_com_data ) } bytes|.

  WRITE: / 'Loading ZORK1.DAT from SMW0...'.
  lv_dat_data = lcl_smw0_loader=>load( 'ZORK1.DAT' ).
  IF lv_dat_data IS INITIAL.
    WRITE: / 'ERROR: Failed to load ZORK1.DAT'.
    RETURN.
  ENDIF.
  WRITE: / |  Loaded { xstrlen( lv_dat_data ) } bytes|.

  WRITE: / ''.
  WRITE: / 'Initializing CP/M emulator...'.

  " Create and initialize emulator
  lo_cpm = NEW zcl_cpm_emulator( ).
  lo_cpm->reset( ).

  " Load program
  lo_cpm->load_program( lv_com_data ).

  " Register ZORK1.DAT for file operations
  lo_cpm->register_file( iv_filename = 'ZORK1.DAT' iv_data = lv_dat_data ).

  " Set up default FCB at 0x005C with ZORK1.DAT
  lo_cpm->setup_fcb( iv_fcb_addr = 92 iv_filename = 'ZORK1.DAT' ).  " 0x005C = 92

  " Provide test commands - CP/M uses CR as line terminator
  DATA lv_cr TYPE c LENGTH 1.
  lv_cr = cl_abap_char_utilities=>cr_lf+0(1).  " Just CR
  lo_cpm->provide_input( |look{ lv_cr }| ).
  lo_cpm->provide_input( |open mailbox{ lv_cr }| ).
  lo_cpm->provide_input( |take leaflet{ lv_cr }| ).
  lo_cpm->provide_input( |read leaflet{ lv_cr }| ).
  lo_cpm->provide_input( |go north{ lv_cr }| ).

  WRITE: / 'Running ZORK1...'.
  WRITE: / '----------------------------------------'.

  " Run emulator
  lv_output = lo_cpm->run( iv_max_cycles = lv_max_instr ).

  " Post-process output: replace CRLF with LF
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_output WITH cl_abap_char_utilities=>newline.
  " Also remove any remaining standalone CR
  REPLACE ALL OCCURRENCES OF lv_cr IN lv_output WITH ``.

  " Display output
  WRITE: / ''.
  WRITE: / '=== ZORK OUTPUT ==='.

  DATA lt_lines TYPE string_table.
  SPLIT lv_output AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
  LOOP AT lt_lines INTO DATA(lv_line).
    WRITE: / lv_line.
  ENDLOOP.

  WRITE: / ''.
  WRITE: / '=== END OF OUTPUT ==='.

  IF lo_cpm->is_running( ) = abap_true.
    WRITE: / 'Status: Program waiting for more input'.
  ELSE.
    WRITE: / 'Status: Program ended'.
  ENDIF.