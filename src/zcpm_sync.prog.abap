*&---------------------------------------------------------------------*
*& Report ZCPM_SYNC
*&---------------------------------------------------------------------*
*& Upload files from local folder to ZCPM_00_BIN table
*&---------------------------------------------------------------------*
REPORT zcpm_sync.

PARAMETERS:
  p_folder TYPE string LOWER CASE OBLIGATORY,
  p_disk   TYPE char30 DEFAULT 'A' OBLIGATORY.

CLASS lcl_sync DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        iv_folder TYPE string
        iv_disk   TYPE char30.
  PRIVATE SECTION.
    CLASS-METHODS get_file_list
      IMPORTING
        iv_folder      TYPE string
      RETURNING
        VALUE(rt_files) TYPE filetable.
    CLASS-METHODS upload_file
      IMPORTING
        iv_folder TYPE string
        iv_file   TYPE string
        iv_disk   TYPE char30.
ENDCLASS.

CLASS lcl_sync IMPLEMENTATION.
  METHOD run.
    DATA(lt_files) = get_file_list( iv_folder ).

    IF lt_files IS INITIAL.
      WRITE: / 'No files found in folder:', iv_folder.
      RETURN.
    ENDIF.

    WRITE: / 'Syncing files to disk:', iv_disk.
    WRITE: / 'From folder:', iv_folder.
    WRITE: / '---'.

    LOOP AT lt_files INTO DATA(ls_file).
      upload_file(
        iv_folder = iv_folder
        iv_file   = ls_file-filename
        iv_disk   = iv_disk ).
    ENDLOOP.

    WRITE: / '---'.
    WRITE: / 'Done. Files synced:', lines( lt_files ).
  ENDMETHOD.

  METHOD get_file_list.
    DATA lv_count TYPE i.

    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory = iv_folder
        filter    = '*.*'
        files_only = abap_true
      CHANGING
        file_table = rt_files
        count      = lv_count
      EXCEPTIONS
        OTHERS     = 1 ).

    IF sy-subrc <> 0.
      WRITE: / 'Error listing directory:', iv_folder.
    ENDIF.
  ENDMETHOD.

  METHOD upload_file.
    DATA lt_bin TYPE TABLE OF x255.
    DATA lv_size TYPE i.
    DATA lv_path TYPE string.

    " Build full path
    lv_path = iv_folder && '\' && iv_file.

    " Upload file as binary
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename   = lv_path
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_size
      CHANGING
        data_tab   = lt_bin
      EXCEPTIONS
        OTHERS     = 1 ).

    IF sy-subrc <> 0.
      WRITE: / 'Error uploading:', iv_file.
      RETURN.
    ENDIF.

    " Convert to xstring
    DATA lv_xstring TYPE xstring.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_size
      IMPORTING
        buffer       = lv_xstring
      TABLES
        binary_tab   = lt_bin
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      WRITE: / 'Error converting:', iv_file.
      RETURN.
    ENDIF.

    " Prepare record
    DATA ls_bin TYPE zcpm_00_bin.
    ls_bin-bin   = iv_disk.
    ls_bin-name  = to_upper( iv_file ).
    ls_bin-v     = lv_xstring.
    ls_bin-ts    = utclong_current( ).
    ls_bin-cdate = sy-datum.

    " Upsert
    MODIFY zcpm_00_bin FROM ls_bin.
    IF sy-subrc = 0.
      WRITE: / 'Synced:', ls_bin-name, '(', lv_size, 'bytes )'.
    ELSE.
      WRITE: / 'Error saving:', ls_bin-name.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_sync=>run(
    iv_folder = p_folder
    iv_disk   = p_disk ).
