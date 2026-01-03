*&---------------------------------------------------------------------*
*& Report ZCPM_ZIP_SYNC
*&---------------------------------------------------------------------*
*& Extract files from .zip and upload to ZCPM_00_BIN table
*&---------------------------------------------------------------------*
REPORT zcpm_zip_sync.

PARAMETERS:
  p_zip  TYPE string LOWER CASE OBLIGATORY,
  p_disk TYPE char30 DEFAULT 'A' OBLIGATORY.

CLASS lcl_zip_sync DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        iv_zip_path TYPE string
        iv_disk     TYPE char30.
  PRIVATE SECTION.
    CLASS-METHODS upload_zip
      IMPORTING
        iv_path          TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring.
    CLASS-METHODS extract_and_save
      IMPORTING
        iv_zip_data TYPE xstring
        iv_disk     TYPE char30.
ENDCLASS.

CLASS lcl_zip_sync IMPLEMENTATION.
  METHOD run.
    " Upload ZIP file
    DATA(lv_zip_data) = upload_zip( iv_zip_path ).
    IF lv_zip_data IS INITIAL.
      WRITE: / 'Error: Could not upload ZIP file'.
      RETURN.
    ENDIF.

    WRITE: / 'ZIP file uploaded:', iv_zip_path.
    WRITE: / 'Target disk:', iv_disk.
    WRITE: / '---'.

    " Extract and save files
    extract_and_save(
      iv_zip_data = lv_zip_data
      iv_disk     = iv_disk ).
  ENDMETHOD.

  METHOD upload_zip.
    DATA lt_bin TYPE TABLE OF x255.
    DATA lv_size TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename   = iv_path
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_size
      CHANGING
        data_tab   = lt_bin
      EXCEPTIONS
        OTHERS     = 1 ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_size
      IMPORTING
        buffer       = rv_xstring
      TABLES
        binary_tab   = lt_bin
      EXCEPTIONS
        OTHERS       = 1.
  ENDMETHOD.

  METHOD extract_and_save.
    DATA lo_zip TYPE REF TO cl_abap_zip.
    DATA lv_count TYPE i.

    " Create ZIP object and load data
    CREATE OBJECT lo_zip.
    lo_zip->load( iv_zip_data ).

    " Get file list
    DATA(lt_files) = lo_zip->files.
    WRITE: / 'Files in ZIP:', lines( lt_files ).
    WRITE: / '---'.

    " Extract each file
    LOOP AT lt_files INTO DATA(ls_file).
      " Skip directories
      IF ls_file-name CP '*/' OR ls_file-name IS INITIAL.
        CONTINUE.
      ENDIF.

      " Extract file content
      DATA lv_content TYPE xstring.
      lo_zip->get(
        EXPORTING
          name    = ls_file-name
        IMPORTING
          content = lv_content ).

      IF lv_content IS INITIAL AND ls_file-size > 0.
        WRITE: / 'Error extracting:', ls_file-name.
        CONTINUE.
      ENDIF.

      " Get just the filename (strip path)
      DATA lv_filename TYPE string.
      lv_filename = ls_file-name.
      IF lv_filename CA '/'.
        FIND ALL OCCURRENCES OF '/' IN lv_filename MATCH OFFSET DATA(lv_pos).
        lv_filename = lv_filename+lv_pos.
        SHIFT lv_filename LEFT BY 1 PLACES.
      ENDIF.
      IF lv_filename CA '\'.
        FIND ALL OCCURRENCES OF '\' IN lv_filename MATCH OFFSET lv_pos.
        lv_filename = lv_filename+lv_pos.
        SHIFT lv_filename LEFT BY 1 PLACES.
      ENDIF.

      " Skip if no filename left
      IF lv_filename IS INITIAL.
        CONTINUE.
      ENDIF.

      " Prepare record
      DATA ls_bin TYPE zcpm_00_bin.
      ls_bin-bin   = iv_disk.
      ls_bin-name  = to_upper( lv_filename ).
      ls_bin-v     = lv_content.
      ls_bin-ts    = utclong_current( ).
      ls_bin-cdate = sy-datum.

      " Upsert
      MODIFY zcpm_00_bin FROM ls_bin.
      IF sy-subrc = 0.
        lv_count = lv_count + 1.
        WRITE: / 'Extracted:', ls_bin-name, '(', ls_file-size, 'bytes )'.
      ELSE.
        WRITE: / 'Error saving:', ls_bin-name.
      ENDIF.
    ENDLOOP.

    WRITE: / '---'.
    WRITE: / 'Done. Files extracted:', lv_count.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_zip_sync=>run(
    iv_zip_path = p_zip
    iv_disk     = p_disk ).
