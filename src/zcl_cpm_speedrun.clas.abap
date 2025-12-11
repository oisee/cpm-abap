*&---------------------------------------------------------------------*
*& Class ZCL_CPM_SPEEDRUN
*& Run CP/M programs against command scripts and capture log
*&---------------------------------------------------------------------*
CLASS zcl_cpm_speedrun DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      c_role_output   TYPE c LENGTH 1 VALUE 'O',
      c_role_input    TYPE c LENGTH 1 VALUE 'I',
      c_role_system   TYPE c LENGTH 1 VALUE 'S',
      c_role_error    TYPE c LENGTH 1 VALUE 'E',
      c_role_pass     TYPE c LENGTH 1 VALUE '+',
      c_role_fail     TYPE c LENGTH 1 VALUE '-'.

    TYPES: BEGIN OF ts_log_entry,
             seq  TYPE i,
             role TYPE c LENGTH 1,
             text TYPE string,
           END OF ts_log_entry,
           tt_log TYPE STANDARD TABLE OF ts_log_entry WITH EMPTY KEY.

    TYPES: tt_commands TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    TYPES: BEGIN OF ts_result,
             success          TYPE abap_bool,
             commands_total   TYPE i,
             commands_run     TYPE i,
             assertions_total TYPE i,
             assertions_pass  TYPE i,
             assertions_fail  TYPE i,
             program_ended    TYPE abap_bool,
             error_message    TYPE string,
           END OF ts_result.

    METHODS constructor
      IMPORTING
        iv_program  TYPE xstring
        it_commands TYPE tt_commands.

    METHODS run
      RETURNING VALUE(rs_result) TYPE ts_result.

    METHODS get_log
      RETURNING VALUE(rt_log) TYPE tt_log.

    METHODS print_log.

  PRIVATE SECTION.
    CONSTANTS:
      c_assert_wildcard TYPE c LENGTH 1 VALUE '*',
      c_assert_exact    TYPE c LENGTH 1 VALUE '=',
      c_assert_not      TYPE c LENGTH 1 VALUE '!'.

    TYPES: BEGIN OF ts_script_entry,
             is_command     TYPE abap_bool,
             is_assertion   TYPE abap_bool,
             is_raw         TYPE abap_bool,
             text           TYPE string,
             assert_type    TYPE c LENGTH 1,
             assert_pattern TYPE string,
           END OF ts_script_entry,
           tt_script TYPE STANDARD TABLE OF ts_script_entry WITH EMPTY KEY.

    DATA: mv_program      TYPE xstring,
          mt_script       TYPE tt_script,
          mt_log          TYPE tt_log,
          mv_seq          TYPE i,
          mv_assert_total TYPE i,
          mv_assert_pass  TYPE i,
          mv_assert_fail  TYPE i.

    METHODS parse_script
      IMPORTING it_commands TYPE tt_commands.

    METHODS process_escapes
      IMPORTING iv_input  TYPE string
      EXPORTING ev_output TYPE string
                ev_is_raw TYPE abap_bool.

    METHODS hex_to_char
      IMPORTING iv_hex         TYPE string
      RETURNING VALUE(rv_char) TYPE string.

    METHODS add_log
      IMPORTING
        iv_role TYPE c
        iv_text TYPE string.

    METHODS add_log_multiline
      IMPORTING
        iv_role TYPE c
        iv_text TYPE string.

    METHODS check_assertions
      IMPORTING iv_output     TYPE string
                it_assertions TYPE tt_script.

    METHODS check_assertion
      IMPORTING is_assertion   TYPE ts_script_entry
                iv_output      TYPE string
      RETURNING VALUE(rv_pass) TYPE abap_bool.

ENDCLASS.


CLASS zcl_cpm_speedrun IMPLEMENTATION.

  METHOD constructor.
    mv_program = iv_program.
    parse_script( it_commands ).
    mv_seq = 0.
    mv_assert_total = 0.
    mv_assert_pass = 0.
    mv_assert_fail = 0.
  ENDMETHOD.


  METHOD parse_script.
    DATA ls_entry TYPE ts_script_entry.
    DATA lv_processed TYPE string.
    DATA lv_is_raw TYPE abap_bool.
    DATA lv_type TYPE c LENGTH 1.
    DATA lv_first_char TYPE c LENGTH 1.
    DATA lv_line TYPE string.

    CLEAR mt_script.

    LOOP AT it_commands INTO lv_line.
      CLEAR ls_entry.

      IF lv_line IS INITIAL.
        CONTINUE.
      ENDIF.

      lv_first_char = lv_line.

      IF lv_first_char = '%'.
        ls_entry-is_assertion = abap_true.
        ls_entry-text = lv_line.

        IF strlen( lv_line ) > 1.
          lv_type = substring( val = lv_line off = 1 len = 1 ).
          CASE lv_type.
            WHEN '*'.
              ls_entry-assert_type = c_assert_wildcard.
              IF strlen( lv_line ) > 2.
                ls_entry-assert_pattern = substring( val = lv_line off = 2 ).
              ENDIF.
            WHEN '='.
              ls_entry-assert_type = c_assert_exact.
              IF strlen( lv_line ) > 2.
                ls_entry-assert_pattern = substring( val = lv_line off = 2 ).
              ENDIF.
            WHEN '!'.
              ls_entry-assert_type = c_assert_not.
              IF strlen( lv_line ) > 2.
                ls_entry-assert_pattern = substring( val = lv_line off = 2 ).
              ENDIF.
            WHEN OTHERS.
              ls_entry-assert_type = c_assert_wildcard.
              ls_entry-assert_pattern = substring( val = lv_line off = 1 ).
          ENDCASE.
        ENDIF.

        APPEND ls_entry TO mt_script.
      ELSE.
        ls_entry-is_command = abap_true.

        process_escapes(
          EXPORTING iv_input = lv_line
          IMPORTING ev_output = lv_processed
                    ev_is_raw = lv_is_raw ).

        ls_entry-text = lv_processed.
        ls_entry-is_raw = lv_is_raw.
        APPEND ls_entry TO mt_script.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD process_escapes.
    DATA lv_result TYPE string.
    DATA lv_esc TYPE string.
    DATA lv_ctrl TYPE x LENGTH 1.
    DATA lv_pos TYPE i.
    DATA lv_hex TYPE string.
    DATA lv_char TYPE string.
    DATA lv_prefix TYPE string.

    ev_is_raw = abap_false.
    lv_result = iv_input.

    " Check for $RAW: prefix
    IF strlen( lv_result ) >= 5.
      lv_prefix = substring( val = lv_result off = 0 len = 5 ).
      IF lv_prefix = '$RAW:'.
        ev_is_raw = abap_true.
        lv_result = substring( val = lv_result off = 5 ).
      ENDIF.
    ENDIF.

    " Replace escape sequences - order matters (longer patterns first)
    REPLACE ALL OCCURRENCES OF '$CRLF' IN lv_result WITH cl_abap_char_utilities=>cr_lf.

    " CTRL codes
    lv_ctrl = '03'.
    lv_esc = cl_abap_conv_in_ce=>uccp( lv_ctrl ).
    REPLACE ALL OCCURRENCES OF '$CTRLC' IN lv_result WITH lv_esc.

    lv_ctrl = '04'.
    lv_esc = cl_abap_conv_in_ce=>uccp( lv_ctrl ).
    REPLACE ALL OCCURRENCES OF '$CTRLD' IN lv_result WITH lv_esc.

    lv_ctrl = '1A'.
    lv_esc = cl_abap_conv_in_ce=>uccp( lv_ctrl ).
    REPLACE ALL OCCURRENCES OF '$CTRLZ' IN lv_result WITH lv_esc.

    " CR = 0x0D
    lv_ctrl = '0D'.
    lv_esc = cl_abap_conv_in_ce=>uccp( lv_ctrl ).
    REPLACE ALL OCCURRENCES OF '$CR' IN lv_result WITH lv_esc.

    REPLACE ALL OCCURRENCES OF '$LF' IN lv_result WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF '$TAB' IN lv_result WITH cl_abap_char_utilities=>horizontal_tab.
    REPLACE ALL OCCURRENCES OF '$BS' IN lv_result WITH cl_abap_char_utilities=>backspace.

    " ESC = 0x1B
    lv_ctrl = '1B'.
    lv_esc = cl_abap_conv_in_ce=>uccp( lv_ctrl ).
    REPLACE ALL OCCURRENCES OF '$ESC' IN lv_result WITH lv_esc.

    " DEL = 0x7F
    lv_ctrl = '7F'.
    lv_esc = cl_abap_conv_in_ce=>uccp( lv_ctrl ).
    REPLACE ALL OCCURRENCES OF '$DEL' IN lv_result WITH lv_esc.

    " NUL = 0x00
    lv_ctrl = '00'.
    lv_esc = cl_abap_conv_in_ce=>uccp( lv_ctrl ).
    REPLACE ALL OCCURRENCES OF '$NUL' IN lv_result WITH lv_esc.

    " Process $xHH hex escapes manually
    WHILE lv_result CS '$x'.
      FIND FIRST OCCURRENCE OF '$x' IN lv_result MATCH OFFSET lv_pos.
      IF sy-subrc = 0 AND strlen( lv_result ) >= lv_pos + 4.
        lv_hex = substring( val = lv_result off = lv_pos + 2 len = 2 ).
        lv_char = hex_to_char( lv_hex ).
        lv_result = substring( val = lv_result off = 0 len = lv_pos )
                 && lv_char
                 && substring( val = lv_result off = lv_pos + 4 ).
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    ev_output = lv_result.
  ENDMETHOD.


  METHOD hex_to_char.
    DATA lv_hex_byte TYPE x LENGTH 1.

    TRY.
        lv_hex_byte = iv_hex.
        rv_char = cl_abap_conv_in_ce=>uccp( lv_hex_byte ).
      CATCH cx_root.
        rv_char = '?'.
    ENDTRY.
  ENDMETHOD.


  METHOD run.
    DATA lo_cpm TYPE REF TO zcl_cpm_emulator.
    DATA lv_output TYPE string.
    DATA lt_assertions TYPE tt_script.
    DATA lv_cmd_count TYPE i.
    DATA lv_input TYPE string.
    DATA ls_entry TYPE ts_script_entry.

    CLEAR mt_log.
    mv_seq = 0.
    mv_assert_total = 0.
    mv_assert_pass = 0.
    mv_assert_fail = 0.

    IF mv_program IS INITIAL.
      add_log( iv_role = c_role_error iv_text = 'No program provided' ).
      rs_result-success = abap_false.
      rs_result-error_message = 'No program provided'.
      RETURN.
    ENDIF.

    IF mt_script IS INITIAL.
      add_log( iv_role = c_role_error iv_text = 'No script provided' ).
      rs_result-success = abap_false.
      rs_result-error_message = 'No script provided'.
      RETURN.
    ENDIF.

    LOOP AT mt_script INTO ls_entry WHERE is_command = abap_true.
      rs_result-commands_total = rs_result-commands_total + 1.
    ENDLOOP.

    TRY.
        lo_cpm = NEW zcl_cpm_emulator( ).
        lo_cpm->reset( ).
        lo_cpm->load_program( mv_program ).

        lv_output = lo_cpm->run( iv_max_cycles = 10000000 ).

        IF lv_output IS NOT INITIAL.
          add_log_multiline( iv_role = c_role_output iv_text = lv_output ).
        ENDIF.

        IF lo_cpm->is_running( ) = abap_false.
          rs_result-program_ended = abap_true.
          add_log( iv_role = c_role_system iv_text = '--- Program ended ---' ).
        ENDIF.

        CLEAR lt_assertions.

        LOOP AT mt_script INTO ls_entry.
          IF ls_entry-is_assertion = abap_true.
            APPEND ls_entry TO lt_assertions.
          ELSE.
            IF lt_assertions IS NOT INITIAL.
              check_assertions( iv_output = lv_output it_assertions = lt_assertions ).
              CLEAR lt_assertions.
            ENDIF.

            IF lo_cpm->is_running( ) = abap_false.
              EXIT.
            ENDIF.

            lv_cmd_count = lv_cmd_count + 1.
            rs_result-commands_run = lv_cmd_count.

            add_log( iv_role = c_role_input iv_text = ls_entry-text ).

            IF ls_entry-is_raw = abap_true.
              lv_input = ls_entry-text.
            ELSE.
              lv_input = ls_entry-text && cl_abap_char_utilities=>cr_lf.
            ENDIF.

            lo_cpm->provide_input( lv_input ).

            lv_output = lo_cpm->run( iv_max_cycles = 10000000 ).

            IF lv_output IS NOT INITIAL.
              add_log_multiline( iv_role = c_role_output iv_text = lv_output ).
            ELSE.
              CLEAR lv_output.
            ENDIF.

            IF lo_cpm->is_running( ) = abap_false.
              rs_result-program_ended = abap_true.
              add_log( iv_role = c_role_system iv_text = '--- Program ended ---' ).
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF lt_assertions IS NOT INITIAL.
          check_assertions( iv_output = lv_output it_assertions = lt_assertions ).
        ENDIF.

        rs_result-success = abap_true.
        rs_result-assertions_total = mv_assert_total.
        rs_result-assertions_pass = mv_assert_pass.
        rs_result-assertions_fail = mv_assert_fail.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-error_message = lx_error->get_text( ).
        add_log( iv_role = c_role_error iv_text = rs_result-error_message ).
    ENDTRY.
  ENDMETHOD.


  METHOD check_assertions.
    DATA ls_assert TYPE ts_script_entry.
    DATA lv_pass TYPE abap_bool.

    LOOP AT it_assertions INTO ls_assert.
      mv_assert_total = mv_assert_total + 1.

      lv_pass = check_assertion( is_assertion = ls_assert iv_output = iv_output ).

      IF lv_pass = abap_true.
        mv_assert_pass = mv_assert_pass + 1.
        add_log( iv_role = c_role_pass iv_text = |[PASS] { ls_assert-text }| ).
      ELSE.
        mv_assert_fail = mv_assert_fail + 1.
        add_log( iv_role = c_role_fail iv_text = |[FAIL] { ls_assert-text }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_assertion.
    DATA lv_output_lower TYPE string.
    DATA lv_pattern_lower TYPE string.

    rv_pass = abap_false.

    CASE is_assertion-assert_type.
      WHEN c_assert_wildcard.
        lv_output_lower = iv_output.
        lv_pattern_lower = is_assertion-assert_pattern.
        TRANSLATE lv_output_lower TO LOWER CASE.
        TRANSLATE lv_pattern_lower TO LOWER CASE.
        IF lv_output_lower CS lv_pattern_lower.
          rv_pass = abap_true.
        ENDIF.

      WHEN c_assert_exact.
        IF iv_output CS is_assertion-assert_pattern.
          rv_pass = abap_true.
        ENDIF.

      WHEN c_assert_not.
        lv_output_lower = iv_output.
        lv_pattern_lower = is_assertion-assert_pattern.
        TRANSLATE lv_output_lower TO LOWER CASE.
        TRANSLATE lv_pattern_lower TO LOWER CASE.
        IF NOT ( lv_output_lower CS lv_pattern_lower ).
          rv_pass = abap_true.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD get_log.
    rt_log = mt_log.
  ENDMETHOD.


  METHOD print_log.
    DATA ls_entry TYPE ts_log_entry.

    LOOP AT mt_log INTO ls_entry.
      CASE ls_entry-role.
        WHEN c_role_input.
          WRITE: / |>>> { ls_entry-text }| COLOR COL_KEY.
        WHEN c_role_output.
          WRITE: / ls_entry-text.
        WHEN c_role_system.
          WRITE: / ls_entry-text COLOR COL_TOTAL.
        WHEN c_role_pass.
          WRITE: / ls_entry-text COLOR COL_POSITIVE.
        WHEN c_role_fail.
          WRITE: / ls_entry-text COLOR COL_NEGATIVE.
        WHEN c_role_error.
          WRITE: / ls_entry-text COLOR COL_NEGATIVE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_log.
    DATA ls_entry TYPE ts_log_entry.

    mv_seq = mv_seq + 1.
    ls_entry-seq = mv_seq.
    ls_entry-role = iv_role.
    ls_entry-text = iv_text.
    APPEND ls_entry TO mt_log.
  ENDMETHOD.


  METHOD add_log_multiline.
    DATA lt_lines TYPE string_table.
    DATA lv_line TYPE string.

    IF iv_text IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT iv_text AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO lv_line.
      add_log( iv_role = iv_role iv_text = lv_line ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.