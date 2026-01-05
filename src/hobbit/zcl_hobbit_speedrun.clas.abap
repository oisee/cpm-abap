CLASS zcl_hobbit_speedrun DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: tt_commands TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_result,
             assertions_pass TYPE i,
             assertions_fail TYPE i,
             commands_run TYPE i,
           END OF ts_result.
    TYPES: tt_log TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        iv_tap_data TYPE xstring
        it_commands TYPE tt_commands.
    METHODS run RETURNING VALUE(rs_result) TYPE ts_result.
    METHODS get_log RETURNING VALUE(rt_log) TYPE tt_log.
    METHODS get_log_as_text RETURNING VALUE(rv_text) TYPE string.

  PRIVATE SECTION.
    DATA mo_emulator TYPE REF TO zcl_hobbit_emulator.
    DATA mt_commands TYPE tt_commands.
    DATA mt_log TYPE tt_log.
    DATA ms_result TYPE ts_result.
    DATA mv_tap_data TYPE xstring.
ENDCLASS.

CLASS zcl_hobbit_speedrun IMPLEMENTATION.

  METHOD constructor.
    mv_tap_data = iv_tap_data.
    mt_commands = it_commands.
  ENDMETHOD.

  METHOD run.
    DATA lv_output TYPE string.
    DATA lv_accumulated TYPE string.
    DATA lv_cmd_idx TYPE i VALUE 1.
    DATA lv_cmd TYPE string.
    DATA lv_pattern TYPE string.
    DATA lv_match TYPE abap_bool.

    mo_emulator = NEW zcl_hobbit_emulator( ).
    mo_emulator->load_tap( mv_tap_data ).

    " Run until first input prompt
    mo_emulator->run( iv_max_cycles = 5000000 ).
    lv_output = mo_emulator->get_output( ).
    lv_accumulated = lv_output.
    APPEND |[INIT] { lv_output }| TO mt_log.

    " Process commands
    WHILE lv_cmd_idx <= lines( mt_commands ).
      READ TABLE mt_commands INTO lv_cmd INDEX lv_cmd_idx.
      lv_cmd_idx = lv_cmd_idx + 1.

      " Check for assertion commands (order matters - check specific prefixes first)
      IF strlen( lv_cmd ) >= 2 AND lv_cmd+0(2) = '%='.
        " Exact match assertion
        lv_pattern = lv_cmd+2.
        IF lv_accumulated CS lv_pattern.
          ms_result-assertions_pass = ms_result-assertions_pass + 1.
          APPEND |[PASS] Found "{ lv_pattern }"| TO mt_log.
        ELSE.
          ms_result-assertions_fail = ms_result-assertions_fail + 1.
          APPEND |[FAIL] Missing "{ lv_pattern }" in: { lv_accumulated }| TO mt_log.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF strlen( lv_cmd ) >= 2 AND lv_cmd+0(2) = '%!'.
        " Negative assertion - check that pattern is NOT in output
        lv_pattern = lv_cmd+2.
        IF lv_accumulated CS lv_pattern.
          ms_result-assertions_fail = ms_result-assertions_fail + 1.
          APPEND |[FAIL] Unwanted "{ lv_pattern }" found in: { lv_accumulated }| TO mt_log.
        ELSE.
          ms_result-assertions_pass = ms_result-assertions_pass + 1.
          APPEND |[PASS] Correctly absent "{ lv_pattern }"| TO mt_log.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF strlen( lv_cmd ) >= 2 AND lv_cmd+0(2) = '%*'.
        " Wildcard match assertion
        lv_pattern = lv_cmd+2.
        IF lv_accumulated CP lv_pattern.
          ms_result-assertions_pass = ms_result-assertions_pass + 1.
          APPEND |[PASS] Pattern "{ lv_pattern }" matched| TO mt_log.
        ELSE.
          ms_result-assertions_fail = ms_result-assertions_fail + 1.
          APPEND |[FAIL] Pattern "{ lv_pattern }" not found in: { lv_accumulated }| TO mt_log.
        ENDIF.
        CONTINUE.
      ENDIF.

      " Regular command - send to game
      IF mo_emulator->is_waiting_input( ) = abap_false.
        APPEND |[WARN] Game not waiting for input at cmd { lv_cmd_idx - 1 }| TO mt_log.
        EXIT.
      ENDIF.

      " Send only CR, not CRLF - LF would be converted to CR causing double-enter
      DATA lv_cr TYPE c LENGTH 1.
      lv_cr = cl_abap_conv_in_ce=>uccpi( 13 ).
      mo_emulator->provide_input( lv_cmd && lv_cr ).
      ms_result-commands_run = ms_result-commands_run + 1.
      APPEND |[CMD] { lv_cmd }| TO mt_log.

      " Run until next input prompt or max cycles
      mo_emulator->run( iv_max_cycles = 5000000 ).
      lv_output = mo_emulator->get_output( ).
      lv_accumulated = lv_output.
      APPEND |[OUT] { lv_output }| TO mt_log.
    ENDWHILE.

    rs_result = ms_result.
  ENDMETHOD.

  METHOD get_log.
    rt_log = mt_log.
  ENDMETHOD.

  METHOD get_log_as_text.
    LOOP AT mt_log INTO DATA(lv_line).
      rv_text = rv_text && lv_line && cl_abap_char_utilities=>newline.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.