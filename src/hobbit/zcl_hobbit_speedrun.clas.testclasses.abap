*"* use this source file for your ABAP unit test classes
CLASS ltc_hobbit_speedrun DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA: gv_tap_data TYPE xstring.
    CLASS-METHODS class_setup.
    METHODS test_open_door FOR TESTING.
    METHODS test_echo_format FOR TESTING.
    METHODS test_double_prompt FOR TESTING.
ENDCLASS.

CLASS ltc_hobbit_speedrun IMPLEMENTATION.

  METHOD class_setup.
    SELECT SINGLE v INTO gv_tap_data FROM zcpm_00_bin WHERE bin = 'A' AND name = 'HOBBIT.TAP'.
  ENDMETHOD.

  METHOD test_open_door.
    DATA lt_script TYPE zcl_hobbit_speedrun=>tt_commands.
    APPEND 'open door' TO lt_script.
    APPEND '%=OPEN DOOR' TO lt_script.
    APPEND '%=You open the round green door' TO lt_script.
    DATA(lo_speedrun) = NEW zcl_hobbit_speedrun( iv_tap_data = gv_tap_data it_commands = lt_script ).
    DATA(ls_result) = lo_speedrun->run( ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-assertions_fail exp = 0
      msg = |Echo test failed. Log: { lo_speedrun->get_log_as_text( ) }| ).
  ENDMETHOD.

  METHOD test_echo_format.
    DATA lt_script TYPE zcl_hobbit_speedrun=>tt_commands.
    APPEND 'look at door' TO lt_script.
    APPEND '%=LOOK AT DOOR' TO lt_script.
    DATA(lo_speedrun) = NEW zcl_hobbit_speedrun( iv_tap_data = gv_tap_data it_commands = lt_script ).
    DATA(ls_result) = lo_speedrun->run( ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-assertions_fail exp = 0
      msg = |Multi-word echo failed. Log: { lo_speedrun->get_log_as_text( ) }| ).
  ENDMETHOD.

  METHOD test_double_prompt.
    " Test for double ">" prompt issue
    DATA lt_script TYPE zcl_hobbit_speedrun=>tt_commands.
    APPEND 'look' TO lt_script.
    " Check we don't have >> in the output
    APPEND '%!>>' TO lt_script.
    DATA(lo_speedrun) = NEW zcl_hobbit_speedrun( iv_tap_data = gv_tap_data it_commands = lt_script ).
    DATA(ls_result) = lo_speedrun->run( ).
    " Dump log for analysis
    DATA(lv_log) = lo_speedrun->get_log_as_text( ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-assertions_fail exp = 0
      msg = |Double prompt found. Log: { lv_log }| ).
  ENDMETHOD.

ENDCLASS.