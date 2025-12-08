*"* use this source file for your ABAP unit test classes
CLASS ltcl_io_console_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_io TYPE REF TO zcl_zork_00_io_console.

    METHODS:
      setup,
      test_print_text FOR TESTING,
      test_print_char FOR TESTING,
      test_print_num FOR TESTING,
      test_new_line FOR TESTING,
      test_read_line FOR TESTING,
      test_read_char FOR TESTING,
      test_status_line FOR TESTING,
      test_erase_window FOR TESTING,
      test_clear_output FOR TESTING.
ENDCLASS.


CLASS ltcl_io_console_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_io.
  ENDMETHOD.

  METHOD test_print_text.
    " Test printing text
    DATA(lo_io) = CAST zif_zork_00_io( mo_io ).

    lo_io->print_text( 'Hello, Zork!' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_output( )
      exp = 'Hello, Zork!'
      msg = 'print_text should append text to output' ).

    " Test appending more text
    lo_io->print_text( ' Welcome.' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_output( )
      exp = 'Hello, Zork! Welcome.'
      msg = 'print_text should append to existing output' ).
  ENDMETHOD.

  METHOD test_print_char.
    " Test printing individual characters
    DATA(lo_io) = CAST zif_zork_00_io( mo_io ).

    " Print 'H' (72), 'i' (105), '!' (33)
    lo_io->print_char( 72 ).
    lo_io->print_char( 105 ).
    lo_io->print_char( 33 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_output( )
      exp = 'Hi!'
      msg = 'print_char should convert ZSCII to characters' ).
  ENDMETHOD.

  METHOD test_print_num.
    " Test printing numbers
    DATA(lo_io) = CAST zif_zork_00_io( mo_io ).

    lo_io->print_num( 42 ).
    lo_io->print_text( ' points' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_output( )
      exp = '42 points'
      msg = 'print_num should print integer as text' ).

    mo_io->clear_output( ).
    lo_io->print_num( -10 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_output( )
      exp = '-10'
      msg = 'print_num should handle negative numbers' ).
  ENDMETHOD.

  METHOD test_new_line.
    " Test newline
    DATA(lo_io) = CAST zif_zork_00_io( mo_io ).

    lo_io->print_text( 'Line 1' ).
    lo_io->new_line( ).
    lo_io->print_text( 'Line 2' ).

    DATA(lv_expected) = |Line 1{ cl_abap_char_utilities=>newline }Line 2|.

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_output( )
      exp = lv_expected
      msg = 'new_line should add newline character' ).
  ENDMETHOD.

  METHOD test_read_line.
    " Test reading input lines
    DATA(lo_io) = CAST zif_zork_00_io( mo_io ).

    " Queue input
    mo_io->queue_input( 'open mailbox' ).

    " Read it back
    DATA(lv_input) = lo_io->read_line( iv_max_len = 80 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_input
      exp = 'open mailbox'
      msg = 'read_line should return queued input' ).

    " Second read should return empty
    lv_input = lo_io->read_line( iv_max_len = 80 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_input
      exp = ''
      msg = 'read_line should return empty when queue is empty' ).
  ENDMETHOD.

  METHOD test_read_char.
    " Test reading single characters
    DATA(lo_io) = CAST zif_zork_00_io( mo_io ).

    " Queue input
    mo_io->queue_input( 'abc' ).

    " Read characters
    DATA(lv_char1) = lo_io->read_char( ).
    DATA(lv_char2) = lo_io->read_char( ).
    DATA(lv_char3) = lo_io->read_char( ).
    DATA(lv_char4) = lo_io->read_char( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_char1
      exp = 97  " 'a'
      msg = 'read_char should return first character' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_char2
      exp = 98  " 'b'
      msg = 'read_char should return second character' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_char3
      exp = 99  " 'c'
      msg = 'read_char should return third character' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_char4
      exp = 0   " empty
      msg = 'read_char should return 0 when queue is empty' ).
  ENDMETHOD.

  METHOD test_status_line.
    " Test status line display
    DATA(lo_io) = CAST zif_zork_00_io( mo_io ).

    lo_io->show_status(
      iv_location = 'West of House'
      iv_score    = 10
      iv_moves    = 5 ).

    " Check stored values
    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_status_location( )
      exp = 'West of House'
      msg = 'show_status should store location' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_status_score( )
      exp = 10
      msg = 'show_status should store score' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_status_moves( )
      exp = 5
      msg = 'show_status should store moves' ).

    " Check output contains status info
    DATA(lv_output) = mo_io->get_output( ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_output
      exp = '*West of House*'
      msg = 'Output should contain location' ).
  ENDMETHOD.

  METHOD test_erase_window.
    " Test erasing window
    DATA(lo_io) = CAST zif_zork_00_io( mo_io ).

    lo_io->print_text( 'Some text' ).
    lo_io->erase_window( -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_output( )
      exp = ''
      msg = 'erase_window(-1) should clear output' ).
  ENDMETHOD.

  METHOD test_clear_output.
    " Test clear_output method
    DATA(lo_io) = CAST zif_zork_00_io( mo_io ).

    lo_io->print_text( 'Some text to clear' ).
    mo_io->clear_output( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_io->get_output( )
      exp = ''
      msg = 'clear_output should empty the output buffer' ).
  ENDMETHOD.

ENDCLASS.
