*&---------------------------------------------------------------------*
*& Unit Tests for ZCL_ZORK_00_EXECUTOR
*&---------------------------------------------------------------------*

CLASS lcl_test_io DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_zork_00_io.
    DATA: mv_output TYPE string,
          mv_lines  TYPE i.
    METHODS:
      get_output RETURNING VALUE(rv_output) TYPE string,
      clear_output.
ENDCLASS.

CLASS lcl_test_io IMPLEMENTATION.
  METHOD zif_zork_00_io~print_char.
    DATA: lv_char TYPE c LENGTH 1.
    IF iv_zscii >= 32 AND iv_zscii <= 126.
      lv_char = cl_abap_conv_in_ce=>uccpi( iv_zscii ).
      mv_output = mv_output && lv_char.
    ENDIF.
  ENDMETHOD.

  METHOD zif_zork_00_io~print_text.
    mv_output = mv_output && iv_text.
  ENDMETHOD.

  METHOD zif_zork_00_io~new_line.
    mv_output = mv_output && cl_abap_char_utilities=>newline.
    mv_lines = mv_lines + 1.
  ENDMETHOD.

  METHOD zif_zork_00_io~print_num.
    mv_output = mv_output && |{ iv_num }|.
  ENDMETHOD.

  METHOD zif_zork_00_io~buffer_mode.
  ENDMETHOD.

  METHOD zif_zork_00_io~read_line.
    rv_text = ''.
  ENDMETHOD.

  METHOD zif_zork_00_io~read_char.
    rv_zscii = 13.
  ENDMETHOD.

  METHOD zif_zork_00_io~show_status.
    mv_output = mv_output && |[Status: { iv_location }]|.
  ENDMETHOD.

  METHOD zif_zork_00_io~split_window.
  ENDMETHOD.

  METHOD zif_zork_00_io~set_window.
  ENDMETHOD.

  METHOD zif_zork_00_io~erase_window.
  ENDMETHOD.

  METHOD zif_zork_00_io~set_cursor.
  ENDMETHOD.

  METHOD zif_zork_00_io~set_style.
  ENDMETHOD.

  METHOD zif_zork_00_io~sound_effect.
  ENDMETHOD.

  METHOD get_output.
    rv_output = mv_output.
  ENDMETHOD.

  METHOD clear_output.
    mv_output = ''.
    mv_lines = 0.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_executor_test DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_memory   TYPE REF TO zcl_zork_00_memory,
      mo_stack    TYPE REF TO zcl_zork_00_stack,
      mo_decoder  TYPE REF TO zcl_zork_00_decoder,
      mo_executor TYPE REF TO zcl_zork_00_executor,
      mo_io       TYPE REF TO lcl_test_io.

    METHODS:
      setup,
      create_test_story RETURNING VALUE(rv_story) TYPE string,
      test_init FOR TESTING,
      test_arithmetic_add FOR TESTING,
      test_arithmetic_sub FOR TESTING,
      test_arithmetic_mul FOR TESTING,
      test_arithmetic_div FOR TESTING,
      test_jump_equal FOR TESTING,
      test_jump_zero FOR TESTING,
      test_jump_less FOR TESTING,
      test_jump_greater FOR TESTING,
      test_bitwise_and FOR TESTING,
      test_bitwise_or FOR TESTING,
      test_inc_dec FOR TESTING,
      test_load_store FOR TESTING,
      test_stack_push_pull FOR TESTING.
ENDCLASS.


CLASS ltcl_executor_test IMPLEMENTATION.

  METHOD setup.
    mo_memory = NEW zcl_zork_00_memory( ).
    mo_stack = NEW zcl_zork_00_stack( mo_memory ).
    mo_decoder = NEW zcl_zork_00_decoder( mo_memory ).
    mo_io = NEW lcl_test_io( ).
    DATA(lv_story) = create_test_story( ).
    mo_memory->load_story( lv_story ).
    mo_executor = NEW zcl_zork_00_executor(
      io_memory  = mo_memory
      io_stack   = mo_stack
      io_decoder = mo_decoder
      io_io      = mo_io ).
  ENDMETHOD.

  METHOD create_test_story.
    DATA: lv_hex TYPE string, lv_i TYPE i.
    lv_hex = '03000001030001000400038000400300'.
    lv_hex = lv_hex && '00000000000000000001000000'.
    lv_hex = lv_hex && '00000000000000000000000000000000'.
    lv_hex = lv_hex && '00000000000000000000'.
    lv_i = 0.
    WHILE lv_i < 96.
      lv_hex = lv_hex && '0000'.
      lv_i = lv_i + 1.
    ENDWHILE.
    lv_i = 0.
    WHILE lv_i < 512.
      lv_hex = lv_hex && '00'.
      lv_i = lv_i + 1.
    ENDWHILE.
    rv_story = lv_hex.
  ENDMETHOD.

  METHOD test_init.
    cl_abap_unit_assert=>assert_equals( act = mo_executor->get_pc( ) exp = 256 msg = 'PC=256' ).
    cl_abap_unit_assert=>assert_equals( act = mo_executor->get_status( ) exp = zcl_zork_00_executor=>c_status_running msg = 'RUNNING' ).
  ENDMETHOD.

  METHOD test_arithmetic_add.
    mo_memory->write_byte( iv_addr = 256 iv_val = 20 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 100 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 50 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 0 ).
    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_stack->pop_stack( ) exp = 150 msg = '100+50=150' ).
  ENDMETHOD.

  METHOD test_arithmetic_sub.
    mo_memory->write_word( iv_addr = 64 iv_val = 200 ).
    mo_memory->write_word( iv_addr = 66 iv_val = 75 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 213 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).
    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_memory->read_word( 68 ) exp = 125 msg = '200-75=125' ).
  ENDMETHOD.

  METHOD test_arithmetic_mul.
    mo_memory->write_word( iv_addr = 64 iv_val = 12 ).
    mo_memory->write_word( iv_addr = 66 iv_val = 7 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 214 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).
    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_memory->read_word( 68 ) exp = 84 msg = '12*7=84' ).
  ENDMETHOD.

  METHOD test_arithmetic_div.
    mo_memory->write_word( iv_addr = 64 iv_val = 100 ).
    mo_memory->write_word( iv_addr = 66 iv_val = 7 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 215 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).
    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_memory->read_word( 68 ) exp = 14 msg = '100/7=14' ).
  ENDMETHOD.

  METHOD test_jump_equal.
    mo_memory->write_word( iv_addr = 64 iv_val = 42 ).
    mo_memory->write_word( iv_addr = 66 iv_val = 42 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 193 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 197 ).
    mo_executor->set_pc( 256 ).
    DATA(lv_pc) = mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = lv_pc exp = 264 msg = 'JE branch' ).
  ENDMETHOD.

  METHOD test_jump_zero.
    mo_memory->write_word( iv_addr = 64 iv_val = 0 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 160 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 197 ).
    mo_executor->set_pc( 256 ).
    DATA(lv_pc) = mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = lv_pc exp = 262 msg = 'JZ branch' ).
  ENDMETHOD.

  METHOD test_jump_less.
    mo_memory->write_word( iv_addr = 64 iv_val = 10 ).
    mo_memory->write_word( iv_addr = 66 iv_val = 20 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 194 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 197 ).
    mo_executor->set_pc( 256 ).
    DATA(lv_pc) = mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = lv_pc exp = 264 msg = 'JL branch' ).
  ENDMETHOD.

  METHOD test_jump_greater.
    mo_memory->write_word( iv_addr = 64 iv_val = 30 ).
    mo_memory->write_word( iv_addr = 66 iv_val = 20 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 195 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 197 ).
    mo_executor->set_pc( 256 ).
    DATA(lv_pc) = mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = lv_pc exp = 264 msg = 'JG branch' ).
  ENDMETHOD.

  METHOD test_bitwise_and.
    mo_memory->write_word( iv_addr = 64 iv_val = 255 ).
    mo_memory->write_word( iv_addr = 66 iv_val = 15 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 201 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).
    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_memory->read_word( 68 ) exp = 15 msg = 'AND' ).
  ENDMETHOD.

  METHOD test_bitwise_or.
    mo_memory->write_word( iv_addr = 64 iv_val = 240 ).
    mo_memory->write_word( iv_addr = 66 iv_val = 15 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 200 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).
    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_memory->read_word( 68 ) exp = 255 msg = 'OR' ).
  ENDMETHOD.

  METHOD test_inc_dec.
    mo_memory->write_word( iv_addr = 64 iv_val = 10 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 165 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 16 ).
    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_memory->read_word( 64 ) exp = 11 msg = 'INC' ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 166 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 16 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_memory->read_word( 64 ) exp = 10 msg = 'DEC' ).
  ENDMETHOD.

  METHOD test_load_store.
    mo_memory->write_word( iv_addr = 300 iv_val = 12345 ).
    mo_memory->write_word( iv_addr = 64 iv_val = 300 ).
    mo_memory->write_word( iv_addr = 66 iv_val = 0 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 207 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 175 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 17 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 18 ).
    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_memory->read_word( 68 ) exp = 12345 msg = 'LOADW' ).
  ENDMETHOD.

  METHOD test_stack_push_pull.
    mo_memory->write_word( iv_addr = 64 iv_val = 999 ).
    mo_memory->write_byte( iv_addr = 256 iv_val = 232 ).
    mo_memory->write_byte( iv_addr = 257 iv_val = 191 ).
    mo_memory->write_byte( iv_addr = 258 iv_val = 16 ).
    mo_executor->set_pc( 256 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_stack->peek_stack( ) exp = 999 msg = 'PUSH' ).
    mo_memory->write_byte( iv_addr = 259 iv_val = 233 ).
    mo_memory->write_byte( iv_addr = 260 iv_val = 95 ).
    mo_memory->write_byte( iv_addr = 261 iv_val = 17 ).
    mo_executor->execute_step( ).
    cl_abap_unit_assert=>assert_equals( act = mo_memory->read_word( 66 ) exp = 999 msg = 'PULL' ).
  ENDMETHOD.

ENDCLASS.