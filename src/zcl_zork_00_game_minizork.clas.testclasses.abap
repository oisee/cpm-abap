*----------------------------------------------------------------------*
* Unit Tests for Mini-Zork Game Data Provider
*----------------------------------------------------------------------*
CLASS ltcl_minizork_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_get_name FOR TESTING,
      test_get_story_header FOR TESTING,
      test_factory_get_game FOR TESTING.
ENDCLASS.


CLASS ltcl_minizork_test IMPLEMENTATION.

  METHOD test_get_name.
    DATA: lo_game TYPE REF TO zcl_zork_00_game_minizork.
    CREATE OBJECT lo_game.

    DATA(lv_name) = lo_game->zif_zork_00_game~get_name( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'Mini-Zork'
      msg = 'Game name should be Mini-Zork' ).
  ENDMETHOD.


  METHOD test_get_story_header.
    DATA: lo_game TYPE REF TO zcl_zork_00_game_minizork.
    CREATE OBJECT lo_game.

    DATA(lv_story) = lo_game->zif_zork_00_game~get_story( ).

    " Verify story is not empty
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_story
      msg = 'Story data should not be empty' ).

    " Verify length (Mini-Zork is ~52KB = ~104K hex chars)
    DATA(lv_len) = strlen( lv_story ).
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( lv_len > 100000 )
      msg = |Story should be > 100K hex chars, got { lv_len }| ).

    " Verify Z-machine version 3 header
    DATA(lv_version) = lv_story(2).
    cl_abap_unit_assert=>assert_equals(
      act = lv_version
      exp = '03'
      msg = 'First byte should be 03 (Z-machine version 3)' ).
  ENDMETHOD.


  METHOD test_factory_get_game.
    DATA: lo_game TYPE REF TO zif_zork_00_game.

    TRY.
        lo_game = zcl_zork_00_games=>get_game( 'minizork' ).
      CATCH cx_sy_create_object_error.
        cl_abap_unit_assert=>fail( 'Factory should return minizork game' ).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
      act = lo_game
      msg = 'Game object should be bound' ).

    DATA(lv_name) = lo_game->get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'Mini-Zork'
      msg = 'Factory should return Mini-Zork game' ).
  ENDMETHOD.

ENDCLASS.
