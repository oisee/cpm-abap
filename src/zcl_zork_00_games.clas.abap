CLASS zcl_zork_00_games DEFINITION PUBLIC FINAL CREATE PRIVATE.
*----------------------------------------------------------------------*
* Game Factory
* Returns game data providers by name
*----------------------------------------------------------------------*

  PUBLIC SECTION.
    CLASS-METHODS:
      get_game
        IMPORTING iv_name        TYPE string
        RETURNING VALUE(ro_game) TYPE REF TO zif_zork_00_game
        RAISING   cx_sy_create_object_error,

      get_available_games
        RETURNING VALUE(rt_games) TYPE string_table.

ENDCLASS.


CLASS zcl_zork_00_games IMPLEMENTATION.

  METHOD get_game.
    DATA: lv_class TYPE string.

    CASE to_upper( iv_name ).
      WHEN 'MINIZORK'.
        lv_class = 'ZCL_ZORK_00_GAME_MINIZORK'.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_sy_create_object_error.
    ENDCASE.

    CREATE OBJECT ro_game TYPE (lv_class).
  ENDMETHOD.


  METHOD get_available_games.
    APPEND 'MINIZORK' TO rt_games.
  ENDMETHOD.

ENDCLASS.
