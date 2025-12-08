*&---------------------------------------------------------------------*
*& Report ZORK_00_CONSOLE
*& Z-Machine Interactive Fiction Console (OOP Version)
*& HTML-based display with popup input dialog
*&---------------------------------------------------------------------*
REPORT zork_00_console.

DATA: gv_input TYPE text132.
DATA: gv_ok    TYPE sy-ucomm.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_game TYPE string LOWER CASE DEFAULT 'minizork'.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Local Class Definition - Console Controller
*----------------------------------------------------------------------*
CLASS lcl_console_controller DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor,

      " Screen handling
      initialize_screen
        IMPORTING io_container TYPE REF TO cl_gui_container,

      handle_command
        IMPORTING iv_command TYPE sy-ucomm,

      cleanup,

      " Game control
      start_game,
      restart_game,
      get_user_input,

      " Status
      is_running
        RETURNING VALUE(rv_running) TYPE abap_bool,

      get_game_name
        RETURNING VALUE(rv_name) TYPE string,

      set_game_name
        IMPORTING iv_name TYPE string.

  PRIVATE SECTION.
    DATA:
      mo_memory   TYPE REF TO zcl_zork_00_memory,
      mo_stack    TYPE REF TO zcl_zork_00_stack,
      mo_decoder  TYPE REF TO zcl_zork_00_decoder,
      mo_objects  TYPE REF TO zcl_zork_00_objects,
      mo_executor TYPE REF TO zcl_zork_00_executor,
      mo_io       TYPE REF TO zcl_zork_00_io_html,
      mv_running  TYPE abap_bool,
      mv_game     TYPE string.

    METHODS:
      show_welcome,
      show_error
        IMPORTING iv_message TYPE string,
      execute_game_step,
      load_game_story
        RETURNING VALUE(rv_success) TYPE abap_bool.
ENDCLASS.


CLASS lcl_console_controller IMPLEMENTATION.

  METHOD constructor.
    mv_running = abap_false.
    mv_game = 'minizork'.
  ENDMETHOD.


  METHOD initialize_screen.
    " Create I/O handler with provided container
    mo_io = NEW zcl_zork_00_io_html( io_container ).
    show_welcome( ).
  ENDMETHOD.


  METHOD handle_command.
    CASE iv_command.
      WHEN 'EXIT' OR 'BACK' OR 'CANC'.
        cleanup( ).
        LEAVE PROGRAM.

      WHEN 'ENTER' OR 'COMMAND' OR ''.
        IF mv_running = abap_true.
          get_user_input( ).
        ELSE.
          start_game( ).
        ENDIF.

      WHEN 'RESTART'.
        restart_game( ).
    ENDCASE.
  ENDMETHOD.


  METHOD start_game.
    " Load the story file
    IF load_game_story( ) = abap_false.
      show_error( |Failed to load game: { mv_game }| ).
      RETURN.
    ENDIF.

    mv_running = abap_true.

    " Execute until waiting for input or halted
    execute_game_step( ).
  ENDMETHOD.


  METHOD load_game_story.
    DATA: lo_game  TYPE REF TO zif_zork_00_game,
          lv_story TYPE string.

    TRY.
        " Get game from factory
        lo_game = zcl_zork_00_games=>get_game( mv_game ).
        lv_story = lo_game->get_story( ).

        IF lv_story IS INITIAL.
          rv_success = abap_false.
          RETURN.
        ENDIF.

        " Initialize interpreter components
        mo_memory = NEW zcl_zork_00_memory( ).
        mo_memory->load_story( lv_story ).

        mo_stack = NEW zcl_zork_00_stack( mo_memory ).
        mo_decoder = NEW zcl_zork_00_decoder( mo_memory ).
        mo_objects = NEW zcl_zork_00_objects( mo_memory ).

        mo_executor = NEW zcl_zork_00_executor(
          io_memory  = mo_memory
          io_stack   = mo_stack
          io_decoder = mo_decoder
          io_objects = mo_objects
          io_io      = mo_io ).

        rv_success = abap_true.

      CATCH cx_root.
        rv_success = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD execute_game_step.
    DATA: lv_steps  TYPE i,
          lv_status TYPE i.

    CHECK mo_executor IS BOUND.

    " Execute instructions until waiting for input or halted
    lv_steps = mo_executor->execute_until_halt( iv_max_steps = 50000 ).
    lv_status = mo_executor->get_status( ).

    " Refresh display
    mo_io->refresh_display( ).

    " Check status
    CASE lv_status.
      WHEN zcl_zork_00_executor=>c_status_halted
        OR zcl_zork_00_executor=>c_status_quit.
        mv_running = abap_false.

      WHEN zcl_zork_00_executor=>c_status_waiting.
        " Waiting for input - prompt user
        mo_io->set_waiting_input( abap_true ).
        mo_io->refresh_display( ).
    ENDCASE.
  ENDMETHOD.


  METHOD get_user_input.
    DATA: lv_command TYPE string.

    lv_command = to_upper( gv_input ).
*          lv_title   TYPE string VALUE 'Enter Command',
*          lv_text    TYPE string VALUE 'What do you want to do?'.
*
*    CHECK mo_io IS BOUND.
*
*    " Use popup to get command
*    CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
*      EXPORTING
*        textline1   = lv_text
*        titel       = lv_title
*        valuelength = 80
*      IMPORTING
*        value1      = lv_command
*      EXCEPTIONS
*        OTHERS      = 1.
*
*    IF sy-subrc = 0 AND lv_command IS NOT INITIAL.
    " Queue input and continue execution
    mo_io->queue_input( lv_command ).
    execute_game_step( ).
*    ENDIF.
  ENDMETHOD.


  METHOD restart_game.
    mv_running = abap_false.

    " Clear interpreter state
    FREE: mo_executor, mo_objects, mo_decoder, mo_stack, mo_memory.

    " Show welcome screen
    show_welcome( ).
  ENDMETHOD.


  METHOD show_welcome.
    DATA: lt_html TYPE TABLE OF char1024,
          lv_url  TYPE c LENGTH 250.

    CHECK mo_io IS BOUND.

    DATA(lv_html) =
      |<html>| &&
      |<head>| &&
      |<style>| &&
      |body \{ | &&
      |  background-color: #1a1a2e; | &&
      |  color: #00ff00; | &&
      |  font-family: 'Courier New', monospace; | &&
      |  padding: 20px; | &&
      |\}| &&
      |h1 \{ color: #ffff00; \}| &&
      |.info \{ color: #00cccc; margin: 10px 0; \}| &&
      |.cmd \{ color: #ff6600; \}| &&
      |</style>| &&
      |</head>| &&
      |<body>| &&
      |<h1>Z-Machine Console</h1>| &&
      |<p class="info">ABAP Z-Machine v3 Interpreter</p>| &&
      |<p class="info">Compatible with Zork I, II, III and other Infocom classics</p>| &&
      |<br>| &&
      |<p>Game: <span class="cmd">{ mv_game }</span></p>| &&
      |<br>| &&
      |<p>Press <span class="cmd">Enter</span> to start the game.</p>| &&
      |</body>| &&
      |</html>|.

    " Split into table for HTML viewer
    WHILE strlen( lv_html ) > 0.
      IF strlen( lv_html ) > 1024.
        APPEND lv_html+0(1024) TO lt_html.
        lv_html = lv_html+1024.
      ELSE.
        APPEND lv_html TO lt_html.
        CLEAR lv_html.
      ENDIF.
    ENDWHILE.

    mo_io->mo_html_viewer->load_data(
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_html ).

    mo_io->mo_html_viewer->show_url( url = lv_url ).
  ENDMETHOD.


  METHOD show_error.
    DATA: lt_html TYPE TABLE OF char1024,
          lv_url  TYPE c LENGTH 250.

    CHECK mo_io IS BOUND.

    DATA(lv_html) =
      |<html>| &&
      |<head>| &&
      |<style>| &&
      |body \{ | &&
      |  background-color: #1a1a2e; | &&
      |  color: #ff0000; | &&
      |  font-family: 'Courier New', monospace; | &&
      |  padding: 20px; | &&
      |\}| &&
      |h1 \{ color: #ffff00; \}| &&
      |</style>| &&
      |</head>| &&
      |<body>| &&
      |<h1>Error</h1>| &&
      |<p>{ iv_message }</p>| &&
      |</body>| &&
      |</html>|.

    WHILE strlen( lv_html ) > 0.
      IF strlen( lv_html ) > 1024.
        APPEND lv_html+0(1024) TO lt_html.
        lv_html = lv_html+1024.
      ELSE.
        APPEND lv_html TO lt_html.
        CLEAR lv_html.
      ENDIF.
    ENDWHILE.

    mo_io->mo_html_viewer->load_data(
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_html ).

    mo_io->mo_html_viewer->show_url( url = lv_url ).
  ENDMETHOD.


  METHOD cleanup.
    IF mo_io IS BOUND.
      mo_io->free( ).
      FREE mo_io.
    ENDIF.
    FREE: mo_executor, mo_objects, mo_decoder, mo_stack, mo_memory.
  ENDMETHOD.


  METHOD is_running.
    rv_running = mv_running.
  ENDMETHOD.


  METHOD get_game_name.
    rv_name = mv_game.
  ENDMETHOD.


  METHOD set_game_name.
    mv_game = iv_name.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* Global Controller Instance
*----------------------------------------------------------------------*
DATA: go_controller TYPE REF TO lcl_console_controller,
      go_container  TYPE REF TO cl_gui_custom_container,
      ok_code       TYPE sy-ucomm.


*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CREATE OBJECT go_controller.
  go_controller->set_game_name( p_game ).
  CALL SCREEN 100.


*----------------------------------------------------------------------*
* Screen 100 PBO - Process Before Output
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE100'.

  IF go_container IS NOT BOUND.
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'HTML_CONTAINER'
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc = 0.
      go_controller->initialize_screen( go_container ).
    ENDIF.
  ENDIF.
ENDMODULE.


*----------------------------------------------------------------------*
* Screen 100 PAI - Process After Input
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_controller->handle_command( ok_code ).
  CLEAR ok_code.
ENDMODULE.


*----------------------------------------------------------------------*
* Local Test Classes
*----------------------------------------------------------------------*
CLASS ltcl_console_test DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_controller TYPE REF TO lcl_console_controller.

    METHODS:
      setup,
      test_initial_state FOR TESTING,
      test_set_game_name FOR TESTING,
      test_is_not_running_initially FOR TESTING.
ENDCLASS.


CLASS ltcl_console_test IMPLEMENTATION.

  METHOD setup.
    mo_controller = NEW lcl_console_controller( ).
  ENDMETHOD.


  METHOD test_initial_state.
    cl_abap_unit_assert=>assert_false(
      act = mo_controller->is_running( )
      msg = 'Controller should not be running initially' ).
  ENDMETHOD.


  METHOD test_set_game_name.
    mo_controller->set_game_name( 'testgame' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_controller->get_game_name( )
      exp = 'testgame'
      msg = 'Game name should be set correctly' ).
  ENDMETHOD.


  METHOD test_is_not_running_initially.
    cl_abap_unit_assert=>assert_equals(
      act = mo_controller->is_running( )
      exp = abap_false
      msg = 'Game should not be running initially' ).
  ENDMETHOD.

ENDCLASS.