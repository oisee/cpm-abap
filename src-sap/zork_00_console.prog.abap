*&---------------------------------------------------------------------*
*& Report ZORK_00_CONSOLE
*& Z-Machine Interactive Fiction Console
*& HTML-based display with popup input dialog
*&---------------------------------------------------------------------*
REPORT zork_00_console.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: go_container   TYPE REF TO cl_gui_custom_container,
      go_html_viewer TYPE REF TO cl_gui_html_viewer,
      go_io          TYPE REF TO zcl_zork_00_io_html,
      go_memory      TYPE REF TO zcl_zork_00_memory,
      gv_output      TYPE string,
      gv_running     TYPE abap_bool,
      gv_story_file  TYPE string.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE string LOWER CASE DEFAULT 'ZORK1.Z3'.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Screen 100 - Main Console Screen
*----------------------------------------------------------------------*
* Note: Create screen 100 in SE51 with:
*   - Custom container 'HTML_CONTAINER' (full screen)
*   - OK code field 'OK_CODE'
*   - GUI status 'MAIN100' with buttons: ENTER, EXIT, RESTART
*----------------------------------------------------------------------*

DATA: ok_code TYPE sy-ucomm.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  gv_running = abap_false.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  gv_story_file = p_file.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'TITLE100'.

  " Create container and HTML viewer if not exists
  IF go_container IS NOT BOUND.
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'HTML_CONTAINER'
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0.
      MESSAGE 'Cannot create HTML container' TYPE 'E'.
      RETURN.
    ENDIF.

    " Create I/O handler
    CREATE OBJECT go_io
      EXPORTING
        io_container = go_container.

    " Initialize with welcome message
    PERFORM show_welcome.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      PERFORM cleanup.
      LEAVE PROGRAM.

    WHEN 'ENTER' OR 'COMMAND' OR ''.
      IF gv_running = abap_true.
        " Get command from user
        PERFORM get_user_command.
      ELSE.
        " Start the game
        PERFORM start_game.
      ENDIF.

    WHEN 'RESTART'.
      PERFORM restart_game.

    WHEN 'LOAD'.
      PERFORM load_story.

    WHEN 'SAVE'.
      PERFORM save_game.

    WHEN 'RESTORE'.
      PERFORM restore_game.

  ENDCASE.

  CLEAR ok_code.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form SHOW_WELCOME
*&---------------------------------------------------------------------*
FORM show_welcome.
  DATA: lt_html TYPE TABLE OF char1024.

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
    |<p>Story file: <span class="cmd">{ gv_story_file }</span></p>| &&
    |<br>| &&
    |<p>Press <span class="cmd">Enter</span> or click <span class="cmd">Start</span> to begin.</p>| &&
    |<p>Use <span class="cmd">Load</span> to select a different story file.</p>| &&
    |</body>| &&
    |</html>|.

  " Split into table
  WHILE strlen( lv_html ) > 0.
    IF strlen( lv_html ) > 1024.
      APPEND lv_html+0(1024) TO lt_html.
      lv_html = lv_html+1024.
    ELSE.
      APPEND lv_html TO lt_html.
      CLEAR lv_html.
    ENDIF.
  ENDWHILE.

  " Get underlying HTML viewer from I/O
  DATA(lo_viewer) = CAST cl_gui_html_viewer( go_io->mo_html_viewer ).

  lo_viewer->load_data(
    IMPORTING
      assigned_url = DATA(lv_url)
    CHANGING
      data_table   = lt_html ).

  lo_viewer->show_url( url = lv_url ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form START_GAME
*&---------------------------------------------------------------------*
FORM start_game.
  " For now, show a demo since interpreter isn't complete yet
  gv_running = abap_true.

  " Show demo output
  DATA: lt_html TYPE TABLE OF char1024.

  DATA(lv_html) =
    |<html>| &&
    |<head>| &&
    |<style>| &&
    |body \{ | &&
    |  background-color: #1a1a2e; | &&
    |  color: #00ff00; | &&
    |  font-family: 'Courier New', monospace; | &&
    |  padding: 10px; | &&
    |\}| &&
    |.status \{ | &&
    |  background-color: #00ff00; | &&
    |  color: #1a1a2e; | &&
    |  padding: 5px 10px; | &&
    |  font-weight: bold; | &&
    |\}| &&
    |.title \{ color: #ffff00; font-size: 18px; \}| &&
    |.input \{ color: #ffff00; \}| &&
    |.prompt \{ color: #ff6600; \}| &&
    |</style>| &&
    |</head>| &&
    |<body>| &&
    |<div class="status">West of House&nbsp;&nbsp;&nbsp;&nbsp;Score: 0&nbsp;&nbsp;Moves: 0</div>| &&
    |<br>| &&
    |<p class="title">ZORK I: The Great Underground Empire</p>| &&
    |<p>Copyright (c) 1981, 1982, 1983 Infocom, Inc. All rights reserved.</p>| &&
    |<p>ZORK is a registered trademark of Infocom, Inc.</p>| &&
    |<p>Revision 88 / Serial number 840726</p>| &&
    |<br>| &&
    |<p>West of House</p>| &&
    |<p>You are standing in an open field west of a white house, with a boarded front door.</p>| &&
    |<p>There is a small mailbox here.</p>| &&
    |<br>| &&
    |<p class="prompt">&gt; _</p>| &&
    |<br><br>| &&
    |<p style="color:#666;">[Demo mode - interpreter not yet complete]</p>| &&
    |<p style="color:#666;">[Press Enter to input a command]</p>| &&
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

  DATA(lo_viewer) = CAST cl_gui_html_viewer( go_io->mo_html_viewer ).

  lo_viewer->load_data(
    IMPORTING
      assigned_url = DATA(lv_url)
    CHANGING
      data_table   = lt_html ).

  lo_viewer->show_url( url = lv_url ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_USER_COMMAND
*&---------------------------------------------------------------------*
FORM get_user_command.
  DATA: lv_command TYPE string,
        lv_title   TYPE string VALUE 'Enter Command',
        lv_text    TYPE string VALUE 'What do you want to do?'.

  " Use popup to get command
  CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
    EXPORTING
      textline1    = lv_text
      titel        = lv_title
      valuelength  = 80
    IMPORTING
      value1       = lv_command
    EXCEPTIONS
      OTHERS       = 1.

  IF sy-subrc = 0 AND lv_command IS NOT INITIAL.
    " Process command (demo mode - just echo it)
    PERFORM process_command USING lv_command.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_COMMAND
*&---------------------------------------------------------------------*
FORM process_command USING iv_command TYPE string.
  DATA: lt_html   TYPE TABLE OF char1024,
        lv_output TYPE string.

  " Demo: simple command processing
  DATA(lv_cmd_upper) = to_upper( iv_command ).

  CASE lv_cmd_upper.
    WHEN 'QUIT' OR 'Q'.
      PERFORM cleanup.
      LEAVE PROGRAM.

    WHEN 'LOOK' OR 'L'.
      lv_output = |West of House<br>| &&
                  |You are standing in an open field west of a white house, | &&
                  |with a boarded front door.<br>| &&
                  |There is a small mailbox here.|.

    WHEN 'OPEN MAILBOX'.
      lv_output = |Opening the small mailbox reveals a leaflet.|.

    WHEN 'READ LEAFLET'.
      lv_output = |"WELCOME TO ZORK!<br><br>| &&
                  |ZORK is a game of adventure, danger, and low cunning. | &&
                  |In it you will explore some of the most amazing territory | &&
                  |ever seen by mortals. No computer should be without one!"|.

    WHEN 'INVENTORY' OR 'I'.
      lv_output = |You are empty-handed.|.

    WHEN 'NORTH' OR 'N'.
      lv_output = |North of House<br>| &&
                  |You are facing the north side of a white house. | &&
                  |There is no door here, and all the windows are boarded up. | &&
                  |To the north a narrow path winds through the trees.|.

    WHEN 'HELP'.
      lv_output = |Some useful commands:<br>| &&
                  |  LOOK (L) - Look around<br>| &&
                  |  INVENTORY (I) - Check your belongings<br>| &&
                  |  NORTH/SOUTH/EAST/WEST (N/S/E/W) - Move<br>| &&
                  |  TAKE/GET [object] - Pick up something<br>| &&
                  |  OPEN [object] - Open something<br>| &&
                  |  QUIT (Q) - End the game|.

    WHEN OTHERS.
      lv_output = |I don't understand "{ iv_command }".|.
  ENDCASE.

  " Build updated HTML
  DATA(lv_html) =
    |<html>| &&
    |<head>| &&
    |<style>| &&
    |body \{ | &&
    |  background-color: #1a1a2e; | &&
    |  color: #00ff00; | &&
    |  font-family: 'Courier New', monospace; | &&
    |  padding: 10px; | &&
    |\}| &&
    |.status \{ | &&
    |  background-color: #00ff00; | &&
    |  color: #1a1a2e; | &&
    |  padding: 5px 10px; | &&
    |  font-weight: bold; | &&
    |\}| &&
    |.input \{ color: #ffff00; \}| &&
    |.prompt \{ color: #ff6600; \}| &&
    |</style>| &&
    |</head>| &&
    |<body>| &&
    |<div class="status">West of House&nbsp;&nbsp;&nbsp;&nbsp;Score: 0&nbsp;&nbsp;Moves: 1</div>| &&
    |<br>| &&
    |<p class="input">&gt; { iv_command }</p>| &&
    |<p>{ lv_output }</p>| &&
    |<br>| &&
    |<p class="prompt">&gt; _</p>| &&
    |<br><br>| &&
    |<p style="color:#666;">[Demo mode - press Enter for next command]</p>| &&
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

  DATA(lo_viewer) = CAST cl_gui_html_viewer( go_io->mo_html_viewer ).

  lo_viewer->load_data(
    IMPORTING
      assigned_url = DATA(lv_url)
    CHANGING
      data_table   = lt_html ).

  lo_viewer->show_url( url = lv_url ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form RESTART_GAME
*&---------------------------------------------------------------------*
FORM restart_game.
  gv_running = abap_false.
  PERFORM show_welcome.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_STORY
*&---------------------------------------------------------------------*
FORM load_story.
  DATA: lv_path TYPE string,
        lv_title TYPE string VALUE 'Load Story File'.

  " Get file path from user
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = lv_title
      default_extension = 'z3'
      file_filter       = 'Z-Machine Files (*.z3;*.z5)|*.z3;*.z5|All Files (*.*)|*.*'
    CHANGING
      file_table        = DATA(lt_files)
      rc                = DATA(lv_rc)
    EXCEPTIONS
      OTHERS            = 1.

  IF sy-subrc = 0 AND lv_rc > 0.
    READ TABLE lt_files INTO lv_path INDEX 1.
    IF sy-subrc = 0.
      gv_story_file = lv_path.
      gv_running = abap_false.
      PERFORM show_welcome.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_GAME
*&---------------------------------------------------------------------*
FORM save_game.
  MESSAGE 'Save not yet implemented' TYPE 'I'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form RESTORE_GAME
*&---------------------------------------------------------------------*
FORM restore_game.
  MESSAGE 'Restore not yet implemented' TYPE 'I'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CLEANUP
*&---------------------------------------------------------------------*
FORM cleanup.
  IF go_io IS BOUND.
    go_io->free( ).
    FREE go_io.
  ENDIF.

  IF go_container IS BOUND.
    go_container->free( ).
    FREE go_container.
  ENDIF.
ENDFORM.
