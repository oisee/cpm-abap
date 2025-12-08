*&---------------------------------------------------------------------*
*& Z-Machine I/O Interface
*& Abstract interface for all Z-machine input/output operations
*&---------------------------------------------------------------------*
INTERFACE zif_zork_00_io
  PUBLIC.

  "======================================================================
  " Output Methods
  "======================================================================
  METHODS:
    " Print a single ZSCII character
    print_char
      IMPORTING iv_zscii TYPE i,

    " Print a text string (already decoded)
    print_text
      IMPORTING iv_text TYPE string,

    " Print a newline
    new_line,

    " Print a number
    print_num
      IMPORTING iv_num TYPE i,

    " Buffer mode (0=unbuffered, 1=buffered)
    buffer_mode
      IMPORTING iv_mode TYPE i.

  "======================================================================
  " Input Methods
  "======================================================================
  METHODS:
    " Read a line of text input
    " Returns the input text, stores in buffer at iv_text_addr
    read_line
      IMPORTING iv_max_len     TYPE i
      RETURNING VALUE(rv_text) TYPE string,

    " Read a single character
    read_char
      RETURNING VALUE(rv_zscii) TYPE i.

  "======================================================================
  " Status Line (V3)
  "======================================================================
  METHODS:
    " Show status line
    show_status
      IMPORTING iv_location TYPE string
                iv_score    TYPE i
                iv_moves    TYPE i
                iv_hours    TYPE i DEFAULT 0
                iv_minutes  TYPE i DEFAULT 0
                iv_is_time  TYPE abap_bool DEFAULT abap_false.

  "======================================================================
  " Screen Management
  "======================================================================
  METHODS:
    " Split screen into upper/lower windows
    split_window
      IMPORTING iv_lines TYPE i,

    " Set active window (0=lower, 1=upper)
    set_window
      IMPORTING iv_window TYPE i,

    " Erase window (-1=whole screen, -2=clear+unsplit)
    erase_window
      IMPORTING iv_window TYPE i,

    " Set cursor position (upper window only)
    set_cursor
      IMPORTING iv_line   TYPE i
                iv_column TYPE i,

    " Set text style (0=roman, 1=reverse, 2=bold, 4=italic, 8=fixed)
    set_style
      IMPORTING iv_style TYPE i.

  "======================================================================
  " Sound (V3+)
  "======================================================================
  METHODS:
    " Sound effect
    sound_effect
      IMPORTING iv_number  TYPE i
                iv_effect  TYPE i
                iv_volume  TYPE i DEFAULT 8
                iv_routine TYPE i DEFAULT 0.

ENDINTERFACE.
