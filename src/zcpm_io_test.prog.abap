*&---------------------------------------------------------------------*
*& Report ZCPM_IO_TEST
*& Test CP/M I/O operations using HELLO_NAME.COM
*& This program: prints "Hello World", asks name, prints "Hello, <name>!"
*&---------------------------------------------------------------------*
REPORT zcpm_io_test.

START-OF-SELECTION.
  DATA: lo_cpm           TYPE REF TO zcl_cpm_emulator,
        lv_output        TYPE string,
        lv_max_instr     TYPE i VALUE 1000000,
        lv_hello_name    TYPE xstring.

  " HELLO_NAME.COM - Tests BDOS functions 2, 9, 10
  " Split across multiple lines due to ABAP line length limit
  " Fixed: DJNZ offset corrected (0xF3 instead of 0xF4)
  lv_hello_name =
    '1148010E09CD05001158010E09CD0500117B010E0ACD0500' &&
    '116C010E09CD0500116F010E09CD05003A7C01B7280E4721' &&
    '7D015E0E02E5C5CD0500C1E12310F31177010E09CD0500C9' &&
    '48656C6C6F2C20576F726C64210D0A24' &&  " Hello, World!\r\n$
    '5768617420697320796F7572206E616D653F2024' &&  " What is your name? $
    '0D0A24' &&  " \r\n$
    '48656C6C6F2C2024' &&  " Hello, $
    '210D0A24' &&  " !\r\n$
    '2000' &&  " Buffer: max=32, actual=0
    '00000000000000000000000000000000000000000000000000000000000000000000'.

  WRITE: / 'CP/M I/O Test - HELLO_NAME.COM'.
  WRITE: / '================================'.
  WRITE: / ''.
  WRITE: / 'This program tests:'.
  WRITE: / '  - BDOS 9:  Print string'.
  WRITE: / '  - BDOS 10: Read console buffer'.
  WRITE: / '  - BDOS 2:  Console output (char by char)'.
  WRITE: / ''.
  WRITE: / 'Loading HELLO_NAME.COM...'.
  WRITE: / |  Program size: { xstrlen( lv_hello_name ) } bytes|.

  " Create and initialize emulator
  lo_cpm = NEW zcl_cpm_emulator( ).
  lo_cpm->reset( ).
  lo_cpm->load_program( lv_hello_name ).

  " Provide test input: "Claude" followed by CR
  DATA lv_cr TYPE c LENGTH 1.
  lv_cr = cl_abap_char_utilities=>cr_lf+0(1).  " Just CR
  lo_cpm->provide_input( |Claude{ lv_cr }| ).

  WRITE: / ''.
  WRITE: / 'Running with input: "Claude"'.
  WRITE: / '----------------------------------------'.

  " Run emulator
  lv_output = lo_cpm->run( iv_max_cycles = lv_max_instr ).

  " Post-process output
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
    IN lv_output WITH cl_abap_char_utilities=>newline.
  REPLACE ALL OCCURRENCES OF lv_cr IN lv_output WITH ``.

  " Display output
  WRITE: / ''.
  WRITE: / '=== PROGRAM OUTPUT ==='.

  DATA lt_lines TYPE string_table.
  SPLIT lv_output AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
  LOOP AT lt_lines INTO DATA(lv_line).
    WRITE: / lv_line.
  ENDLOOP.

  WRITE: / ''.
  WRITE: / '=== EXPECTED OUTPUT ==='.
  WRITE: / 'Hello, World!'.
  WRITE: / 'What is your name? '.
  WRITE: / 'Hello, Claude!'.

  WRITE: / ''.
  IF lo_cpm->is_running( ) = abap_true.
    WRITE: / 'Status: Program waiting for more input'.
  ELSE.
    WRITE: / 'Status: Program ended normally'.
  ENDIF.

  " Verify output
  WRITE: / ''.
  WRITE: / '=== VERIFICATION ==='.
  IF lv_output CS 'Hello, World!'.
    WRITE: / 'PASS: "Hello, World!" found'.
  ELSE.
    WRITE: / 'FAIL: "Hello, World!" not found'.
  ENDIF.

  IF lv_output CS 'What is your name?'.
    WRITE: / 'PASS: "What is your name?" found'.
  ELSE.
    WRITE: / 'FAIL: "What is your name?" not found'.
  ENDIF.

  IF lv_output CS 'Hello, Claude!'.
    WRITE: / 'PASS: "Hello, Claude!" found - I/O working correctly!'.
  ELSE.
    WRITE: / 'FAIL: "Hello, Claude!" not found - I/O issue detected'.
  ENDIF.