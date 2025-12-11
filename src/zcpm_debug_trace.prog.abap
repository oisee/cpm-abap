*&---------------------------------------------------------------------*
*& Report ZCPM_DEBUG_TRACE
*& Debug trace for CP/M emulator
*&---------------------------------------------------------------------*
REPORT zcpm_debug_trace.

DATA: lo_cpm    TYPE REF TO zcl_cpm_emulator,
      lv_prog   TYPE xstring,
      lv_output TYPE string,
      lt_trace  TYPE zcl_cpm_emulator=>tt_trace.

" HELLO.COM: LD C,9 / LD DE,MSG / CALL 5 / RET / MSG
lv_prog = '0E09110801CD0500C948656C6C6F2C2043502F4D20576F726C64210D0A24'.

lo_cpm = NEW zcl_cpm_emulator( ).
lo_cpm->reset( ).
lo_cpm->load_program( lv_prog ).
lo_cpm->run(
  EXPORTING
    iv_max_cycles = 70000
    iv_trace = abap_true
  IMPORTING
    et_trace = lt_trace
  RECEIVING
    rv_output = lv_output ).

cl_demo_output=>write( |Output: { lv_output }| ).
cl_demo_output=>write( lt_trace ).
cl_demo_output=>display( ).