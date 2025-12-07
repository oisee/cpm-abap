*&---------------------------------------------------------------------*
*& Z-Machine Stack Tests
*&---------------------------------------------------------------------*
CLASS ltcl_stack_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_memory TYPE REF TO zcl_zork_00_memory,
          mo_stack  TYPE REF TO zcl_zork_00_stack.

    METHODS:
      setup,

      " Evaluation stack tests
      test_push_pop FOR TESTING,
      test_peek FOR TESTING,
      test_stack_empty FOR TESTING,

      " Local variable tests
      test_local_vars FOR TESTING,

      " Global variable tests
      test_global_vars FOR TESTING,

      " Unified variable tests
      test_variable_access FOR TESTING,

      " Frame tests
      test_push_pop_frame FOR TESTING,
      test_frame_args FOR TESTING,

      " Helper
      create_test_memory
        RETURNING VALUE(ro_mem) TYPE REF TO zcl_zork_00_memory.

ENDCLASS.


CLASS ltcl_stack_test IMPLEMENTATION.

  METHOD setup.
    mo_memory = create_test_memory( ).
    CREATE OBJECT mo_stack
      EXPORTING
        io_memory = mo_memory.
  ENDMETHOD.


  METHOD create_test_memory.
    " Create memory with minimal header
    DATA: lv_story TYPE string.

    " Header (64 bytes)
    lv_story = ''.
    lv_story = lv_story && '03'.        " $00: Version 3
    lv_story = lv_story && '00'.        " $01: Flags
    lv_story = lv_story && '0001'.      " $02-03: Release
    lv_story = lv_story && '0100'.      " $04-05: High memory = $0100
    lv_story = lv_story && '0040'.      " $06-07: Init PC = $0040
    lv_story = lv_story && '0200'.      " $08-09: Dictionary
    lv_story = lv_story && '0100'.      " $0A-0B: Objects
    lv_story = lv_story && '0050'.      " $0C-0D: Globals = $0050 (80)
    lv_story = lv_story && '0080'.      " $0E-0F: Static memory = $0080
    lv_story = lv_story && '00'.        " $10: Flags2
    lv_story = lv_story && '00000000000000'.
    lv_story = lv_story && '0060'.      " $18-19: Abbreviations
    lv_story = lv_story && '0100'.      " $1A-1B: File length/2
    lv_story = lv_story && '0000'.      " $1C-1D: Checksum
    lv_story = lv_story && '0000'.      " $1E-1F: Interpreter

    " Pad header to 64 bytes
    lv_story = lv_story && '00000000000000000000000000000000'.
    lv_story = lv_story && '00000000000000000000000000000000'.

    " Add some space for globals at $50-$7F (48 bytes = 24 words)
    " Globals are at $50, so from offset $50 to $7F is 48 bytes
    lv_story = lv_story && '00000000000000000000000000000000'.  " $40-4F (padding)
    lv_story = lv_story && '00000000000000000000000000000000'.  " $50-5F (globals 16-23)
    lv_story = lv_story && '00000000000000000000000000000000'.  " $60-6F (globals 24-31)
    lv_story = lv_story && '00000000000000000000000000000000'.  " $70-7F (globals 32-39)

    " Pad to 256 bytes
    WHILE strlen( lv_story ) < 512.
      lv_story = lv_story && '00'.
    ENDWHILE.

    CREATE OBJECT ro_mem.
    ro_mem->load_story( lv_story ).
  ENDMETHOD.


  METHOD test_push_pop.
    " Test basic push/pop
    mo_stack->push_stack( 100 ).
    mo_stack->push_stack( 200 ).
    mo_stack->push_stack( 300 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_stack_depth( )
      exp = 3
      msg = 'Stack depth should be 3' ).

    DATA(lv_val) = mo_stack->pop_stack( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_val
      exp = 300
      msg = 'Should pop 300 first (LIFO)' ).

    lv_val = mo_stack->pop_stack( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_val
      exp = 200
      msg = 'Should pop 200 second' ).

    lv_val = mo_stack->pop_stack( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_val
      exp = 100
      msg = 'Should pop 100 third' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_stack_depth( )
      exp = 0
      msg = 'Stack should be empty' ).
  ENDMETHOD.


  METHOD test_peek.
    " Test peek (non-destructive read)
    mo_stack->push_stack( 42 ).

    DATA(lv_val) = mo_stack->peek_stack( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_val
      exp = 42
      msg = 'Peek should return 42' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_stack_depth( )
      exp = 1
      msg = 'Stack depth should still be 1 after peek' ).

    lv_val = mo_stack->pop_stack( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_val
      exp = 42
      msg = 'Pop should also return 42' ).
  ENDMETHOD.


  METHOD test_stack_empty.
    " Test empty stack behavior
    DATA(lv_val) = mo_stack->pop_stack( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_val
      exp = 0
      msg = 'Pop from empty stack should return 0' ).

    lv_val = mo_stack->peek_stack( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_val
      exp = 0
      msg = 'Peek empty stack should return 0' ).
  ENDMETHOD.


  METHOD test_local_vars.
    " Test local variable access
    " First create a frame with locals
    mo_stack->push_frame(
      iv_return_pc = 100
      iv_result_var = 0
      iv_num_locals = 5
      iv_args = ''
      iv_arg_count = 0 ).

    " Set some locals
    mo_stack->set_local( iv_num = 1 iv_val = 1000 ).
    mo_stack->set_local( iv_num = 3 iv_val = 3000 ).
    mo_stack->set_local( iv_num = 5 iv_val = 5000 ).

    " Read them back
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 1 )
      exp = 1000
      msg = 'Local 1 should be 1000' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 2 )
      exp = 0
      msg = 'Local 2 should be 0 (unset)' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 3 )
      exp = 3000
      msg = 'Local 3 should be 3000' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 5 )
      exp = 5000
      msg = 'Local 5 should be 5000' ).
  ENDMETHOD.


  METHOD test_global_vars.
    " Test global variable access (stored in memory)
    " Globals start at var 16, stored at address $50

    mo_stack->set_global( iv_num = 16 iv_val = 1234 ).
    mo_stack->set_global( iv_num = 17 iv_val = 5678 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_global( 16 )
      exp = 1234
      msg = 'Global 16 should be 1234' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_global( 17 )
      exp = 5678
      msg = 'Global 17 should be 5678' ).

    " Verify it's actually in memory
    DATA(lv_mem_val) = mo_memory->read_word( 80 ).  " $50 = globals base
    cl_abap_unit_assert=>assert_equals(
      act = lv_mem_val
      exp = 1234
      msg = 'Memory at globals base should have 1234' ).
  ENDMETHOD.


  METHOD test_variable_access.
    " Test unified variable access
    " Create frame with locals
    mo_stack->push_frame(
      iv_return_pc = 0
      iv_result_var = 0
      iv_num_locals = 3
      iv_args = ''
      iv_arg_count = 0 ).

    " var 0 = stack
    mo_stack->set_variable( iv_var = 0 iv_val = 999 ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_stack_depth( )
      exp = 1
      msg = 'Setting var 0 should push to stack' ).

    DATA(lv_val) = mo_stack->get_variable( 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_val
      exp = 999
      msg = 'Getting var 0 should pop from stack' ).

    " var 1-15 = locals
    mo_stack->set_variable( iv_var = 2 iv_val = 222 ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_variable( 2 )
      exp = 222
      msg = 'Var 2 should be local with value 222' ).

    " var 16+ = globals
    mo_stack->set_variable( iv_var = 20 iv_val = 2020 ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_variable( 20 )
      exp = 2020
      msg = 'Var 20 should be global with value 2020' ).
  ENDMETHOD.


  METHOD test_push_pop_frame.
    " Test frame push/pop
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_frame_depth( )
      exp = 0
      msg = 'Initial frame depth should be 0' ).

    " Push first frame
    mo_stack->push_frame(
      iv_return_pc = 100
      iv_result_var = 5
      iv_num_locals = 3
      iv_args = ''
      iv_arg_count = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_frame_depth( )
      exp = 1
      msg = 'Frame depth should be 1' ).

    " Set a local in this frame
    mo_stack->set_local( iv_num = 1 iv_val = 111 ).

    " Push second frame
    mo_stack->push_frame(
      iv_return_pc = 200
      iv_result_var = 10
      iv_num_locals = 2
      iv_args = ''
      iv_arg_count = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_frame_depth( )
      exp = 2
      msg = 'Frame depth should be 2' ).

    " Set local in new frame
    mo_stack->set_local( iv_num = 1 iv_val = 222 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 1 )
      exp = 222
      msg = 'Local 1 in frame 2 should be 222' ).

    " Pop frame 2
    DATA: lv_return_pc TYPE i,
          lv_result_var TYPE i.

    mo_stack->pop_frame(
      IMPORTING
        ev_return_pc = lv_return_pc
        ev_result_var = lv_result_var ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_return_pc
      exp = 200
      msg = 'Return PC should be 200' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result_var
      exp = 10
      msg = 'Result var should be 10' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_frame_depth( )
      exp = 1
      msg = 'Frame depth should be 1 after pop' ).
  ENDMETHOD.


  METHOD test_frame_args.
    " Test that arguments are passed to locals
    " Args: 1000, 2000, 3000 (as hex: 03E8, 07D0, 0BB8)
    DATA(lv_args) = '03E807D00BB8'.

    mo_stack->push_frame(
      iv_return_pc = 0
      iv_result_var = 0
      iv_num_locals = 5
      iv_args = lv_args
      iv_arg_count = 3 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 1 )
      exp = 1000
      msg = 'Local 1 should have arg 1 = 1000' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 2 )
      exp = 2000
      msg = 'Local 2 should have arg 2 = 2000' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 3 )
      exp = 3000
      msg = 'Local 3 should have arg 3 = 3000' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 4 )
      exp = 0
      msg = 'Local 4 should be 0 (no arg)' ).
  ENDMETHOD.

ENDCLASS.
