*&---------------------------------------------------------------------*
*& Z-Machine Executor - Instruction Execution Engine
*& Executes decoded Z-machine instructions (v3)
*& Phase 5 of Z-machine interpreter implementation
*&---------------------------------------------------------------------*
CLASS zcl_zork_00_executor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "======================================================================
    " Status Constants
    "======================================================================
    CONSTANTS:
      c_status_running TYPE i VALUE 0,
      c_status_halted  TYPE i VALUE 1,
      c_status_waiting TYPE i VALUE 2,  " Waiting for input
      c_status_quit    TYPE i VALUE 3.

    METHODS:
      constructor
        IMPORTING io_memory  TYPE REF TO zcl_zork_00_memory
                  io_stack   TYPE REF TO zcl_zork_00_stack
                  io_decoder TYPE REF TO zcl_zork_00_decoder
                  io_objects TYPE REF TO zcl_zork_00_objects OPTIONAL
                  io_io      TYPE REF TO zif_zork_00_io OPTIONAL,

      " Main execution methods
      execute_instruction
        IMPORTING is_instr         TYPE zif_zork_00_types=>ts_instruction
        RETURNING VALUE(rv_next_pc) TYPE i,

      execute_step
        RETURNING VALUE(rv_next_pc) TYPE i,

      execute_until_halt
        IMPORTING iv_max_steps    TYPE i DEFAULT 100000
        RETURNING VALUE(rv_steps) TYPE i,

      " State access
      get_pc
        RETURNING VALUE(rv_pc) TYPE i,

      set_pc
        IMPORTING iv_pc TYPE i,

      get_status
        RETURNING VALUE(rv_status) TYPE i,

      reset
        IMPORTING iv_pc TYPE i OPTIONAL.

  PRIVATE SECTION.

    DATA:
      mo_memory  TYPE REF TO zcl_zork_00_memory,
      mo_stack   TYPE REF TO zcl_zork_00_stack,
      mo_decoder TYPE REF TO zcl_zork_00_decoder,
      mo_objects TYPE REF TO zcl_zork_00_objects,
      mo_io      TYPE REF TO zif_zork_00_io,
      mv_pc      TYPE i,
      mv_status  TYPE i.

    METHODS:
      " Get operand value (resolve variables)
      get_operand_value
        IMPORTING is_instr      TYPE zif_zork_00_types=>ts_instruction
                  iv_index      TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      " Store result in variable
      store_result
        IMPORTING is_instr TYPE zif_zork_00_types=>ts_instruction
                  iv_val   TYPE i,

      " Handle branch
      handle_branch
        IMPORTING is_instr         TYPE zif_zork_00_types=>ts_instruction
                  iv_condition     TYPE abap_bool
        RETURNING VALUE(rv_next_pc) TYPE i,

      " Convert signed 16-bit
      to_signed
        IMPORTING iv_val           TYPE i
        RETURNING VALUE(rv_signed) TYPE i,

      to_unsigned
        IMPORTING iv_val             TYPE i
        RETURNING VALUE(rv_unsigned) TYPE i,

      " Opcode execution methods by category
      execute_0op
        IMPORTING is_instr         TYPE zif_zork_00_types=>ts_instruction
        RETURNING VALUE(rv_next_pc) TYPE i,

      execute_1op
        IMPORTING is_instr         TYPE zif_zork_00_types=>ts_instruction
        RETURNING VALUE(rv_next_pc) TYPE i,

      execute_2op
        IMPORTING is_instr         TYPE zif_zork_00_types=>ts_instruction
        RETURNING VALUE(rv_next_pc) TYPE i,

      execute_var
        IMPORTING is_instr         TYPE zif_zork_00_types=>ts_instruction
        RETURNING VALUE(rv_next_pc) TYPE i,

      " Z-string decoding (for print instructions)
      decode_zstring
        IMPORTING iv_addr         TYPE i
        RETURNING VALUE(rv_text)  TYPE string,

      " Hex conversion helpers
      word_to_hex
        IMPORTING iv_word        TYPE i
        RETURNING VALUE(rv_hex)  TYPE string,

      byte_to_hex
        IMPORTING iv_byte        TYPE i
        RETURNING VALUE(rv_hex)  TYPE string.

ENDCLASS.


CLASS zcl_zork_00_executor IMPLEMENTATION.

  METHOD constructor.
    mo_memory = io_memory.
    mo_stack = io_stack.
    mo_decoder = io_decoder.
    mo_io = io_io.

    " Create or use provided objects handler
    IF io_objects IS BOUND.
      mo_objects = io_objects.
    ELSE.
      mo_objects = NEW zcl_zork_00_objects( mo_memory ).
    ENDIF.

    " Initialize PC from header
    mv_pc = mo_memory->get_init_pc( ).
    mv_status = c_status_running.
  ENDMETHOD.


  METHOD reset.
    IF iv_pc IS SUPPLIED.
      mv_pc = iv_pc.
    ELSE.
      mv_pc = mo_memory->get_init_pc( ).
    ENDIF.
    mv_status = c_status_running.
    mo_stack->reset( ).
  ENDMETHOD.


  METHOD get_pc.
    rv_pc = mv_pc.
  ENDMETHOD.


  METHOD set_pc.
    mv_pc = iv_pc.
  ENDMETHOD.


  METHOD get_status.
    rv_status = mv_status.
  ENDMETHOD.


  METHOD execute_step.
    " Decode instruction at current PC
    DATA(ls_instr) = mo_decoder->decode( mv_pc ).

    " Execute and get next PC
    rv_next_pc = execute_instruction( ls_instr ).

    " Update PC
    mv_pc = rv_next_pc.
  ENDMETHOD.


  METHOD execute_until_halt.
    DATA: lv_steps TYPE i.

    lv_steps = 0.
    mv_status = c_status_running.

    WHILE mv_status = c_status_running AND lv_steps < iv_max_steps.
      execute_step( ).
      lv_steps = lv_steps + 1.
    ENDWHILE.

    rv_steps = lv_steps.
  ENDMETHOD.


  METHOD execute_instruction.
    " Dispatch based on operand count
    CASE is_instr-op_count.
      WHEN zif_zork_00_types=>c_opcount_0op.
        rv_next_pc = execute_0op( is_instr ).
      WHEN zif_zork_00_types=>c_opcount_1op.
        rv_next_pc = execute_1op( is_instr ).
      WHEN zif_zork_00_types=>c_opcount_2op.
        rv_next_pc = execute_2op( is_instr ).
      WHEN zif_zork_00_types=>c_opcount_var.
        rv_next_pc = execute_var( is_instr ).
      WHEN OTHERS.
        " Unknown - advance to next instruction
        rv_next_pc = is_instr-next_pc.
    ENDCASE.
  ENDMETHOD.


  METHOD get_operand_value.
    " Get value of operand, resolving variables if needed
    DATA(lv_count) = lines( is_instr-operands ).

    IF iv_index < 0 OR iv_index >= lv_count.
      rv_val = 0.
      RETURN.
    ENDIF.

    " Read operand from table (1-indexed)
    READ TABLE is_instr-operands INDEX iv_index + 1 INTO DATA(ls_op).
    IF sy-subrc <> 0.
      rv_val = 0.
      RETURN.
    ENDIF.

    " Resolve based on type
    IF ls_op-type = zif_zork_00_types=>c_optype_var.
      " Variable reference - get from stack
      rv_val = mo_stack->get_variable( ls_op-value ).
    ELSE.
      " Constant - use directly
      rv_val = ls_op-value.
    ENDIF.
  ENDMETHOD.


  METHOD store_result.
    " Store result in variable specified by instruction
    IF is_instr-has_store = abap_true.
      mo_stack->set_variable( iv_var = is_instr-store_var
                              iv_val = iv_val ).
    ENDIF.
  ENDMETHOD.


  METHOD handle_branch.
    " Handle conditional branch
    DATA: lv_take_branch TYPE abap_bool.

    " Determine if we should branch
    IF is_instr-branch_on = abap_true.
      lv_take_branch = iv_condition.
    ELSE.
      " Branch on false
      IF iv_condition = abap_false.
        lv_take_branch = abap_true.
      ELSE.
        lv_take_branch = abap_false.
      ENDIF.
    ENDIF.

    IF lv_take_branch = abap_false.
      " Don't branch - continue to next instruction
      rv_next_pc = is_instr-next_pc.
      RETURN.
    ENDIF.

    " Take the branch
    CASE is_instr-branch_off.
      WHEN 0.
        " Return false from current routine
        DATA: lv_return_pc TYPE i,
              lv_result_var TYPE i.
        mo_stack->pop_frame( IMPORTING ev_return_pc = lv_return_pc
                                        ev_result_var = lv_result_var ).
        IF lv_result_var >= 0.
          mo_stack->set_variable( iv_var = lv_result_var iv_val = 0 ).
        ENDIF.
        rv_next_pc = lv_return_pc.

      WHEN 1.
        " Return true from current routine
        mo_stack->pop_frame( IMPORTING ev_return_pc = lv_return_pc
                                        ev_result_var = lv_result_var ).
        IF lv_result_var >= 0.
          mo_stack->set_variable( iv_var = lv_result_var iv_val = 1 ).
        ENDIF.
        rv_next_pc = lv_return_pc.

      WHEN OTHERS.
        " Jump to offset from end of instruction
        rv_next_pc = is_instr-next_pc + is_instr-branch_off - 2.
    ENDCASE.
  ENDMETHOD.


  METHOD to_signed.
    " Convert unsigned 16-bit to signed
    IF iv_val >= 32768.
      rv_signed = iv_val - 65536.
    ELSE.
      rv_signed = iv_val.
    ENDIF.
  ENDMETHOD.


  METHOD to_unsigned.
    " Convert signed to unsigned 16-bit
    IF iv_val < 0.
      rv_unsigned = iv_val + 65536.
    ELSE.
      rv_unsigned = iv_val MOD 65536.
    ENDIF.
  ENDMETHOD.


  METHOD execute_0op.
    " 0OP instructions (no operands)
    rv_next_pc = is_instr-next_pc.

    CASE is_instr-opcode.
      WHEN 0.  " rtrue - return true (1)
        DATA: lv_return_pc TYPE i,
              lv_result_var TYPE i.
        mo_stack->pop_frame( IMPORTING ev_return_pc = lv_return_pc
                                        ev_result_var = lv_result_var ).
        IF lv_result_var >= 0.
          mo_stack->set_variable( iv_var = lv_result_var iv_val = 1 ).
        ENDIF.
        rv_next_pc = lv_return_pc.

      WHEN 1.  " rfalse - return false (0)
        mo_stack->pop_frame( IMPORTING ev_return_pc = lv_return_pc
                                        ev_result_var = lv_result_var ).
        IF lv_result_var >= 0.
          mo_stack->set_variable( iv_var = lv_result_var iv_val = 0 ).
        ENDIF.
        rv_next_pc = lv_return_pc.

      WHEN 2.  " print - print embedded string
        IF mo_io IS BOUND.
          DATA(lv_text) = decode_zstring( is_instr-text_addr ).
          mo_io->print_text( lv_text ).
        ENDIF.

      WHEN 3.  " print_ret - print embedded string, newline, return true
        IF mo_io IS BOUND.
          lv_text = decode_zstring( is_instr-text_addr ).
          mo_io->print_text( lv_text ).
          mo_io->new_line( ).
        ENDIF.
        mo_stack->pop_frame( IMPORTING ev_return_pc = lv_return_pc
                                        ev_result_var = lv_result_var ).
        IF lv_result_var >= 0.
          mo_stack->set_variable( iv_var = lv_result_var iv_val = 1 ).
        ENDIF.
        rv_next_pc = lv_return_pc.

      WHEN 4.  " nop - no operation
        " Nothing to do

      WHEN 5.  " save (v3: branch on success)
        " For now, always fail (branch on false)
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = abap_false ).

      WHEN 6.  " restore (v3: branch on success)
        " For now, always fail (branch on false)
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = abap_false ).

      WHEN 7.  " restart
        reset( ).
        rv_next_pc = mv_pc.

      WHEN 8.  " ret_popped - return with popped stack value
        DATA(lv_retval) = mo_stack->pop_stack( ).
        mo_stack->pop_frame( IMPORTING ev_return_pc = lv_return_pc
                                        ev_result_var = lv_result_var ).
        IF lv_result_var >= 0.
          mo_stack->set_variable( iv_var = lv_result_var iv_val = lv_retval ).
        ENDIF.
        rv_next_pc = lv_return_pc.

      WHEN 9.  " pop (v1-4) / catch (v5+) - discard top of stack
        mo_stack->pop_stack( ).

      WHEN 10.  " quit
        mv_status = c_status_quit.

      WHEN 11.  " new_line
        IF mo_io IS BOUND.
          mo_io->new_line( ).
        ENDIF.

      WHEN 12.  " show_status (v3)
        IF mo_io IS BOUND.
          " TODO: Get location/score from object table and globals
          mo_io->show_status( iv_location = 'Unknown'
                              iv_score = 0
                              iv_moves = 0 ).
        ENDIF.

      WHEN 13.  " verify - verify game file checksum (branch on success)
        " For now, always succeed
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = abap_true ).

      WHEN OTHERS.
        " Unknown 0OP - just continue
    ENDCASE.
  ENDMETHOD.


  METHOD execute_1op.
    " 1OP instructions (one operand)
    DATA: lv_op1      TYPE i,
          lv_result   TYPE i,
          lv_cond     TYPE abap_bool.

    rv_next_pc = is_instr-next_pc.
    lv_op1 = get_operand_value( is_instr = is_instr iv_index = 0 ).

    CASE is_instr-opcode.
      WHEN 0.  " jz - jump if zero
        IF lv_op1 = 0.
          lv_cond = abap_true.
        ELSE.
          lv_cond = abap_false.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 1.  " get_sibling - get sibling object (store + branch)
        lv_result = mo_objects->get_sibling( lv_op1 ).
        store_result( is_instr = is_instr iv_val = lv_result ).
        IF lv_result <> 0.
          lv_cond = abap_true.
        ELSE.
          lv_cond = abap_false.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 2.  " get_child - get first child object (store + branch)
        lv_result = mo_objects->get_child( lv_op1 ).
        store_result( is_instr = is_instr iv_val = lv_result ).
        IF lv_result <> 0.
          lv_cond = abap_true.
        ELSE.
          lv_cond = abap_false.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 3.  " get_parent - get parent object (store)
        lv_result = mo_objects->get_parent( lv_op1 ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 4.  " get_prop_len - get property data length
        lv_result = mo_objects->get_prop_len( lv_op1 ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 5.  " inc - increment variable
        " lv_op1 is the variable number
        DATA(lv_val) = mo_stack->get_variable( lv_op1 ).
        lv_val = to_signed( lv_val ) + 1.
        mo_stack->set_variable( iv_var = lv_op1 iv_val = to_unsigned( lv_val ) ).

      WHEN 6.  " dec - decrement variable
        lv_val = mo_stack->get_variable( lv_op1 ).
        lv_val = to_signed( lv_val ) - 1.
        mo_stack->set_variable( iv_var = lv_op1 iv_val = to_unsigned( lv_val ) ).

      WHEN 7.  " print_addr - print string at byte address
        IF mo_io IS BOUND.
          lv_text = decode_zstring( lv_op1 ).
          mo_io->print_text( lv_text ).
        ENDIF.

      WHEN 8.  " call_1s (v4+) - not in v3
        " Skip for v3

      WHEN 9.  " remove_obj - remove object from tree
        mo_objects->remove_obj( lv_op1 ).

      WHEN 10.  " print_obj - print short name of object
        IF mo_io IS BOUND.
          " Get object short name from property table
          DATA(lv_prop_addr) = mo_objects->get_prop_table_addr( lv_op1 ).
          IF lv_prop_addr > 0.
            DATA(lv_text_len) = mo_memory->read_byte( lv_prop_addr ).
            IF lv_text_len > 0.
              lv_text = decode_zstring( lv_prop_addr + 1 ).
              mo_io->print_text( lv_text ).
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN 11.  " ret - return with value
        DATA: lv_return_pc  TYPE i,
              lv_result_var TYPE i.
        mo_stack->pop_frame( IMPORTING ev_return_pc = lv_return_pc
                                        ev_result_var = lv_result_var ).
        IF lv_result_var >= 0.
          mo_stack->set_variable( iv_var = lv_result_var iv_val = lv_op1 ).
        ENDIF.
        rv_next_pc = lv_return_pc.

      WHEN 12.  " jump - unconditional jump (signed offset)
        DATA(lv_offset) = to_signed( lv_op1 ).
        rv_next_pc = is_instr-next_pc + lv_offset - 2.

      WHEN 13.  " print_paddr - print string at packed address
        IF mo_io IS BOUND.
          DATA(lv_addr) = mo_memory->unpack_string_addr( lv_op1 ).
          lv_text = decode_zstring( lv_addr ).
          mo_io->print_text( lv_text ).
        ENDIF.

      WHEN 14.  " load - load variable value (indirect)
        " lv_op1 is variable number to load FROM
        IF lv_op1 = 0.
          " Special: load from stack without popping
          lv_result = mo_stack->peek_stack( ).
        ELSE.
          lv_result = mo_stack->get_variable( lv_op1 ).
        ENDIF.
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 15.  " not (v1-4) - bitwise NOT
        lv_result = 65535 - lv_op1.  " ~lv_op1 for 16-bit
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN OTHERS.
        " Unknown 1OP
    ENDCASE.
  ENDMETHOD.


  METHOD execute_2op.
    " 2OP instructions (two operands)
    DATA: lv_op1      TYPE i,
          lv_op2      TYPE i,
          lv_result   TYPE i,
          lv_cond     TYPE abap_bool,
          lv_signed1  TYPE i,
          lv_signed2  TYPE i.

    rv_next_pc = is_instr-next_pc.
    lv_op1 = get_operand_value( is_instr = is_instr iv_index = 0 ).
    lv_op2 = get_operand_value( is_instr = is_instr iv_index = 1 ).

    CASE is_instr-opcode.
      WHEN 1.  " je - jump if equal (can have 2-4 operands)
        " Check if op1 equals any of the other operands
        lv_cond = abap_false.
        IF lv_op1 = lv_op2.
          lv_cond = abap_true.
        ELSE.
          " Check additional operands if present
          DATA(lv_count) = lines( is_instr-operands ).
          IF lv_count >= 3.
            DATA(lv_op3) = get_operand_value( is_instr = is_instr iv_index = 2 ).
            IF lv_op1 = lv_op3.
              lv_cond = abap_true.
            ENDIF.
          ENDIF.
          IF lv_count >= 4 AND lv_cond = abap_false.
            DATA(lv_op4) = get_operand_value( is_instr = is_instr iv_index = 3 ).
            IF lv_op1 = lv_op4.
              lv_cond = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 2.  " jl - jump if less than (signed)
        lv_signed1 = to_signed( lv_op1 ).
        lv_signed2 = to_signed( lv_op2 ).
        IF lv_signed1 < lv_signed2.
          lv_cond = abap_true.
        ELSE.
          lv_cond = abap_false.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 3.  " jg - jump if greater than (signed)
        lv_signed1 = to_signed( lv_op1 ).
        lv_signed2 = to_signed( lv_op2 ).
        IF lv_signed1 > lv_signed2.
          lv_cond = abap_true.
        ELSE.
          lv_cond = abap_false.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 4.  " dec_chk - decrement and branch if less than
        " op1 = variable, op2 = value to compare
        DATA(lv_val) = mo_stack->get_variable( lv_op1 ).
        lv_val = to_signed( lv_val ) - 1.
        mo_stack->set_variable( iv_var = lv_op1 iv_val = to_unsigned( lv_val ) ).
        IF lv_val < to_signed( lv_op2 ).
          lv_cond = abap_true.
        ELSE.
          lv_cond = abap_false.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 5.  " inc_chk - increment and branch if greater than
        lv_val = mo_stack->get_variable( lv_op1 ).
        lv_val = to_signed( lv_val ) + 1.
        mo_stack->set_variable( iv_var = lv_op1 iv_val = to_unsigned( lv_val ) ).
        IF lv_val > to_signed( lv_op2 ).
          lv_cond = abap_true.
        ELSE.
          lv_cond = abap_false.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 6.  " jin - jump if object in object (parent check)
        " Branch if op1's parent is op2
        IF mo_objects->get_parent( lv_op1 ) = lv_op2.
          lv_cond = abap_true.
        ELSE.
          lv_cond = abap_false.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 7.  " test - test bitmap
        " Branch if (op1 AND op2) = op2
        DATA(lv_and) = 0.
        " Simulate bitwise AND using arithmetic
        DATA: lv_a TYPE i, lv_b TYPE i, lv_bit TYPE i, lv_pow TYPE i.
        lv_a = lv_op1.
        lv_b = lv_op2.
        lv_pow = 1.
        DO 16 TIMES.
          lv_bit = lv_a MOD 2.
          IF lv_bit = 1 AND lv_b MOD 2 = 1.
            lv_and = lv_and + lv_pow.
          ENDIF.
          lv_a = lv_a DIV 2.
          lv_b = lv_b DIV 2.
          lv_pow = lv_pow * 2.
        ENDDO.
        IF lv_and = lv_op2.
          lv_cond = abap_true.
        ELSE.
          lv_cond = abap_false.
        ENDIF.
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 8.  " or - bitwise OR
        " Simulate bitwise OR
        lv_a = lv_op1.
        lv_b = lv_op2.
        lv_result = 0.
        lv_pow = 1.
        DO 16 TIMES.
          IF lv_a MOD 2 = 1 OR lv_b MOD 2 = 1.
            lv_result = lv_result + lv_pow.
          ENDIF.
          lv_a = lv_a DIV 2.
          lv_b = lv_b DIV 2.
          lv_pow = lv_pow * 2.
        ENDDO.
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 9.  " and - bitwise AND
        lv_a = lv_op1.
        lv_b = lv_op2.
        lv_result = 0.
        lv_pow = 1.
        DO 16 TIMES.
          IF lv_a MOD 2 = 1 AND lv_b MOD 2 = 1.
            lv_result = lv_result + lv_pow.
          ENDIF.
          lv_a = lv_a DIV 2.
          lv_b = lv_b DIV 2.
          lv_pow = lv_pow * 2.
        ENDDO.
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 10.  " test_attr - test object attribute
        lv_cond = mo_objects->test_attr( iv_object = lv_op1 iv_attr = lv_op2 ).
        rv_next_pc = handle_branch( is_instr = is_instr iv_condition = lv_cond ).

      WHEN 11.  " set_attr - set object attribute
        mo_objects->set_attr( iv_object = lv_op1 iv_attr = lv_op2 ).

      WHEN 12.  " clear_attr - clear object attribute
        mo_objects->clear_attr( iv_object = lv_op1 iv_attr = lv_op2 ).

      WHEN 13.  " store - store variable (indirect)
        " op1 = variable number, op2 = value
        IF lv_op1 = 0.
          " Special: replace top of stack (pop then push)
          mo_stack->pop_stack( ).
          mo_stack->push_stack( lv_op2 ).
        ELSE.
          mo_stack->set_variable( iv_var = lv_op1 iv_val = lv_op2 ).
        ENDIF.

      WHEN 14.  " insert_obj - insert object in object
        mo_objects->insert_obj( iv_object = lv_op1 iv_destination = lv_op2 ).

      WHEN 15.  " loadw - load word from array
        " op1 = array base, op2 = word index
        DATA(lv_addr) = lv_op1 + lv_op2 * 2.
        lv_result = mo_memory->read_word( lv_addr ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 16.  " loadb - load byte from array
        lv_addr = lv_op1 + lv_op2.
        lv_result = mo_memory->read_byte( lv_addr ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 17.  " get_prop - get object property
        lv_result = mo_objects->get_prop( iv_object = lv_op1 iv_property = lv_op2 ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 18.  " get_prop_addr - get property address
        lv_result = mo_objects->get_prop_addr( iv_object = lv_op1 iv_property = lv_op2 ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 19.  " get_next_prop - get next property
        lv_result = mo_objects->get_next_prop( iv_object = lv_op1 iv_property = lv_op2 ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 20.  " add - signed addition
        lv_signed1 = to_signed( lv_op1 ).
        lv_signed2 = to_signed( lv_op2 ).
        lv_result = to_unsigned( lv_signed1 + lv_signed2 ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 21.  " sub - signed subtraction
        lv_signed1 = to_signed( lv_op1 ).
        lv_signed2 = to_signed( lv_op2 ).
        lv_result = to_unsigned( lv_signed1 - lv_signed2 ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 22.  " mul - signed multiplication
        lv_signed1 = to_signed( lv_op1 ).
        lv_signed2 = to_signed( lv_op2 ).
        lv_result = to_unsigned( lv_signed1 * lv_signed2 ).
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 23.  " div - signed division
        lv_signed1 = to_signed( lv_op1 ).
        lv_signed2 = to_signed( lv_op2 ).
        IF lv_signed2 = 0.
          " Division by zero - undefined, return 0
          lv_result = 0.
        ELSE.
          lv_result = to_unsigned( lv_signed1 DIV lv_signed2 ).
        ENDIF.
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 24.  " mod - signed modulo
        lv_signed1 = to_signed( lv_op1 ).
        lv_signed2 = to_signed( lv_op2 ).
        IF lv_signed2 = 0.
          lv_result = 0.
        ELSE.
          lv_result = to_unsigned( lv_signed1 MOD lv_signed2 ).
        ENDIF.
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN OTHERS.
        " Unknown 2OP
    ENDCASE.
  ENDMETHOD.


  METHOD execute_var.
    " VAR instructions (variable operands)
    DATA: lv_op1      TYPE i,
          lv_op2      TYPE i,
          lv_result   TYPE i.

    rv_next_pc = is_instr-next_pc.

    " Get operands (some VAR ops use different numbers)
    DATA(lv_count) = lines( is_instr-operands ).
    IF lv_count >= 1.
      lv_op1 = get_operand_value( is_instr = is_instr iv_index = 0 ).
    ENDIF.
    IF lv_count >= 2.
      lv_op2 = get_operand_value( is_instr = is_instr iv_index = 1 ).
    ENDIF.

    CASE is_instr-opcode.
      WHEN 0.  " call / call_vs - call routine
        IF lv_op1 = 0.
          " Calling address 0 returns 0
          store_result( is_instr = is_instr iv_val = 0 ).
        ELSE.
          " Unpack routine address
          DATA(lv_routine) = mo_memory->unpack_routine_addr( lv_op1 ).

          " Read routine header
          DATA(lv_num_locals) = mo_memory->read_byte( lv_routine ).
          lv_routine = lv_routine + 1.

          " Build arguments string (hex-encoded)
          DATA: lv_args TYPE string.
          lv_args = ''.
          DATA(lv_arg_count) = lv_count - 1.  " First operand is address

          " In v3, locals are initialized from header
          " Read initial local values from routine header
          DATA: lv_i TYPE i.
          lv_i = 0.
          WHILE lv_i < lv_num_locals.
            DATA(lv_init_val) = mo_memory->read_word( lv_routine + lv_i * 2 ).
            " Use argument value if provided, else use default
            IF lv_i < lv_arg_count.
              DATA(lv_arg_val) = get_operand_value( is_instr = is_instr iv_index = lv_i + 1 ).
              lv_args = lv_args && word_to_hex( lv_arg_val ).
            ELSE.
              lv_args = lv_args && word_to_hex( lv_init_val ).
            ENDIF.
            lv_i = lv_i + 1.
          ENDWHILE.

          " Push frame
          mo_stack->push_frame(
            iv_return_pc  = is_instr-next_pc
            iv_result_var = is_instr-store_var
            iv_num_locals = lv_num_locals
            iv_args       = lv_args
            iv_arg_count  = lv_arg_count ).

          " Jump to routine code (after header)
          rv_next_pc = lv_routine + lv_num_locals * 2.
        ENDIF.

      WHEN 1.  " storew - store word in array
        " op1 = array base, op2 = word index, op3 = value
        DATA(lv_op3) = 0.
        IF lv_count >= 3.
          lv_op3 = get_operand_value( is_instr = is_instr iv_index = 2 ).
        ENDIF.
        DATA(lv_addr) = lv_op1 + lv_op2 * 2.
        mo_memory->write_word( iv_addr = lv_addr iv_val = lv_op3 ).

      WHEN 2.  " storeb - store byte in array
        IF lv_count >= 3.
          lv_op3 = get_operand_value( is_instr = is_instr iv_index = 2 ).
        ENDIF.
        lv_addr = lv_op1 + lv_op2.
        mo_memory->write_byte( iv_addr = lv_addr iv_val = lv_op3 ).

      WHEN 3.  " put_prop - put object property
        " op1 = object, op2 = property, op3 = value
        IF lv_count >= 3.
          DATA(lv_prop_val) = get_operand_value( is_instr = is_instr iv_index = 2 ).
          mo_objects->put_prop( iv_object = lv_op1
                                iv_property = lv_op2
                                iv_value = lv_prop_val ).
        ENDIF.

      WHEN 4.  " read / sread - read input line
        " For now, set status to waiting
        mv_status = c_status_waiting.
        " TODO: Implement input handling

      WHEN 5.  " print_char - print character
        IF mo_io IS BOUND.
          " Convert ZSCII to character
          DATA(lv_char) = ||.
          IF lv_op1 >= 32 AND lv_op1 <= 126.
            " Standard ASCII range
            CASE lv_op1.
              WHEN 32. lv_char = ` `.
              WHEN 33. lv_char = `!`.
              WHEN 34. lv_char = `"`.
              WHEN 35. lv_char = `#`.
              WHEN 36. lv_char = `$`.
              WHEN 37. lv_char = `%`.
              WHEN 38. lv_char = `&`.
              WHEN 39. lv_char = `'`.
              WHEN 40. lv_char = `(`.
              WHEN 41. lv_char = `)`.
              WHEN 42. lv_char = `*`.
              WHEN 43. lv_char = `+`.
              WHEN 44. lv_char = `,`.
              WHEN 45. lv_char = `-`.
              WHEN 46. lv_char = `.`.
              WHEN 47. lv_char = `/`.
              WHEN 48. lv_char = `0`.
              WHEN 49. lv_char = `1`.
              WHEN 50. lv_char = `2`.
              WHEN 51. lv_char = `3`.
              WHEN 52. lv_char = `4`.
              WHEN 53. lv_char = `5`.
              WHEN 54. lv_char = `6`.
              WHEN 55. lv_char = `7`.
              WHEN 56. lv_char = `8`.
              WHEN 57. lv_char = `9`.
              WHEN 58. lv_char = `:`.
              WHEN 59. lv_char = `;`.
              WHEN 60. lv_char = `<`.
              WHEN 61. lv_char = `=`.
              WHEN 62. lv_char = `>`.
              WHEN 63. lv_char = `?`.
              WHEN 64. lv_char = `@`.
              WHEN 65. lv_char = `A`.
              WHEN 66. lv_char = `B`.
              WHEN 67. lv_char = `C`.
              WHEN 68. lv_char = `D`.
              WHEN 69. lv_char = `E`.
              WHEN 70. lv_char = `F`.
              WHEN 71. lv_char = `G`.
              WHEN 72. lv_char = `H`.
              WHEN 73. lv_char = `I`.
              WHEN 74. lv_char = `J`.
              WHEN 75. lv_char = `K`.
              WHEN 76. lv_char = `L`.
              WHEN 77. lv_char = `M`.
              WHEN 78. lv_char = `N`.
              WHEN 79. lv_char = `O`.
              WHEN 80. lv_char = `P`.
              WHEN 81. lv_char = `Q`.
              WHEN 82. lv_char = `R`.
              WHEN 83. lv_char = `S`.
              WHEN 84. lv_char = `T`.
              WHEN 85. lv_char = `U`.
              WHEN 86. lv_char = `V`.
              WHEN 87. lv_char = `W`.
              WHEN 88. lv_char = `X`.
              WHEN 89. lv_char = `Y`.
              WHEN 90. lv_char = `Z`.
              WHEN 91. lv_char = `[`.
              WHEN 92. lv_char = `\`.
              WHEN 93. lv_char = `]`.
              WHEN 94. lv_char = `^`.
              WHEN 95. lv_char = `_`.
              WHEN 96. lv_char = |`|.
              WHEN 97. lv_char = `a`.
              WHEN 98. lv_char = `b`.
              WHEN 99. lv_char = `c`.
              WHEN 100. lv_char = `d`.
              WHEN 101. lv_char = `e`.
              WHEN 102. lv_char = `f`.
              WHEN 103. lv_char = `g`.
              WHEN 104. lv_char = `h`.
              WHEN 105. lv_char = `i`.
              WHEN 106. lv_char = `j`.
              WHEN 107. lv_char = `k`.
              WHEN 108. lv_char = `l`.
              WHEN 109. lv_char = `m`.
              WHEN 110. lv_char = `n`.
              WHEN 111. lv_char = `o`.
              WHEN 112. lv_char = `p`.
              WHEN 113. lv_char = `q`.
              WHEN 114. lv_char = `r`.
              WHEN 115. lv_char = `s`.
              WHEN 116. lv_char = `t`.
              WHEN 117. lv_char = `u`.
              WHEN 118. lv_char = `v`.
              WHEN 119. lv_char = `w`.
              WHEN 120. lv_char = `x`.
              WHEN 121. lv_char = `y`.
              WHEN 122. lv_char = `z`.
              WHEN 123. lv_char = `{`.
              WHEN 124. lv_char = `|`.
              WHEN 125. lv_char = `}`.
              WHEN 126. lv_char = `~`.
              WHEN OTHERS. lv_char = `?`.
            ENDCASE.
          ELSEIF lv_op1 = 13.
            " Newline
            mo_io->new_line( ).
            RETURN.
          ELSE.
            lv_char = `?`.
          ENDIF.
          mo_io->print_text( lv_char ).
        ENDIF.

      WHEN 6.  " print_num - print signed number
        IF mo_io IS BOUND.
          DATA(lv_signed) = to_signed( lv_op1 ).
          mo_io->print_num( lv_signed ).
        ENDIF.

      WHEN 7.  " random - random number
        " op1: if positive, return random 1..op1
        "      if negative, seed random
        "      if zero, seed from clock
        DATA(lv_signed_op) = to_signed( lv_op1 ).
        IF lv_signed_op > 0.
          " Generate random 1..lv_signed_op
          " Simple LCG random - TODO: improve
          DATA(lv_rnd) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                                      min = 1
                                                      max = lv_signed_op ).
          lv_result = lv_rnd->get_next( ).
        ELSE.
          " Seed or predictable mode
          lv_result = 0.
        ENDIF.
        store_result( is_instr = is_instr iv_val = lv_result ).

      WHEN 8.  " push - push value onto stack
        mo_stack->push_stack( lv_op1 ).

      WHEN 9.  " pull - pull value from stack into variable
        lv_result = mo_stack->pop_stack( ).
        " op1 is variable to store into
        IF lv_op1 = 0.
          " Push back onto stack (replace top)
          mo_stack->push_stack( lv_result ).
        ELSE.
          mo_stack->set_variable( iv_var = lv_op1 iv_val = lv_result ).
        ENDIF.

      WHEN 10.  " split_window - split screen (v3)
        " TODO: Implement screen model

      WHEN 11.  " set_window - set output window
        " TODO: Implement screen model

      WHEN OTHERS.
        " Unknown VAR op
    ENDCASE.
  ENDMETHOD.


  METHOD decode_zstring.
    " Decode Z-encoded string at given address
    " Returns decoded text
    DATA: lv_addr    TYPE i,
          lv_word    TYPE i,
          lv_zchar   TYPE i,
          lv_shift   TYPE i,
          lv_char    TYPE string,
          lt_zchars  TYPE TABLE OF i,
          lv_i       TYPE i,
          lv_alphabet TYPE i.

    " Z-machine alphabet tables (v3)
    " A0: a-z (6-31 = a-z)
    " A1: A-Z
    " A2: special characters

    rv_text = ''.
    lv_addr = iv_addr.
    lv_alphabet = 0.  " Start in alphabet 0 (lowercase)

    " Read Z-characters until end marker (high bit set)
    DO.
      lv_word = mo_memory->read_word( lv_addr ).
      lv_addr = lv_addr + 2.

      " Extract 3 Z-characters from word (5 bits each)
      " Bits: 15 (end), 14-10 (z1), 9-5 (z2), 4-0 (z3)
      CLEAR lt_zchars.

      lv_zchar = ( lv_word DIV 1024 ) MOD 32.  " Bits 14-10
      APPEND lv_zchar TO lt_zchars.

      lv_zchar = ( lv_word DIV 32 ) MOD 32.    " Bits 9-5
      APPEND lv_zchar TO lt_zchars.

      lv_zchar = lv_word MOD 32.               " Bits 4-0
      APPEND lv_zchar TO lt_zchars.

      " Process Z-characters
      LOOP AT lt_zchars INTO lv_zchar.
        CASE lv_zchar.
          WHEN 0.
            " Space
            rv_text = rv_text && ` `.

          WHEN 1 OR 2 OR 3.
            " Abbreviation (v2+)
            " TODO: Implement abbreviations
            rv_text = rv_text && `[ABBR]`.

          WHEN 4.
            " Shift to alphabet 1 for next char
            lv_alphabet = 1.
            CONTINUE.

          WHEN 5.
            " Shift to alphabet 2 for next char
            lv_alphabet = 2.
            CONTINUE.

          WHEN OTHERS.
            " Character 6-31
            IF lv_alphabet = 2 AND lv_zchar = 6.
              " ZSCII escape sequence (next two z-chars form 10-bit code)
              " TODO: Implement ZSCII escape
              rv_text = rv_text && `?`.
            ELSE.
              " Map to character based on alphabet
              CASE lv_alphabet.
                WHEN 0.  " Lowercase a-z
                  CASE lv_zchar.
                    WHEN 6.  lv_char = `a`.
                    WHEN 7.  lv_char = `b`.
                    WHEN 8.  lv_char = `c`.
                    WHEN 9.  lv_char = `d`.
                    WHEN 10. lv_char = `e`.
                    WHEN 11. lv_char = `f`.
                    WHEN 12. lv_char = `g`.
                    WHEN 13. lv_char = `h`.
                    WHEN 14. lv_char = `i`.
                    WHEN 15. lv_char = `j`.
                    WHEN 16. lv_char = `k`.
                    WHEN 17. lv_char = `l`.
                    WHEN 18. lv_char = `m`.
                    WHEN 19. lv_char = `n`.
                    WHEN 20. lv_char = `o`.
                    WHEN 21. lv_char = `p`.
                    WHEN 22. lv_char = `q`.
                    WHEN 23. lv_char = `r`.
                    WHEN 24. lv_char = `s`.
                    WHEN 25. lv_char = `t`.
                    WHEN 26. lv_char = `u`.
                    WHEN 27. lv_char = `v`.
                    WHEN 28. lv_char = `w`.
                    WHEN 29. lv_char = `x`.
                    WHEN 30. lv_char = `y`.
                    WHEN 31. lv_char = `z`.
                    WHEN OTHERS. lv_char = `?`.
                  ENDCASE.

                WHEN 1.  " Uppercase A-Z
                  CASE lv_zchar.
                    WHEN 6.  lv_char = `A`.
                    WHEN 7.  lv_char = `B`.
                    WHEN 8.  lv_char = `C`.
                    WHEN 9.  lv_char = `D`.
                    WHEN 10. lv_char = `E`.
                    WHEN 11. lv_char = `F`.
                    WHEN 12. lv_char = `G`.
                    WHEN 13. lv_char = `H`.
                    WHEN 14. lv_char = `I`.
                    WHEN 15. lv_char = `J`.
                    WHEN 16. lv_char = `K`.
                    WHEN 17. lv_char = `L`.
                    WHEN 18. lv_char = `M`.
                    WHEN 19. lv_char = `N`.
                    WHEN 20. lv_char = `O`.
                    WHEN 21. lv_char = `P`.
                    WHEN 22. lv_char = `Q`.
                    WHEN 23. lv_char = `R`.
                    WHEN 24. lv_char = `S`.
                    WHEN 25. lv_char = `T`.
                    WHEN 26. lv_char = `U`.
                    WHEN 27. lv_char = `V`.
                    WHEN 28. lv_char = `W`.
                    WHEN 29. lv_char = `X`.
                    WHEN 30. lv_char = `Y`.
                    WHEN 31. lv_char = `Z`.
                    WHEN OTHERS. lv_char = `?`.
                  ENDCASE.

                WHEN 2.  " Punctuation/symbols
                  CASE lv_zchar.
                    WHEN 6.  lv_char = `^`.  " Actually escape, handled above
                    WHEN 7.  lv_char = |{ cl_abap_char_utilities=>newline }|.
                    WHEN 8.  lv_char = `0`.
                    WHEN 9.  lv_char = `1`.
                    WHEN 10. lv_char = `2`.
                    WHEN 11. lv_char = `3`.
                    WHEN 12. lv_char = `4`.
                    WHEN 13. lv_char = `5`.
                    WHEN 14. lv_char = `6`.
                    WHEN 15. lv_char = `7`.
                    WHEN 16. lv_char = `8`.
                    WHEN 17. lv_char = `9`.
                    WHEN 18. lv_char = `.`.
                    WHEN 19. lv_char = `,`.
                    WHEN 20. lv_char = `!`.
                    WHEN 21. lv_char = `?`.
                    WHEN 22. lv_char = `_`.
                    WHEN 23. lv_char = `#`.
                    WHEN 24. lv_char = `'`.
                    WHEN 25. lv_char = `"`.
                    WHEN 26. lv_char = `/`.
                    WHEN 27. lv_char = `\`.
                    WHEN 28. lv_char = `-`.
                    WHEN 29. lv_char = `:`.
                    WHEN 30. lv_char = `(`.
                    WHEN 31. lv_char = `)`.
                    WHEN OTHERS. lv_char = `?`.
                  ENDCASE.
              ENDCASE.

              rv_text = rv_text && lv_char.
            ENDIF.

            " Reset to alphabet 0 after each character
            lv_alphabet = 0.
        ENDCASE.
      ENDLOOP.

      " Check end marker (bit 15)
      IF lv_word >= 32768.
        EXIT.
      ENDIF.

      " Safety limit
      IF lv_addr - iv_addr > 1000.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD word_to_hex.
    " Convert word to 4-char hex string (helper for call)
    DATA: lv_hi TYPE i,
          lv_lo TYPE i.

    DATA(lv_val) = iv_word MOD 65536.
    IF lv_val < 0.
      lv_val = lv_val + 65536.
    ENDIF.

    lv_hi = lv_val DIV 256.
    lv_lo = lv_val MOD 256.

    rv_hex = byte_to_hex( lv_hi ) && byte_to_hex( lv_lo ).
  ENDMETHOD.


  METHOD byte_to_hex.
    " Convert byte to 2-char hex string
    DATA: lv_high TYPE i,
          lv_low  TYPE i,
          lv_c1   TYPE string,
          lv_c2   TYPE string.

    DATA(lv_val) = iv_byte MOD 256.
    IF lv_val < 0.
      lv_val = lv_val + 256.
    ENDIF.

    lv_high = lv_val DIV 16.
    lv_low = lv_val MOD 16.

    CASE lv_high.
      WHEN 0.  lv_c1 = '0'.
      WHEN 1.  lv_c1 = '1'.
      WHEN 2.  lv_c1 = '2'.
      WHEN 3.  lv_c1 = '3'.
      WHEN 4.  lv_c1 = '4'.
      WHEN 5.  lv_c1 = '5'.
      WHEN 6.  lv_c1 = '6'.
      WHEN 7.  lv_c1 = '7'.
      WHEN 8.  lv_c1 = '8'.
      WHEN 9.  lv_c1 = '9'.
      WHEN 10. lv_c1 = 'A'.
      WHEN 11. lv_c1 = 'B'.
      WHEN 12. lv_c1 = 'C'.
      WHEN 13. lv_c1 = 'D'.
      WHEN 14. lv_c1 = 'E'.
      WHEN 15. lv_c1 = 'F'.
      WHEN OTHERS. lv_c1 = '0'.
    ENDCASE.

    CASE lv_low.
      WHEN 0.  lv_c2 = '0'.
      WHEN 1.  lv_c2 = '1'.
      WHEN 2.  lv_c2 = '2'.
      WHEN 3.  lv_c2 = '3'.
      WHEN 4.  lv_c2 = '4'.
      WHEN 5.  lv_c2 = '5'.
      WHEN 6.  lv_c2 = '6'.
      WHEN 7.  lv_c2 = '7'.
      WHEN 8.  lv_c2 = '8'.
      WHEN 9.  lv_c2 = '9'.
      WHEN 10. lv_c2 = 'A'.
      WHEN 11. lv_c2 = 'B'.
      WHEN 12. lv_c2 = 'C'.
      WHEN 13. lv_c2 = 'D'.
      WHEN 14. lv_c2 = 'E'.
      WHEN 15. lv_c2 = 'F'.
      WHEN OTHERS. lv_c2 = '0'.
    ENDCASE.

    rv_hex = lv_c1 && lv_c2.
  ENDMETHOD.

ENDCLASS.
