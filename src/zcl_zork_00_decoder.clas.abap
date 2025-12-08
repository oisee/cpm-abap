*&---------------------------------------------------------------------*
*& Z-Machine Instruction Decoder
*& Decodes Z-machine instructions from memory into structured format
*& Supports v3 instruction formats (short, long, variable)
*&---------------------------------------------------------------------*
CLASS zcl_zork_00_decoder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING io_memory TYPE REF TO zcl_zork_00_memory,

      " Main decode method - decode single instruction
      decode
        IMPORTING iv_pc           TYPE i
        RETURNING VALUE(rs_instr) TYPE zif_zork_00_types=>ts_instruction,

      " Decode multiple instructions (for analysis/disassembly)
      decode_range
        IMPORTING iv_start_pc     TYPE i
                  iv_count        TYPE i DEFAULT 10
        RETURNING VALUE(rt_instr) TYPE zif_zork_00_types=>tt_instructions,

      " Get operand value from decoded instruction (convenience method)
      get_operand
        IMPORTING is_instr       TYPE zif_zork_00_types=>ts_instruction
                  iv_index       TYPE i
        RETURNING VALUE(rv_val)  TYPE i,

      " Get operand type from decoded instruction (convenience method)
      get_operand_type
        IMPORTING is_instr       TYPE zif_zork_00_types=>ts_instruction
                  iv_index       TYPE i
        RETURNING VALUE(rv_type) TYPE i.

  PRIVATE SECTION.

    DATA: mo_memory TYPE REF TO zcl_zork_00_memory.

    METHODS:
      " Decode operand types byte into individual types
      decode_operand_types
        IMPORTING iv_types_byte   TYPE i
        EXPORTING ev_type1        TYPE i
                  ev_type2        TYPE i
                  ev_type3        TYPE i
                  ev_type4        TYPE i,

      " Read operand value based on type
      read_operand
        IMPORTING iv_type         TYPE i
                  iv_pc_in        TYPE i
        EXPORTING ev_pc_out       TYPE i
                  ev_val          TYPE i,

      " Add operand to instruction
      add_operand
        IMPORTING iv_type  TYPE i
                  iv_value TYPE i
        CHANGING  cs_instr TYPE zif_zork_00_types=>ts_instruction,

      " Check if opcode stores a result
      has_store
        IMPORTING iv_op_count     TYPE i
                  iv_opcode       TYPE i
        RETURNING VALUE(rv_store) TYPE abap_bool,

      " Check if opcode has branch
      has_branch
        IMPORTING iv_op_count     TYPE i
                  iv_opcode       TYPE i
        RETURNING VALUE(rv_branch) TYPE abap_bool,

      " Decode branch data
      decode_branch
        IMPORTING iv_pc_in        TYPE i
        EXPORTING ev_pc_out       TYPE i
                  ev_on_true      TYPE abap_bool
                  ev_offset       TYPE i.

ENDCLASS.


CLASS zcl_zork_00_decoder IMPLEMENTATION.

  METHOD constructor.
    mo_memory = io_memory.
  ENDMETHOD.


  METHOD decode.
    " Decode Z-machine instruction at given PC
    " Returns instruction structure with all decoded fields
    DATA: lv_pc       TYPE i,
          lv_byte     TYPE i,
          lv_top2     TYPE i,
          lv_optype   TYPE i,
          lv_types_b  TYPE i,
          lv_type1    TYPE i,
          lv_type2    TYPE i,
          lv_type3    TYPE i,
          lv_type4    TYPE i,
          lv_op_val   TYPE i.

    CLEAR rs_instr.
    rs_instr-address = iv_pc.
    lv_pc = iv_pc.

    " Read first byte
    lv_byte = mo_memory->read_byte( lv_pc ).
    lv_pc = lv_pc + 1.

    " Determine instruction form from top 2 bits
    lv_top2 = lv_byte DIV 64.  " Bits 7-6

    CASE lv_top2.
      WHEN 3.  " 11xxxxxx = Variable form
        rs_instr-form = zif_zork_00_types=>c_form_variable.
        " Bit 5 determines 2OP vs VAR
        IF lv_byte MOD 64 DIV 32 = 0.  " Bit 5 = 0
          rs_instr-op_count = zif_zork_00_types=>c_opcount_2op.
        ELSE.
          rs_instr-op_count = zif_zork_00_types=>c_opcount_var.
        ENDIF.
        rs_instr-opcode = lv_byte MOD 32.  " Bits 4-0

        " Read operand types byte
        lv_types_b = mo_memory->read_byte( lv_pc ).
        lv_pc = lv_pc + 1.

        " Decode up to 4 operand types
        decode_operand_types(
          EXPORTING iv_types_byte = lv_types_b
          IMPORTING ev_type1 = lv_type1
                    ev_type2 = lv_type2
                    ev_type3 = lv_type3
                    ev_type4 = lv_type4 ).

        " Read operands based on types
        IF lv_type1 <> zif_zork_00_types=>c_optype_omit.
          read_operand(
            EXPORTING iv_type = lv_type1 iv_pc_in = lv_pc
            IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
          add_operand( EXPORTING iv_type = lv_type1 iv_value = lv_op_val
                       CHANGING cs_instr = rs_instr ).

          IF lv_type2 <> zif_zork_00_types=>c_optype_omit.
            read_operand(
              EXPORTING iv_type = lv_type2 iv_pc_in = lv_pc
              IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
            add_operand( EXPORTING iv_type = lv_type2 iv_value = lv_op_val
                         CHANGING cs_instr = rs_instr ).

            IF lv_type3 <> zif_zork_00_types=>c_optype_omit.
              read_operand(
                EXPORTING iv_type = lv_type3 iv_pc_in = lv_pc
                IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
              add_operand( EXPORTING iv_type = lv_type3 iv_value = lv_op_val
                           CHANGING cs_instr = rs_instr ).

              IF lv_type4 <> zif_zork_00_types=>c_optype_omit.
                read_operand(
                  EXPORTING iv_type = lv_type4 iv_pc_in = lv_pc
                  IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
                add_operand( EXPORTING iv_type = lv_type4 iv_value = lv_op_val
                             CHANGING cs_instr = rs_instr ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN 2.  " 10xxxxxx = Short form
        rs_instr-form = zif_zork_00_types=>c_form_short.
        " Bits 5-4 = operand type, Bits 3-0 = opcode
        lv_optype = lv_byte MOD 64 DIV 16.  " Bits 5-4
        rs_instr-opcode = lv_byte MOD 16.   " Bits 3-0

        IF lv_optype = 3.  " Type 11 = no operand
          rs_instr-op_count = zif_zork_00_types=>c_opcount_0op.
        ELSE.
          rs_instr-op_count = zif_zork_00_types=>c_opcount_1op.
          " Read the one operand
          read_operand(
            EXPORTING iv_type = lv_optype iv_pc_in = lv_pc
            IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
          add_operand( EXPORTING iv_type = lv_optype iv_value = lv_op_val
                       CHANGING cs_instr = rs_instr ).
        ENDIF.

      WHEN OTHERS.  " 00xxxxxx or 01xxxxxx = Long form
        rs_instr-form = zif_zork_00_types=>c_form_long.
        rs_instr-op_count = zif_zork_00_types=>c_opcount_2op.
        rs_instr-opcode = lv_byte MOD 32.  " Bits 4-0

        " Bit 6 = type of operand 1 (0=small const, 1=variable)
        " Bit 5 = type of operand 2 (0=small const, 1=variable)
        IF lv_byte MOD 128 DIV 64 = 0.
          lv_type1 = zif_zork_00_types=>c_optype_small.
        ELSE.
          lv_type1 = zif_zork_00_types=>c_optype_var.
        ENDIF.

        IF lv_byte MOD 64 DIV 32 = 0.
          lv_type2 = zif_zork_00_types=>c_optype_small.
        ELSE.
          lv_type2 = zif_zork_00_types=>c_optype_var.
        ENDIF.

        " Read two operands (both 1-byte each in long form)
        lv_op_val = mo_memory->read_byte( lv_pc ).
        lv_pc = lv_pc + 1.
        add_operand( EXPORTING iv_type = lv_type1 iv_value = lv_op_val
                     CHANGING cs_instr = rs_instr ).

        lv_op_val = mo_memory->read_byte( lv_pc ).
        lv_pc = lv_pc + 1.
        add_operand( EXPORTING iv_type = lv_type2 iv_value = lv_op_val
                     CHANGING cs_instr = rs_instr ).
    ENDCASE.

    " Check for store byte
    rs_instr-has_store = has_store( iv_op_count = rs_instr-op_count
                                    iv_opcode = rs_instr-opcode ).
    IF rs_instr-has_store = abap_true.
      rs_instr-store_var = mo_memory->read_byte( lv_pc ).
      lv_pc = lv_pc + 1.
    ENDIF.

    " Check for branch data
    rs_instr-has_branch = has_branch( iv_op_count = rs_instr-op_count
                                      iv_opcode = rs_instr-opcode ).
    IF rs_instr-has_branch = abap_true.
      decode_branch(
        EXPORTING iv_pc_in = lv_pc
        IMPORTING ev_pc_out = lv_pc
                  ev_on_true = rs_instr-branch_on
                  ev_offset = rs_instr-branch_off ).
    ENDIF.

    " Handle text literal for print/print_ret (0OP:2, 0OP:3)
    IF rs_instr-op_count = zif_zork_00_types=>c_opcount_0op.
      IF rs_instr-opcode = 2 OR rs_instr-opcode = 3.
        " Text follows immediately - record address, skip to end
        rs_instr-text_addr = lv_pc.
        " Skip text by reading until end marker (high bit set)
        DATA(lv_word) = 0.
        DO.
          lv_word = mo_memory->read_word( lv_pc ).
          lv_pc = lv_pc + 2.
          IF lv_word >= 32768.  " High bit set = end
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.

    " Calculate instruction length and set next PC
    rs_instr-instr_len = lv_pc - iv_pc.
    rs_instr-next_pc = lv_pc.
  ENDMETHOD.


  METHOD decode_range.
    " Decode multiple instructions starting at given PC
    DATA: lv_pc    TYPE i,
          ls_instr TYPE zif_zork_00_types=>ts_instruction,
          lv_cnt   TYPE i.

    lv_pc = iv_start_pc.
    lv_cnt = 0.

    WHILE lv_cnt < iv_count.
      ls_instr = decode( lv_pc ).
      APPEND ls_instr TO rt_instr.
      lv_pc = ls_instr-next_pc.
      lv_cnt = lv_cnt + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD add_operand.
    " Add operand to instruction's operands table
    DATA: ls_operand TYPE zif_zork_00_types=>ts_operand.

    ls_operand-type = iv_type.
    ls_operand-value = iv_value.
    APPEND ls_operand TO cs_instr-operands.
  ENDMETHOD.


  METHOD decode_operand_types.
    " Decode types byte: 2 bits per operand, high bits first
    " Types: 00=large const, 01=small const, 10=variable, 11=omitted
    ev_type1 = iv_types_byte DIV 64.          " Bits 7-6
    ev_type2 = iv_types_byte MOD 64 DIV 16.   " Bits 5-4
    ev_type3 = iv_types_byte MOD 16 DIV 4.    " Bits 3-2
    ev_type4 = iv_types_byte MOD 4.           " Bits 1-0
  ENDMETHOD.


  METHOD read_operand.
    " Read operand based on type
    DATA: lv_pc TYPE i.

    lv_pc = iv_pc_in.

    CASE iv_type.
      WHEN zif_zork_00_types=>c_optype_large.  " 2-byte constant
        ev_val = mo_memory->read_word( lv_pc ).
        lv_pc = lv_pc + 2.
      WHEN zif_zork_00_types=>c_optype_small.  " 1-byte constant
        ev_val = mo_memory->read_byte( lv_pc ).
        lv_pc = lv_pc + 1.
      WHEN zif_zork_00_types=>c_optype_var.    " Variable reference
        ev_val = mo_memory->read_byte( lv_pc ).
        lv_pc = lv_pc + 1.
      WHEN OTHERS.  " Omitted
        ev_val = 0.
    ENDCASE.

    ev_pc_out = lv_pc.
  ENDMETHOD.


  METHOD has_store.
    " Determine if opcode stores a result
    rv_store = abap_false.

    CASE iv_op_count.
      WHEN zif_zork_00_types=>c_opcount_2op.
        " 2OP opcodes that store: 8-9 (or/and), 15-25 (loadw..mod)
        CASE iv_opcode.
          WHEN 8 OR 9.      " or, and
            rv_store = abap_true.
          WHEN 15 OR 16.    " loadw, loadb
            rv_store = abap_true.
          WHEN 17 OR 18 OR 19.  " get_prop, get_prop_addr, get_next_prop
            rv_store = abap_true.
          WHEN 20 OR 21 OR 22 OR 23 OR 24.  " add, sub, mul, div, mod
            rv_store = abap_true.
        ENDCASE.

      WHEN zif_zork_00_types=>c_opcount_1op.
        " 1OP opcodes that store: 1-4 (get_sibling..get_prop_len), 14 (load)
        CASE iv_opcode.
          WHEN 1 OR 2 OR 3 OR 4.  " get_sibling, get_child, get_parent, get_prop_len
            rv_store = abap_true.
          WHEN 14.  " load
            rv_store = abap_true.
          WHEN 15.  " not (v1-4 only, store)
            rv_store = abap_true.
        ENDCASE.

      WHEN zif_zork_00_types=>c_opcount_var.
        " VAR opcodes that store: 0 (call), 7 (random)
        CASE iv_opcode.
          WHEN 0.   " call
            rv_store = abap_true.
          WHEN 7.   " random
            rv_store = abap_true.
        ENDCASE.

      WHEN zif_zork_00_types=>c_opcount_0op.
        " 0OP: none store in v3
    ENDCASE.
  ENDMETHOD.


  METHOD has_branch.
    " Determine if opcode has branch
    rv_branch = abap_false.

    CASE iv_op_count.
      WHEN zif_zork_00_types=>c_opcount_2op.
        " 2OP opcodes that branch: 1-7 (je..test), 10 (test_attr)
        CASE iv_opcode.
          WHEN 1 OR 2 OR 3.  " je, jl, jg
            rv_branch = abap_true.
          WHEN 4 OR 5.       " dec_chk, inc_chk
            rv_branch = abap_true.
          WHEN 6 OR 7.       " jin, test
            rv_branch = abap_true.
          WHEN 10.           " test_attr
            rv_branch = abap_true.
        ENDCASE.

      WHEN zif_zork_00_types=>c_opcount_1op.
        " 1OP opcodes that branch: 0-2 (jz, get_sibling, get_child)
        CASE iv_opcode.
          WHEN 0.   " jz
            rv_branch = abap_true.
          WHEN 1.   " get_sibling (also stores!)
            rv_branch = abap_true.
          WHEN 2.   " get_child (also stores!)
            rv_branch = abap_true.
        ENDCASE.

      WHEN zif_zork_00_types=>c_opcount_0op.
        " 0OP opcodes that branch: 5-6 (save, restore in v3), 13 (verify)
        CASE iv_opcode.
          WHEN 5 OR 6.  " save, restore (v3 branch)
            rv_branch = abap_true.
          WHEN 13.      " verify
            rv_branch = abap_true.
        ENDCASE.

      WHEN zif_zork_00_types=>c_opcount_var.
        " VAR: none branch in v3
    ENDCASE.
  ENDMETHOD.


  METHOD decode_branch.
    " Decode branch data (1 or 2 bytes)
    DATA: lv_byte1  TYPE i,
          lv_byte2  TYPE i,
          lv_offset TYPE i,
          lv_pc     TYPE i.

    lv_pc = iv_pc_in.

    lv_byte1 = mo_memory->read_byte( lv_pc ).
    lv_pc = lv_pc + 1.

    " Bit 7 = branch on true (1) or false (0)
    IF lv_byte1 >= 128.
      ev_on_true = abap_true.
      lv_byte1 = lv_byte1 - 128.
    ELSE.
      ev_on_true = abap_false.
    ENDIF.

    " Bit 6 = single byte offset (1) or two byte offset (0)
    IF lv_byte1 >= 64.
      " Single byte: 6-bit offset (0-63)
      lv_offset = lv_byte1 - 64.
    ELSE.
      " Two bytes: 14-bit signed offset
      lv_byte2 = mo_memory->read_byte( lv_pc ).
      lv_pc = lv_pc + 1.
      lv_offset = lv_byte1 * 256 + lv_byte2.
      " Sign extend 14-bit value
      IF lv_offset >= 8192.  " 2^13
        lv_offset = lv_offset - 16384.  " 2^14
      ENDIF.
    ENDIF.

    ev_offset = lv_offset.
    ev_pc_out = lv_pc.
  ENDMETHOD.


  METHOD get_operand.
    " Get operand value by index from decoded instruction
    DATA(lv_count) = lines( is_instr-operands ).

    IF iv_index < 0 OR iv_index >= lv_count.
      rv_val = 0.
      RETURN.
    ENDIF.

    " Tables are 1-indexed in ABAP
    DATA(lv_idx) = iv_index + 1.
    READ TABLE is_instr-operands INDEX lv_idx INTO DATA(ls_op).
    IF sy-subrc = 0.
      rv_val = ls_op-value.
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.


  METHOD get_operand_type.
    " Get operand type by index from decoded instruction
    DATA(lv_count) = lines( is_instr-operands ).

    IF iv_index < 0 OR iv_index >= lv_count.
      rv_type = zif_zork_00_types=>c_optype_omit.
      RETURN.
    ENDIF.

    " Tables are 1-indexed in ABAP
    DATA(lv_idx) = iv_index + 1.
    READ TABLE is_instr-operands INDEX lv_idx INTO DATA(ls_op).
    IF sy-subrc = 0.
      rv_type = ls_op-type.
    ELSE.
      rv_type = zif_zork_00_types=>c_optype_omit.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
