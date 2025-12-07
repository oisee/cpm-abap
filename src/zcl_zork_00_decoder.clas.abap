*&---------------------------------------------------------------------*
*& Z-Machine Instruction Decoder
*& Decodes Z-machine instructions from memory
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

      " Main decode method
      decode
        IMPORTING iv_pc           TYPE i
        EXPORTING ev_next_pc      TYPE i
        RETURNING VALUE(rs_instr) TYPE zif_zork_00_types=>ty_instruction,

      " Get operand value from decoded instruction
      get_operand
        IMPORTING is_instr       TYPE zif_zork_00_types=>ty_instruction
                  iv_index       TYPE i
        RETURNING VALUE(rv_val)  TYPE i,

      " Get operand type from decoded instruction
      get_operand_type
        IMPORTING is_instr       TYPE zif_zork_00_types=>ty_instruction
                  iv_index       TYPE i
        RETURNING VALUE(rv_type) TYPE i.

  PRIVATE SECTION.

    DATA: mo_memory TYPE REF TO zcl_zork_00_memory.

    "======================================================================
    " Opcode Information Tables
    " Which opcodes have store/branch for each operand count
    "======================================================================

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
                  ev_offset       TYPE i,

      " Helper: store operand value to hex string
      store_operand_hex
        IMPORTING iv_index        TYPE i
                  iv_val          TYPE i
        CHANGING  cv_operands     TYPE string,

      " Helper: store operand type
      store_type_hex
        IMPORTING iv_index        TYPE i
                  iv_type         TYPE i
        CHANGING  cv_types        TYPE string,

      " Convert word to hex (4 chars)
      word_to_hex
        IMPORTING iv_word         TYPE i
        RETURNING VALUE(rv_hex)   TYPE string,

      " Convert hex to word
      hex_to_word
        IMPORTING iv_hex          TYPE string
        RETURNING VALUE(rv_word)  TYPE i,

      " Convert byte to hex (2 chars)
      byte_to_hex
        IMPORTING iv_byte         TYPE i
        RETURNING VALUE(rv_hex)   TYPE string,

      " Convert hex to byte
      hex_to_byte
        IMPORTING iv_hex          TYPE string
        RETURNING VALUE(rv_byte)  TYPE i.

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
          lv_op_val   TYPE i,
          lv_i        TYPE i.

    CLEAR rs_instr.
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
        rs_instr-operand_cnt = 0.
        rs_instr-operands = ''.
        rs_instr-op_types = ''.

        IF lv_type1 <> zif_zork_00_types=>c_optype_omit.
          read_operand(
            EXPORTING iv_type = lv_type1 iv_pc_in = lv_pc
            IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
          store_operand_hex( EXPORTING iv_index = 0 iv_val = lv_op_val
                             CHANGING cv_operands = rs_instr-operands ).
          store_type_hex( EXPORTING iv_index = 0 iv_type = lv_type1
                          CHANGING cv_types = rs_instr-op_types ).
          rs_instr-operand_cnt = 1.

          IF lv_type2 <> zif_zork_00_types=>c_optype_omit.
            read_operand(
              EXPORTING iv_type = lv_type2 iv_pc_in = lv_pc
              IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
            store_operand_hex( EXPORTING iv_index = 1 iv_val = lv_op_val
                               CHANGING cv_operands = rs_instr-operands ).
            store_type_hex( EXPORTING iv_index = 1 iv_type = lv_type2
                            CHANGING cv_types = rs_instr-op_types ).
            rs_instr-operand_cnt = 2.

            IF lv_type3 <> zif_zork_00_types=>c_optype_omit.
              read_operand(
                EXPORTING iv_type = lv_type3 iv_pc_in = lv_pc
                IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
              store_operand_hex( EXPORTING iv_index = 2 iv_val = lv_op_val
                                 CHANGING cv_operands = rs_instr-operands ).
              store_type_hex( EXPORTING iv_index = 2 iv_type = lv_type3
                              CHANGING cv_types = rs_instr-op_types ).
              rs_instr-operand_cnt = 3.

              IF lv_type4 <> zif_zork_00_types=>c_optype_omit.
                read_operand(
                  EXPORTING iv_type = lv_type4 iv_pc_in = lv_pc
                  IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
                store_operand_hex( EXPORTING iv_index = 3 iv_val = lv_op_val
                                   CHANGING cv_operands = rs_instr-operands ).
                store_type_hex( EXPORTING iv_index = 3 iv_type = lv_type4
                                CHANGING cv_types = rs_instr-op_types ).
                rs_instr-operand_cnt = 4.
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
          rs_instr-operand_cnt = 0.
          rs_instr-operands = ''.
          rs_instr-op_types = ''.
        ELSE.
          rs_instr-op_count = zif_zork_00_types=>c_opcount_1op.
          rs_instr-operand_cnt = 1.
          " Read the one operand
          read_operand(
            EXPORTING iv_type = lv_optype iv_pc_in = lv_pc
            IMPORTING ev_pc_out = lv_pc ev_val = lv_op_val ).
          rs_instr-operands = word_to_hex( lv_op_val ).
          rs_instr-op_types = byte_to_hex( lv_optype ).
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
        rs_instr-operands = word_to_hex( lv_op_val ).
        rs_instr-op_types = byte_to_hex( lv_type1 ).

        lv_op_val = mo_memory->read_byte( lv_pc ).
        lv_pc = lv_pc + 1.
        store_operand_hex( EXPORTING iv_index = 1 iv_val = lv_op_val
                           CHANGING cv_operands = rs_instr-operands ).
        store_type_hex( EXPORTING iv_index = 1 iv_type = lv_type2
                        CHANGING cv_types = rs_instr-op_types ).
        rs_instr-operand_cnt = 2.
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

    " Calculate instruction length and return next PC
    rs_instr-instr_len = lv_pc - iv_pc.
    ev_next_pc = lv_pc.
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
    " Extract operand value from instruction
    DATA: lv_offset TYPE i,
          lv_hex    TYPE string.

    IF iv_index < 0 OR iv_index >= is_instr-operand_cnt.
      rv_val = 0.
      RETURN.
    ENDIF.

    " Each operand stored as 4 hex chars (word)
    lv_offset = iv_index * 4.
    IF lv_offset + 4 <= strlen( is_instr-operands ).
      lv_hex = is_instr-operands+lv_offset(4).
      rv_val = hex_to_word( lv_hex ).
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.


  METHOD get_operand_type.
    " Extract operand type from instruction
    DATA: lv_offset TYPE i,
          lv_hex    TYPE string.

    IF iv_index < 0 OR iv_index >= is_instr-operand_cnt.
      rv_type = zif_zork_00_types=>c_optype_omit.
      RETURN.
    ENDIF.

    " Each type stored as 2 hex chars (byte)
    lv_offset = iv_index * 2.
    IF lv_offset + 2 <= strlen( is_instr-op_types ).
      lv_hex = is_instr-op_types+lv_offset(2).
      rv_type = hex_to_byte( lv_hex ).
    ELSE.
      rv_type = zif_zork_00_types=>c_optype_omit.
    ENDIF.
  ENDMETHOD.


  METHOD store_operand_hex.
    " Store operand value at given index in hex string
    DATA: lv_hex TYPE string,
          lv_offset TYPE i,
          lv_needed TYPE i,
          lv_before TYPE string,
          lv_after TYPE string,
          lv_len TYPE i.

    lv_hex = word_to_hex( iv_val ).
    lv_offset = iv_index * 4.
    lv_needed = lv_offset + 4.

    " Pad operands string if needed
    WHILE strlen( cv_operands ) < lv_needed.
      cv_operands = cv_operands && '0000'.
    ENDWHILE.

    " Replace at position
    lv_len = strlen( cv_operands ).
    IF lv_offset > 0.
      lv_before = cv_operands+0(lv_offset).
    ELSE.
      lv_before = ''.
    ENDIF.

    IF lv_offset + 4 < lv_len.
      DATA(lv_rem) = lv_len - lv_offset - 4.
      DATA(lv_start) = lv_offset + 4.
      lv_after = cv_operands+lv_start(lv_rem).
    ELSE.
      lv_after = ''.
    ENDIF.

    cv_operands = lv_before && lv_hex && lv_after.
  ENDMETHOD.


  METHOD store_type_hex.
    " Store operand type at given index in hex string
    DATA: lv_hex TYPE string,
          lv_offset TYPE i,
          lv_needed TYPE i,
          lv_before TYPE string,
          lv_after TYPE string,
          lv_len TYPE i.

    lv_hex = byte_to_hex( iv_type ).
    lv_offset = iv_index * 2.
    lv_needed = lv_offset + 2.

    " Pad types string if needed
    WHILE strlen( cv_types ) < lv_needed.
      cv_types = cv_types && '00'.
    ENDWHILE.

    " Replace at position
    lv_len = strlen( cv_types ).
    IF lv_offset > 0.
      lv_before = cv_types+0(lv_offset).
    ELSE.
      lv_before = ''.
    ENDIF.

    IF lv_offset + 2 < lv_len.
      DATA(lv_rem) = lv_len - lv_offset - 2.
      DATA(lv_start) = lv_offset + 2.
      lv_after = cv_types+lv_start(lv_rem).
    ELSE.
      lv_after = ''.
    ENDIF.

    cv_types = lv_before && lv_hex && lv_after.
  ENDMETHOD.


  METHOD word_to_hex.
    " Convert 16-bit word to 4-char hex string
    DATA: lv_hi TYPE i,
          lv_lo TYPE i.

    lv_hi = iv_word DIV 256.
    lv_lo = iv_word MOD 256.

    IF lv_hi < 0.
      lv_hi = lv_hi + 256.
    ENDIF.
    IF lv_lo < 0.
      lv_lo = lv_lo + 256.
    ENDIF.

    rv_hex = byte_to_hex( lv_hi ) && byte_to_hex( lv_lo ).
  ENDMETHOD.


  METHOD hex_to_word.
    " Convert 4-char hex string to 16-bit word
    DATA: lv_hi TYPE i,
          lv_lo TYPE i.

    IF strlen( iv_hex ) >= 4.
      lv_hi = hex_to_byte( iv_hex+0(2) ).
      lv_lo = hex_to_byte( iv_hex+2(2) ).
      rv_word = lv_hi * 256 + lv_lo.
    ELSE.
      rv_word = 0.
    ENDIF.
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


  METHOD hex_to_byte.
    " Convert 2-char hex string to byte
    DATA: lv_high TYPE i,
          lv_low  TYPE i.

    IF strlen( iv_hex ) < 2.
      rv_byte = 0.
      RETURN.
    ENDIF.

    DATA(lv_c1) = iv_hex+0(1).
    DATA(lv_c2) = iv_hex+1(1).

    CASE lv_c1.
      WHEN '0'. lv_high = 0.
      WHEN '1'. lv_high = 1.
      WHEN '2'. lv_high = 2.
      WHEN '3'. lv_high = 3.
      WHEN '4'. lv_high = 4.
      WHEN '5'. lv_high = 5.
      WHEN '6'. lv_high = 6.
      WHEN '7'. lv_high = 7.
      WHEN '8'. lv_high = 8.
      WHEN '9'. lv_high = 9.
      WHEN 'A' OR 'a'. lv_high = 10.
      WHEN 'B' OR 'b'. lv_high = 11.
      WHEN 'C' OR 'c'. lv_high = 12.
      WHEN 'D' OR 'd'. lv_high = 13.
      WHEN 'E' OR 'e'. lv_high = 14.
      WHEN 'F' OR 'f'. lv_high = 15.
      WHEN OTHERS. lv_high = 0.
    ENDCASE.

    CASE lv_c2.
      WHEN '0'. lv_low = 0.
      WHEN '1'. lv_low = 1.
      WHEN '2'. lv_low = 2.
      WHEN '3'. lv_low = 3.
      WHEN '4'. lv_low = 4.
      WHEN '5'. lv_low = 5.
      WHEN '6'. lv_low = 6.
      WHEN '7'. lv_low = 7.
      WHEN '8'. lv_low = 8.
      WHEN '9'. lv_low = 9.
      WHEN 'A' OR 'a'. lv_low = 10.
      WHEN 'B' OR 'b'. lv_low = 11.
      WHEN 'C' OR 'c'. lv_low = 12.
      WHEN 'D' OR 'd'. lv_low = 13.
      WHEN 'E' OR 'e'. lv_low = 14.
      WHEN 'F' OR 'f'. lv_low = 15.
      WHEN OTHERS. lv_low = 0.
    ENDCASE.

    rv_byte = lv_high * 16 + lv_low.
  ENDMETHOD.

ENDCLASS.
