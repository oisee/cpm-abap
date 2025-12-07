*&---------------------------------------------------------------------*
*& Z-Machine Stack Management Class
*& Handles call frames, local variables, and evaluation stack
*& Transpiler-compatible: uses STRING-based storage
*&---------------------------------------------------------------------*
CLASS zcl_zork_00_stack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING io_memory TYPE REF TO zcl_zork_00_memory,

      " Call stack management
      push_frame
        IMPORTING iv_return_pc   TYPE i
                  iv_result_var  TYPE i     " -1 = discard result
                  iv_num_locals  TYPE i     " Number of locals (0-15)
                  iv_args        TYPE string " Arguments as hex (4 chars each)
                  iv_arg_count   TYPE i,

      pop_frame
        EXPORTING ev_return_pc   TYPE i
                  ev_result_var  TYPE i,

      get_frame_depth
        RETURNING VALUE(rv_depth) TYPE i,

      " Local variable access (1-15)
      get_local
        IMPORTING iv_num        TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      set_local
        IMPORTING iv_num TYPE i
                  iv_val TYPE i,

      " Global variable access (16-255)
      get_global
        IMPORTING iv_num        TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      set_global
        IMPORTING iv_num TYPE i
                  iv_val TYPE i,

      " Unified variable access (0=stack, 1-15=local, 16-255=global)
      get_variable
        IMPORTING iv_var        TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      set_variable
        IMPORTING iv_var TYPE i
                  iv_val TYPE i,

      " Evaluation stack (per frame)
      push_stack
        IMPORTING iv_val TYPE i,

      pop_stack
        RETURNING VALUE(rv_val) TYPE i,

      peek_stack
        RETURNING VALUE(rv_val) TYPE i,

      get_stack_depth
        RETURNING VALUE(rv_depth) TYPE i,

      " Reset to initial state
      reset.

  PRIVATE SECTION.

    " Frame storage as concatenated hex strings
    " Each frame: return_pc(4) + result_var(2) + num_locals(2) +
    "             locals(30 = 15 words) + stack_ptr(4) + eval_stack(variable)
    " For simplicity, we store frames in a simple format:
    "   - Current frame is in instance variables (hot)
    "   - Older frames stored in mv_frames string

    DATA:
      mo_memory       TYPE REF TO zcl_zork_00_memory,

      " Current frame (hot path)
      mv_return_pc    TYPE i,
      mv_result_var   TYPE i,
      mv_num_locals   TYPE i,
      mv_locals       TYPE string,    " 30 hex chars = 15 words
      mv_eval_stack   TYPE string,    " Variable length (4 chars per word)
      mv_stack_ptr    TYPE i,

      " Previous frames (cold storage)
      mv_frames       TYPE string,    " Serialized frames
      mv_frame_count  TYPE i.

    METHODS:
      " Serialize current frame to string
      serialize_frame
        RETURNING VALUE(rv_frame) TYPE string,

      " Deserialize frame from string
      deserialize_frame
        IMPORTING iv_frame TYPE string,

      " Hex conversion helpers
      word_to_hex
        IMPORTING iv_word       TYPE i
        RETURNING VALUE(rv_hex) TYPE string,

      hex_to_word
        IMPORTING iv_hex        TYPE string
        RETURNING VALUE(rv_word) TYPE i,

      byte_to_hex
        IMPORTING iv_byte       TYPE i
        RETURNING VALUE(rv_hex) TYPE string,

      hex_to_byte
        IMPORTING iv_hex        TYPE string
        RETURNING VALUE(rv_byte) TYPE i.

ENDCLASS.


CLASS zcl_zork_00_stack IMPLEMENTATION.

  METHOD constructor.
    mo_memory = io_memory.
    reset( ).
  ENDMETHOD.


  METHOD reset.
    " Initialize to empty state
    mv_return_pc = 0.
    mv_result_var = -1.
    mv_num_locals = 0.
    mv_locals = '000000000000000000000000000000000000000000000000000000000000'.  " 15 words = 60 hex chars
    mv_eval_stack = ''.
    mv_stack_ptr = 0.
    mv_frames = ''.
    mv_frame_count = 0.
  ENDMETHOD.


  METHOD push_frame.
    DATA: lv_arg_idx     TYPE i,
          lv_arg_offset  TYPE i,
          lv_arg_hex     TYPE string,
          lv_local_off   TYPE i,
          lv_before      TYPE string,
          lv_after       TYPE string,
          lv_len         TYPE i,
          lv_after_start TYPE i,
          lv_rem         TYPE i.

    " Save current frame first (if not the initial frame)
    IF mv_frame_count > 0 OR mv_stack_ptr > 0 OR mv_num_locals > 0.
      mv_frames = mv_frames && serialize_frame( ).
    ENDIF.

    " Initialize new frame
    mv_return_pc = iv_return_pc.
    mv_result_var = iv_result_var.
    mv_num_locals = iv_num_locals.

    " Initialize locals to zeros (15 words = 60 hex chars)
    mv_locals = '000000000000000000000000000000000000000000000000000000000000'.

    " Copy arguments to locals (args go to locals 1-n)
    lv_arg_idx = 0.
    WHILE lv_arg_idx < iv_arg_count AND lv_arg_idx < iv_num_locals.
      lv_arg_offset = lv_arg_idx * 4.
      IF lv_arg_offset + 4 <= strlen( iv_args ).
        lv_arg_hex = iv_args+lv_arg_offset(4).
        " Store in local (local 1 = index 0 in storage)
        lv_local_off = lv_arg_idx * 4.
        " Replace in locals string
        lv_len = strlen( mv_locals ).

        IF lv_local_off > 0.
          lv_before = mv_locals+0(lv_local_off).
        ELSE.
          lv_before = ''.
        ENDIF.

        lv_after_start = lv_local_off + 4.
        IF lv_after_start < lv_len.
          lv_rem = lv_len - lv_after_start.
          lv_after = mv_locals+lv_after_start(lv_rem).
        ELSE.
          lv_after = ''.
        ENDIF.

        mv_locals = lv_before && lv_arg_hex && lv_after.
      ENDIF.
      lv_arg_idx = lv_arg_idx + 1.
    ENDWHILE.

    " Empty evaluation stack for new frame
    mv_eval_stack = ''.
    mv_stack_ptr = 0.

    mv_frame_count = mv_frame_count + 1.
  ENDMETHOD.


  METHOD pop_frame.
    IF mv_frame_count <= 0.
      " No frame to pop - return defaults
      ev_return_pc = 0.
      ev_result_var = -1.
      RETURN.
    ENDIF.

    " Save return info
    ev_return_pc = mv_return_pc.
    ev_result_var = mv_result_var.

    mv_frame_count = mv_frame_count - 1.

    " Restore previous frame if any
    IF mv_frame_count > 0.
      " Each serialized frame is 78 hex chars:
      " return_pc(4) + result_var(4) + num_locals(2) + locals(60) + stack_ptr(4) + stack_len(4)
      " = 78 chars total

      DATA(lv_frames_len) = strlen( mv_frames ).
      IF lv_frames_len >= 78.
        " Fixed frame size without eval stack preservation
        DATA(lv_frame_size) = 78.  " 4+4+2+60+4+4

        " Pop last frame
        DATA(lv_start) = lv_frames_len - lv_frame_size.
        IF lv_start >= 0.
          DATA(lv_frame) = mv_frames+lv_start(lv_frame_size).
          mv_frames = mv_frames+0(lv_start).
          deserialize_frame( lv_frame ).
        ENDIF.
      ENDIF.
    ELSE.
      " Reset to empty state
      reset( ).
    ENDIF.
  ENDMETHOD.


  METHOD serialize_frame.
    " Serialize current frame to hex string
    " Format: return_pc(4) + result_var(4) + num_locals(2) + locals(30) + stack_ptr(4) + stack_len(4)
    rv_frame = word_to_hex( mv_return_pc ).

    " Result var can be -1, encode as unsigned
    DATA(lv_result) = mv_result_var.
    IF lv_result < 0.
      lv_result = lv_result + 65536.
    ENDIF.
    rv_frame = rv_frame && word_to_hex( lv_result ).

    rv_frame = rv_frame && byte_to_hex( mv_num_locals ).

    " Ensure locals is 60 chars (15 words * 4 hex chars)
    DATA(lv_locals) = mv_locals.
    WHILE strlen( lv_locals ) < 60.
      lv_locals = lv_locals && '0000'.
    ENDWHILE.
    rv_frame = rv_frame && lv_locals+0(60).

    rv_frame = rv_frame && word_to_hex( mv_stack_ptr ).

    " For simplicity, don't preserve eval stack on frame save
    " Real Z-machine would need this for complex programs
    rv_frame = rv_frame && word_to_hex( 0 ).  " stack_len = 0
  ENDMETHOD.


  METHOD deserialize_frame.
    " Deserialize frame from hex string
    " Format: return_pc(4) + result_var(4) + num_locals(2) + locals(60) + stack_ptr(4) + stack_len(4)
    " Total: 78 chars minimum
    IF strlen( iv_frame ) < 78.
      RETURN.
    ENDIF.

    mv_return_pc = hex_to_word( iv_frame+0(4) ).

    DATA(lv_result) = hex_to_word( iv_frame+4(4) ).
    IF lv_result >= 32768.  " Treat as signed
      lv_result = lv_result - 65536.
    ENDIF.
    mv_result_var = lv_result.

    mv_num_locals = hex_to_byte( iv_frame+8(2) ).
    mv_locals = iv_frame+10(60).
    mv_stack_ptr = hex_to_word( iv_frame+70(4) ).

    " Eval stack not preserved in this simple implementation
    mv_eval_stack = ''.
  ENDMETHOD.


  METHOD get_frame_depth.
    rv_depth = mv_frame_count.
  ENDMETHOD.


  METHOD get_local.
    " Get local variable (1-15)
    DATA: lv_offset TYPE i,
          lv_hex    TYPE string.

    IF iv_num < 1 OR iv_num > mv_num_locals OR iv_num > 15.
      rv_val = 0.
      RETURN.
    ENDIF.

    " Local n is at offset (n-1)*4 in locals string
    lv_offset = ( iv_num - 1 ) * 4.
    IF lv_offset + 4 <= strlen( mv_locals ).
      lv_hex = mv_locals+lv_offset(4).
      rv_val = hex_to_word( lv_hex ).
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.


  METHOD set_local.
    " Set local variable (1-15)
    DATA: lv_offset      TYPE i,
          lv_hex         TYPE string,
          lv_before      TYPE string,
          lv_after       TYPE string,
          lv_len         TYPE i,
          lv_after_start TYPE i,
          lv_rem         TYPE i.

    IF iv_num < 1 OR iv_num > 15.
      RETURN.
    ENDIF.

    lv_offset = ( iv_num - 1 ) * 4.
    lv_hex = word_to_hex( iv_val ).

    " Ensure locals string is long enough
    WHILE strlen( mv_locals ) < lv_offset + 4.
      mv_locals = mv_locals && '0000'.
    ENDWHILE.

    lv_len = strlen( mv_locals ).

    " Build before part
    IF lv_offset > 0.
      lv_before = mv_locals+0(lv_offset).
    ELSE.
      lv_before = ''.
    ENDIF.

    " Build after part
    lv_after_start = lv_offset + 4.
    IF lv_after_start < lv_len.
      lv_rem = lv_len - lv_after_start.
      lv_after = mv_locals+lv_after_start(lv_rem).
    ELSE.
      lv_after = ''.
    ENDIF.

    mv_locals = lv_before && lv_hex && lv_after.
  ENDMETHOD.


  METHOD get_global.
    " Get global variable (16-255)
    " Globals stored in memory at globals_addr + 2*(var-16)
    IF iv_num < 16 OR iv_num > 255.
      rv_val = 0.
      RETURN.
    ENDIF.

    DATA(lv_addr) = mo_memory->get_globals_addr( ) + ( iv_num - 16 ) * 2.
    rv_val = mo_memory->read_word( lv_addr ).
  ENDMETHOD.


  METHOD set_global.
    " Set global variable (16-255)
    IF iv_num < 16 OR iv_num > 255.
      RETURN.
    ENDIF.

    DATA(lv_addr) = mo_memory->get_globals_addr( ) + ( iv_num - 16 ) * 2.
    mo_memory->write_word( iv_addr = lv_addr iv_val = iv_val ).
  ENDMETHOD.


  METHOD get_variable.
    " Unified variable access
    " 0 = stack (pop), 1-15 = local, 16-255 = global
    IF iv_var = 0.
      rv_val = pop_stack( ).
    ELSEIF iv_var >= 1 AND iv_var <= 15.
      rv_val = get_local( iv_var ).
    ELSE.
      rv_val = get_global( iv_var ).
    ENDIF.
  ENDMETHOD.


  METHOD set_variable.
    " Unified variable access
    " 0 = stack (push), 1-15 = local, 16-255 = global
    IF iv_var = 0.
      push_stack( iv_val ).
    ELSEIF iv_var >= 1 AND iv_var <= 15.
      set_local( iv_num = iv_var iv_val = iv_val ).
    ELSE.
      set_global( iv_num = iv_var iv_val = iv_val ).
    ENDIF.
  ENDMETHOD.


  METHOD push_stack.
    " Push value onto evaluation stack
    DATA(lv_hex) = word_to_hex( iv_val ).
    mv_eval_stack = mv_eval_stack && lv_hex.
    mv_stack_ptr = mv_stack_ptr + 1.
  ENDMETHOD.


  METHOD pop_stack.
    " Pop value from evaluation stack
    IF mv_stack_ptr <= 0.
      rv_val = 0.
      RETURN.
    ENDIF.

    " Get last word (4 chars)
    DATA(lv_len) = strlen( mv_eval_stack ).
    IF lv_len >= 4.
      DATA(lv_start) = lv_len - 4.
      DATA(lv_hex) = mv_eval_stack+lv_start(4).
      rv_val = hex_to_word( lv_hex ).

      " Remove from stack
      IF lv_start > 0.
        mv_eval_stack = mv_eval_stack+0(lv_start).
      ELSE.
        mv_eval_stack = ''.
      ENDIF.
      mv_stack_ptr = mv_stack_ptr - 1.
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.


  METHOD peek_stack.
    " Peek at top of evaluation stack (don't pop)
    IF mv_stack_ptr <= 0.
      rv_val = 0.
      RETURN.
    ENDIF.

    DATA(lv_len) = strlen( mv_eval_stack ).
    IF lv_len >= 4.
      DATA(lv_start) = lv_len - 4.
      DATA(lv_hex) = mv_eval_stack+lv_start(4).
      rv_val = hex_to_word( lv_hex ).
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.


  METHOD get_stack_depth.
    rv_depth = mv_stack_ptr.
  ENDMETHOD.


  METHOD word_to_hex.
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


  METHOD hex_to_word.
    IF strlen( iv_hex ) >= 4.
      DATA(lv_hi) = hex_to_byte( iv_hex+0(2) ).
      DATA(lv_lo) = hex_to_byte( iv_hex+2(2) ).
      rv_word = lv_hi * 256 + lv_lo.
    ELSE.
      rv_word = 0.
    ENDIF.
  ENDMETHOD.


  METHOD byte_to_hex.
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
