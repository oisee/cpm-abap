CLASS zcl_hobbit_emulator DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_tap_block,
             flag   TYPE i,
             data   TYPE xstring,
             param1 TYPE i,
             param2 TYPE i,
             name   TYPE string,
           END OF ts_tap_block,
           tt_tap_blocks TYPE STANDARD TABLE OF ts_tap_block WITH EMPTY KEY,
           tt_locations TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    CONSTANTS: c_entry_point    TYPE i VALUE 27648,  " 0x6C00
               c_print_char     TYPE i VALUE 34426,  " 0x867A
               c_print_prop     TYPE i VALUE 34761,  " 0x87C9
               c_print_newline  TYPE i VALUE 34179,  " 0x8583 - Print newline routine
               c_print_msg      TYPE i VALUE 29405,  " 0x72DD - PrintMsg routine
               c_get_key        TYPE i VALUE 35731,  " 0x8B93
               c_draw_routine   TYPE i VALUE 32632,  " 0x7F78
               c_initial_wait   TYPE i VALUE 27757,  " 0x6C6D
               c_wait_key2      TYPE i VALUE 38554,  " 0x969A
               c_graphics_table TYPE i VALUE 52224.  " 0xCC00

    METHODS constructor.
    METHODS load_tap IMPORTING iv_data TYPE xstring.
    METHODS run IMPORTING iv_max_cycles TYPE i DEFAULT 10000000
                RETURNING VALUE(rv_output) TYPE string.
    METHODS step RETURNING VALUE(rv_continue) TYPE abap_bool.
    METHODS provide_input IMPORTING iv_text TYPE string.
    METHODS is_running RETURNING VALUE(rv_running) TYPE abap_bool.
    METHODS is_waiting_input RETURNING VALUE(rv_waiting) TYPE abap_bool.
    METHODS get_output RETURNING VALUE(rv_output) TYPE string.
    METHODS get_graphics_svg RETURNING VALUE(rv_svg) TYPE string.
    METHODS has_pending_graphics RETURNING VALUE(rv_pending) TYPE abap_bool.
    METHODS clear_pending_graphics.
    " Graphics mode control via port I/O (port 0xFB)
    METHODS has_gfx_mode_change RETURNING VALUE(rv_changed) TYPE abap_bool.
    METHODS get_gfx_mode_message RETURNING VALUE(rv_msg) TYPE string.
    " GFX viewer mode - render graphics for specific location
    METHODS render_location_gfx
      IMPORTING iv_loc TYPE i
      RETURNING VALUE(rv_svg) TYPE string.
    METHODS get_gfx_locations
      RETURNING VALUE(rt_locs) TYPE tt_locations.
    METHODS set_debug_mode IMPORTING iv_debug TYPE abap_bool.

  PRIVATE SECTION.
    DATA mo_cpu TYPE REF TO zcl_cpu_z80.
    DATA mo_core TYPE REF TO zif_cpu_z80_core.
    DATA mo_bus TYPE REF TO zif_cpu_z80_bus.
    DATA mo_hobbit_bus TYPE REF TO zcl_hobbit_bus.
    DATA mv_running TYPE abap_bool.
    DATA mv_waiting_input TYPE abap_bool.
    DATA mv_output TYPE string.
    DATA mv_input_queue TYPE string.
    DATA mt_auto_commands TYPE string_table.
    DATA mt_pixels TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA mv_pending_graphics TYPE abap_bool.
    DATA mv_current_location TYPE i.
    DATA mv_pen_x TYPE i.
    DATA mv_pen_y TYPE i.
    DATA mv_hook_hit TYPE abap_bool.
    DATA mv_cnt_print_char TYPE i.
    DATA mv_cnt_print_prop TYPE i.
    DATA mv_cnt_print_newline TYPE i.
    DATA mv_cnt_print_msg TYPE i.
    DATA mv_cnt_get_key TYPE i.
    DATA mv_cnt_draw TYPE i.
    DATA mv_cnt_wait TYPE i.
    DATA mv_debug_chars TYPE string.
    DATA mv_first_char_cycle TYPE i.
    DATA mv_cycle_counter TYPE i.
    DATA mv_last_pc TYPE i.
    DATA mv_last_pc_count TYPE i.
    DATA mv_stuck_pc TYPE i.
    DATA mv_stuck_count TYPE i.
    DATA mv_leading_spaces TYPE i.
    DATA mv_skip_leading_spaces TYPE abap_bool.
    DATA mv_current_column TYPE i.
    DATA mv_consecutive_newlines TYPE i.
    DATA mt_exec_count TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA mt_pc_trace TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA mv_trace_idx TYPE i.
    DATA mv_trace_enabled TYPE abap_bool.
    DATA mv_debug_mode TYPE abap_bool.
    DATA mv_last_was_plus TYPE abap_bool.  " Track consecutive '+' for echo

    METHODS parse_tap IMPORTING iv_data TYPE xstring
                      RETURNING VALUE(rt_blocks) TYPE tt_tap_blocks.
    METHODS setup_rom_stubs.
    METHODS check_hooks RETURNING VALUE(rv_handled) TYPE abap_bool.
    METHODS hook_print_char.
    METHODS hook_print_prop.
    METHODS hook_print_newline.
    METHODS hook_print_msg.
    METHODS hook_get_key.
    METHODS hook_draw.
    METHODS hook_wait.
    METHODS do_ret.
    METHODS render_graphics IMPORTING iv_loc TYPE i.
    METHODS find_gfx_addr IMPORTING iv_loc TYPE i RETURNING VALUE(rv_addr) TYPE i.
    METHODS parse_gfx_cmds IMPORTING iv_addr TYPE i.
    METHODS draw_line IMPORTING iv_x TYPE i iv_y TYPE i iv_d TYPE i iv_n TYPE i iv_m TYPE i
                      EXPORTING ev_x TYPE i ev_y TYPE i.
    METHODS set_pixel IMPORTING iv_x TYPE i iv_y TYPE i.
    METHODS clear_screen.
    METHODS flood_fill IMPORTING iv_x TYPE i iv_y TYPE i.
    METHODS output_char IMPORTING iv_char TYPE i.
    METHODS read_byte IMPORTING iv_addr TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS write_byte IMPORTING iv_addr TYPE i iv_val TYPE i.
ENDCLASS.

CLASS zcl_hobbit_emulator IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_hobbit_bus.
    mo_bus = mo_hobbit_bus.
    CREATE OBJECT mo_cpu EXPORTING io_bus = mo_bus.
    mo_core = mo_cpu.
    mv_running = abap_false.
    mv_waiting_input = abap_false.
    mv_pending_graphics = abap_false.
    mv_current_location = 0.
    DO 32768 TIMES.
      APPEND 0 TO mt_pixels.
    ENDDO.
    DO 65536 TIMES.
      APPEND 0 TO mt_exec_count.
    ENDDO.
    DO 100 TIMES.
      APPEND 0 TO mt_pc_trace.
    ENDDO.
    mv_trace_idx = 1.
    mv_trace_enabled = abap_true.
  ENDMETHOD.

  METHOD load_tap.
    DATA lt_blocks TYPE tt_tap_blocks.
    DATA ls_block TYPE ts_tap_block.
    DATA lv_loaded TYPE i.
    mv_output = |TAP data size: { xstrlen( iv_data ) } bytes| && cl_abap_char_utilities=>newline.
    lt_blocks = parse_tap( iv_data ).
    mv_output = |TAP blocks: { lines( lt_blocks ) }| && cl_abap_char_utilities=>newline.
    LOOP AT lt_blocks INTO ls_block.
      mv_output = mv_output && |Block { sy-tabix }: flag={ ls_block-flag } len={ xstrlen( ls_block-data ) } addr={ ls_block-param1 } name={ ls_block-name }| && cl_abap_char_utilities=>newline.
      IF ls_block-flag = 255 AND xstrlen( ls_block-data ) > 10 AND ls_block-param1 > 0.
        mo_bus->load( iv_addr = ls_block-param1 iv_data = ls_block-data ).
        lv_loaded = lv_loaded + 1.
      ENDIF.
    ENDLOOP.
    setup_rom_stubs( ).
    mv_output = mv_output && |Loaded { lv_loaded } blocks, starting at PC={ c_entry_point } (0x6C00) SP=65280 (0xFF00)| && cl_abap_char_utilities=>newline.
    mo_core->set_pc( c_entry_point ).
    mo_core->set_sp( 65280 ).
    mv_running = abap_true.
    mv_leading_spaces = 0.
    mv_skip_leading_spaces = abap_true.
    mv_current_column = 0.
    mv_consecutive_newlines = 0.
    mv_last_was_plus = abap_false.
  ENDMETHOD.

  METHOD parse_tap.
    DATA ls_block TYPE ts_tap_block.
    DATA lv_pos TYPE i VALUE 0.
    DATA lv_len TYPE i.
    DATA lv_block_len TYPE i.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_byte_int TYPE i.
    DATA lv_i TYPE i.
    DATA lv_ch TYPE c LENGTH 1.
    DATA lv_name TYPE string.
    lv_len = xstrlen( iv_data ).
    WHILE lv_pos < lv_len - 2.
      lv_byte = iv_data+lv_pos(1).
      lv_block_len = lv_byte.
      lv_pos = lv_pos + 1.
      lv_byte = iv_data+lv_pos(1).
      lv_block_len = lv_block_len + lv_byte * 256.
      lv_pos = lv_pos + 1.
      IF lv_block_len = 0 OR lv_pos + lv_block_len > lv_len.
        EXIT.
      ENDIF.
      lv_byte = iv_data+lv_pos(1).
      CLEAR ls_block.
      ls_block-flag = lv_byte.
      IF lv_byte = 0 AND lv_block_len = 19.
        lv_pos = lv_pos + 2.
        lv_name = ''.
        lv_i = 0.
        WHILE lv_i < 10.
          lv_byte = iv_data+lv_pos(1).
          lv_byte_int = lv_byte.
          IF lv_byte_int >= 32 AND lv_byte_int < 127.
            lv_ch = cl_abap_conv_in_ce=>uccpi( lv_byte_int ).
            lv_name = lv_name && lv_ch.
          ENDIF.
          lv_pos = lv_pos + 1.
          lv_i = lv_i + 1.
        ENDWHILE.
        ls_block-name = condense( lv_name ).
        lv_byte = iv_data+lv_pos(1).
        DATA lv_dlen TYPE i.
        lv_dlen = lv_byte.
        lv_pos = lv_pos + 1.
        lv_byte = iv_data+lv_pos(1).
        lv_dlen = lv_dlen + lv_byte * 256.
        lv_pos = lv_pos + 1.
        lv_byte = iv_data+lv_pos(1).
        ls_block-param1 = lv_byte.
        lv_pos = lv_pos + 1.
        lv_byte = iv_data+lv_pos(1).
        ls_block-param1 = ls_block-param1 + lv_byte * 256.
        lv_pos = lv_pos + 1.
        lv_byte = iv_data+lv_pos(1).
        ls_block-param2 = lv_byte.
        lv_pos = lv_pos + 1.
        lv_byte = iv_data+lv_pos(1).
        ls_block-param2 = ls_block-param2 + lv_byte * 256.
        lv_pos = lv_pos + 2.
      ELSE.
        DATA lv_ds TYPE i.
        DATA lv_dstart TYPE i.
        lv_dstart = lv_pos + 1.
        lv_ds = lv_block_len - 2.
        IF lv_ds > 0 AND lv_dstart + lv_ds <= lv_len.
          ls_block-data = iv_data+lv_dstart(lv_ds).
        ENDIF.
        DATA lv_pidx TYPE i.
        lv_pidx = lines( rt_blocks ).
        IF lv_pidx > 0.
          DATA ls_prev TYPE ts_tap_block.
          READ TABLE rt_blocks INTO ls_prev INDEX lv_pidx.
          IF ls_prev-flag = 0.
            ls_block-param1 = ls_prev-param1.
            ls_block-name = ls_prev-name.
          ENDIF.
        ENDIF.
        lv_pos = lv_pos + lv_block_len.
      ENDIF.
      APPEND ls_block TO rt_blocks.
    ENDWHILE.
  ENDMETHOD.

  METHOD setup_rom_stubs.
    write_byte( iv_addr = 0 iv_val = 201 ).
    write_byte( iv_addr = 8 iv_val = 201 ).
    write_byte( iv_addr = 16 iv_val = 201 ).
    write_byte( iv_addr = 24 iv_val = 201 ).
    write_byte( iv_addr = 32 iv_val = 201 ).
    write_byte( iv_addr = 40 iv_val = 201 ).
    write_byte( iv_addr = 48 iv_val = 201 ).
    write_byte( iv_addr = 56 iv_val = 251 ).
    write_byte( iv_addr = 57 iv_val = 201 ).
    write_byte( iv_addr = 102 iv_val = 201 ).
  ENDMETHOD.

  METHOD run.
    DATA lv_cycles TYPE i VALUE 0.
    DATA lv_pc TYPE i.
    DATA lv_idle_cycles TYPE i VALUE 0.
    CONSTANTS lc_idle_limit TYPE i VALUE 0.
    mo_bus->clear_output( ).
    CLEAR: mv_cnt_print_char, mv_cnt_print_prop, mv_cnt_print_newline,
           mv_cnt_print_msg, mv_cnt_get_key, mv_cnt_draw, mv_cnt_wait,
           mv_debug_chars, mv_first_char_cycle,
           mv_last_pc, mv_last_pc_count, mv_stuck_pc, mv_stuck_count.
    mv_cycle_counter = 0.
    WHILE mv_running = abap_true AND lv_cycles < iv_max_cycles.
      IF mv_waiting_input = abap_true.
        EXIT.
      ENDIF.
      lv_pc = mo_core->get_pc( ).
      IF lv_pc < 256 AND lv_pc <> 0 AND lv_pc <> 8 AND lv_pc <> 16 AND lv_pc <> 24 AND
         lv_pc <> 32 AND lv_pc <> 40 AND lv_pc <> 48 AND lv_pc <> 56 AND lv_pc <> 57 AND lv_pc <> 102.
        mv_output = mv_output && |[CRASH] PC={ lv_pc } at cycle { lv_cycles }| && cl_abap_char_utilities=>newline.
      ENDIF.
      IF lv_pc >= 256 AND lv_pc < 16384.
        mv_output = mv_output && |[ROM ERROR] PC={ lv_pc } at cycle { lv_cycles }| && cl_abap_char_utilities=>newline.
        EXIT.
      ENDIF.
      mv_cycle_counter = lv_cycles.
      IF lv_pc = mv_last_pc.
        mv_last_pc_count = mv_last_pc_count + 1.
        IF mv_last_pc_count > mv_stuck_count.
          mv_stuck_pc = lv_pc.
          mv_stuck_count = mv_last_pc_count.
        ENDIF.
      ELSE.
        mv_last_pc = lv_pc.
        mv_last_pc_count = 1.
      ENDIF.
      IF step( ) = abap_false.
        EXIT.
      ENDIF.
      IF mv_hook_hit = abap_true.
        lv_idle_cycles = 0.
      ELSE.
        lv_idle_cycles = lv_idle_cycles + 1.
      ENDIF.
      IF lc_idle_limit > 0 AND lv_idle_cycles > lc_idle_limit.
        mv_output = mv_output && |[Idle timeout after { lv_cycles } cycles at PC={ lv_pc }]| && cl_abap_char_utilities=>newline.
        EXIT.
      ENDIF.
      lv_cycles = lv_cycles + 1.
    ENDWHILE.
    lv_pc = mo_core->get_pc( ).
    IF mv_debug_mode = abap_true.
      IF mv_waiting_input = abap_false AND lv_cycles >= iv_max_cycles.
        mv_output = mv_output && |[Max cycles { lv_cycles } at PC={ lv_pc }]| && cl_abap_char_utilities=>newline.
      ENDIF.
      mv_output = mv_output && |[Hooks: char={ mv_cnt_print_char } prop={ mv_cnt_print_prop } nl={ mv_cnt_print_newline }| &&
                               | msg={ mv_cnt_print_msg } key={ mv_cnt_get_key } draw={ mv_cnt_draw } wait={ mv_cnt_wait }]| &&
                               cl_abap_char_utilities=>newline.
      mv_output = mv_output && |[Raw: { mv_debug_chars }]| && cl_abap_char_utilities=>newline.
      mv_output = mv_output && |[First char at cycle { mv_first_char_cycle }]| && cl_abap_char_utilities=>newline.
      IF mv_stuck_count > 1000.
        mv_output = mv_output && |[Stuck at PC={ mv_stuck_pc } hit { mv_stuck_count } times]| && cl_abap_char_utilities=>newline.
      ENDIF.
    ENDIF.
    rv_output = mv_output.
  ENDMETHOD.

  METHOD step.
    IF check_hooks( ) = abap_true.
      rv_continue = abap_true.
      RETURN.
    ENDIF.
    mo_cpu->step( ).
    rv_continue = abap_true.
  ENDMETHOD.

  METHOD check_hooks.
    DATA lv_pc TYPE i.
    lv_pc = mo_core->get_pc( ).
    mv_hook_hit = abap_false.
    CASE lv_pc.
      WHEN c_print_char.
        mv_cnt_print_char = mv_cnt_print_char + 1.
        IF mv_first_char_cycle = 0. mv_first_char_cycle = mv_cycle_counter. ENDIF.
        hook_print_char( ). rv_handled = abap_true. mv_hook_hit = abap_true.
      WHEN c_print_prop.
        mv_cnt_print_prop = mv_cnt_print_prop + 1.
        hook_print_prop( ). rv_handled = abap_true. mv_hook_hit = abap_true.
      WHEN c_print_newline.
        mv_cnt_print_newline = mv_cnt_print_newline + 1.
        hook_print_newline( ). rv_handled = abap_false. mv_hook_hit = abap_true.
      WHEN c_print_msg.
        mv_cnt_print_msg = mv_cnt_print_msg + 1.
        hook_print_msg( ). rv_handled = abap_false. mv_hook_hit = abap_true.
      WHEN c_get_key.
        mv_cnt_get_key = mv_cnt_get_key + 1.
        hook_get_key( ). rv_handled = abap_true. mv_hook_hit = abap_true.
      WHEN c_draw_routine.
        mv_cnt_draw = mv_cnt_draw + 1.
        hook_draw( ). rv_handled = abap_true. mv_hook_hit = abap_true.
      WHEN c_initial_wait OR c_wait_key2.
        mv_cnt_wait = mv_cnt_wait + 1.
        hook_wait( ). rv_handled = abap_true. mv_hook_hit = abap_true.
      WHEN OTHERS.
        rv_handled = abap_false.
    ENDCASE.
  ENDMETHOD.

  METHOD hook_print_char.
    output_char( mo_core->get_a( ) ).
    do_ret( ).
  ENDMETHOD.

  METHOD hook_print_prop.
    DATA lv_char TYPE i.
    DATA lv_ch TYPE c LENGTH 1.
    lv_char = mo_core->get_a( ).
    IF lv_char >= 32 AND lv_char < 127.
      IF lv_char = 32.
        mv_leading_spaces = mv_leading_spaces + 1.
        IF mv_skip_leading_spaces = abap_true.
        ELSEIF mv_leading_spaces >= 8 AND mv_current_column > 0.
          mv_consecutive_newlines = mv_consecutive_newlines + 1.
          IF mv_consecutive_newlines <= 2.
            mv_output = mv_output && cl_abap_char_utilities=>newline.
          ENDIF.
          mv_current_column = 0.
          mv_leading_spaces = 0.
          mv_skip_leading_spaces = abap_true.
        ENDIF.
      ELSE.
        IF mv_leading_spaces >= 1 AND mv_leading_spaces < 8 AND mv_skip_leading_spaces = abap_false.
          mv_output = mv_output && | |.
          mv_current_column = mv_current_column + 1.
        ENDIF.
        mv_leading_spaces = 0.
        mv_skip_leading_spaces = abap_false.
        " Filter out '+' which is a formatting artifact
        IF lv_char <> 43.
          lv_ch = cl_abap_conv_in_ce=>uccpi( lv_char ).
          mv_output = mv_output && lv_ch.
          mv_current_column = mv_current_column + 1.
          mv_consecutive_newlines = 0.
        ENDIF.
      ENDIF.
    ELSEIF lv_char = 13 OR lv_char = 10.
      mv_consecutive_newlines = mv_consecutive_newlines + 1.
      IF mv_consecutive_newlines <= 2.
        mv_output = mv_output && cl_abap_char_utilities=>newline.
      ENDIF.
      mv_current_column = 0.
      mv_leading_spaces = 0.
      mv_skip_leading_spaces = abap_true.
    ELSEIF lv_char = 127.
      mv_leading_spaces = 0.
      mv_output = mv_output && |(C)|.
      mv_current_column = mv_current_column + 3.
    ENDIF.
    do_ret( ).
  ENDMETHOD.

  METHOD hook_print_newline.
    " Output newline - this is the main source of line breaks
    " Only output if we have content on the line (like Python's logic)
    IF mv_current_column > 0.
      mv_output = mv_output && cl_abap_char_utilities=>newline.
      mv_consecutive_newlines = 1.
    ELSE.
      " Already at start of line - check consecutive counter
      mv_consecutive_newlines = mv_consecutive_newlines + 1.
      IF mv_consecutive_newlines <= 2.
        mv_output = mv_output && cl_abap_char_utilities=>newline.
      ENDIF.
    ENDIF.
    mv_current_column = 0.
    mv_leading_spaces = 0.
    mv_skip_leading_spaces = abap_true.
    " Don't call do_ret - let original code run for game state updates
  ENDMETHOD.

  METHOD hook_print_msg.
  ENDMETHOD.

  METHOD hook_get_key.
    DATA lv_char TYPE i.
    IF lines( mt_auto_commands ) > 0.
      DATA lv_cmd TYPE string.
      READ TABLE mt_auto_commands INTO lv_cmd INDEX 1.
      DELETE mt_auto_commands INDEX 1.
      mv_input_queue = lv_cmd && cl_abap_char_utilities=>cr_lf.
    ENDIF.
    IF strlen( mv_input_queue ) > 0.
      DATA lv_ch TYPE c LENGTH 1.
      lv_ch = mv_input_queue+0(1).
      mv_input_queue = mv_input_queue+1.
      lv_char = cl_abap_conv_out_ce=>uccpi( lv_ch ).
      IF lv_char = 10.
        lv_char = 13.
      ENDIF.
    ELSE.
      mv_waiting_input = abap_true.
      RETURN.
    ENDIF.
    mo_core->set_a( lv_char ).
    do_ret( ).
  ENDMETHOD.

  METHOD hook_draw.
    DATA lv_loc TYPE i.
    DATA lv_gfx_enabled TYPE i.
    DATA lv_gfx_addr TYPE i.
    lv_gfx_enabled = read_byte( 46855 ).
    lv_loc = mo_core->get_a( ).
    lv_gfx_addr = find_gfx_addr( lv_loc ).
    IF mv_debug_mode = abap_true.
      mv_output = mv_output && |[DRAW:loc={ lv_loc } gfx={ lv_gfx_enabled } addr={ lv_gfx_addr }]|.
    ENDIF.
    IF lv_gfx_enabled <> 0 AND lv_loc > 0 AND lv_gfx_addr > 0.
      mv_current_location = lv_loc.
      mv_pending_graphics = abap_true.
    ENDIF.
    do_ret( ).
  ENDMETHOD.

  METHOD hook_wait.
    DATA lv_pc TYPE i.
    lv_pc = mo_core->get_pc( ).
    IF lv_pc = c_initial_wait.
      mo_core->set_pc( 27766 ).
    ELSEIF lv_pc = c_wait_key2.
      mo_core->set_pc( c_wait_key2 + 9 ).
    ENDIF.
  ENDMETHOD.

  METHOD do_ret.
    DATA lv_sp TYPE i.
    DATA lv_addr TYPE i.
    lv_sp = mo_core->get_sp( ).
    lv_addr = read_byte( lv_sp ) + read_byte( lv_sp + 1 ) * 256.
    mo_core->set_sp( lv_sp + 2 ).
    mo_core->set_pc( lv_addr ).
  ENDMETHOD.

  METHOD output_char.
    " Match Python _hook_print_char logic exactly
    DATA lv_ch TYPE c LENGTH 1.
    IF strlen( mv_debug_chars ) < 200.
      IF iv_char < 32.
        mv_debug_chars = mv_debug_chars && |<{ iv_char }>|.
      ELSEIF iv_char >= 32 AND iv_char < 127.
        lv_ch = cl_abap_conv_in_ce=>uccpi( iv_char ).
        mv_debug_chars = mv_debug_chars && lv_ch.
      ELSE.
        mv_debug_chars = mv_debug_chars && |[{ iv_char }]|.
      ENDIF.
    ENDIF.
    IF iv_char = 13 OR iv_char = 10.
      " CR/LF - newline
      mv_consecutive_newlines = mv_consecutive_newlines + 1.
      IF mv_consecutive_newlines <= 2.
        mv_output = mv_output && cl_abap_char_utilities=>newline.
      ENDIF.
      mv_current_column = 0.
      mv_leading_spaces = 0.
      mv_skip_leading_spaces = abap_true.
    ELSEIF iv_char = 8.
      " Backspace - ignore for now
    ELSEIF iv_char = 32.
      " Space - use delayed space logic like Python
      mv_leading_spaces = mv_leading_spaces + 1.
      IF mv_skip_leading_spaces = abap_true.
        " Skip leading spaces
      ELSEIF mv_leading_spaces >= 8 AND mv_current_column > 0.
        " Column positioning - convert to newline
        mv_output = mv_output && cl_abap_char_utilities=>newline.
        mv_current_column = 0.
        mv_leading_spaces = 0.
        mv_skip_leading_spaces = abap_true.
      ENDIF.
      " else: accumulating spaces, wait to see if more follow
    ELSEIF iv_char > 32 AND iv_char < 127.
      " Non-space printable - output any pending single spaces first
      IF mv_leading_spaces >= 1 AND mv_leading_spaces < 8 AND mv_skip_leading_spaces = abap_false.
        mv_output = mv_output && | |.
        mv_current_column = mv_current_column + 1.
      ENDIF.
      mv_leading_spaces = 0.
      mv_skip_leading_spaces = abap_false.
      " Filter out '+' which is a formatting artifact
      IF iv_char <> 43.
        lv_ch = cl_abap_conv_in_ce=>uccpi( iv_char ).
        mv_output = mv_output && lv_ch.
        mv_current_column = mv_current_column + 1.
      ENDIF.
      mv_consecutive_newlines = 0.
    ELSEIF iv_char = 127.
      mv_leading_spaces = 0.
      mv_output = mv_output && |(C)|.
      mv_current_column = mv_current_column + 3.
    ELSEIF iv_char >= 128 AND iv_char <= 143.
      mv_leading_spaces = 0.
      mv_output = mv_output && |#|.
      mv_current_column = mv_current_column + 1.
    ELSEIF iv_char >= 144.
      mv_leading_spaces = 0.
      mv_output = mv_output && |?|.
      mv_current_column = mv_current_column + 1.
    ENDIF.
  ENDMETHOD.

  METHOD provide_input.
    mv_input_queue = mv_input_queue && iv_text.
    mv_waiting_input = abap_false.
  ENDMETHOD.

  METHOD is_running.
    rv_running = mv_running.
  ENDMETHOD.

  METHOD is_waiting_input.
    rv_waiting = mv_waiting_input.
  ENDMETHOD.

  METHOD get_output.
    rv_output = mv_output.
    CLEAR mv_output.
  ENDMETHOD.

  METHOD has_pending_graphics.
    rv_pending = mv_pending_graphics.
  ENDMETHOD.

  METHOD clear_pending_graphics.
    mv_pending_graphics = abap_false.
  ENDMETHOD.

  METHOD read_byte.
    rv_val = mo_bus->read_mem( iv_addr ).
  ENDMETHOD.

  METHOD write_byte.
    mo_bus->write_mem( iv_addr = iv_addr iv_val = iv_val ).
  ENDMETHOD.

  METHOD render_graphics.
    DATA lv_addr TYPE i.
    lv_addr = find_gfx_addr( iv_loc ).
    IF lv_addr > 0.
      clear_screen( ).
      parse_gfx_cmds( lv_addr ).
    ENDIF.
  ENDMETHOD.

  METHOD find_gfx_addr.
    DATA lv_addr TYPE i.
    DATA lv_entry_loc TYPE i.
    DATA lv_data_start TYPE i VALUE 52291.
    lv_addr = c_graphics_table.
    WHILE lv_addr < lv_data_start.
      lv_entry_loc = read_byte( lv_addr ).
      IF lv_entry_loc = 255.
        EXIT.
      ENDIF.
      IF lv_entry_loc = iv_loc.
        rv_addr = read_byte( lv_addr + 1 ) + read_byte( lv_addr + 2 ) * 256.
        RETURN.
      ENDIF.
      lv_addr = lv_addr + 3.
    ENDWHILE.
    rv_addr = 0.
  ENDMETHOD.

  METHOD parse_gfx_cmds.
    DATA lv_addr TYPE i.
    DATA lv_cmd TYPE i.
    DATA lv_x TYPE i.
    DATA lv_y TYPE i.
    DATA lv_nx TYPE i.
    DATA lv_ny TYPE i.
    DATA lv_cnt TYPE i VALUE 0.
    DATA lv_next_byte TYPE i.
    DATA lv_d TYPE i.
    DATA lv_n TYPE i.
    DATA lv_m TYPE i.
    lv_addr = iv_addr.
    mv_pen_x = 0.
    mv_pen_y = 0.
    lv_addr = lv_addr + 2.
    WHILE lv_cnt < 2000.
      lv_cmd = read_byte( lv_addr ).
      lv_addr = lv_addr + 1.
      lv_cnt = lv_cnt + 1.
      IF lv_cmd = 0.
        EXIT.
      ELSEIF lv_cmd = 8.
        mv_pen_x = read_byte( lv_addr ).
        lv_addr = lv_addr + 1.
        mv_pen_y = 127 - read_byte( lv_addr ).
        lv_addr = lv_addr + 1.
        set_pixel( iv_x = mv_pen_x iv_y = mv_pen_y ).
      ELSEIF lv_cmd > 127.
        lv_next_byte = read_byte( lv_addr ).
        lv_addr = lv_addr + 1.
        lv_d = lv_cmd MOD 8.
        lv_n = lv_next_byte MOD 64.
        lv_m = ( ( lv_cmd MOD 128 ) / 2 ) + ( lv_next_byte / 64 ).
        draw_line( EXPORTING iv_x = mv_pen_x iv_y = mv_pen_y iv_d = lv_d iv_n = lv_n iv_m = lv_m
                   IMPORTING ev_x = lv_nx ev_y = lv_ny ).
        mv_pen_x = lv_nx.
        mv_pen_y = lv_ny.
      ELSEIF lv_cmd > 63.
        lv_x = read_byte( lv_addr ).
        lv_addr = lv_addr + 1.
        lv_y = 127 - read_byte( lv_addr ).
        lv_addr = lv_addr + 1.
        flood_fill( iv_x = lv_x iv_y = lv_y ).
      ELSEIF lv_cmd > 31.
        lv_addr = lv_addr + 2.
        WHILE read_byte( lv_addr ) <> 255.
          lv_addr = lv_addr + 1.
        ENDWHILE.
        lv_addr = lv_addr + 1.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD draw_line.
    DATA lv_x TYPE i.
    DATA lv_y TYPE i.
    DATA lv_m TYPE i.
    DATA lv_m0 TYPE i.
    DATA lv_i TYPE i.
    lv_x = iv_x.
    lv_y = iv_y.
    lv_m = iv_m.
    lv_m0 = iv_m.
    IF iv_d MOD 2 = 1.
      lv_i = 0.
      WHILE lv_i <= iv_n.
        set_pixel( iv_x = lv_x iv_y = lv_y ).
        IF iv_d MOD 4 >= 2.
          IF lv_y < 127. lv_y = lv_y + 1. ELSE. EXIT. ENDIF.
        ELSE.
          IF lv_y > 0. lv_y = lv_y - 1. ELSE. EXIT. ENDIF.
        ENDIF.
        IF lv_m <= 0.
          lv_m = lv_m0.
          IF iv_d >= 4.
            IF lv_x > 0. lv_x = lv_x - 1. ELSE. EXIT. ENDIF.
          ELSE.
            IF lv_x < 255. lv_x = lv_x + 1. ELSE. EXIT. ENDIF.
          ENDIF.
        ELSE.
          lv_m = lv_m - 1.
        ENDIF.
        lv_i = lv_i + 1.
      ENDWHILE.
    ELSE.
      lv_i = 0.
      WHILE lv_i <= iv_n.
        set_pixel( iv_x = lv_x iv_y = lv_y ).
        IF iv_d >= 4.
          IF lv_x > 0. lv_x = lv_x - 1. ELSE. EXIT. ENDIF.
        ELSE.
          IF lv_x < 255. lv_x = lv_x + 1. ELSE. EXIT. ENDIF.
        ENDIF.
        IF lv_m <= 0.
          lv_m = lv_m0.
          IF iv_d MOD 4 >= 2.
            IF lv_y < 127. lv_y = lv_y + 1. ELSE. EXIT. ENDIF.
          ELSE.
            IF lv_y > 0. lv_y = lv_y - 1. ELSE. EXIT. ENDIF.
          ENDIF.
        ELSE.
          lv_m = lv_m - 1.
        ENDIF.
        lv_i = lv_i + 1.
      ENDWHILE.
    ENDIF.
    ev_x = lv_x.
    ev_y = lv_y.
  ENDMETHOD.

  METHOD set_pixel.
    DATA lv_idx TYPE i.
    IF iv_x >= 0 AND iv_x < 256 AND iv_y >= 0 AND iv_y < 128.
      lv_idx = iv_y * 256 + iv_x + 1.
      MODIFY mt_pixels INDEX lv_idx FROM 1.
    ENDIF.
  ENDMETHOD.

  METHOD clear_screen.
    DATA lv_i TYPE i.
    lv_i = 1.
    WHILE lv_i <= 32768.
      MODIFY mt_pixels INDEX lv_i FROM 0.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD flood_fill.
    TYPES: BEGIN OF ty_pt, x TYPE i, y TYPE i, END OF ty_pt.
    DATA lt_stk TYPE STANDARD TABLE OF ty_pt.
    DATA ls_pt TYPE ty_pt.
    DATA lv_idx TYPE i.
    DATA lv_val TYPE i.
    DATA lv_x TYPE i.
    DATA lv_y TYPE i.
    DATA lv_n TYPE i.
    DATA lv_iter TYPE i VALUE 0.
    IF iv_x < 0 OR iv_x >= 256 OR iv_y < 0 OR iv_y >= 128.
      RETURN.
    ENDIF.
    lv_idx = iv_y * 256 + iv_x + 1.
    READ TABLE mt_pixels INTO lv_val INDEX lv_idx.
    IF lv_val <> 0.
      RETURN.
    ENDIF.
    ls_pt-x = iv_x.
    ls_pt-y = iv_y.
    APPEND ls_pt TO lt_stk.
    WHILE lines( lt_stk ) > 0 AND lv_iter < 50000.
      lv_n = lines( lt_stk ).
      READ TABLE lt_stk INTO ls_pt INDEX lv_n.
      DELETE lt_stk INDEX lv_n.
      lv_x = ls_pt-x.
      lv_y = ls_pt-y.
      IF lv_x < 0 OR lv_x >= 256 OR lv_y < 0 OR lv_y >= 128.
        CONTINUE.
      ENDIF.
      lv_idx = lv_y * 256 + lv_x + 1.
      READ TABLE mt_pixels INTO lv_val INDEX lv_idx.
      IF lv_val <> 0.
        CONTINUE.
      ENDIF.
      MODIFY mt_pixels INDEX lv_idx FROM 1.
      ls_pt-x = lv_x + 1. ls_pt-y = lv_y. APPEND ls_pt TO lt_stk.
      ls_pt-x = lv_x - 1. ls_pt-y = lv_y. APPEND ls_pt TO lt_stk.
      ls_pt-x = lv_x. ls_pt-y = lv_y + 1. APPEND ls_pt TO lt_stk.
      ls_pt-x = lv_x. ls_pt-y = lv_y - 1. APPEND ls_pt TO lt_stk.
      lv_iter = lv_iter + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_graphics_svg.
    DATA lv_addr TYPE i.
    DATA lv_start TYPE i.
    DATA lv_op TYPE i.
    DATA lv_cnt TYPE i VALUE 0.
    DATA lv_xbyte TYPE x LENGTH 1.
    DATA lv_xstr TYPE xstring.
    lv_addr = find_gfx_addr( mv_current_location ).
    lv_start = lv_addr.
    IF mv_debug_mode = abap_true.
      mv_output = mv_output && |[SVG:loc={ mv_current_location } addr={ lv_addr }]|.
    ENDIF.
    IF lv_addr = 0.
      rv_svg = ''.
      RETURN.
    ENDIF.
    lv_addr = lv_addr + 2.
    WHILE lv_cnt < 2000.
      lv_op = read_byte( lv_addr ).
      lv_addr = lv_addr + 1.
      lv_cnt = lv_cnt + 1.
      IF lv_op = 0.
        EXIT.
      ELSEIF lv_op = 8.
        lv_addr = lv_addr + 2.
      ELSEIF lv_op > 127.
        lv_addr = lv_addr + 1.
      ELSEIF lv_op > 63.
        lv_addr = lv_addr + 2.
      ELSEIF lv_op > 31.
        lv_addr = lv_addr + 2.
        WHILE read_byte( lv_addr ) <> 255 AND lv_addr < lv_start + 2000.
          lv_addr = lv_addr + 1.
        ENDWHILE.
        lv_addr = lv_addr + 1.
      ENDIF.
    ENDWHILE.
    DATA lv_len TYPE i.
    DATA lv_i TYPE i.
    DATA lv_b TYPE i.
    lv_len = lv_addr - lv_start.
    lv_i = 0.
    WHILE lv_i < lv_len.
      lv_b = read_byte( lv_start + lv_i ).
      lv_xbyte = lv_b.
      lv_xstr = lv_xstr && lv_xbyte.
      lv_i = lv_i + 1.
    ENDWHILE.
    rv_svg = lv_xstr.
    IF mv_debug_mode = abap_true.
      mv_output = mv_output && |[HEX:{ xstrlen( lv_xstr ) }bytes]|.
    ENDIF.
  ENDMETHOD.

  METHOD has_gfx_mode_change.
    IF mo_hobbit_bus IS BOUND.
      rv_changed = mo_hobbit_bus->has_gfx_change( ).
    ELSE.
      rv_changed = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_gfx_mode_message.
    IF mo_hobbit_bus IS BOUND.
      rv_msg = mo_hobbit_bus->get_gfx_message( ).
    ELSE.
      rv_msg = ''.
    ENDIF.
  ENDMETHOD.

  METHOD render_location_gfx.
    DATA lv_addr TYPE i.
    DATA lv_start TYPE i.
    DATA lv_op TYPE i.
    DATA lv_cnt TYPE i VALUE 0.
    DATA lv_xbyte TYPE x LENGTH 1.
    DATA lv_xstr TYPE xstring.
    lv_addr = find_gfx_addr( iv_loc ).
    lv_start = lv_addr.
    IF lv_addr = 0.
      rv_svg = ''.
      RETURN.
    ENDIF.
    lv_addr = lv_addr + 2.
    WHILE lv_cnt < 2000.
      lv_op = read_byte( lv_addr ).
      lv_addr = lv_addr + 1.
      lv_cnt = lv_cnt + 1.
      IF lv_op = 0.
        EXIT.
      ELSEIF lv_op = 8.
        lv_addr = lv_addr + 2.
      ELSEIF lv_op > 127.
        lv_addr = lv_addr + 1.
      ELSEIF lv_op > 63.
        lv_addr = lv_addr + 2.
      ELSEIF lv_op > 31.
        lv_addr = lv_addr + 2.
        WHILE read_byte( lv_addr ) <> 255 AND lv_addr < lv_start + 2000.
          lv_addr = lv_addr + 1.
        ENDWHILE.
        lv_addr = lv_addr + 1.
      ENDIF.
    ENDWHILE.
    DATA lv_len TYPE i.
    DATA lv_i TYPE i.
    DATA lv_b TYPE i.
    lv_len = lv_addr - lv_start.
    lv_i = 0.
    WHILE lv_i < lv_len.
      lv_b = read_byte( lv_start + lv_i ).
      lv_xbyte = lv_b.
      lv_xstr = lv_xstr && lv_xbyte.
      lv_i = lv_i + 1.
    ENDWHILE.
    rv_svg = lv_xstr.
  ENDMETHOD.

  METHOD get_gfx_locations.
    DATA lv_addr TYPE i.
    DATA lv_loc TYPE i.
    DATA lv_data_start TYPE i VALUE 52291.
    lv_addr = c_graphics_table.
    WHILE lv_addr < lv_data_start.
      lv_loc = read_byte( lv_addr ).
      IF lv_loc = 255.
        EXIT.
      ENDIF.
      APPEND lv_loc TO rt_locs.
      lv_addr = lv_addr + 3.
    ENDWHILE.
  ENDMETHOD.

  METHOD set_debug_mode.
    mv_debug_mode = iv_debug.
  ENDMETHOD.

ENDCLASS.