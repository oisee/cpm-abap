CLASS zcl_cpm_00_apc DEFINITION
  PUBLIC
  INHERITING FROM cl_apc_wsp_ext_stateful_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_apc_wsp_extension~on_accept REDEFINITION.
    METHODS if_apc_wsp_extension~on_start REDEFINITION.
    METHODS if_apc_wsp_extension~on_message REDEFINITION.
    METHODS if_apc_wsp_extension~on_close REDEFINITION.
    METHODS if_apc_wsp_extension~on_error REDEFINITION.

    CLASS-METHODS get_connection_count RETURNING VALUE(rv_count) TYPE i.

  PRIVATE SECTION.
    CONSTANTS c_crlf TYPE string VALUE cl_abap_char_utilities=>cr_lf.
    CONSTANTS c_memory_id TYPE char32 VALUE 'CPM_APC_CONNECTIONS'.

    DATA mo_cpm TYPE REF TO zcl_cpm_emulator.
    DATA mo_hobbit TYPE REF TO zcl_hobbit_emulator.
    DATA mo_ccp TYPE REF TO zcl_cpm_00_ccp.
    DATA mv_session_id TYPE string.
    DATA mv_running_program TYPE abap_bool.
    DATA mv_spectrum_mode TYPE abap_bool.
    DATA mv_disk TYPE char30 VALUE 'A'.
    DATA mv_gfx_viewer TYPE abap_bool.
    DATA mt_gfx_locs TYPE zcl_hobbit_emulator=>tt_locations.
    DATA mv_gfx_idx TYPE i.

    CLASS-METHODS increment_connections.
    CLASS-METHODS decrement_connections.

    METHODS send_text IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager
                                iv_text TYPE string.
    METHODS run_program_and_output IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager.
    METHODS run_hobbit_and_output IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager.
    METHODS send_prompt IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager.
    METHODS get_welcome_banner RETURNING VALUE(rv_text) TYPE string.
    METHODS load_companion_files IMPORTING iv_program_name TYPE string.
    METHODS load_bin_file IMPORTING iv_name TYPE string RETURNING VALUE(rv_data) TYPE xstring.
    METHODS load_smw0_file IMPORTING iv_name TYPE string RETURNING VALUE(rv_data) TYPE xstring.
    METHODS parse_disk_command IMPORTING iv_text TYPE string RETURNING VALUE(rv_disk) TYPE char30.
    METHODS show_gfx_location IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager.
ENDCLASS.

CLASS zcl_cpm_00_apc IMPLEMENTATION.

  METHOD if_apc_wsp_extension~on_accept.
    e_connect_mode = co_connect_mode_accept.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_start.
    increment_connections( ).
    TRY.
        mv_session_id = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        mv_session_id = '00000000'.
    ENDTRY.
    DATA(lv_count) = get_connection_count( ).

    TRY.
        DATA(lo_context) = i_context.
        DATA(lv_protocol) = lo_context->get_initial_request( )->get_header_field( 'sec-websocket-protocol' ).
        IF lv_protocol IS NOT INITIAL AND lv_protocol CP 'disk-*'.
          mv_disk = to_upper( lv_protocol+5 ).
        ENDIF.
      CATCH cx_root.
        mv_disk = 'A'.
    ENDTRY.

    mo_ccp = NEW zcl_cpm_00_ccp( iv_disk = mv_disk ).
    mo_cpm = NEW zcl_cpm_emulator( ).
    mv_running_program = abap_false.
    mv_spectrum_mode = abap_false.
    mv_gfx_viewer = abap_false.

    send_text( i_message_manager = i_message_manager iv_text = get_welcome_banner( ) ).
    send_text( i_message_manager = i_message_manager iv_text = |Session: { mv_session_id(8) }  Active: { lv_count }{ c_crlf }| ).
    send_text( i_message_manager = i_message_manager iv_text = mo_ccp->get_welcome( ) ).
    send_prompt( i_message_manager ).
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_message.
    DATA: lv_output TYPE string,
          lv_run_program TYPE abap_bool,
          lv_program_data TYPE xstring,
          lv_program_name TYPE string,
          lv_program_args TYPE string,
          lv_spectrum_mode TYPE abap_bool,
          lv_gfx_viewer TYPE abap_bool,
          lv_debug_mode TYPE abap_bool.

    DATA(lv_input) = i_message->get_text( ).

    IF mo_ccp IS NOT BOUND.
      send_text( i_message_manager = i_message_manager iv_text = |ERROR: CCP not initialized!{ c_crlf }| ).
      send_prompt( i_message_manager ).
      RETURN.
    ENDIF.

    DATA(lv_new_disk) = parse_disk_command( lv_input ).
    IF lv_new_disk IS NOT INITIAL.
      mv_disk = lv_new_disk.
      mo_ccp->set_disk( mv_disk ).
      RETURN.
    ENDIF.

    IF mv_gfx_viewer = abap_true.
      DATA(lv_upper_input) = to_upper( lv_input ).
      IF lv_upper_input = 'Q' OR lv_input = '__EOF__'.
        mv_gfx_viewer = abap_false.
        mv_running_program = abap_false.
        send_text( i_message_manager = i_message_manager iv_text = |__GFXV__0| ).
        send_text( i_message_manager = i_message_manager iv_text = |{ c_crlf }GFX viewer closed{ c_crlf }| ).
        send_prompt( i_message_manager ).
        RETURN.
      ENDIF.
      IF lv_input = '__NEXT__' OR lv_upper_input = 'N' OR lv_upper_input = ''.
        mv_gfx_idx = mv_gfx_idx + 1.
        IF mv_gfx_idx > lines( mt_gfx_locs ).
          mv_gfx_idx = 1.
        ENDIF.
      ELSEIF lv_input = '__PREV__' OR lv_upper_input = 'P'.
        mv_gfx_idx = mv_gfx_idx - 1.
        IF mv_gfx_idx < 1.
          mv_gfx_idx = lines( mt_gfx_locs ).
        ENDIF.
      ENDIF.
      show_gfx_location( i_message_manager ).
      RETURN.
    ENDIF.

    IF lv_input = '__EOF__'.
      IF mv_running_program = abap_true.
        mv_running_program = abap_false.
        mv_spectrum_mode = abap_false.
        mo_cpm->reset( ).
        send_text( i_message_manager = i_message_manager iv_text = |{ c_crlf }^D - Program terminated{ c_crlf }| ).
      ENDIF.
      send_prompt( i_message_manager ).
      RETURN.
    ENDIF.

    IF mv_running_program = abap_true AND mv_spectrum_mode = abap_true.
      IF mo_hobbit IS BOUND AND mo_hobbit->is_waiting_input( ) = abap_true.
        " Send only CR (first char of cr_lf), not full CRLF - LF causes double-enter
        mo_hobbit->provide_input( lv_input && c_crlf+0(1) ).
        run_hobbit_and_output( i_message_manager ).
        RETURN.
      ENDIF.
    ENDIF.

    IF mv_running_program = abap_true AND mo_cpm->is_waiting_input( ) = abap_true.
      mo_cpm->provide_input( lv_input && cl_abap_char_utilities=>cr_lf ).
      run_program_and_output( i_message_manager ).
      RETURN.
    ENDIF.

    mo_ccp->process_command(
      EXPORTING iv_command = lv_input
      IMPORTING ev_output = lv_output
                ev_run_program = lv_run_program
                ev_program_data = lv_program_data
                ev_program_name = lv_program_name
                ev_program_args = lv_program_args
                ev_spectrum_mode = lv_spectrum_mode
                ev_gfx_viewer = lv_gfx_viewer
                ev_debug_mode = lv_debug_mode ).

    IF lv_output IS NOT INITIAL.
      send_text( i_message_manager = i_message_manager iv_text = lv_output ).
    ENDIF.

    IF lv_program_name = '__DUMP80__'.
      DATA(lv_dump) = mo_cpm->dump_memory( iv_addr = 128 iv_len = 32 ).
      send_text( i_message_manager = i_message_manager iv_text = lv_dump ).
      lv_dump = mo_cpm->dump_memory( iv_addr = 92 iv_len = 36 ).
      send_text( i_message_manager = i_message_manager iv_text = |FCB at 0x5C:{ c_crlf }{ lv_dump }| ).
      send_prompt( i_message_manager ).
      RETURN.
    ENDIF.

    IF lv_run_program = abap_true AND lv_program_data IS NOT INITIAL.
      mv_running_program = abap_true.

      IF lv_gfx_viewer = abap_true.
        mv_gfx_viewer = abap_true.
        mv_spectrum_mode = abap_true.
        mo_hobbit = NEW zcl_hobbit_emulator( ).
        mo_hobbit->load_tap( lv_program_data ).
        mt_gfx_locs = mo_hobbit->get_gfx_locations( ).
        mv_gfx_idx = 1.
        send_text( i_message_manager = i_message_manager iv_text = |__GFXV__1| ).
        send_text( i_message_manager = i_message_manager
                   iv_text = |Found { lines( mt_gfx_locs ) } locations with graphics{ c_crlf }| ).
        IF lines( mt_gfx_locs ) > 0.
          show_gfx_location( i_message_manager ).
        ELSE.
          mv_gfx_viewer = abap_false.
          mv_running_program = abap_false.
          send_text( i_message_manager = i_message_manager iv_text = |__GFXV__0| ).
          send_text( i_message_manager = i_message_manager iv_text = |No graphics found{ c_crlf }| ).
          send_prompt( i_message_manager ).
        ENDIF.

      ELSEIF lv_spectrum_mode = abap_true.
        mv_spectrum_mode = abap_true.
        mo_hobbit = NEW zcl_hobbit_emulator( ).
        mo_hobbit->set_debug_mode( lv_debug_mode ).
        mo_hobbit->load_tap( lv_program_data ).
        run_hobbit_and_output( i_message_manager ).
      ELSE.
        mv_spectrum_mode = abap_false.
        mo_cpm->reset( ).
        load_companion_files( lv_program_name ).

        IF lv_program_args IS NOT INITIAL.
          DATA lv_arg_file TYPE string.
          SPLIT lv_program_args AT space INTO lv_arg_file DATA(lv_rest_args).
          IF lv_arg_file IS NOT INITIAL.
            IF lv_arg_file+0(1) = '=' OR lv_arg_file+0(1) = ','.
              lv_arg_file = lv_arg_file+1.
            ENDIF.
          ENDIF.
          IF lv_arg_file IS NOT INITIAL.
            DATA(lv_arg_data) = load_bin_file( lv_arg_file ).
            IF lv_arg_data IS INITIAL.
              lv_arg_data = load_smw0_file( lv_arg_file ).
            ENDIF.
            IF lv_arg_data IS NOT INITIAL.
              mo_cpm->register_file( iv_filename = lv_arg_file iv_data = lv_arg_data ).
              send_text( i_message_manager = i_message_manager
                         iv_text = |[Registered: { lv_arg_file } ({ xstrlen( lv_arg_data ) } bytes)]{ c_crlf }| ).
            ENDIF.
          ENDIF.
        ENDIF.

        mo_cpm->setup_command_tail( lv_program_args ).
        IF lv_program_args IS NOT INITIAL.
          send_text( i_message_manager = i_message_manager iv_text = |[Args: { lv_program_args }]{ c_crlf }| ).
        ENDIF.

        mo_cpm->load_program( iv_data = lv_program_data iv_addr = 256 ).
        run_program_and_output( i_message_manager ).
      ENDIF.
    ELSE.
      send_prompt( i_message_manager ).
    ENDIF.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_close.
    decrement_connections( ).
    CLEAR: mo_cpm, mo_ccp, mo_hobbit.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_error.
    decrement_connections( ).
    CLEAR: mo_cpm, mo_ccp, mo_hobbit.
  ENDMETHOD.

  METHOD send_text.
    TRY.
        DATA(lo_message) = i_message_manager->create_message( ).
        lo_message->set_text( iv_text ).
        i_message_manager->send( lo_message ).
      CATCH cx_apc_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD send_prompt.
    send_text( i_message_manager = i_message_manager iv_text = mo_ccp->get_prompt( ) ).
  ENDMETHOD.

  METHOD run_program_and_output.
    mo_cpm->run( iv_max_cycles = 10000000 ).
    DATA(lv_output) = mo_cpm->get_output( ).

    IF lv_output IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_output WITH c_crlf.
      send_text( i_message_manager = i_message_manager iv_text = lv_output ).
    ENDIF.

    IF mo_cpm->is_running( ) = abap_false.
      mv_running_program = abap_false.
      send_text( i_message_manager = i_message_manager iv_text = c_crlf ).
      send_prompt( i_message_manager ).
    ENDIF.
  ENDMETHOD.

  METHOD run_hobbit_and_output.
    DATA lv_pre TYPE string.

    IF mo_hobbit IS NOT BOUND.
      RETURN.
    ENDIF.

    lv_pre = mo_hobbit->get_output( ).
    IF lv_pre IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_pre WITH c_crlf.
      send_text( i_message_manager = i_message_manager iv_text = lv_pre ).
    ENDIF.

    mo_hobbit->run( iv_max_cycles = 10000000 ).

    IF mo_hobbit->has_pending_graphics( ) = abap_true.
      DATA(lv_svg) = mo_hobbit->get_graphics_svg( ).
      send_text( i_message_manager = i_message_manager iv_text = |__GFX__{ lv_svg }| ).
      mo_hobbit->clear_pending_graphics( ).
    ENDIF.

    IF mo_hobbit->has_gfx_mode_change( ) = abap_true.
      DATA(lv_gfx_msg) = mo_hobbit->get_gfx_mode_message( ).
      send_text( i_message_manager = i_message_manager iv_text = lv_gfx_msg ).
    ENDIF.

    DATA(lv_output) = mo_hobbit->get_output( ).
    IF lv_output IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_output WITH c_crlf.
      send_text( i_message_manager = i_message_manager iv_text = lv_output ).
    ENDIF.

    IF mo_hobbit->is_running( ) = abap_false.
      mv_running_program = abap_false.
      mv_spectrum_mode = abap_false.
      send_text( i_message_manager = i_message_manager iv_text = c_crlf ).
      send_prompt( i_message_manager ).
    ENDIF.
  ENDMETHOD.

  METHOD show_gfx_location.
    DATA lv_loc TYPE i.
    IF mv_gfx_idx < 1 OR mv_gfx_idx > lines( mt_gfx_locs ).
      RETURN.
    ENDIF.
    READ TABLE mt_gfx_locs INTO lv_loc INDEX mv_gfx_idx.
    send_text( i_message_manager = i_message_manager
               iv_text = |{ c_crlf }Location { lv_loc } ({ mv_gfx_idx }/{ lines( mt_gfx_locs ) })...{ c_crlf }| ).
    DATA(lv_gfx) = mo_hobbit->render_location_gfx( lv_loc ).
    IF lv_gfx IS NOT INITIAL.
      send_text( i_message_manager = i_message_manager iv_text = |__GFX__{ lv_gfx }| ).
    ELSE.
      send_text( i_message_manager = i_message_manager iv_text = |(No graphics data){ c_crlf }| ).
    ENDIF.
  ENDMETHOD.

  METHOD get_welcome_banner.
    DATA lv_disk_line TYPE string.
    lv_disk_line = |Disk: { mv_disk }|.
    WHILE strlen( lv_disk_line ) < 39.
      lv_disk_line = lv_disk_line && | |.
    ENDWHILE.
    rv_text =
      |{ c_crlf }| &&
      | ╔════════════════════════════════════════════╗{ c_crlf }| &&
      | ║     C P / M   o n   S A P   H A N A        ║{ c_crlf }| &&
      | ╠════════════════════════════════════════════╣{ c_crlf }| &&
      | ║   Z80 CP/M 2.2 Emulator in ABAP            ║{ c_crlf }| &&
      | ║   { lv_disk_line }║{ c_crlf }| &&
      | ╚════════════════════════════════════════════╝{ c_crlf }| &&
      |{ c_crlf }|.
  ENDMETHOD.

  METHOD parse_disk_command.
    IF iv_text CP '__DISK__=*'.
      rv_disk = to_upper( iv_text+9 ).
      CONDENSE rv_disk NO-GAPS.
    ENDIF.
  ENDMETHOD.

  METHOD load_companion_files.
    DATA(lv_base) = iv_program_name.
    DATA(lv_pos) = find( val = lv_base sub = '.' ).
    IF lv_pos > 0.
      lv_base = lv_base+0(lv_pos).
    ENDIF.

    DATA(lv_dat_name) = lv_base && '.DAT'.
    DATA(lv_dat_data) = load_bin_file( lv_dat_name ).
    IF lv_dat_data IS INITIAL.
      lv_dat_data = load_smw0_file( lv_dat_name ).
    ENDIF.
    IF lv_dat_data IS NOT INITIAL.
      mo_cpm->register_file( iv_filename = lv_dat_name iv_data = lv_dat_data ).
    ENDIF.

    DATA(lv_ovr_name) = lv_base && '.OVR'.
    DATA(lv_ovr_data) = load_bin_file( lv_ovr_name ).
    IF lv_ovr_data IS INITIAL.
      lv_ovr_data = load_smw0_file( lv_ovr_name ).
    ENDIF.
    IF lv_ovr_data IS NOT INITIAL.
      mo_cpm->register_file( iv_filename = lv_ovr_name iv_data = lv_ovr_data ).
    ENDIF.
  ENDMETHOD.

  METHOD load_bin_file.
    DATA lv_name TYPE text60.
    lv_name = to_upper( iv_name ).
    SELECT SINGLE v FROM zcpm_00_bin INTO @rv_data WHERE bin = @mv_disk AND name = @lv_name.
  ENDMETHOD.

  METHOD load_smw0_file.
    DATA: lt_mime TYPE w3mimetabtype, ls_key TYPE wwwdatatab, lv_size TYPE i, lv_objid TYPE wwwparams-objid.

    SELECT SINGLE objid FROM wwwparams INTO lv_objid WHERE relid = 'MI' AND objid = iv_name.
    IF sy-subrc <> 0.
      DATA lt_all TYPE STANDARD TABLE OF wwwparams-objid.
      SELECT DISTINCT objid FROM wwwparams INTO TABLE lt_all WHERE relid = 'MI'.
      LOOP AT lt_all INTO DATA(lv_obj).
        IF to_upper( lv_obj ) = to_upper( iv_name ).
          lv_objid = lv_obj.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_objid IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    ls_key-relid = 'MI'.
    ls_key-objid = lv_objid.

    DATA lv_size_str TYPE wwwparams-value.
    SELECT SINGLE value FROM wwwparams INTO lv_size_str WHERE relid = 'MI' AND objid = lv_objid AND name = 'filesize'.
    IF sy-subrc = 0.
      lv_size = lv_size_str.
    ENDIF.

    CALL FUNCTION 'WWWDATA_IMPORT' EXPORTING key = ls_key TABLES mime = lt_mime EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING' EXPORTING input_length = lv_size IMPORTING buffer = rv_data TABLES binary_tab = lt_mime.
  ENDMETHOD.

  METHOD increment_connections.
    DATA lv_count TYPE i.
    IMPORT count = lv_count FROM SHARED MEMORY indx(zk) ID c_memory_id.
    IF sy-subrc <> 0.
      lv_count = 0.
    ENDIF.
    lv_count = lv_count + 1.
    EXPORT count = lv_count TO SHARED MEMORY indx(zk) ID c_memory_id.
  ENDMETHOD.

  METHOD decrement_connections.
    DATA lv_count TYPE i.
    IMPORT count = lv_count FROM SHARED MEMORY indx(zk) ID c_memory_id.
    IF sy-subrc = 0 AND lv_count > 0.
      lv_count = lv_count - 1.
      EXPORT count = lv_count TO SHARED MEMORY indx(zk) ID c_memory_id.
    ENDIF.
  ENDMETHOD.

  METHOD get_connection_count.
    IMPORT count = rv_count FROM SHARED MEMORY indx(zk) ID c_memory_id.
    IF sy-subrc <> 0.
      rv_count = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.