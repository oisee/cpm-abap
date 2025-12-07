*&---------------------------------------------------------------------*
*& Z-Machine Memory Tests
*&---------------------------------------------------------------------*
CLASS ltcl_zork_memory_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_mem TYPE REF TO zcl_zork_00_memory.

    METHODS:
      setup,

      " Basic memory operations
      test_read_byte FOR TESTING,
      test_write_byte FOR TESTING,
      test_read_word FOR TESTING,
      test_write_word FOR TESTING,

      " Header parsing
      test_header_version FOR TESTING,
      test_header_addresses FOR TESTING,

      " Memory regions
      test_dynamic_memory FOR TESTING,
      test_static_protection FOR TESTING,

      " Packed address conversion
      test_unpack_routine_v3 FOR TESTING,
      test_unpack_string_v3 FOR TESTING,

      " Helper to create test story
      create_test_story
        RETURNING VALUE(rv_story) TYPE string.

ENDCLASS.


CLASS ltcl_zork_memory_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_mem.
  ENDMETHOD.


  METHOD create_test_story.
    " Create minimal v3 story file (64-byte header + some data)
    " Hex string: 2 chars per byte

    " === Header (64 bytes = 128 hex chars) ===
    rv_story = ''.

    " $00: Version = 3
    rv_story = rv_story && '03'.

    " $01: Flags 1 = 0
    rv_story = rv_story && '00'.

    " $02-$03: Release number (not used here)
    rv_story = rv_story && '0001'.

    " $04-$05: High memory base = 0x0100 (256)
    rv_story = rv_story && '0100'.

    " $06-$07: Initial PC = 0x0080 (128)
    rv_story = rv_story && '0080'.

    " $08-$09: Dictionary = 0x0200 (512)
    rv_story = rv_story && '0200'.

    " $0A-$0B: Objects = 0x0100 (256)
    rv_story = rv_story && '0100'.

    " $0C-$0D: Globals = 0x0050 (80)
    rv_story = rv_story && '0050'.

    " $0E-$0F: Static memory base = 0x0040 (64)
    rv_story = rv_story && '0040'.

    " $10: Flags 2 = 0
    rv_story = rv_story && '00'.

    " $11-$17: Serial number (6 bytes) + padding
    rv_story = rv_story && '00000000000000'.

    " $18-$19: Abbreviations table = 0x0048 (72)
    rv_story = rv_story && '0048'.

    " $1A-$1B: File length / 2 = 0x0080 (128, so actual = 256)
    rv_story = rv_story && '0080'.

    " $1C-$1D: Checksum = 0x1234
    rv_story = rv_story && '1234'.

    " $1E-$1F: Interpreter number/version
    rv_story = rv_story && '0000'.

    " $20-$3F: Rest of header (32 bytes = 64 hex chars)
    rv_story = rv_story && '00000000000000000000000000000000'.
    rv_story = rv_story && '00000000000000000000000000000000'.

    " === Dynamic memory ($40-$4F) - 16 bytes of test data ===
    " Put 'AB' pattern that we can verify
    rv_story = rv_story && '41424344454647484950515253545556'.

    " === More data to reach 256 bytes total (384 more hex chars) ===
    " Fill with incrementing bytes for testing
    DATA(lv_i) = 0.
    WHILE strlen( rv_story ) < 512.  " 256 bytes = 512 hex chars
      DATA(lv_byte) = ( lv_i MOD 256 ).
      DATA(lv_hex) = ''.

      " Convert to hex manually
      DATA(lv_hi) = lv_byte DIV 16.
      DATA(lv_lo) = lv_byte MOD 16.

      CASE lv_hi.
        WHEN 0. lv_hex = '0'.
        WHEN 1. lv_hex = '1'.
        WHEN 2. lv_hex = '2'.
        WHEN 3. lv_hex = '3'.
        WHEN 4. lv_hex = '4'.
        WHEN 5. lv_hex = '5'.
        WHEN 6. lv_hex = '6'.
        WHEN 7. lv_hex = '7'.
        WHEN 8. lv_hex = '8'.
        WHEN 9. lv_hex = '9'.
        WHEN 10. lv_hex = 'A'.
        WHEN 11. lv_hex = 'B'.
        WHEN 12. lv_hex = 'C'.
        WHEN 13. lv_hex = 'D'.
        WHEN 14. lv_hex = 'E'.
        WHEN 15. lv_hex = 'F'.
      ENDCASE.

      CASE lv_lo.
        WHEN 0. lv_hex = lv_hex && '0'.
        WHEN 1. lv_hex = lv_hex && '1'.
        WHEN 2. lv_hex = lv_hex && '2'.
        WHEN 3. lv_hex = lv_hex && '3'.
        WHEN 4. lv_hex = lv_hex && '4'.
        WHEN 5. lv_hex = lv_hex && '5'.
        WHEN 6. lv_hex = lv_hex && '6'.
        WHEN 7. lv_hex = lv_hex && '7'.
        WHEN 8. lv_hex = lv_hex && '8'.
        WHEN 9. lv_hex = lv_hex && '9'.
        WHEN 10. lv_hex = lv_hex && 'A'.
        WHEN 11. lv_hex = lv_hex && 'B'.
        WHEN 12. lv_hex = lv_hex && 'C'.
        WHEN 13. lv_hex = lv_hex && 'D'.
        WHEN 14. lv_hex = lv_hex && 'E'.
        WHEN 15. lv_hex = lv_hex && 'F'.
      ENDCASE.

      rv_story = rv_story && lv_hex.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD test_read_byte.
    " Test basic byte reading
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    " Read version byte at $00
    DATA(lv_version) = mo_mem->read_byte( 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_version
      exp = 3
      msg = 'Version byte should be 3' ).

    " Read flags at $01
    DATA(lv_flags) = mo_mem->read_byte( 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_flags
      exp = 0
      msg = 'Flags1 should be 0' ).

    " Read test pattern at $40 (dynamic memory start)
    DATA(lv_byte) = mo_mem->read_byte( 64 ).  " 'A' = 0x41
    cl_abap_unit_assert=>assert_equals(
      act = lv_byte
      exp = 65   " ASCII 'A'
      msg = 'Byte at $40 should be 0x41 (A)' ).
  ENDMETHOD.


  METHOD test_write_byte.
    " Test byte writing to dynamic memory
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    " Write to dynamic memory (before static_mem = $40)
    " Our test story has static_mem at $40, so we can write to $00-$3F
    " But header should be protected in real Z-machine...
    " For this test, just verify the mechanic works

    " Read original value at offset in dynamic area
    " (Note: Our test story has static_mem = $40, which means
    "  ONLY bytes 0-63 are dynamic. Let's use offset 32.)
    DATA(lv_orig) = mo_mem->read_byte( 32 ).

    " Write new value
    mo_mem->write_byte( iv_addr = 32 iv_val = 255 ).

    " Verify write
    DATA(lv_new) = mo_mem->read_byte( 32 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_new
      exp = 255
      msg = 'Written byte should be 255' ).
  ENDMETHOD.


  METHOD test_read_word.
    " Test big-endian word reading
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    " Read high memory base at $04-$05 (0x0100 = 256)
    DATA(lv_high_mem) = mo_mem->read_word( 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_high_mem
      exp = 256
      msg = 'High memory should be 0x0100 (256)' ).

    " Read initial PC at $06-$07 (0x0080 = 128)
    DATA(lv_pc) = mo_mem->read_word( 6 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_pc
      exp = 128
      msg = 'Initial PC should be 0x0080 (128)' ).

    " Read dictionary at $08-$09 (0x0200 = 512)
    DATA(lv_dict) = mo_mem->read_word( 8 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_dict
      exp = 512
      msg = 'Dictionary should be 0x0200 (512)' ).
  ENDMETHOD.


  METHOD test_write_word.
    " Test big-endian word writing
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    " Write word to dynamic memory
    mo_mem->write_word( iv_addr = 32 iv_val = 4660 ).  " 0x1234

    " Verify high byte
    DATA(lv_high) = mo_mem->read_byte( 32 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_high
      exp = 18   " 0x12
      msg = 'High byte should be 0x12' ).

    " Verify low byte
    DATA(lv_low) = mo_mem->read_byte( 33 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_low
      exp = 52   " 0x34
      msg = 'Low byte should be 0x34' ).

    " Verify word read
    DATA(lv_word) = mo_mem->read_word( 32 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_word
      exp = 4660
      msg = 'Word should be 0x1234 (4660)' ).
  ENDMETHOD.


  METHOD test_header_version.
    " Test header version parsing
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    DATA(lv_version) = mo_mem->get_version( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_version
      exp = 3
      msg = 'Version should be 3' ).
  ENDMETHOD.


  METHOD test_header_addresses.
    " Test header address parsing
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    DATA(ls_header) = mo_mem->get_header( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_header-init_pc
      exp = 128
      msg = 'Initial PC should be 128' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_header-dictionary
      exp = 512
      msg = 'Dictionary should be 512' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_header-objects
      exp = 256
      msg = 'Objects should be 256' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_header-globals
      exp = 80
      msg = 'Globals should be 80' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_header-static_mem
      exp = 64
      msg = 'Static memory should be 64' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_header-abbrev
      exp = 72
      msg = 'Abbreviations should be 72' ).
  ENDMETHOD.


  METHOD test_dynamic_memory.
    " Test is_dynamic_memory check
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    " Static memory starts at $40 (64), so 0-63 is dynamic
    DATA(lv_dyn0) = mo_mem->is_dynamic_memory( 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_dyn0
      exp = abap_true
      msg = 'Address 0 should be dynamic' ).

    DATA(lv_dyn63) = mo_mem->is_dynamic_memory( 63 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_dyn63
      exp = abap_true
      msg = 'Address 63 should be dynamic' ).

    DATA(lv_dyn64) = mo_mem->is_dynamic_memory( 64 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_dyn64
      exp = abap_false
      msg = 'Address 64 should be static' ).
  ENDMETHOD.


  METHOD test_static_protection.
    " Test that writes to static memory are ignored
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    " Read original value at static address
    DATA(lv_orig) = mo_mem->read_byte( 64 ).

    " Attempt write to static memory (should be ignored)
    mo_mem->write_byte( iv_addr = 64 iv_val = 255 ).

    " Verify value unchanged
    DATA(lv_after) = mo_mem->read_byte( 64 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_after
      exp = lv_orig
      msg = 'Static memory should be protected from writes' ).
  ENDMETHOD.


  METHOD test_unpack_routine_v3.
    " Test v3 packed address unpacking (multiply by 2)
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    DATA(lv_addr) = mo_mem->unpack_routine_addr( 128 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_addr
      exp = 256
      msg = 'Packed 128 should unpack to 256 in v3' ).

    lv_addr = mo_mem->unpack_routine_addr( 512 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_addr
      exp = 1024
      msg = 'Packed 512 should unpack to 1024 in v3' ).
  ENDMETHOD.


  METHOD test_unpack_string_v3.
    " Test v3 packed string address unpacking (multiply by 2)
    DATA(lv_story) = create_test_story( ).
    mo_mem->load_story( lv_story ).

    DATA(lv_addr) = mo_mem->unpack_string_addr( 100 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_addr
      exp = 200
      msg = 'Packed 100 should unpack to 200 in v3' ).
  ENDMETHOD.

ENDCLASS.
