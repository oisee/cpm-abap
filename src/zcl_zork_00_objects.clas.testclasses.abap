*&---------------------------------------------------------------------*
*& Unit Tests for ZCL_ZORK_00_OBJECTS
*& Tests object table operations for Z-machine v3
*&---------------------------------------------------------------------*

CLASS ltcl_objects_test DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_memory  TYPE REF TO zcl_zork_00_memory,
      mo_objects TYPE REF TO zcl_zork_00_objects.

    METHODS:
      setup,
      create_test_story
        RETURNING VALUE(rv_story) TYPE string,

      " Test methods
      test_get_parent FOR TESTING,
      test_get_child FOR TESTING,
      test_get_sibling FOR TESTING,
      test_attr_operations FOR TESTING,
      test_insert_remove FOR TESTING,
      test_get_prop FOR TESTING,
      test_get_prop_default FOR TESTING,
      test_get_next_prop FOR TESTING.

ENDCLASS.


CLASS ltcl_objects_test IMPLEMENTATION.

  METHOD setup.
    mo_memory = NEW zcl_zork_00_memory( ).
    DATA(lv_story) = create_test_story( ).
    mo_memory->load_story( lv_story ).
    mo_objects = NEW zcl_zork_00_objects( mo_memory ).
  ENDMETHOD.


  METHOD create_test_story.
    " Create minimal v3 story file with object table
    DATA: lv_hex TYPE string.

    " Header (64 bytes)
    lv_hex = ''.

    " 0x00: Version = 3
    lv_hex = lv_hex && '03'.

    " 0x01: Flags 1 = 0
    lv_hex = lv_hex && '00'.

    " 0x02-0x03: Release number = 1
    lv_hex = lv_hex && '0001'.

    " 0x04-0x05: High memory base = 0x0200
    lv_hex = lv_hex && '0200'.

    " 0x06-0x07: Initial PC = 0x0200
    lv_hex = lv_hex && '0200'.

    " 0x08-0x09: Dictionary = 0x0180
    lv_hex = lv_hex && '0180'.

    " 0x0A-0x0B: Object table = 0x0080 (128)
    lv_hex = lv_hex && '0080'.

    " 0x0C-0x0D: Globals table = 0x0040 (64)
    lv_hex = lv_hex && '0040'.

    " 0x0E-0x0F: Static memory base = 0x0100
    lv_hex = lv_hex && '0100'.

    " 0x10-0x3F: Rest of header (48 bytes) - zeros
    DATA(lv_i) = 0.
    WHILE lv_i < 48.
      lv_hex = lv_hex && '00'.
      lv_i = lv_i + 1.
    ENDWHILE.

    " 0x40-0x7F: Globals area (64 bytes = 32 words) - zeros
    lv_i = 0.
    WHILE lv_i < 64.
      lv_hex = lv_hex && '00'.
      lv_i = lv_i + 1.
    ENDWHILE.

    " 0x80: Object table starts here
    " First: 31 property default words (62 bytes)
    " Default for property 1 = 0x1234
    lv_hex = lv_hex && '1234'.
    " Defaults for properties 2-31 = 0x0000
    lv_i = 2.
    WHILE lv_i <= 31.
      lv_hex = lv_hex && '0000'.
      lv_i = lv_i + 1.
    ENDWHILE.

    " Object entries start at 0x80 + 62 = 0xBE
    " Each object: 9 bytes
    "   4 bytes attributes, 1 byte parent, 1 byte sibling, 1 byte child, 2 bytes prop addr

    " Object 1: "Room" - no parent, no sibling, child=2, prop at 0x0100
    " Attributes: bit 0 set (0x80000000)
    lv_hex = lv_hex && '80000000'.  " Attr: bit 0 set
    lv_hex = lv_hex && '00'.        " Parent: none
    lv_hex = lv_hex && '00'.        " Sibling: none
    lv_hex = lv_hex && '02'.        " Child: object 2
    lv_hex = lv_hex && '0100'.      " Props at 0x0100

    " Object 2: "Lamp" - parent=1, sibling=3, no child, prop at 0x0110
    " Attributes: bit 5 set (0x04000000)
    lv_hex = lv_hex && '04000000'.  " Attr: bit 5 set
    lv_hex = lv_hex && '01'.        " Parent: object 1
    lv_hex = lv_hex && '03'.        " Sibling: object 3
    lv_hex = lv_hex && '00'.        " Child: none
    lv_hex = lv_hex && '0110'.      " Props at 0x0110

    " Object 3: "Sword" - parent=1, no sibling, no child, prop at 0x0120
    " Attributes: none
    lv_hex = lv_hex && '00000000'.  " No attributes
    lv_hex = lv_hex && '01'.        " Parent: object 1
    lv_hex = lv_hex && '00'.        " Sibling: none
    lv_hex = lv_hex && '00'.        " Child: none
    lv_hex = lv_hex && '0120'.      " Props at 0x0120

    " Pad to 0x100 (property tables start)
    DATA(lv_current) = strlen( lv_hex ) / 2.
    WHILE lv_current < 256.
      lv_hex = lv_hex && '00'.
      lv_current = lv_current + 1.
    ENDWHILE.

    " Property table for Object 1 at 0x0100
    " Format: text_len, text, properties (descending), 0
    lv_hex = lv_hex && '02'.        " Text length: 2 words (4 bytes)
    lv_hex = lv_hex && '94A55CA5'.  " "room" in Z-chars (placeholder)
    " Property 5: 2 bytes value 0x0042
    lv_hex = lv_hex && '25'.        " Size byte: (1 << 5) + 5 = 0x25
    lv_hex = lv_hex && '0042'.      " Value
    " Property 2: 1 byte value 0x07
    lv_hex = lv_hex && '02'.        " Size byte: (0 << 5) + 2 = 0x02
    lv_hex = lv_hex && '07'.        " Value
    " End marker
    lv_hex = lv_hex && '00'.

    " Pad to 0x110
    lv_current = strlen( lv_hex ) / 2.
    WHILE lv_current < 272.
      lv_hex = lv_hex && '00'.
      lv_current = lv_current + 1.
    ENDWHILE.

    " Property table for Object 2 at 0x0110
    lv_hex = lv_hex && '02'.        " Text length: 2 words
    lv_hex = lv_hex && '00000000'.  " Placeholder text
    " Property 3: 1 byte value 0xFF
    lv_hex = lv_hex && '03'.        " Size byte: (0 << 5) + 3 = 0x03
    lv_hex = lv_hex && 'FF'.        " Value
    " End
    lv_hex = lv_hex && '00'.

    " Pad to 0x120
    lv_current = strlen( lv_hex ) / 2.
    WHILE lv_current < 288.
      lv_hex = lv_hex && '00'.
      lv_current = lv_current + 1.
    ENDWHILE.

    " Property table for Object 3 at 0x0120
    lv_hex = lv_hex && '02'.        " Text length: 2 words
    lv_hex = lv_hex && '00000000'.  " Placeholder text
    " No properties
    lv_hex = lv_hex && '00'.

    " Pad to 512 bytes minimum
    lv_current = strlen( lv_hex ) / 2.
    WHILE lv_current < 512.
      lv_hex = lv_hex && '00'.
      lv_current = lv_current + 1.
    ENDWHILE.

    rv_story = lv_hex.
  ENDMETHOD.


  METHOD test_get_parent.
    " Test get_parent for objects 1, 2, 3
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_parent( 1 )
      exp = 0
      msg = 'Object 1 should have no parent' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_parent( 2 )
      exp = 1
      msg = 'Object 2 parent should be 1' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_parent( 3 )
      exp = 1
      msg = 'Object 3 parent should be 1' ).
  ENDMETHOD.


  METHOD test_get_child.
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_child( 1 )
      exp = 2
      msg = 'Object 1 child should be 2' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_child( 2 )
      exp = 0
      msg = 'Object 2 should have no child' ).
  ENDMETHOD.


  METHOD test_get_sibling.
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_sibling( 2 )
      exp = 3
      msg = 'Object 2 sibling should be 3' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_sibling( 3 )
      exp = 0
      msg = 'Object 3 should have no sibling' ).
  ENDMETHOD.


  METHOD test_attr_operations.
    " Test attribute 0 (should be set on object 1)
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->test_attr( iv_object = 1 iv_attr = 0 )
      exp = abap_true
      msg = 'Object 1 attr 0 should be set' ).

    " Test attribute 5 (should be set on object 2)
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->test_attr( iv_object = 2 iv_attr = 5 )
      exp = abap_true
      msg = 'Object 2 attr 5 should be set' ).

    " Test unset attribute
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->test_attr( iv_object = 3 iv_attr = 0 )
      exp = abap_false
      msg = 'Object 3 attr 0 should not be set' ).

    " Test set_attr
    mo_objects->set_attr( iv_object = 3 iv_attr = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->test_attr( iv_object = 3 iv_attr = 10 )
      exp = abap_true
      msg = 'Object 3 attr 10 should be set after set_attr' ).

    " Test clear_attr
    mo_objects->clear_attr( iv_object = 3 iv_attr = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->test_attr( iv_object = 3 iv_attr = 10 )
      exp = abap_false
      msg = 'Object 3 attr 10 should be clear after clear_attr' ).
  ENDMETHOD.


  METHOD test_insert_remove.
    " Remove object 2 from tree
    mo_objects->remove_obj( 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_parent( 2 )
      exp = 0
      msg = 'Object 2 should have no parent after remove' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_child( 1 )
      exp = 3
      msg = 'Object 1 child should now be 3' ).

    " Insert object 2 into object 3
    mo_objects->insert_obj( iv_object = 2 iv_destination = 3 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_parent( 2 )
      exp = 3
      msg = 'Object 2 parent should be 3 after insert' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_child( 3 )
      exp = 2
      msg = 'Object 3 child should be 2 after insert' ).
  ENDMETHOD.


  METHOD test_get_prop.
    " Object 1 has property 5 = 0x0042
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_prop( iv_object = 1 iv_property = 5 )
      exp = 66  " 0x0042
      msg = 'Object 1 property 5 should be 0x0042' ).

    " Object 1 has property 2 = 0x07
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_prop( iv_object = 1 iv_property = 2 )
      exp = 7
      msg = 'Object 1 property 2 should be 7' ).

    " Object 2 has property 3 = 0xFF
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_prop( iv_object = 2 iv_property = 3 )
      exp = 255
      msg = 'Object 2 property 3 should be 255' ).
  ENDMETHOD.


  METHOD test_get_prop_default.
    " Object 3 has no properties, so should get default
    " Property 1 default is 0x1234

    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_prop( iv_object = 3 iv_property = 1 )
      exp = 4660  " 0x1234
      msg = 'Object 3 prop 1 should return default 0x1234' ).
  ENDMETHOD.


  METHOD test_get_next_prop.
    " Object 1 has properties 5, 2 (in descending order)
    " get_next_prop(1, 0) should return 5 (first property)
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_next_prop( iv_object = 1 iv_property = 0 )
      exp = 5
      msg = 'First property of object 1 should be 5' ).

    " get_next_prop(1, 5) should return 2
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_next_prop( iv_object = 1 iv_property = 5 )
      exp = 2
      msg = 'Next property after 5 should be 2' ).

    " get_next_prop(1, 2) should return 0 (end)
    cl_abap_unit_assert=>assert_equals(
      act = mo_objects->get_next_prop( iv_object = 1 iv_property = 2 )
      exp = 0
      msg = 'Next property after 2 should be 0 (end)' ).
  ENDMETHOD.

ENDCLASS.
