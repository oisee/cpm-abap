*&---------------------------------------------------------------------*
*& Z-Machine Object Table Operations
*& Handles object tree, attributes, and properties for v3
*& Phase 6 of Z-machine interpreter implementation
*&---------------------------------------------------------------------*
CLASS zcl_zork_00_objects DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "======================================================================
    " V3 Object Table Constants
    "======================================================================
    CONSTANTS:
      c_max_objects    TYPE i VALUE 255,    " Max objects in v3
      c_max_attributes TYPE i VALUE 32,     " Attributes 0-31
      c_max_properties TYPE i VALUE 31,     " Properties 1-31
      c_entry_size     TYPE i VALUE 9,      " Bytes per object entry
      c_defaults_size  TYPE i VALUE 62.     " 31 words = 62 bytes

    METHODS:
      constructor
        IMPORTING io_memory TYPE REF TO zcl_zork_00_memory,

      "----------------------------------------------------------------
      " Attribute Operations (32 flags per object)
      "----------------------------------------------------------------
      test_attr
        IMPORTING iv_object      TYPE i
                  iv_attr        TYPE i
        RETURNING VALUE(rv_set) TYPE abap_bool,

      set_attr
        IMPORTING iv_object TYPE i
                  iv_attr   TYPE i,

      clear_attr
        IMPORTING iv_object TYPE i
                  iv_attr   TYPE i,

      "----------------------------------------------------------------
      " Object Tree Operations
      "----------------------------------------------------------------
      get_parent
        IMPORTING iv_object        TYPE i
        RETURNING VALUE(rv_parent) TYPE i,

      get_sibling
        IMPORTING iv_object         TYPE i
        RETURNING VALUE(rv_sibling) TYPE i,

      get_child
        IMPORTING iv_object       TYPE i
        RETURNING VALUE(rv_child) TYPE i,

      " Insert object as first child of destination
      insert_obj
        IMPORTING iv_object      TYPE i
                  iv_destination TYPE i,

      " Remove object from its parent
      remove_obj
        IMPORTING iv_object TYPE i,

      "----------------------------------------------------------------
      " Property Operations
      "----------------------------------------------------------------
      " Get property value (returns default if not present)
      get_prop
        IMPORTING iv_object      TYPE i
                  iv_property    TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      " Get address of property data (0 if not found)
      get_prop_addr
        IMPORTING iv_object       TYPE i
                  iv_property     TYPE i
        RETURNING VALUE(rv_addr) TYPE i,

      " Get length of property data at given address
      get_prop_len
        IMPORTING iv_prop_addr   TYPE i
        RETURNING VALUE(rv_len)  TYPE i,

      " Get next property number (0 if iv_property is last)
      get_next_prop
        IMPORTING iv_object       TYPE i
                  iv_property     TYPE i
        RETURNING VALUE(rv_next) TYPE i,

      " Set property value
      put_prop
        IMPORTING iv_object   TYPE i
                  iv_property TYPE i
                  iv_value    TYPE i,

      "----------------------------------------------------------------
      " Object Name
      "----------------------------------------------------------------
      get_object_name
        IMPORTING iv_object       TYPE i
        RETURNING VALUE(rv_name) TYPE string,

      get_prop_table_addr
        IMPORTING iv_object       TYPE i
        RETURNING VALUE(rv_addr) TYPE i.

  PRIVATE SECTION.

    DATA: mo_memory TYPE REF TO zcl_zork_00_memory.

    METHODS:
      " Get address of object entry in table
      get_object_addr
        IMPORTING iv_object       TYPE i
        RETURNING VALUE(rv_addr) TYPE i,

      " Get property default value
      get_prop_default
        IMPORTING iv_property    TYPE i
        RETURNING VALUE(rv_val) TYPE i,

      " Find property in property table
      find_property
        IMPORTING iv_prop_table  TYPE i
                  iv_property    TYPE i
        EXPORTING ev_addr        TYPE i    " Address of property data
                  ev_size        TYPE i,   " Size in bytes

      " Set parent/sibling/child in object entry
      set_parent
        IMPORTING iv_object TYPE i
                  iv_parent TYPE i,

      set_sibling
        IMPORTING iv_object  TYPE i
                  iv_sibling TYPE i,

      set_child
        IMPORTING iv_object TYPE i
                  iv_child  TYPE i.

ENDCLASS.


CLASS zcl_zork_00_objects IMPLEMENTATION.

  METHOD constructor.
    mo_memory = io_memory.
  ENDMETHOD.


  METHOD get_object_addr.
    " Calculate address of object entry in table
    " Object table: 31 default words + object entries
    " Object 1 is at offset 62, object N at offset 62 + (N-1)*9

    IF iv_object < 1 OR iv_object > c_max_objects.
      rv_addr = 0.
      RETURN.
    ENDIF.

    DATA(lv_table) = mo_memory->get_objects_addr( ).
    rv_addr = lv_table + c_defaults_size + ( iv_object - 1 ) * c_entry_size.
  ENDMETHOD.


  METHOD get_prop_default.
    " Get default value for property (from first 31 words of object table)
    IF iv_property < 1 OR iv_property > c_max_properties.
      rv_val = 0.
      RETURN.
    ENDIF.

    DATA(lv_table) = mo_memory->get_objects_addr( ).
    DATA(lv_addr) = lv_table + ( iv_property - 1 ) * 2.
    rv_val = mo_memory->read_word( lv_addr ).
  ENDMETHOD.


  METHOD test_attr.
    " Test if attribute is set (attributes 0-31)
    " Attribute 0 is bit 7 of byte 0, attribute 31 is bit 0 of byte 3

    rv_set = abap_false.

    IF iv_attr < 0 OR iv_attr >= c_max_attributes.
      RETURN.
    ENDIF.

    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      RETURN.
    ENDIF.

    " Calculate which byte and bit
    DATA(lv_byte_num) = iv_attr DIV 8.
    DATA(lv_bit_num) = 7 - ( iv_attr MOD 8 ).  " Bit 7 is leftmost

    " Read the attribute byte
    DATA(lv_byte) = mo_memory->read_byte( lv_obj_addr + lv_byte_num ).

    " Check the bit using arithmetic
    DATA(lv_mask) = 1.
    DATA(lv_i) = 0.
    WHILE lv_i < lv_bit_num.
      lv_mask = lv_mask * 2.
      lv_i = lv_i + 1.
    ENDWHILE.

    " Test bit: (byte / mask) mod 2
    IF ( lv_byte DIV lv_mask ) MOD 2 = 1.
      rv_set = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD set_attr.
    " Set attribute (make it true)

    IF iv_attr < 0 OR iv_attr >= c_max_attributes.
      RETURN.
    ENDIF.

    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      RETURN.
    ENDIF.

    DATA(lv_byte_num) = iv_attr DIV 8.
    DATA(lv_bit_num) = 7 - ( iv_attr MOD 8 ).

    DATA(lv_byte) = mo_memory->read_byte( lv_obj_addr + lv_byte_num ).

    " Calculate mask
    DATA(lv_mask) = 1.
    DATA(lv_i) = 0.
    WHILE lv_i < lv_bit_num.
      lv_mask = lv_mask * 2.
      lv_i = lv_i + 1.
    ENDWHILE.

    " Set bit using OR: if bit not set, add mask
    IF ( lv_byte DIV lv_mask ) MOD 2 = 0.
      lv_byte = lv_byte + lv_mask.
    ENDIF.

    mo_memory->write_byte( iv_addr = lv_obj_addr + lv_byte_num
                           iv_val = lv_byte ).
  ENDMETHOD.


  METHOD clear_attr.
    " Clear attribute (make it false)

    IF iv_attr < 0 OR iv_attr >= c_max_attributes.
      RETURN.
    ENDIF.

    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      RETURN.
    ENDIF.

    DATA(lv_byte_num) = iv_attr DIV 8.
    DATA(lv_bit_num) = 7 - ( iv_attr MOD 8 ).

    DATA(lv_byte) = mo_memory->read_byte( lv_obj_addr + lv_byte_num ).

    " Calculate mask
    DATA(lv_mask) = 1.
    DATA(lv_i) = 0.
    WHILE lv_i < lv_bit_num.
      lv_mask = lv_mask * 2.
      lv_i = lv_i + 1.
    ENDWHILE.

    " Clear bit: if bit is set, subtract mask
    IF ( lv_byte DIV lv_mask ) MOD 2 = 1.
      lv_byte = lv_byte - lv_mask.
    ENDIF.

    mo_memory->write_byte( iv_addr = lv_obj_addr + lv_byte_num
                           iv_val = lv_byte ).
  ENDMETHOD.


  METHOD get_parent.
    " Get parent object number (byte 4 of object entry)

    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      rv_parent = 0.
      RETURN.
    ENDIF.

    rv_parent = mo_memory->read_byte( lv_obj_addr + 4 ).
  ENDMETHOD.


  METHOD get_sibling.
    " Get sibling object number (byte 5 of object entry)

    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      rv_sibling = 0.
      RETURN.
    ENDIF.

    rv_sibling = mo_memory->read_byte( lv_obj_addr + 5 ).
  ENDMETHOD.


  METHOD get_child.
    " Get first child object number (byte 6 of object entry)

    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      rv_child = 0.
      RETURN.
    ENDIF.

    rv_child = mo_memory->read_byte( lv_obj_addr + 6 ).
  ENDMETHOD.


  METHOD set_parent.
    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      RETURN.
    ENDIF.
    mo_memory->write_byte( iv_addr = lv_obj_addr + 4 iv_val = iv_parent ).
  ENDMETHOD.


  METHOD set_sibling.
    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      RETURN.
    ENDIF.
    mo_memory->write_byte( iv_addr = lv_obj_addr + 5 iv_val = iv_sibling ).
  ENDMETHOD.


  METHOD set_child.
    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      RETURN.
    ENDIF.
    mo_memory->write_byte( iv_addr = lv_obj_addr + 6 iv_val = iv_child ).
  ENDMETHOD.


  METHOD remove_obj.
    " Remove object from its current position in the tree
    " After removal: object has no parent, no sibling

    IF iv_object < 1 OR iv_object > c_max_objects.
      RETURN.
    ENDIF.

    DATA(lv_parent) = get_parent( iv_object ).
    IF lv_parent = 0.
      " Already detached
      RETURN.
    ENDIF.

    DATA(lv_sibling) = get_sibling( iv_object ).

    " Find if we're the first child of parent
    DATA(lv_parent_child) = get_child( lv_parent ).

    IF lv_parent_child = iv_object.
      " We are first child - update parent's child to our sibling
      set_child( iv_object = lv_parent iv_child = lv_sibling ).
    ELSE.
      " Find previous sibling
      DATA(lv_prev) = lv_parent_child.
      DATA(lv_next) = get_sibling( lv_prev ).
      WHILE lv_next <> 0 AND lv_next <> iv_object.
        lv_prev = lv_next.
        lv_next = get_sibling( lv_next ).
      ENDWHILE.

      IF lv_next = iv_object.
        " Link previous sibling to our sibling
        set_sibling( iv_object = lv_prev iv_sibling = lv_sibling ).
      ENDIF.
    ENDIF.

    " Clear our parent and sibling
    set_parent( iv_object = iv_object iv_parent = 0 ).
    set_sibling( iv_object = iv_object iv_sibling = 0 ).
  ENDMETHOD.


  METHOD insert_obj.
    " Insert object as first child of destination
    " First remove from current position, then insert

    IF iv_object < 1 OR iv_object > c_max_objects.
      RETURN.
    ENDIF.
    IF iv_destination < 1 OR iv_destination > c_max_objects.
      RETURN.
    ENDIF.

    " Remove from current position
    remove_obj( iv_object ).

    " Get current first child of destination
    DATA(lv_old_child) = get_child( iv_destination ).

    " Make object the new first child
    set_parent( iv_object = iv_object iv_parent = iv_destination ).
    set_sibling( iv_object = iv_object iv_sibling = lv_old_child ).
    set_child( iv_object = iv_destination iv_child = iv_object ).
  ENDMETHOD.


  METHOD get_prop_table_addr.
    " Get address of object's property table (bytes 7-8 of entry)

    DATA(lv_obj_addr) = get_object_addr( iv_object ).
    IF lv_obj_addr = 0.
      rv_addr = 0.
      RETURN.
    ENDIF.

    rv_addr = mo_memory->read_word( lv_obj_addr + 7 ).
  ENDMETHOD.


  METHOD find_property.
    " Find property in property table
    " Returns address of property DATA (not size byte) and size

    ev_addr = 0.
    ev_size = 0.

    IF iv_property < 1 OR iv_property > c_max_properties.
      RETURN.
    ENDIF.

    " Property table format:
    " - 1 byte: text length (in words)
    " - N*2 bytes: short name (Z-encoded)
    " - Properties in descending order:
    "   - Size byte: high 3 bits = size-1, low 5 bits = property number
    "   - Data bytes
    " - Terminator: size byte = 0

    DATA(lv_addr) = iv_prop_table.

    " Skip short name
    DATA(lv_text_len) = mo_memory->read_byte( lv_addr ).
    lv_addr = lv_addr + 1 + lv_text_len * 2.

    " Scan properties
    DATA(lv_size_byte) = mo_memory->read_byte( lv_addr ).

    WHILE lv_size_byte <> 0.
      " Extract property number (bits 0-4) and size (bits 5-7)
      DATA(lv_prop_num) = lv_size_byte MOD 32.
      DATA(lv_prop_size) = ( lv_size_byte DIV 32 ) + 1.  " Size is stored as size-1

      IF lv_prop_num = iv_property.
        " Found it!
        ev_addr = lv_addr + 1.  " Data starts after size byte
        ev_size = lv_prop_size.
        RETURN.
      ENDIF.

      IF lv_prop_num < iv_property.
        " Properties are in descending order, so we won't find it
        RETURN.
      ENDIF.

      " Move to next property
      lv_addr = lv_addr + 1 + lv_prop_size.
      lv_size_byte = mo_memory->read_byte( lv_addr ).
    ENDWHILE.
  ENDMETHOD.


  METHOD get_prop.
    " Get property value
    " If property exists and is 1 byte, return that byte
    " If property exists and is 2 bytes, return word
    " If property doesn't exist, return default

    DATA(lv_prop_table) = get_prop_table_addr( iv_object ).
    IF lv_prop_table = 0.
      rv_val = get_prop_default( iv_property ).
      RETURN.
    ENDIF.

    DATA: lv_addr TYPE i,
          lv_size TYPE i.

    find_property(
      EXPORTING iv_prop_table = lv_prop_table
                iv_property = iv_property
      IMPORTING ev_addr = lv_addr
                ev_size = lv_size ).

    IF lv_addr = 0.
      " Property not found - return default
      rv_val = get_prop_default( iv_property ).
    ELSEIF lv_size = 1.
      rv_val = mo_memory->read_byte( lv_addr ).
    ELSE.
      rv_val = mo_memory->read_word( lv_addr ).
    ENDIF.
  ENDMETHOD.


  METHOD get_prop_addr.
    " Get address of property data (0 if not found)

    DATA(lv_prop_table) = get_prop_table_addr( iv_object ).
    IF lv_prop_table = 0.
      rv_addr = 0.
      RETURN.
    ENDIF.

    DATA: lv_size TYPE i.

    find_property(
      EXPORTING iv_prop_table = lv_prop_table
                iv_property = iv_property
      IMPORTING ev_addr = rv_addr
                ev_size = lv_size ).
  ENDMETHOD.


  METHOD get_prop_len.
    " Get length of property data at given address
    " The address points to the data, size byte is at address-1

    IF iv_prop_addr = 0.
      rv_len = 0.
      RETURN.
    ENDIF.

    DATA(lv_size_byte) = mo_memory->read_byte( iv_prop_addr - 1 ).
    rv_len = ( lv_size_byte DIV 32 ) + 1.
  ENDMETHOD.


  METHOD get_next_prop.
    " Get next property number after iv_property
    " If iv_property = 0, return first property

    DATA(lv_prop_table) = get_prop_table_addr( iv_object ).
    IF lv_prop_table = 0.
      rv_next = 0.
      RETURN.
    ENDIF.

    " Skip short name
    DATA(lv_addr) = lv_prop_table.
    DATA(lv_text_len) = mo_memory->read_byte( lv_addr ).
    lv_addr = lv_addr + 1 + lv_text_len * 2.

    IF iv_property = 0.
      " Return first property
      DATA(lv_size_byte) = mo_memory->read_byte( lv_addr ).
      IF lv_size_byte = 0.
        rv_next = 0.
      ELSE.
        rv_next = lv_size_byte MOD 32.
      ENDIF.
      RETURN.
    ENDIF.

    " Find iv_property first
    DATA: lv_find_addr TYPE i,
          lv_find_size TYPE i.

    find_property(
      EXPORTING iv_prop_table = lv_prop_table
                iv_property = iv_property
      IMPORTING ev_addr = lv_find_addr
                ev_size = lv_find_size ).

    IF lv_find_addr = 0.
      " Property not found
      rv_next = 0.
      RETURN.
    ENDIF.

    " Next property is after this one's data
    lv_addr = lv_find_addr + lv_find_size.
    lv_size_byte = mo_memory->read_byte( lv_addr ).

    IF lv_size_byte = 0.
      rv_next = 0.
    ELSE.
      rv_next = lv_size_byte MOD 32.
    ENDIF.
  ENDMETHOD.


  METHOD put_prop.
    " Set property value
    " Only works if property exists and is 1 or 2 bytes

    DATA(lv_prop_table) = get_prop_table_addr( iv_object ).
    IF lv_prop_table = 0.
      RETURN.
    ENDIF.

    DATA: lv_addr TYPE i,
          lv_size TYPE i.

    find_property(
      EXPORTING iv_prop_table = lv_prop_table
                iv_property = iv_property
      IMPORTING ev_addr = lv_addr
                ev_size = lv_size ).

    IF lv_addr = 0.
      " Property not found - cannot create
      RETURN.
    ENDIF.

    IF lv_size = 1.
      mo_memory->write_byte( iv_addr = lv_addr iv_val = iv_value MOD 256 ).
    ELSE.
      mo_memory->write_word( iv_addr = lv_addr iv_val = iv_value ).
    ENDIF.
  ENDMETHOD.


  METHOD get_object_name.
    " Get short name of object (Z-encoded string)
    " For now, return placeholder - full Z-string decoding is in executor

    DATA(lv_prop_table) = get_prop_table_addr( iv_object ).
    IF lv_prop_table = 0.
      rv_name = ''.
      RETURN.
    ENDIF.

    DATA(lv_text_len) = mo_memory->read_byte( lv_prop_table ).
    IF lv_text_len = 0.
      rv_name = ''.
      RETURN.
    ENDIF.

    " Text starts at prop_table + 1
    " For now return hex address - will decode Z-string later
    rv_name = |[name@{ lv_prop_table + 1 }]|.
  ENDMETHOD.

ENDCLASS.
