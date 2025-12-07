# Z-Machine v3 Opcode Reference

Complete reference for implementing Z-machine version 3 opcodes in ABAP.

## Notation

- **St** = Stores result in variable
- **Br** = Has branch data
- `a`, `b`, `c`, `d` = Operands
- `(result)` = Value stored
- `?(label)` = Branch condition

## 2OP Opcodes (Two Operand)

### 2OP:1 — je a b [c] [d] ?(label)

**Jump if Equal**

```abap
" Branch if a equals any of b, c, d
IF iv_a = iv_b OR iv_a = iv_c OR iv_a = iv_d.
  do_branch( abap_true ).
ELSE.
  do_branch( abap_false ).
ENDIF.
```

**Notes:**
- Can have 2-4 operands
- Used extensively for menu selection, object comparison

---

### 2OP:2 — jl a b ?(label)

**Jump if Less (signed)**

```abap
" Signed 16-bit comparison
DATA(lv_a) = signed_16( iv_a ).
DATA(lv_b) = signed_16( iv_b ).
do_branch( lv_a < lv_b ).

METHOD signed_16.
  IF iv_val >= 32768.
    rv_signed = iv_val - 65536.
  ELSE.
    rv_signed = iv_val.
  ENDIF.
ENDMETHOD.
```

---

### 2OP:3 — jg a b ?(label)

**Jump if Greater (signed)**

```abap
DATA(lv_a) = signed_16( iv_a ).
DATA(lv_b) = signed_16( iv_b ).
do_branch( lv_a > lv_b ).
```

---

### 2OP:4 — dec_chk (variable) value ?(label)

**Decrement and Check**

```abap
DATA(lv_val) = get_variable( iv_var ) - 1.
lv_val = lv_val MOD 65536.
IF lv_val >= 32768.
  lv_val = lv_val - 65536.  " For signed comparison
ENDIF.
set_variable( iv_var lv_val MOD 65536 ).
do_branch( lv_val < signed_16( iv_value ) ).
```

**Note:** First operand is variable *number*, not value.

---

### 2OP:5 — inc_chk (variable) value ?(label)

**Increment and Check**

```abap
DATA(lv_val) = get_variable( iv_var ) + 1.
lv_val = lv_val MOD 65536.
set_variable( iv_var lv_val ).
do_branch( signed_16( lv_val ) > signed_16( iv_value ) ).
```

---

### 2OP:6 — jin obj1 obj2 ?(label)

**Jump if In (object tree)**

```abap
DATA(lv_parent) = mo_objects->get_parent( iv_obj1 ).
do_branch( lv_parent = iv_obj2 ).
```

---

### 2OP:7 — test bitmap flags ?(label)

**Test Bits**

```abap
" Branch if all flags bits are set in bitmap
" Using arithmetic since no BIT-AND
DATA(lv_result) = abap_true.
DATA(lv_mask) = 1.
DO 16 TIMES.
  DATA(lv_flag_bit) = iv_flags MOD ( lv_mask * 2 ) DIV lv_mask.
  DATA(lv_bmap_bit) = iv_bitmap MOD ( lv_mask * 2 ) DIV lv_mask.
  IF lv_flag_bit = 1 AND lv_bmap_bit = 0.
    lv_result = abap_false.
    EXIT.
  ENDIF.
  lv_mask = lv_mask * 2.
ENDDO.
do_branch( lv_result ).
```

---

### 2OP:8 — or a b -> (result)

**Bitwise OR**

```abap
DATA(lv_result) = 0.
DATA(lv_mask) = 1.
DO 16 TIMES.
  DATA(lv_bit_a) = iv_a MOD ( lv_mask * 2 ) DIV lv_mask.
  DATA(lv_bit_b) = iv_b MOD ( lv_mask * 2 ) DIV lv_mask.
  IF lv_bit_a = 1 OR lv_bit_b = 1.
    lv_result = lv_result + lv_mask.
  ENDIF.
  lv_mask = lv_mask * 2.
ENDDO.
do_store( lv_result ).
```

---

### 2OP:9 — and a b -> (result)

**Bitwise AND**

```abap
DATA(lv_result) = 0.
DATA(lv_mask) = 1.
DO 16 TIMES.
  DATA(lv_bit_a) = iv_a MOD ( lv_mask * 2 ) DIV lv_mask.
  DATA(lv_bit_b) = iv_b MOD ( lv_mask * 2 ) DIV lv_mask.
  IF lv_bit_a = 1 AND lv_bit_b = 1.
    lv_result = lv_result + lv_mask.
  ENDIF.
  lv_mask = lv_mask * 2.
ENDDO.
do_store( lv_result ).
```

---

### 2OP:10 — test_attr object attribute ?(label)

**Test Attribute**

```abap
do_branch( mo_objects->test_attr( iv_obj = iv_obj iv_attr = iv_attr ) ).
```

---

### 2OP:11 — set_attr object attribute

**Set Attribute**

```abap
mo_objects->set_attr( iv_obj = iv_obj iv_attr = iv_attr ).
```

---

### 2OP:12 — clear_attr object attribute

**Clear Attribute**

```abap
mo_objects->clear_attr( iv_obj = iv_obj iv_attr = iv_attr ).
```

---

### 2OP:13 — store (variable) value

**Store Variable**

```abap
" First operand is variable number, not value
set_variable( iv_var = iv_variable iv_val = iv_value ).
```

**Note:** This is indirect store - variable number comes from operand.

---

### 2OP:14 — insert_obj object destination

**Insert Object**

```abap
" Remove from current parent, make first child of destination
mo_objects->insert_obj( iv_obj = iv_obj iv_dest = iv_dest ).
```

Implementation:
```abap
METHOD insert_obj.
  " First remove from current parent
  remove_obj( iv_obj ).

  " Get current first child of destination
  DATA(lv_first_child) = get_child( iv_dest ).

  " Make object's sibling point to old first child
  set_sibling( iv_obj = iv_obj iv_sibling = lv_first_child ).

  " Make destination's child point to object
  set_child( iv_obj = iv_dest iv_child = iv_obj ).

  " Set object's parent
  set_parent( iv_obj = iv_obj iv_parent = iv_dest ).
ENDMETHOD.
```

---

### 2OP:15 — loadw array word-index -> (result)

**Load Word**

```abap
" array is byte address, word-index is word offset
DATA(lv_addr) = iv_array + iv_index * 2.
do_store( mo_memory->read_word( lv_addr ) ).
```

---

### 2OP:16 — loadb array byte-index -> (result)

**Load Byte**

```abap
DATA(lv_addr) = iv_array + iv_index.
do_store( mo_memory->read_byte( lv_addr ) ).
```

---

### 2OP:17 — get_prop object property -> (result)

**Get Property**

```abap
DATA(lv_val) = mo_objects->get_prop( iv_obj = iv_obj iv_prop = iv_prop ).
do_store( lv_val ).
```

**Note:** Returns default value if property doesn't exist.

---

### 2OP:18 — get_prop_addr object property -> (result)

**Get Property Address**

```abap
DATA(lv_addr) = mo_objects->get_prop_addr( iv_obj = iv_obj iv_prop = iv_prop ).
do_store( lv_addr ).  " 0 if not found
```

---

### 2OP:19 — get_next_prop object property -> (result)

**Get Next Property**

```abap
DATA(lv_next) = mo_objects->get_next_prop( iv_obj = iv_obj iv_prop = iv_prop ).
do_store( lv_next ).  " 0 if no more properties
```

**Note:** If property = 0, returns first property number.

---

### 2OP:20 — add a b -> (result)

**Add (signed)**

```abap
DATA(lv_result) = ( iv_a + iv_b ) MOD 65536.
do_store( lv_result ).
```

---

### 2OP:21 — sub a b -> (result)

**Subtract (signed)**

```abap
DATA(lv_result) = ( iv_a - iv_b + 65536 ) MOD 65536.
do_store( lv_result ).
```

---

### 2OP:22 — mul a b -> (result)

**Multiply (signed)**

```abap
DATA(lv_a) = signed_16( iv_a ).
DATA(lv_b) = signed_16( iv_b ).
DATA(lv_result) = ( lv_a * lv_b ) MOD 65536.
IF lv_result < 0.
  lv_result = lv_result + 65536.
ENDIF.
do_store( lv_result ).
```

---

### 2OP:23 — div a b -> (result)

**Divide (signed)**

```abap
IF iv_b = 0.
  " Division by zero - halt interpreter
  RAISE EXCEPTION...
ENDIF.
DATA(lv_a) = signed_16( iv_a ).
DATA(lv_b) = signed_16( iv_b ).
DATA(lv_result) = lv_a DIV lv_b.
IF lv_result < 0.
  lv_result = lv_result + 65536.
ENDIF.
do_store( lv_result MOD 65536 ).
```

---

### 2OP:24 — mod a b -> (result)

**Modulo (signed)**

```abap
IF iv_b = 0.
  RAISE EXCEPTION...
ENDIF.
DATA(lv_a) = signed_16( iv_a ).
DATA(lv_b) = signed_16( iv_b ).
DATA(lv_result) = lv_a MOD lv_b.
IF lv_result < 0.
  lv_result = lv_result + 65536.
ENDIF.
do_store( lv_result MOD 65536 ).
```

---

## 1OP Opcodes (One Operand)

### 1OP:0 — jz a ?(label)

**Jump if Zero**

```abap
do_branch( iv_a = 0 ).
```

---

### 1OP:1 — get_sibling object -> (result) ?(label)

**Get Sibling**

```abap
DATA(lv_sibling) = mo_objects->get_sibling( iv_obj ).
do_store( lv_sibling ).
do_branch( lv_sibling <> 0 ).
```

---

### 1OP:2 — get_child object -> (result) ?(label)

**Get Child**

```abap
DATA(lv_child) = mo_objects->get_child( iv_obj ).
do_store( lv_child ).
do_branch( lv_child <> 0 ).
```

---

### 1OP:3 — get_parent object -> (result)

**Get Parent**

```abap
do_store( mo_objects->get_parent( iv_obj ) ).
```

---

### 1OP:4 — get_prop_len property-address -> (result)

**Get Property Length**

```abap
IF iv_addr = 0.
  do_store( 0 ).
ELSE.
  do_store( mo_objects->get_prop_len( iv_addr ) ).
ENDIF.
```

---

### 1OP:5 — inc (variable)

**Increment**

```abap
DATA(lv_val) = ( get_variable( iv_var ) + 1 ) MOD 65536.
set_variable( iv_var = iv_var iv_val = lv_val ).
```

---

### 1OP:6 — dec (variable)

**Decrement**

```abap
DATA(lv_val) = ( get_variable( iv_var ) - 1 + 65536 ) MOD 65536.
set_variable( iv_var = iv_var iv_val = lv_val ).
```

---

### 1OP:7 — print_addr byte-address-of-string

**Print at Address**

```abap
DATA(lv_text) = mo_text->decode_string( iv_addr ).
mo_io->print_text( lv_text ).
```

---

### 1OP:9 — remove_obj object

**Remove Object**

```abap
mo_objects->remove_obj( iv_obj ).
```

Implementation:
```abap
METHOD remove_obj.
  DATA(lv_parent) = get_parent( iv_obj ).
  IF lv_parent = 0.
    RETURN.  " Already detached
  ENDIF.

  DATA(lv_sibling) = get_sibling( iv_obj ).
  DATA(lv_first_child) = get_child( lv_parent ).

  IF lv_first_child = iv_obj.
    " Object is first child - update parent's child pointer
    set_child( iv_obj = lv_parent iv_child = lv_sibling ).
  ELSE.
    " Find previous sibling
    DATA(lv_prev) = lv_first_child.
    WHILE get_sibling( lv_prev ) <> iv_obj.
      lv_prev = get_sibling( lv_prev ).
    ENDWHILE.
    " Link previous to our sibling
    set_sibling( iv_obj = lv_prev iv_sibling = lv_sibling ).
  ENDIF.

  " Clear object's parent and sibling
  set_parent( iv_obj = iv_obj iv_parent = 0 ).
  set_sibling( iv_obj = iv_obj iv_sibling = 0 ).
ENDMETHOD.
```

---

### 1OP:10 — print_obj object

**Print Object Name**

```abap
DATA(lv_name) = mo_objects->get_short_name( iv_obj ).
mo_io->print_text( lv_name ).
```

---

### 1OP:11 — ret value

**Return**

```abap
return_routine( iv_value ).
```

---

### 1OP:12 — jump ?(label)

**Unconditional Jump**

```abap
" Offset is signed 16-bit, already decoded
" Note: This is NOT a branch - it's direct jump
mv_pc = mv_pc + iv_offset - 2.
```

**Note:** Uses signed offset, not branch encoding.

---

### 1OP:13 — print_paddr packed-address

**Print at Packed Address**

```abap
" v3: packed address = 2 * operand
DATA(lv_addr) = iv_paddr * 2.
DATA(lv_text) = mo_text->decode_string( lv_addr ).
mo_io->print_text( lv_text ).
```

---

### 1OP:14 — load (variable) -> (result)

**Load Variable**

```abap
" Indirect load - operand is variable number
do_store( get_variable( iv_var ) ).
```

---

### 1OP:15 — not value -> (result)

**Bitwise NOT** (v1-4) / **call_1n** (v5+)

```abap
" v3: bitwise NOT
DATA(lv_result) = 65535 - iv_value.
do_store( lv_result ).
```

---

## 0OP Opcodes (Zero Operand)

### 0OP:0 — rtrue

**Return True**

```abap
return_routine( 1 ).
```

---

### 0OP:1 — rfalse

**Return False**

```abap
return_routine( 0 ).
```

---

### 0OP:2 — print (literal-string)

**Print Literal**

```abap
" String follows immediately after opcode
DATA(lv_text) = mo_text->decode_string(
  EXPORTING iv_addr = mv_pc
  IMPORTING ev_end_addr = mv_pc ).
mo_io->print_text( lv_text ).
```

---

### 0OP:3 — print_ret (literal-string)

**Print, Newline, Return True**

```abap
DATA(lv_text) = mo_text->decode_string(
  EXPORTING iv_addr = mv_pc
  IMPORTING ev_end_addr = mv_pc ).
mo_io->print_text( lv_text ).
mo_io->new_line( ).
return_routine( 1 ).
```

---

### 0OP:4 — nop

**No Operation**

```abap
" Do nothing
```

---

### 0OP:5 — save ?(label)

**Save Game** (v3 branches)

```abap
DATA(lv_success) = save_game( ).
do_branch( lv_success ).
```

---

### 0OP:6 — restore ?(label)

**Restore Game** (v3 branches)

```abap
DATA(lv_success) = restore_game( ).
IF lv_success = abap_true.
  " After restore, branch test happens with restored state
  do_branch( abap_true ).
ELSE.
  do_branch( abap_false ).
ENDIF.
```

---

### 0OP:7 — restart

**Restart Game**

```abap
" Reload story file, reset state
" Preserve: transcription bit, fixed-font bit
reset( ).
```

---

### 0OP:8 — ret_popped

**Return Stack Value**

```abap
DATA(lv_val) = mo_stack->pop_eval( ).
return_routine( lv_val ).
```

---

### 0OP:9 — pop

**Pop Stack** (v1-4) / **catch** (v5+)

```abap
" v3: discard top of stack
mo_stack->pop_eval( ).
```

---

### 0OP:10 — quit

**Quit**

```abap
mv_running = abap_false.
```

---

### 0OP:11 — new_line

**Print Newline**

```abap
mo_io->new_line( ).
```

---

### 0OP:12 — show_status

**Show Status Line** (v3 only)

```abap
" Read status from game state
DATA(lv_location_obj) = get_variable( 16 ).  " Global 0
DATA(lv_score_or_hours) = signed_16( get_variable( 17 ) ).  " Global 1
DATA(lv_moves_or_mins) = signed_16( get_variable( 18 ) ).   " Global 2

DATA(lv_location) = mo_objects->get_short_name( lv_location_obj ).

" Check status line type (bit 1 of Flags1)
DATA(lv_flags1) = mo_memory->read_byte( 1 ).
DATA(lv_is_time) = ( lv_flags1 MOD 4 DIV 2 ) = 1.

mo_io->show_status(
  iv_location = lv_location
  iv_score = lv_score_or_hours
  iv_moves = lv_moves_or_mins
  iv_hours = lv_score_or_hours
  iv_minutes = lv_moves_or_mins
  iv_is_time = lv_is_time ).
```

---

### 0OP:13 — verify ?(label)

**Verify Checksum**

```abap
DATA(lv_checksum) = calculate_checksum( ).
DATA(lv_expected) = mo_memory->read_word( 28 ).  " Header $1C
do_branch( lv_checksum = lv_expected ).
```

---

## VAR Opcodes (Variable Operand)

### VAR:0 — call routine [arg1] [arg2] [arg3] -> (result)

**Call Routine**

```abap
IF iv_routine = 0.
  " Call to address 0 returns false
  do_store( 0 ).
  RETURN.
ENDIF.

" Unpack address (v3: × 2)
DATA(lv_addr) = iv_routine * 2.

" Read routine header
DATA(lv_num_locals) = mo_memory->read_byte( lv_addr ).
lv_addr = lv_addr + 1.

" Create new stack frame
DATA: ls_frame TYPE ty_frame.
ls_frame-return_pc = mv_pc.
ls_frame-result_var = is_instr-store_var.
ls_frame-num_locals = lv_num_locals.

" Initialize locals from routine header (v1-4)
DO lv_num_locals TIMES.
  DATA(lv_local_val) = mo_memory->read_word( lv_addr ).
  " Store in frame locals string
  ls_frame-locals = ls_frame-locals && int_to_hex4( lv_local_val ).
  lv_addr = lv_addr + 2.
ENDDO.

" Override with passed arguments
" (update ls_frame-locals with actual args)

mo_stack->push_frame( ls_frame ).
mv_pc = lv_addr.
```

---

### VAR:1 — storew array word-index value

**Store Word**

```abap
DATA(lv_addr) = iv_array + iv_index * 2.
mo_memory->write_word( iv_addr = lv_addr iv_val = iv_value ).
```

---

### VAR:2 — storeb array byte-index value

**Store Byte**

```abap
DATA(lv_addr) = iv_array + iv_index.
mo_memory->write_byte( iv_addr = lv_addr iv_val = iv_value MOD 256 ).
```

---

### VAR:3 — put_prop object property value

**Put Property**

```abap
mo_objects->put_prop( iv_obj = iv_obj iv_prop = iv_prop iv_val = iv_value ).
```

---

### VAR:4 — sread text parse

**Read Input** (v1-4: sread, v5+: aread)

```abap
" text = buffer for input, parse = buffer for parsed words
DATA(lv_max_len) = mo_memory->read_byte( iv_text ).
DATA(lv_input) = mo_io->read_line( lv_max_len ).

" Store input in text buffer
DATA(lv_len) = strlen( lv_input ).
mo_memory->write_byte( iv_addr = iv_text + 1 iv_val = lv_len ).
DO lv_len TIMES.
  DATA(lv_char) = lv_input+( sy-index - 1 )(1).
  mo_memory->write_byte( iv_addr = iv_text + 1 + sy-index iv_val = char_to_zscii( lv_char ) ).
ENDDO.

" Tokenize input
tokenize( iv_text = iv_text iv_parse = iv_parse iv_input = lv_input ).
```

---

### VAR:5 — print_char zscii-char

**Print Character**

```abap
mo_io->print_char( iv_char ).
```

---

### VAR:6 — print_num value

**Print Number**

```abap
DATA(lv_signed) = signed_16( iv_value ).
DATA(lv_text) = |{ lv_signed }|.
mo_io->print_text( lv_text ).
```

---

### VAR:7 — random range -> (result)

**Random Number**

```abap
IF signed_16( iv_range ) <= 0.
  " Seed random number generator
  seed_random( iv_range ).
  do_store( 0 ).
ELSE.
  " Return random 1..range
  DATA(lv_rand) = next_random( ) MOD iv_range + 1.
  do_store( lv_rand ).
ENDIF.
```

---

### VAR:8 — push value

**Push to Stack**

```abap
mo_stack->push_eval( iv_value ).
```

---

### VAR:9 — pull (variable)

**Pull from Stack**

```abap
DATA(lv_val) = mo_stack->pop_eval( ).
set_variable( iv_var = iv_variable iv_val = lv_val ).
```

---

## Helper Methods

### do_branch

```abap
METHOD do_branch.
  " is_instr contains branch data
  IF iv_condition = is_instr-branch_on.
    " Branch taken
    IF is_instr-branch_off = 0.
      return_routine( 0 ).  " Return false
    ELSEIF is_instr-branch_off = 1.
      return_routine( 1 ).  " Return true
    ELSE.
      mv_pc = mv_pc + is_instr-branch_off - 2.
    ENDIF.
  ENDIF.
  " Else: continue to next instruction (PC already advanced)
ENDMETHOD.
```

### do_store

```abap
METHOD do_store.
  IF is_instr-has_store = abap_true.
    set_variable( iv_var = is_instr-store_var iv_val = iv_val ).
  ENDIF.
ENDMETHOD.
```

### get_variable / set_variable

```abap
METHOD get_variable.
  CASE iv_var.
    WHEN 0.
      " Stack
      rv_val = mo_stack->pop_eval( ).
    WHEN 1 THRU 15.
      " Local
      rv_val = mo_stack->get_local( iv_var ).
    WHEN OTHERS.
      " Global
      rv_val = get_global( iv_var ).
  ENDCASE.
ENDMETHOD.

METHOD set_variable.
  CASE iv_var.
    WHEN 0.
      mo_stack->push_eval( iv_val ).
    WHEN 1 THRU 15.
      mo_stack->set_local( iv_num = iv_var iv_val = iv_val ).
    WHEN OTHERS.
      set_global( iv_var = iv_var iv_val = iv_val ).
  ENDCASE.
ENDMETHOD.
```

### return_routine

```abap
METHOD return_routine.
  DATA(ls_frame) = mo_stack->pop_frame( ).
  mv_pc = ls_frame-return_pc.

  IF ls_frame-result_var >= 0.
    set_variable( iv_var = ls_frame-result_var iv_val = iv_value ).
  ENDIF.
ENDMETHOD.
```

---

## Opcode Quick Reference Table

| Opcode | Type | Mnemonic | St | Br | Description |
|--------|------|----------|----|----|-------------|
| 1 | 2OP | je | | Y | Jump if equal |
| 2 | 2OP | jl | | Y | Jump if less |
| 3 | 2OP | jg | | Y | Jump if greater |
| 4 | 2OP | dec_chk | | Y | Decrement and branch |
| 5 | 2OP | inc_chk | | Y | Increment and branch |
| 6 | 2OP | jin | | Y | Jump if child of |
| 7 | 2OP | test | | Y | Test bitmap |
| 8 | 2OP | or | Y | | Bitwise OR |
| 9 | 2OP | and | Y | | Bitwise AND |
| 10 | 2OP | test_attr | | Y | Test attribute |
| 11 | 2OP | set_attr | | | Set attribute |
| 12 | 2OP | clear_attr | | | Clear attribute |
| 13 | 2OP | store | | | Indirect store |
| 14 | 2OP | insert_obj | | | Insert object |
| 15 | 2OP | loadw | Y | | Load word |
| 16 | 2OP | loadb | Y | | Load byte |
| 17 | 2OP | get_prop | Y | | Get property |
| 18 | 2OP | get_prop_addr | Y | | Get property address |
| 19 | 2OP | get_next_prop | Y | | Get next property |
| 20 | 2OP | add | Y | | Addition |
| 21 | 2OP | sub | Y | | Subtraction |
| 22 | 2OP | mul | Y | | Multiplication |
| 23 | 2OP | div | Y | | Division |
| 24 | 2OP | mod | Y | | Modulo |
| 128 | 1OP | jz | | Y | Jump if zero |
| 129 | 1OP | get_sibling | Y | Y | Get sibling object |
| 130 | 1OP | get_child | Y | Y | Get child object |
| 131 | 1OP | get_parent | Y | | Get parent object |
| 132 | 1OP | get_prop_len | Y | | Get property length |
| 133 | 1OP | inc | | | Increment |
| 134 | 1OP | dec | | | Decrement |
| 135 | 1OP | print_addr | | | Print at address |
| 137 | 1OP | remove_obj | | | Remove object |
| 138 | 1OP | print_obj | | | Print object name |
| 139 | 1OP | ret | | | Return |
| 140 | 1OP | jump | | | Unconditional jump |
| 141 | 1OP | print_paddr | | | Print packed address |
| 142 | 1OP | load | Y | | Indirect load |
| 143 | 1OP | not | Y | | Bitwise NOT (v1-4) |
| 176 | 0OP | rtrue | | | Return true |
| 177 | 0OP | rfalse | | | Return false |
| 178 | 0OP | print | | | Print literal |
| 179 | 0OP | print_ret | | | Print + newline + rtrue |
| 180 | 0OP | nop | | | No operation |
| 181 | 0OP | save | | Y | Save game (v3) |
| 182 | 0OP | restore | | Y | Restore game (v3) |
| 183 | 0OP | restart | | | Restart game |
| 184 | 0OP | ret_popped | | | Return stack top |
| 185 | 0OP | pop | | | Discard stack top (v1-4) |
| 186 | 0OP | quit | | | Quit |
| 187 | 0OP | new_line | | | Print newline |
| 188 | 0OP | show_status | | | Show status (v3) |
| 189 | 0OP | verify | | Y | Verify checksum |
| 224 | VAR | call | Y | | Call routine |
| 225 | VAR | storew | | | Store word |
| 226 | VAR | storeb | | | Store byte |
| 227 | VAR | put_prop | | | Set property |
| 228 | VAR | sread | | | Read input (v1-4) |
| 229 | VAR | print_char | | | Print character |
| 230 | VAR | print_num | | | Print number |
| 231 | VAR | random | Y | | Random number |
| 232 | VAR | push | | | Push to stack |
| 233 | VAR | pull | | | Pull from stack |

---

*Document created for ABAP Z-Machine Interpreter project*
*Version: 1.0*
*Date: 2025-12-07*
