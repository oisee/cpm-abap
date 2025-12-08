# Z-Machine Debugging Report: The Grue Mystery

## Summary

We investigated why the LOOK command in our Z-Machine v3 interpreter caused an immediate grue death at "West of House" - an outdoor, daylight location where darkness should be impossible.

## Key Findings

### 1. Attribute 1 is NOT Used for Light

**Initial assumption**: Z-Machine uses attribute 1 for "light source" objects.

**Reality**: In MiniZork, **NO objects have attribute 1 set** at all!

```
=== Objects with Attribute 1 ===
  NONE!
```

### 2. Attribute 3 is the "Lit Room" Flag

By tracing `test_attr` calls during LOOK, we discovered:

```
test_attr(167,14)=F   ← First check
test_attr(46,3)=F     ← Room 46 checked for attr 3!
test_attr(24,13)=F
test_attr(114,3)=F
test_attr(30,14)=T    ← Object 30 has attr 14
```

**Key insight**: Room 46 (West of House) was checked for **attribute 3**, which returned FALSE.

### 3. Setting Attribute 3 Fixes Darkness

When we set attribute 3 on room 46:
- Grue death disappeared!
- Game responded with "You can't go that way" (different issue - tokenization)

```abap
lo_obj->set_attr( iv_object = 46 iv_attr = 3 ).  " Makes room "lit"
```

### 4. Object Structure Discovered

| Object | Role | Parent | Notes |
|--------|------|--------|-------|
| 46 | West of House (room) | 0 | Has attrs 5, 7, 19 (NOT 3!) |
| 167 | Player? | 46 | In room 46, has child 89 |
| 30 | Light source? | 46 | Has attr 14 |

### 5. Z-Machine Bit Numbering

Z-Machine uses **big-endian bit ordering** within bytes:

```
Byte 0:  bit7  bit6  bit5  bit4  bit3  bit2  bit1  bit0
Attr:    0     1     2     3     4     5     6     7
```

For `0x05` = `00000101`:
- Attribute 5 SET (bit 2 = 1)
- Attribute 7 SET (bit 0 = 1)
- Attribute 3 NOT SET (bit 4 = 0)

## Root Cause Hypothesis

MiniZork's story file appears to be missing the "lit" attribute (attr 3) on outdoor rooms. This could be:

1. **Corrupted story file** - data issue during extraction/conversion
2. **Different game version** - MiniZork might expect different initialization
3. **Missing game logic** - some routine should set attr 3 but isn't running

## Remaining Issues

### Tokenization Problem

All commands return "You can't go that way":
- LOOK, L, INVENTORY, NORTH all fail
- Dictionary lookup appears broken or words aren't being recognized

This is a separate bug from the light attribute issue.

## Technical Details

### Classes Modified

1. **ZCL_ZORK_00_OBJECTS** - Added tracing support:
   ```abap
   CLASS-METHODS enable_trace, disable_trace, get_log, clear_log.
   ```

2. **ZORK_00_OBJ_DUMP** - Debug program for testing

### Attribute Meanings in MiniZork (Observed)

| Attr | Meaning (hypothesis) |
|------|---------------------|
| 3 | Room is lit / has natural light |
| 5 | Is a room? |
| 7 | Common flag (unknown) |
| 14 | Is a light source |
| 19 | Another room flag? |

## Next Steps

1. **Verify MiniZork binary integrity** - Compare hashes
2. **Test with reference interpreter** - Run MiniZork in Frotz/other
3. **Test with known-good Z3 story** - Use a test adventure
4. **Fix tokenization** - Investigate dictionary/parser issues

## Timeline

- Created ZCL_ZORK_00_DEBUG class with object dumper
- Discovered NO objects have attribute 1
- Traced test_attr calls during LOOK
- Identified attribute 3 as the "lit room" flag
- Confirmed fix by setting attr 3 on room 46
- Uncovered secondary tokenization issue

---

*Debug session conducted on 2025-12-08*
*Z-Machine interpreter: ABAP implementation for SAP*
