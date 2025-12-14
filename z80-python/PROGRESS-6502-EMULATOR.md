# 6502 Emulator on Z80 - Progress Report

**Date:** 2025-12-12
**Status:** Phase 2 Complete, Phase 3 Design Ready

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                     АРХИТЕКТУРА                             │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   6502 binary ──► Converter (Python) ──► Threaded code      │
│                                              │              │
│                                              ▼              │
│                     ┌────────────────────────────────┐      │
│                     │      Z80 RUNTIME (native asm)  │      │
│                     ├────────────────────────────────┤      │
│                     │  Handler Table ($7000-$7FFF)   │      │
│                     │  ├─ LDA# handler               │      │
│                     │  ├─ STA handler                │      │
│                     │  ├─ JSR handler ──┐            │      │
│                     │  └─ ...           │            │      │
│                     │                   ▼            │      │
│                     │  HLE Traps ($6000-$6FFF)       │      │
│                     │  ├─ TRAP_COUT (native I/O)     │      │
│                     │  ├─ TRAP_GETLN (native I/O)    │      │
│                     │  └─ TRAP_HOME                  │      │
│                     └────────────────────────────────┘      │
│                                              │              │
│                                              ▼              │
│                                      VZ80 / Real ZX         │
│                                                             │
└─────────────────────────────────────────────────────────────┘

Python = только конвертер + тестирование
Z80 asm = весь runtime, включая HLE traps
```

---

## Phase 3 Design: Z80 Native HLE

### Two Banking Designs

We have two banking designs optimized for different targets:

| Design | Banks | Window | Target | Status |
|--------|-------|--------|--------|--------|
| **A** (primary) | 8 × 16KB | $C000-$FFFF | ZX 128K, real HW | Active |
| **B** (alternative) | 4 × 32KB | $0000-$7FFF | VZ80, ZX Next | Future |

---

### Design A: 16KB @ $C000 (Primary)

**Memory Map:**
```
Z80 Memory:
$6000-$6FFF   HLE Trap handlers (COUT, GETLN, HOME, etc.)
$7000-$7FFF   Handler Table (16-way interleaved)
$8000-$80FF   6502 Zero Page (linear, fast access)
$8100-$81FF   6502 Stack (linear)
$8200-$82FF   Input buffer, vectors
$8300-$BFFF   Runtime code, shadow stack
$C000-$FFFF   Banked window (16KB, 8 banks)
```

**Banking:**
```
6502 space:  64KB
× 2 expansion: 128KB
÷ 8 banks:     16KB per bank
```

**Bank Mapping:**

| Bank | 6502 Range    | Z80 Window    | OUT Value |
|------|---------------|---------------|-----------|
| 0    | $0000-$1FFF   | $C000-$FFFF   | $00       |
| 1    | $2000-$3FFF   | $C000-$FFFF   | $20       |
| 2    | $4000-$5FFF   | $C000-$FFFF   | $40       |
| 3    | $6000-$7FFF   | $C000-$FFFF   | $60       |
| 4    | $8000-$9FFF   | $C000-$FFFF   | $80       |
| 5    | $A000-$BFFF   | $C000-$FFFF   | $A0       |
| 6    | $C000-$DFFF   | $C000-$FFFF   | $C0       |
| 7    | $E000-$FFFF   | $C000-$FFFF   | $E0       |

**Address Translation (10 instructions):**
```z80
; READ_6502: DE = 6502 addr → A = byte value

READ_6502:
    ; 1. Bank select (upper 3 bits direct to port)
    LD A, D
    AND $E0
    OUT (0), A          ; port accepts bits 7-5 directly

    ; 2. Calculate Z80 address
    LD A, D
    AND $1F             ; offset high (5 bits for 8KB)
    SLA E               ; × 2 low byte
    RLA                 ; × 2 high byte + carry
    OR $C0              ; add base $C000
    LD D, A             ; DE = Z80 address

    LD A, (DE)          ; read byte
    RET
```

---

### Design B: 32KB @ $0000 (Alternative - VZ80/Next)

**Memory Map:**
```
Z80 Memory:
$0000-$7FFF   Banked window (32KB, 4 banks) ← code lives here
$8000-$80FF   6502 Zero Page (linear)
$8100-$81FF   6502 Stack (linear)
$8200-$8FFF   Buffers, runtime data
$9000-$9FFF   HLE Trap handlers
$A000-$BFFF   Handler Table (expanded)
```

**Banking:**
```
6502 space:  64KB
× 2 expansion: 128KB
÷ 4 banks:     32KB per bank
```

**Bank Mapping:**

| Bank | 6502 Range    | Z80 Window    | OUT Value |
|------|---------------|---------------|-----------|
| 0    | $0000-$3FFF   | $0000-$7FFF   | $00       |
| 1    | $4000-$7FFF   | $0000-$7FFF   | $40       |
| 2    | $8000-$BFFF   | $0000-$7FFF   | $80       |
| 3    | $C000-$FFFF   | $0000-$7FFF   | $C0       |

**Address Translation (8 instructions - faster!):**
```z80
; READ_6502_FAST: DE = 6502 addr → A = byte value

READ_6502_FAST:
    ; 1. Bank select (upper 2 bits direct to port)
    LD A, D
    AND $C0
    OUT (0), A          ; port accepts bits 7-6 directly

    ; 2. Calculate Z80 address (no base needed!)
    LD A, D
    AND $3F             ; offset high (6 bits for 16KB)
    SLA E               ; × 2 low byte
    RLA                 ; × 2 high byte + carry
    LD D, A             ; DE = Z80 address (no OR needed!)

    LD A, (DE)          ; read byte
    RET
```

**Advantages of Design B:**
- 2 fewer instructions (no `OR $C0`)
- Simpler address math
- Works on ZX Next (8KB MMU can map anywhere)
- Ideal for VZ80 emulator

### Handler Types

**ZP Handlers (fast, no banking):**
```z80
; 6502: LDA $xx (opcode $A5)
HANDLER_LDA_ZP:
    POP HL              ; L = ZP addr from stream
    LD H, $80           ; ZP at $8000-$80FF
    LD A, (HL)
    LD (REG_A), A
    JP NEXT

; ~8 T-states
```

**Absolute Handlers (with banking):**
```z80
; 6502: LDA $yyxx (opcode $AD)
HANDLER_LDA_ABS:
    POP DE              ; DE = full 6502 addr
    CALL READ_6502      ; banking + read
    LD (REG_A), A
    JP NEXT

; ~50+ T-states
```

### HLE Trap Mechanism

**ROM area detection in JSR handler:**

```z80
HANDLER_JSR:
    POP BC              ; BC = target address

    ; Check if ROM area ($FC00+)
    LD A, B
    CP $FC
    JR C, .normal_jsr

    ; ROM trap! Read handler addr from trap table
    ; Trap table at $8C00 (mirrors 6502 $FC00-$FFFF)
    LD H, $8C
    LD L, C             ; HL = $8Cxx
    LD A, B
    AND $03
    OR $8C
    LD H, A             ; HL = trap table entry

    LD E, (HL)
    INC HL
    LD D, (HL)          ; DE = trap handler

    EX DE, HL
    JP (HL)             ; jump to HLE handler

.normal_jsr:
    ; Normal JSR - push return, jump to target
    ...
```

**Trap Handlers (native Z80):**

```z80
; At $6010
TRAP_COUT:
    LD A, (REG_A)       ; 6502 A register
    OUT ($01), A        ; VZ80 console output
    RET

; At $6020
TRAP_GETLN:
    LD HL, $8200        ; Buffer at 6502 $0200
.loop:
    IN A, ($01)         ; read char
    CP $0D              ; CR?
    JR Z, .done
    LD (HL), A
    INC HL
    JR .loop
.done:
    ; Store length in REG_X
    RET

; At $6030
TRAP_HOME:
    LD A, $0C           ; Form feed (clear)
    OUT ($01), A
    RET
```

### Key Trap Addresses

| 6502 Addr | Function | Z80 Handler |
|-----------|----------|-------------|
| $FDED     | COUT     | $6010       |
| $FD6A     | GETLN    | $6020       |
| $FC58     | HOME     | $6030       |

---

## Completed Work

### Phase 1: Foundation ✓

- VZ80 Virtual Machine (`vz80.py`)
- Z80 Emulator with banking support
- ZORK1.COM runs successfully

### Phase 2: Python Runtime ✓

- 6502-to-Z80 converter (`conv_6502_to_z80_v2.py`)
- 83 opcode handlers implemented
- RET-threading architecture
- Test with reference Python runtime

---

## Project Structure

```
z80-python/
├── vz80.py                    # Virtual Z80 machine
├── z80.py                     # Z80 CPU emulator
├── conv_6502_to_z80_v2.py     # 6502 converter (Phase 2)
├── runtime_6502.py            # Python reference runtime
├── cpm_z80.py                 # CP/M emulator
├── test_z80.py                # Z80 unit tests (59 tests)
└── PROGRESS-6502-EMULATOR.md  # This file

test-games/
├── ZORK1.COM                  # CP/M ZORK
└── ZORK1.DAT                  # ZORK data
```

---

## Next Steps: Phase 3 Implementation

### 3.1 Z80 Runtime Generator

Create Python script that generates Z80 assembly:

```python
# gen_z80_runtime.py
def generate_handler_table() -> bytes:
    """Generate 16-way interleaved handler table"""
    ...

def generate_trap_handlers() -> bytes:
    """Generate HLE trap handlers (COUT, GETLN, etc.)"""
    ...

def generate_runtime() -> bytes:
    """Generate complete Z80 runtime binary"""
    ...
```

### 3.2 Tasks

- [ ] Write Z80 runtime generator
- [ ] Implement handler table ($7000-$7FFF)
- [ ] Implement HLE traps ($6000-$6FFF)
- [ ] Implement READ_6502 / WRITE_6502 with banking
- [ ] Test on VZ80 emulator
- [ ] Test on real ZX Spectrum 128K

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Threading | RET-threading | SP = PC, fast dispatch |
| Handler Table | 16-way interleaved | Self-documenting addresses |
| ROM Emulation | HLE in Z80 | Native speed, no Python |
| Banking | 8 × 16KB | 128KB code fits 3-bit select |
| ZP Access | Linear $8000 | 90% of accesses, no banking |
| Target | VZ80 → ZX 128K | Test → Real hardware |

---

## Performance Estimate

| Access Type | Instructions | Estimated T-states |
|-------------|--------------|-------------------|
| ZP read     | ~6           | ~24               |
| ZP write    | ~6           | ~24               |
| ABS read    | ~15          | ~60               |
| ABS write   | ~18          | ~72               |
| JSR (trap)  | ~20          | ~80               |
| JSR (normal)| ~25          | ~100              |

Typical 6502 code: 70% ZP, 30% ABS
→ Average: ~35 T-states per 6502 instruction
→ ~100x faster than byte-by-byte emulation

---

---

## Session Log: December 12, 2025

### First Successful Test!

**Test Program**: Print "HI\n" using LDA and JSR COUT

```asm
; 6502 code (16 bytes)
LDA #$48        ; 'H'
JSR $FDED       ; COUT
LDA #$49        ; 'I'
JSR $FDED       ; COUT
LDA #$0A        ; newline
JSR $FDED       ; COUT
BRK
```

**Result**: ✓ Output "HI\n" correctly in 86 Z80 cycles

### Key Bugs Fixed

1. **CALL corrupts threading stack** - Inlined all helper routines
2. **A register clobbered in JSR** - Added EX AF,AF' save/restore
3. **Trap table odd alignment** - Fixed fill order

### Files Created

- `gen_z80_runtime.py` - Z80 code generator
- `test_6502_runtime.py` - Test harness
- `runtime_6502_data.py` - Generated runtime module
- `REPORT-6502-Z80-EMULATOR.md` - Detailed technical report

---

## Session Log: December 12, 2025 (Session 2)

### Branch Instructions Fixed!

All branch instructions now work correctly:
- **BEQ/BNE** (equal/not equal) - ✓
- **BCS/BCC** (carry set/clear) - ✓ (fixed inverted carry logic)
- **BMI/BPL** (minus/plus) - ✓

### Key Bug Fixes

1. **LDX #imm double-pop bug**: Handler was popping twice, consuming next instruction's bytes
   - Fixed by removing redundant `pop_hl()`

2. **Branch SP modification incomplete**: Branch handlers popped offset but didn't modify SP
   - Added proper sign-extension and SP update code

3. **Carry flag inversion**: Z80 and 6502 have opposite carry semantics for CMP
   - 6502: C=1 when A >= M (no borrow)
   - Z80: C=1 when A < M (borrow)
   - Fixed BCS/BCC to check inverted condition

### Test Results

```
✓ BEQ taken: 'Y' == 'Y'
✓ BEQ not taken: 'AB' == 'AB'
✓ BNE taken: 'Y' == 'Y'
✓ BNE not taken: 'AB' == 'AB'
✓ BCS taken (5>=3): 'Y' == 'Y'
✓ BCS not taken (3<5): 'AB' == 'AB'
✓ BCC taken (3<5): 'Y' == 'Y'
✓ BCC not taken (5>=3): 'AB' == 'AB'
✓ Loop 1-5: '12345' == '12345'

9/9 tests passed
```

### Interactive Testing Tool

Created `interactive_6502.py` for easy testing:

```python
from interactive_6502 import *
emu = Emu6502()
result = emu.run(emu.print_string("Hello!") + brk())
# Output: 'Hello!'
```

### Files Modified

- `gen_z80_runtime.py` - Fixed LDX handler, branch instructions, added JP P/M
- `z80.py` - Added ED prefix instructions (LD (nn),rp)
- `interactive_6502.py` - New interactive testing tool
- `runtime_6502_data.py` - Regenerated with fixes

### Statistics

- **61 opcodes** implemented
- **679 bytes** of handler code
- **9/9** branch tests passing
- All basic tests (HI, ABC subroutine, count loop) passing

---

## Session Log: December 12, 2025 (Session 3)

### 6502 Echo Program Complete!

First interactive 6502 program running on Z80 threaded code emulator.

**Program Features:**
- Reads keyboard input via RDKEY trap ($FD0C)
- Echoes characters via COUT trap ($FDED)
- Doubles digits 0-9 (e.g., "42" → "4422")
- Exits on ESC key ($1B)

**Test Result:**
```
Input:  "Test 42 OK!\x1b"
Output: "Test 4422 OK!"
```

### Code (29 bytes of 6502)

```asm
        ORG $0800
loop:   JSR $FD0C       ; RDKEY - read key into A
        CMP #$00        ; no input?
        BEQ loop        ; wait for input
        CMP #$1B        ; ESC?
        BEQ done        ; exit
        JSR $FDED       ; COUT - print char
        CMP #'0'        ; below '0'?
        BCC loop        ; not a digit
        CMP #'9'+1      ; above '9'?
        BCS loop        ; not a digit
        JSR $FDED       ; print digit again (double!)
        JMP loop
done:   BRK
```

### Fixes Applied

1. **RDKEY trap not in trap table** - Added all Apple II ROM traps:
   - $FDED → TRAP_COUT (character output)
   - $FD0C → TRAP_RDKEY (read key)
   - $FD6A → TRAP_GETLN (get line)
   - $FC58 → TRAP_HOME (clear screen)

### Current Capabilities

| Feature | Status |
|---------|--------|
| Load/Store (imm, zp, abs) | ✓ |
| Arithmetic (ADC, SBC) | ✓ |
| Logic (AND, ORA, EOR) | ✓ |
| Compare (CMP, CPX, CPY) | ✓ |
| Branches (BEQ, BNE, BCS, BCC, BMI, BPL) | ✓ |
| Jumps (JMP, JSR, RTS) | ✓ |
| Stack (PHA, PLA, PHP, PLP) | ✓ |
| Transfers (TAX, TXA, TAY, TYA) | ✓ |
| Inc/Dec (INX, DEX, INY, DEY, INC, DEC) | ✓ |
| ROM Traps (COUT, RDKEY, GETLN, HOME) | ✓ |
| Indexed addressing (abs,X abs,Y zp,X) | ✓ |
| Indirect addressing ((zp),Y (zp,X)) | ✓ |

### What's Next

- [ ] Add more opcodes as needed
- [ ] Test with larger 6502 programs
- [ ] Port to real ZX Spectrum 128K
- [ ] Try running simple Apple II software

---

## Session Log: December 12, 2025 (Session 4)

### Inline Data Works!

Discovered that data embedded in code stream works correctly because RET-threading preserves original bytes:

```
Original:     $48 ('H')
Threaded:     [$48, $74]
                ↑
           LDA abs,X reads this byte correctly!
```

**Why it works:**
- Each 6502 byte becomes 2-byte handler address: `[byte, 0x70|(byte&0x0F)]`
- READ_6502 doubles the address: `Z80_addr = $C000 + (6502_addr × 2)`
- At doubled address, the LOW byte is the original data!

### Simplified Architecture

Removed `.6502` packed format - no longer needed! Simple `.bin` files work:

```
run_6502.py                 # Unified runner
├── HELLO_WORLD_6502        # Built-in (65 bytes)
├── ECHO_6502               # Built-in (34 bytes)
├── COUNTER_6502            # Built-in (~20 bytes)
└── HELLO_NAME_6502         # Built-in (115 bytes, inline strings!)
```

### Hello Name Program

Complete interactive program with inline strings:

```
$ python3 run_6502.py -n
6502 Hello Name Program
Enter your name and press Enter
----------------------------------------
What is your name? Alice
Hello, Alice!
----------------------------------------
Done.
```

**Code structure:**
```asm
        ORG $0800
        ; Code section (~75 bytes)
        LDX #0
print:  LDA prompt,X    ; Uses LDA abs,X
        BEQ read_name
        JSR COUT
        ...
        ; Data section (inline, right after BRK)
prompt: .byte "What is your name? ",0
hello:  .byte "Hello, ",0
```

### Fixes Applied

1. **LF → CR+LF in output**: Raw terminal mode needs both for proper newlines
   ```python
   def _on_output(self, ch):
       if ch == '\n':
           print('\r\n', end='', flush=True)
       else:
           print(ch, end='', flush=True)
   ```

2. **Accept both CR and LF as input terminators**: Works with both Unix and DOS line endings

### Files Cleaned Up

- Removed: `*.6502` files (obsolete packed format)
- Removed: `pack_6502_binary()`, `unpack_6502_binary()`
- Removed: `HELLO_NAME_STRINGS` (no longer needed)
- Updated: `hello_name.bin`, `echo.bin`

### Memory Model Summary

```
6502 Address    Z80 Location        Access Method
─────────────────────────────────────────────────────
$0000-$00FF     $8000-$80FF         Direct (ZP_LINEAR_BASE)
$0100-$01FF     $8100-$81FF         Direct (Stack)
$0200-$03FF     $8200-$83FF         Direct (Buffers)
$0800+          Bank 0 @ $C000+     READ_6502 (×2 offset)
```

**Key insight:** Data in code area is readable via `LDA abs,X` because threading preserves original bytes in low position of each 2-byte pair.

---

## Session Log: December 13, 2025 (Session 5)

### Attempting Scott Adams Adventureland

Extracted and attempted to run Scott Adams' "Adventureland" (1978) - a classic text adventure originally written for TRS-80 (which uses Z80!). Ironic: games went Z80 → 6502 (Apple II port) → Z80 (our emulator).

**Binary extracted:**
- Source: `scottadamstextadventures1-6.dsk` (Apple II DOS 3.3)
- File: `adventureland.bin` (20,336 bytes)
- Load address: $0800

### Bug #1: Bank Select Shift

**Symptom:** JSR to $2E3C jumped to wrong address

**Root cause:** Bank select was outputting masked value instead of bank number:
```python
# WRONG: A = $20 for address $2xxx
asm.ld_a_d()
asm.and_n(0xE0)
asm.out_n_a(0x00)  # Outputs $20, not bank 1!

# FIXED: Shift right to get bank 0-7
asm.ld_a_d()
asm.and_n(0xE0)
asm.rlca()         # Rotate left 3× = shift right 5
asm.rlca()
asm.rlca()
asm.out_n_a(0x00)  # Outputs 1 for $2xxx
```

Fixed in 9 locations (READ_6502, WRITE_6502, ADDR_TO_Z80, and inline copies).

### Bug #2: Single-Bank Loading

**Symptom:** JSR to $2E3C returned to $0000 (bank 1 was empty)

**Root cause:** `load_program()` only loaded into bank 0:
```python
# WRONG: All code in bank 0
for i, b in enumerate(threaded):
    self.bus.banked_memory[0][code_offset + i] = b

# FIXED: Distribute across banks
for i in range(len(code)):
    addr_6502 = org + i
    bank = addr_6502 >> 13           # Bank = addr / 8192
    offset_in_bank = addr_6502 & 0x1FFF
    z80_offset = offset_in_bank * 2
    self.bus.banked_memory[bank][z80_offset] = threaded[i * 2]
    self.bus.banked_memory[bank][z80_offset + 1] = threaded[i * 2 + 1]
```

### Bug #3: ROM Traps Below $FC00

**Symptom:** Calls to $FBFD, $FBC1, $F801, $F6F0 not trapped

**Fix:** Added explicit checks in JSR handler for Apple II ROM routines below $FC00:
```python
# Check $FC00+ → use trap table
# Check $F800-$FBFF → explicit address checks:
#   $FBFD (VIDOUT) → TRAP_COUT
#   $FBC1 (BASCALC) → TRAP_STUB
#   $F801 (PLOT) → TRAP_STUB
#   $F6F0 (unknown) → TRAP_STUB
```

### Current State

**After fixes:** Execution progresses across banks correctly:
```
Step 122: 6502 $083A -> H_JSR
Step 161: 6502 $2E3D -> H_JMP_ABS  ← Now reaches bank 1!
Step 201: 6502 $0CB2 -> H_JSR
...
*** HALT at step 305: A=$79 ***  ← Missing opcode
```

**Missing opcodes needed by Adventureland:**
- $65 ADC zp (304 uses) - most critical
- $79 ADC abs,Y (50 uses) - caused halt
- $6D ADC abs (90 uses)
- $45 EOR zp (149 uses)
- $6C JMP (ind) (119 uses)
- And many more (~50 unique valid opcodes)

### Summary

| Issue | Status |
|-------|--------|
| Bank select calculation | ✓ Fixed |
| Multi-bank loading | ✓ Fixed |
| ROM traps $F800-$FBFF | ✓ Fixed |
| Missing opcodes (~50) | Pending |

The game now correctly navigates between banks. Next step: implement more 6502 opcodes to run Adventureland fully.

---

## Session Log: December 14, 2025 (Session 6)

### Decimal Mode (BCD) Arithmetic Implemented!

Added full BCD (Binary-Coded Decimal) support for ADC/SBC instructions, required for MS BASIC number handling.

**Key Achievement:** 19+1=20 test now passes correctly:
```python
# BCD addition: 0x19 + 0x01 = 0x20 (not 0x1A!)
result = emu.run_bcd_test()  # Returns 0x20 ✓
```

### BCD Implementation Details

The 6502's SED (Set Decimal) flag enables BCD mode where each nibble represents a digit 0-9:

```
Binary mode:  $19 + $01 = $1A (26 decimal)
BCD mode:     $19 + $01 = $20 (twenty in BCD)
```

**Z80 Implementation Challenge:** Z80's DAA instruction only works after ADD/SUB, not after arbitrary values. Solution: Full software BCD calculation.

```z80
; BCD ADC implementation (~50 instructions)
H_ADC_IMM_BCD:
    ; Check decimal mode flag
    LD A, (FLAGS_6502)
    AND F_DECIMAL
    JP Z, H_ADC_IMM          ; Binary mode

    ; BCD addition:
    ; 1. Add low nibbles
    ; 2. If > 9, add 6 (adjust)
    ; 3. Add high nibbles + carry
    ; 4. If > 9, add $60 (adjust)
    ; 5. Set carry if result > $99
```

### MS BASIC Status

**Working:**
- Memory size prompt displays correctly
- Accepts memory size input
- Shows "BYTES FREE" message
- Displays "MICROSOFT BASIC" banner
- Accepts input at "OK" prompt

**Issue:** PRINT command returns "?SN ERROR" (Syntax Error):
```
OK
PRINT 2+2
?SN ERROR
OK
```

### Current Investigation

The syntax error occurs during number parsing. BASIC's CHRGET routine (at ZP $00B1) correctly reads digits, but the number conversion may have issues:

1. **CHRGET** - Character fetch routine ✓ (verified working)
2. **Number parsing** - Builds BCD number in floating point accumulator
3. **Expression evaluation** - The error triggers here

**Hypothesis:** The issue may be related to:
- FP accumulator initialization
- BCD multiplication during number building (10 * digit)
- Zero page variable state

### Milestones Achieved

| Milestone | Date | Status |
|-----------|------|--------|
| First 6502 program on Z80 | Dec 12 | ✓ |
| Branch instructions working | Dec 12 | ✓ |
| Interactive echo program | Dec 12 | ✓ |
| Inline string data support | Dec 12 | ✓ |
| Multi-bank execution | Dec 13 | ✓ |
| MS BASIC prompts appear | Dec 14 | ✓ |
| BCD arithmetic (19+1=20) | Dec 14 | ✓ |
| PRINT command working | Dec 14 | In Progress |

### Technical Stats

- **Opcodes implemented:** ~150/256 (59%)
- **Code size:** gen_z80_runtime.py = 4,076 lines
- **Runtime size:** ~7.5 KB
- **Test programs:** 6 (hello, echo, counter, hello_name, adventureland, msbasic)

### Files Modified This Session

- `gen_z80_runtime.py` - Added BCD ADC/SBC handlers
- `run_6502.py` - Added entry point option (-E) for separate load/start addresses
- `vz80.py` - Added I/O status port
- `runtime_6502.bin` - Regenerated with BCD support
- `runtime_6502_data.py` - Regenerated

### Next Steps

1. **Debug MS BASIC ?SN ERROR**
   - Trace number parsing in detail
   - Check FP accumulator operations
   - Verify BCD multiplication

2. **Add missing opcodes** as needed:
   - ROR, ROL (rotate through carry)
   - ASL, LSR (shifts)
   - Additional addressing modes

3. **Hardware target** - ZX Spectrum 128K

---

## Challenges Encountered

### Challenge 1: BCD Without DAA

Z80's DAA instruction only works immediately after ADD/SUB. Since we use A for address calculations between the actual addition and adjustment, DAA doesn't work. Solution: Full software BCD using nibble-by-nibble calculation.

### Challenge 2: I/O Polling

MS BASIC uses memory-mapped I/O at $FFF1 for keyboard input. The polling loop would hang without proper status indication:
```asm
poll:   LDA $FFF1    ; Read key
        BEQ poll     ; Loop if no key
```

Fixed by adding $FFF0 as status port that returns non-zero when input is available.

### Challenge 3: Separate Load/Entry Addresses

MS BASIC loads at $0000 but execution starts at $0400. Added `-E` option to run_6502.py:
```bash
python3 run_6502.py -f msbasic.bin -o 0x0000 -E 0x0400
```

---

*Last updated: 2025-12-14*
*Current phase: Phase 4 - MS BASIC Running, BCD Arithmetic Complete*
