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

*Last updated: 2025-12-12*
*Current phase: Phase 3 Design Complete*
