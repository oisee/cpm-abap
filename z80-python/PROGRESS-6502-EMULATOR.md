# 6502 Emulator on Z80 - Progress Report

**Date:** 2025-12-12
**Status:** Phase 1 - Foundation Ready

---

## Completed Work

### 1. VZ80 Virtual Machine (`vz80.py`)

Target platform for 6502 emulator is now implemented and tested.

```
┌─────────────────────────────────────────────────────────────┐
│                  VZ80 VIRTUAL MACHINE                       │
├─────────────────────────────────────────────────────────────┤
│  CPU:     Z80 (using z80.py)                               │
│  Fixed:   $0000-$BFFF (48 KB)                              │
│  Banked:  $C000-$FFFF (256 pages × 16 KB = 4 MB)           │
│  I/O:     Port $00 = bank select, Port $01 = console       │
└─────────────────────────────────────────────────────────────┘
```

**Tests Passing:**
- Hello World (console output)
- Bank Switching (4 MB memory)
- Counter (DJNZ loop)
- Input Echo (console input)

### 2. Z80 Emulator Fixes

- Added missing `DJNZ` instruction to `z80.py`
- All 59 unit tests passing
- ZORK1.COM runs successfully

### 3. Architecture Design (`report-6502-emulator.md`)

Complete design document covering:
- RET-threading technique (SP = PC)
- 16-way interleaved handler table
- HLE (High-Level Emulation) for ROM calls
- Memory map for ZX Spectrum 128K
- Python converter architecture

---

## Project Structure

```
z80-python/
├── vz80.py                 # NEW: Virtual Z80 machine (target platform)
├── z80.py                  # Z80 CPU emulator (simplified)
├── zcl_cpu_z80.py          # Z80 CPU emulator (ABAP-style, full)
├── cpm_z80.py              # CP/M emulator (runs ZORK)
├── test_z80.py             # Z80 unit tests (59 tests)
├── run_zork.py             # ZORK runner
└── play_zork.py            # Interactive ZORK

test-games/
├── ZORK1.COM               # CP/M ZORK executable
└── ZORK1.DAT               # ZORK data file

report-6502-emulator.md     # Architecture design document
```

---

## Next Steps: 6502 Emulator Implementation

### Phase 1: Python Converter (TODO)

```
converter/
├── main.py              # CLI entry point
├── opcodes.py           # 6502 opcode table (256 entries)
├── translator.py        # 6502 binary → threaded code
├── traps.py             # ROM address → trap handler mapping
├── memory_map.py        # Address translation logic
└── tests/
```

**Tasks:**
- [ ] Create opcode table (256 entries with addressing modes)
- [ ] Implement handler address calculation: `($70 | (opcode & $0F)) << 8 | opcode`
- [ ] Write basic translator (no traps)
- [ ] Test with trivial 6502 program

### Phase 2: Z80 Runtime (TODO)

**Memory Layout:**
```
$6000-$6FFF    Runtime: long handlers, traps, utilities
$7000-$7FFF    Handler Table (16-way interleaved, 4 KB)
$8000-$BFFF    6502 DATA Memory (raw bytes)
$C000-$FFFF    6502 CODE Stream (threaded, banked)
```

**Tasks:**
- [ ] Create handler table skeleton
- [ ] Implement ~20 common opcodes (LDA, STA, JMP, JSR, RTS, etc.)
- [ ] Test: "Hello World" print loop

### Phase 3: Scott Adams Integration (TODO)

Target: Run Scott Adams adventure games (SAGA interpreter)

**Required Traps:**
- `$FD6A` GETLN - Read line input
- `$FDED` COUT - Output character
- `$FC58` HOME - Clear screen

---

## Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Threading Model | RET-threading | SP = PC, fast dispatch |
| Handler Table | 16-way interleaved | Self-documenting addresses |
| ROM Emulation | HLE (traps) | 10x faster than emulation |
| Target Platform | VZ80 (virtual) | Portable, testable |
| Converter | Python | Easy to write/debug |

---

## Test Commands

```bash
# VZ80 tests
cd z80-python
python3 vz80.py

# Z80 unit tests
python3 test_z80.py

# ZORK test
python3 run_zork.py "look" "open mailbox"

# Interactive ZORK
python3 play_zork.py
```

---

*Last updated: 2025-12-12*
