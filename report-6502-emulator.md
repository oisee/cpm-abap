# 6502 Emulator for ZX Spectrum 128K
## Architecture Design Report

**Date:** 2025-12-12
**Status:** Design Phase
**Target:** ZX Spectrum 128K (expandable to 256K+)

---

## 1. Executive Summary

This document describes a high-performance 6502 emulator for ZX Spectrum using **RET-threading** (also known as "subroutine threading" or "direct threading"). The key innovation is using Z80's SP register as the 6502 program counter, achieving near-native speed for many instructions.

### Key Features
- **RET-threading**: SP = PC, each instruction ends with RET
- **16-way interleaved handler table**: Self-documenting addresses
- **HLE (High-Level Emulation)**: ROM calls intercepted, not emulated
- **External converter**: PC does translation, Z80 only executes

---

## 2. Core Concept: RET-Threading

### Traditional Fetch-Decode-Execute
```z80
; Slow: 50+ cycles per opcode
LOOP:
    LD A, (PC_REG)      ; Fetch opcode
    INC PC_REG
    LD L, A
    LD H, TABLE_HIGH
    JP (HL)             ; Decode + Execute
```

### Our Approach: SP = PC
```z80
; Fast: RET = fetch + decode + jump in ONE instruction (10 cycles)
HANDLER_XXX:
    ; ... do work ...
    RET                 ; Pops next handler address, jumps there
```

The "code stream" is a sequence of 16-bit handler addresses in memory.
Z80's SP walks through this stream via RET instructions.

---

## 3. Opcode Encoding: 16-Way Interleaving

### Formula
```
Handler_Address = (High_Byte << 8) | Low_Byte

Where:
  Low_Byte  = Opcode (self-documenting!)
  High_Byte = $70 | (Opcode & $0F)
```

### Examples
```
Opcode   Binary      &$0F   High    Handler Address
────────────────────────────────────────────────────
$00 BRK  0000 0000   $0     $70     $7000
$A9 LDA# 1010 1001   $9     $79     $79A9
$85 STA  1000 0101   $5     $75     $7585
$E8 INX  1110 1000   $8     $78     $78E8
$4C JMP  0100 1100   $C     $7C     $7C4C
$20 JSR  0010 0000   $0     $70     $7020
$60 RTS  0110 0000   $0     $70     $7060
```

### Memory Layout: Handler Table ($7000-$7FFF)
```
Page $70: opcodes $x0 → $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$A0,$B0,$C0,$D0,$E0,$F0
Page $71: opcodes $x1 → $01,$11,$21,$31,$41,$51,$61,$71,$81,$91,$A1,$B1,$C1,$D1,$E1,$F1
Page $72: opcodes $x2 → $02,$12,$22...
...
Page $79: opcodes $x9 → $09,$19,$29,$39,$49,$59,$69,$79,$89,$99,$A9,$B9,$C9,$D9,$E9,$F9
...
Page $7F: opcodes $xF → $0F,$1F,$2F,$3F,$4F,$5F,$6F,$7F,$8F,$9F,$AF,$BF,$CF,$DF,$EF,$FF

Each handler slot: 16 bytes
Total: 4 KB
```

### Why This Encoding?
1. **Self-documenting**: See $79A9 in debugger → instantly know it's opcode $A9
2. **16 bytes per handler**: Enough for ~80% of instructions inline
3. **Simple calculation**: `OR $70` + mask
4. **No lookup table needed**: Address computable from opcode

---

## 4. Virtual Z80 Machine Specification

The emulator runs on a **virtual Z80 machine** with simplified architecture.
This can be implemented on real Spectrum 128K+ or in a software emulator.

### Hardware Specification

```
┌─────────────────────────────────────────────────────────────┐
│                  VIRTUAL Z80 MACHINE                        │
├─────────────────────────────────────────────────────────────┤
│  CPU:     Z80 @ 3.5 MHz (or faster)                        │
│  RAM:     4 MB (256 pages × 16 KB)                         │
│  Fixed:   $0000-$BFFF (48 KB always mapped)                │
│  Banked:  $C000-$FFFF (16 KB switchable, 256 pages)        │
│  I/O:     2 ports (minimal)                                │
└─────────────────────────────────────────────────────────────┘
```

### Memory Architecture

```
$0000 ┌─────────────────────┐
      │                     │
      │   Fixed RAM         │  48 KB - always accessible
      │   (code, handlers,  │
      │    data, screen)    │
      │                     │
$BFFF └─────────────────────┘
$C000 ┌─────────────────────┐
      │                     │
      │   Banked RAM        │  16 KB window into 4 MB
      │   Page 0-255        │  Selected via Port $00
      │                     │
$FFFF └─────────────────────┘

Total addressable: 48 KB + (256 × 16 KB) = 4 MB + 48 KB
```

### I/O Ports

```
Port    R/W    Function
────────────────────────────────────────────────────────────
$00     R/W    Bank Select (0-255)
               Write: Select page N to map at $C000-$FFFF
               Read:  Return current page number

$01     R/W    Console I/O
               Write: Output character (blocks until sent)
               Read:  Input character (blocks until available)
                      Returns $FF if no input waiting (non-blocking check)
```

### Port Usage Examples

```z80
; Switch to page 5
    LD A, 5
    OUT ($00), A        ; Page 5 now visible at $C000-$FFFF

; Read current page
    IN A, ($00)         ; A = current page number

; Output character 'A'
    LD A, 'A'
    OUT ($01), A        ; Print 'A' to console

; Read character (blocking)
    IN A, ($01)         ; Wait for keypress, return in A

; Check for input (non-blocking)
    IN A, ($01)
    CP $FF
    JR Z, no_input      ; No key pressed
```

### Implementation on Real Hardware

**ZX Spectrum 128K:**
```
Port $00 → Maps to port $7FFD (standard 128K paging)
           Only pages 0,1,3,4,6,7 available (6 pages)
           Bits: ----XPPP where PPP = page, X = ROM select

Port $01 → Directly calls Spectrum ROM routines
           OUT: RST $10 (print char)
           IN:  Read from keyboard buffer or INKEY$
```

**Scorpion 256 / Pentagon 512:**
```
Port $00 → Extended paging (up to 256 pages)
Port $01 → Same as Spectrum 128K
```

**Software Emulator (PC/Mac/Linux):**
```
Port $00 → Array index into 256×16KB memory array
Port $01 → stdin/stdout
```

### Why This Architecture?

1. **Simple**: Only 2 ports to implement
2. **Portable**: Works on Spectrum 128K through emulators
3. **Sufficient**: 4MB covers any 6502 software
4. **Minimal**: No interrupts, no DMA, no timers needed

---

## 5. Memory Map (ZX Spectrum 128K)

```
Z80 Address    Size    Contents
─────────────────────────────────────────────────────────────
$4000-$5AFF    6.75K   Screen memory (Spectrum standard)
$5B00-$5FFF    1.25K   System variables / work area

$6000-$6FFF    4K      Runtime: long handlers, traps, utilities
$7000-$7FFF    4K      Handler Table (16-way interleaved)

$8000-$BFFF    16K     6502 DATA Memory (raw bytes)
                       ├─ $8000-$80FF: Zero Page ($00-$FF)
                       ├─ $8100-$81FF: 6502 Stack ($100-$1FF)
                       ├─ $8200-$83FF: Input buffer, vectors
                       └─ $8400-$BFFF: Program data, variables

$C000-$FFFF    16K     6502 CODE Stream (threaded, banked)
                       Bank 0,1,3,4,6,7 available (6 × 16K = 96K)
```

### Bank Switching for Large Programs
```
$C000-$FFFF can page between banks 0,1,3,4,6,7
Total code space: 6 × 16KB = 96KB threaded code
Equivalent to ~48KB of 6502 code (2:1 expansion ratio)
```

---

## 6. Instruction Handlers

### Simple Instruction (fits in 16 bytes)
```z80
; INX - Increment X register
; Handler at $78E8
HANDLER_INX:
    INC E               ; E = X register
    ; Set N and Z flags based on result
    JP NZ, .not_zero
    SET FLAG_Z_BIT, (IY+FLAGS)
    JR .done
.not_zero:
    BIT 7, E
    JR Z, .done
    SET FLAG_N_BIT, (IY+FLAGS)
.done:
    RET                 ; 10 cycles → next instruction
```

### Instruction with Immediate Operand
```z80
; LDA #imm - Load Accumulator Immediate
; Handler at $79A9
HANDLER_LDA_IMM:
    POP BC              ; BC = next word from stream (operand)
    LD A, C             ; A = low byte = immediate value
    ; Set N/Z flags
    AND A               ; Sets Z flag
    LD (REG_A), A       ; Store to 6502 A register
    RET
```

### Instruction with Address Operand
```z80
; LDA $nnnn - Load Accumulator Absolute
; Handler at $7DAD
HANDLER_LDA_ABS:
    POP BC              ; BC = 6502 address (low word)
    ; Translate to Z80 address: $8000 + addr
    LD H, $80
    LD L, C
    ; If addr >= $100, need full translation
    LD A, B
    OR A
    JR Z, .zp_range
    ; Full address translation
    LD HL, DATA_BASE
    ADD HL, BC
.zp_range:
    LD A, (HL)          ; Read from 6502 memory
    LD (REG_A), A
    ; Set flags...
    RET
```

### Complex Instruction (jumps to extended handler)
```z80
; ADC (indirect),Y - Add with Carry, Indirect Indexed
; Handler at $71xx (only 16 bytes, not enough)
HANDLER_ADC_IND_Y:
    JP ADC_IND_Y_LONG   ; Jump to extended handler in $6000 area

; Extended handler at $6xxx
ADC_IND_Y_LONG:
    POP BC              ; C = zero page address
    ; Read 16-bit pointer from zero page
    LD H, $80
    LD L, C
    LD E, (HL)
    INC L
    LD D, (HL)
    ; Add Y register
    LD A, (REG_Y)
    ADD A, E
    LD E, A
    JR NC, .no_carry
    INC D
.no_carry:
    ; DE = final 6502 address, translate and read
    ; ... (more code) ...
    RET
```

---

## 7. Control Flow: Jumps and Calls

### JMP (Absolute Jump)
```z80
HANDLER_JMP:
    POP BC              ; BC = target 6502 address
    ; Convert to threaded code address:
    ; Z80_addr = $C000 + (6502_addr * 2)
    SLA C               ; ×2
    RL B
    SET 6, B            ; | $C000
    SET 7, B
    LD SP, BC           ; SP = new "PC"
    RET                 ; Jump to target
```

### JSR (Jump to Subroutine)
```z80
HANDLER_JSR:
    ; Push return address to 6502 stack
    ; Current SP points to target address
    ; Need to save SP+2 (address after JSR operand)

    POP BC              ; BC = target 6502 address

    ; Calculate return address in 6502 terms
    ; (This is tricky - need reverse translation or stored mapping)

    ; For simplicity: use shadow return stack
    LD HL, (RETURN_STACK_PTR)
    LD DE, SP           ; DE = return point in threaded code
    LD (HL), E
    INC HL
    LD (HL), D
    INC HL
    LD (RETURN_STACK_PTR), HL

    ; Jump to target (same as JMP)
    SLA C
    RL B
    SET 6, B
    SET 7, B
    LD SP, BC
    RET
```

### RTS (Return from Subroutine)
```z80
HANDLER_RTS:
    ; Pop from shadow return stack
    LD HL, (RETURN_STACK_PTR)
    DEC HL
    LD D, (HL)
    DEC HL
    LD E, (HL)
    LD (RETURN_STACK_PTR), HL

    ; DE = return address in threaded code
    EX DE, HL
    LD SP, HL
    RET
```

---

## 8. HLE: High-Level Emulation of ROM Calls

### Apple II ROM Entry Points
```
Address   Name      Function              Our Trap
────────────────────────────────────────────────────
$FD6A     GETLN     Read line → $0200     TRAP_GETLN
$FDED     COUT      Print char from A     TRAP_COUT
$FD0C     RDKEY     Read single key       TRAP_RDKEY
$FC58     HOME      Clear screen          TRAP_HOME
```

### Implementation: Static Patching
The converter recognizes `JSR $FDED` and emits `dw TRAP_COUT` instead of a real JSR.

```z80
; TRAP_COUT: Print character
; Called directly from threaded code (no JSR overhead)
TRAP_COUT:
    LD A, (REG_A)       ; Get 6502 accumulator

    ; Convert Apple II char to Spectrum
    AND $7F             ; Strip high bit
    CP 13               ; Carriage return?
    JR Z, .newline

    RST 10h             ; Spectrum ROM: print char
    RET

.newline:
    LD A, 13
    RST 10h
    RET
```

```z80
; TRAP_GETLN: Read line input
TRAP_GETLN:
    ; Use Spectrum ROM to read line
    ; Store result at $8200 (= 6502 $0200)
    ; Return length in X register (E)

    LD HL, $8200        ; Input buffer
    CALL READ_LINE      ; Our input routine
    LD E, A             ; Length → X register
    RET
```

### Dynamic Traps (Memory Mines)
For indirect jumps like `JMP ($FFFC)`, we fill the ROM area with trap addresses:

```
Z80 address $BF6A (maps to 6502 $FD6A):
    dw TRAP_GETLN

When code does JMP ($0020) and ($0020) contains $FD6A,
the handler reads $FD6A, translates to $BF6A,
loads the word there (TRAP_GETLN), and jumps to it.
```

---

## 9. Register Mapping

### Proposal A: Direct Register Mapping
```
6502 Register    Z80 Register    Notes
─────────────────────────────────────────
A (Accumulator)  A               Direct (but A needed for Z80 ops)
X (Index)        E
Y (Index)        D
SP (Stack Ptr)   (memory)        Separate 6502 stack at $8100
PC              Z80 SP           The magic!
Flags           IY+offset        Or dedicated memory location
```

### Proposal B: Memory-Based (Simpler)
```
All 6502 registers stored in fixed memory locations:
REG_A    EQU $5B00
REG_X    EQU $5B01
REG_Y    EQU $5B02
REG_SP   EQU $5B03
REG_P    EQU $5B04    ; Processor status (flags)
```

Memory-based is slower but simpler. Can optimize hot registers later.

---

## 10. Converter Architecture (Python)

### Project Structure
```
converter/
├── main.py              # CLI entry point
├── opcodes.py           # 6502 opcode table (256 entries)
├── translator.py        # 6502 binary → threaded code
├── traps.py             # ROM address → trap handler mapping
├── memory_map.py        # Address translation logic
├── tap_writer.py        # Generate .tap file for Spectrum
└── tests/
    ├── test_opcodes.py
    └── test_translator.py
```

### Opcode Table
```python
# opcodes.py
from dataclasses import dataclass
from enum import Enum

class AddrMode(Enum):
    IMP = "implied"         # CLC, RTS
    IMM = "immediate"       # LDA #$xx
    ZP = "zero_page"        # LDA $xx
    ZPX = "zero_page_x"     # LDA $xx,X
    ZPY = "zero_page_y"     # LDX $xx,Y
    ABS = "absolute"        # LDA $xxxx
    ABX = "absolute_x"      # LDA $xxxx,X
    ABY = "absolute_y"      # LDA $xxxx,Y
    IND = "indirect"        # JMP ($xxxx)
    IZX = "indirect_x"      # LDA ($xx,X)
    IZY = "indirect_y"      # LDA ($xx),Y
    REL = "relative"        # BNE $xx

@dataclass
class Opcode:
    mnemonic: str
    mode: AddrMode
    bytes: int              # 1, 2, or 3
    cycles: int             # For reference

OPCODES = {
    0x00: Opcode("BRK", AddrMode.IMP, 1, 7),
    0x01: Opcode("ORA", AddrMode.IZX, 2, 6),
    # ...
    0xA9: Opcode("LDA", AddrMode.IMM, 2, 2),
    0x85: Opcode("STA", AddrMode.ZP, 2, 3),
    0x4C: Opcode("JMP", AddrMode.ABS, 3, 3),
    0x20: Opcode("JSR", AddrMode.ABS, 3, 6),
    0x60: Opcode("RTS", AddrMode.IMP, 1, 6),
    0xE8: Opcode("INX", AddrMode.IMP, 1, 2),
    # ... (all 256)
}
```

### Handler Address Calculation
```python
# memory_map.py
def opcode_to_handler(opcode: int) -> int:
    """Convert 6502 opcode to Z80 handler address."""
    low = opcode
    high = 0x70 | (opcode & 0x0F)
    return (high << 8) | low

def addr_6502_to_code(addr: int) -> int:
    """Convert 6502 code address to threaded code address."""
    return 0xC000 + (addr * 2)

def addr_6502_to_data(addr: int) -> int:
    """Convert 6502 data address to Z80 data address."""
    return 0x8000 + addr
```

### Translation Logic
```python
# translator.py
from opcodes import OPCODES, AddrMode
from memory_map import opcode_to_handler
from traps import TRAPS

def translate(binary: bytes, org: int = 0x0800) -> bytes:
    """Translate 6502 binary to threaded code."""
    output = []
    pc = 0

    while pc < len(binary):
        opcode = binary[pc]
        info = OPCODES.get(opcode)

        if info is None:
            raise ValueError(f"Unknown opcode ${opcode:02X} at ${org+pc:04X}")

        # Check for trap (ROM call)
        if info.mnemonic == "JSR" and info.mode == AddrMode.ABS:
            target = binary[pc+1] | (binary[pc+2] << 8)
            if target in TRAPS:
                # Emit trap handler directly
                trap_addr = TRAPS[target]
                output.append(trap_addr & 0xFF)
                output.append(trap_addr >> 8)
                pc += 3
                continue

        # Emit handler address
        handler = opcode_to_handler(opcode)
        output.append(handler & 0xFF)
        output.append(handler >> 8)
        pc += 1

        # Emit operand if present
        if info.bytes > 1:
            operand = binary[pc]
            pc += 1
            if info.bytes > 2:
                operand |= binary[pc] << 8
                pc += 1
            output.append(operand & 0xFF)
            output.append(operand >> 8)

    return bytes(output)
```

---

## 11. Target Software: Scott Adams Adventures

### Why Scott Adams?
- Small footprint (~16KB interpreter + ~16KB game data)
- Text-only (no graphics to emulate)
- Well-documented format
- Historical significance (first commercial adventure games)

### SAGA Engine
Scott Adams games use a bytecode interpreter called SAGA.
We're emulating the 6502 interpreter, which reads SAGA bytecode.

```
┌─────────────────────────────────────────────┐
│  Z80 (Spectrum)                             │
│  └─ Our Threaded Code Interpreter           │
│      └─ 6502 SAGA Interpreter (emulated)    │
│          └─ SAGA Bytecode (game data)       │
└─────────────────────────────────────────────┘
```

### Required Traps (Apple II version)
```
$FD6A GETLN  - Input line (used for commands)
$FDED COUT   - Output character (used for descriptions)
$FC58 HOME   - Clear screen (used at game start)
```

### Files Needed
1. SAGA interpreter binary (Apple II version, ~8KB)
2. Game data file (.dat, ~8-16KB)

---

## 12. Development Phases

### Phase 1: Converter MVP
- [ ] Complete opcode table (256 entries)
- [ ] Basic translator (no traps)
- [ ] TAP file generator
- [ ] Test with trivial 6502 program

### Phase 2: Z80 Runtime
- [ ] Handler table ($7000-$7FFF)
- [ ] Implement ~20 common opcodes
- [ ] Basic memory access (ZP, absolute)
- [ ] Test: run "Hello World" (print loop)

### Phase 3: Control Flow
- [ ] JMP, JSR, RTS
- [ ] Conditional branches (BEQ, BNE, etc.)
- [ ] Return stack mechanism
- [ ] Test: simple subroutine call

### Phase 4: HLE Traps
- [ ] COUT (character output)
- [ ] GETLN (line input)
- [ ] HOME (clear screen)
- [ ] Test: input/output loop

### Phase 5: Scott Adams Integration
- [ ] Load SAGA interpreter
- [ ] Load game data
- [ ] Full playthrough test
- [ ] Performance profiling

### Phase 6: Polish
- [ ] Remaining opcodes
- [ ] Decimal mode (if needed)
- [ ] Save/load state
- [ ] Multiple games support

---

## 13. Performance Estimates

### Cycles per Instruction Class
```
Instruction Type        6502 cycles    Z80 cycles (est.)    Speedup
──────────────────────────────────────────────────────────────────
Implied (INX, TAX)      2              ~20                  ~0.7x
Immediate (LDA #)       2              ~30                  ~0.5x
Zero Page (LDA $zp)     3              ~40                  ~0.5x
Absolute (LDA $addr)    4              ~50                  ~0.6x
Indirect Y (LDA (),Y)   5-6            ~80                  ~0.5x
JSR/RTS                 6+6            ~60                  ~1.5x (HLE)
ROM Call (via trap)     100+           ~30                  ~10x (HLE!)
```

### Key Insight
With HLE, ROM calls become **10x faster** than original hardware.
Text output (the bottleneck for adventures) will be instantaneous.

Overall: **60-80% native 6502 speed** for compute-bound code,
**10x faster** for I/O-bound code (like text adventures).

---

## 14. Open Questions

1. **Self-modifying code (SMC)**: Support by keeping parallel raw memory?
2. **Decimal mode**: BCD arithmetic needed for some games?
3. **Interrupts**: NMI/IRQ needed? Probably not for adventures.
4. **Bank switching**: How to handle code crossing 16KB boundary?
5. **Debugging**: Build monitor/disassembler into runtime?

---

## 15. References

- [6502 Instruction Set](http://www.6502.org/tutorials/6502opcodes.html)
- [Apple II Memory Map](https://www.kreativekorp.com/miscpages/a2info/memorymap.shtml)
- [Scott Adams Game Format](https://github.com/pdxiv/scottfree)
- [Z80 Instruction Timing](http://z80.info/z80time.txt)
- [Threaded Code (Wikipedia)](https://en.wikipedia.org/wiki/Threaded_code)

---

*Document version: 1.0*
*Next step: Implement converter Phase 1*
