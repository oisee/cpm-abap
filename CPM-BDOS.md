# CP/M BDOS Functions for Z80 Emulator

## Overview

CP/M (Control Program for Microcomputers) uses BDOS (Basic Disk Operating System) calls via `CALL 5` with function number in register C. This document lists the essential BDOS functions needed to run CP/M programs.

## BDOS Call Convention

```
LD C, function_number
LD DE, parameter (if needed)
CALL 5
; Result in A (and sometimes HL)
```

## Essential BDOS Functions (Minimal Set)

| Func | Name | Input | Output | Description |
|------|------|-------|--------|-------------|
| 0 | System Reset | - | - | Warm boot, return to CCP |
| 1 | Console Input | - | A=char | Wait for and return character |
| 2 | Console Output | E=char | - | Output character to console |
| 6 | Direct Console I/O | E=FF or char | A=char or 0 | Raw I/O (E=FF=input, else output) |
| 9 | Print String | DE=addr | - | Print string until '$' terminator |
| 10 | Read Console Buffer | DE=buffer | - | Read line into buffer |
| 11 | Console Status | - | A=FF/0 | Check if character available |

## File System Functions (For Full CP/M Support)

| Func | Name | Input | Output | Description |
|------|------|-------|--------|-------------|
| 12 | Return Version | - | HL=version | Get CP/M version (22h = 2.2) |
| 13 | Reset Disk System | - | - | Reset all disk drives |
| 14 | Select Disk | E=drive | - | Select drive (0=A:, 1=B:, etc.) |
| 15 | Open File | DE=FCB | A=0/FF | Open file, returns directory code |
| 16 | Close File | DE=FCB | A=0/FF | Close file |
| 17 | Search First | DE=FCB | A=0-3/FF | Find first matching file |
| 18 | Search Next | - | A=0-3/FF | Find next matching file |
| 19 | Delete File | DE=FCB | A=0/FF | Delete file |
| 20 | Read Sequential | DE=FCB | A=0/FF | Read 128 bytes to DMA |
| 21 | Write Sequential | DE=FCB | A=0/FF | Write 128 bytes from DMA |
| 22 | Make File | DE=FCB | A=0/FF | Create new file |
| 23 | Rename File | DE=FCB | A=0/FF | Rename file |
| 26 | Set DMA Address | DE=addr | - | Set disk buffer address |
| 33 | Read Random | DE=FCB | A=0/FF | Random access read |
| 34 | Write Random | DE=FCB | A=0/FF | Random access write |

## Memory Map (Standard CP/M 2.2)

```
0000h-00FFh  Zero page (RST vectors, BIOS jump table)
0100h-xxxxh  TPA (Transient Program Area) - user programs load here
xxxxh-FFFEh  BDOS/BIOS
FFFFh        End of memory
```

### Key Addresses

| Address | Purpose |
|---------|---------|
| 0000h | Warm boot (JP to BIOS warm boot) |
| 0005h | BDOS entry point (JP to BDOS) |
| 005Ch | Default FCB 1 |
| 006Ch | Default FCB 2 |
| 0080h | Default DMA buffer / command line |
| 0100h | TPA start (program load address) |

## FCB (File Control Block) Structure

```
Offset  Size  Description
0       1     Drive (0=default, 1=A:, 2=B:, etc.)
1       8     Filename (padded with spaces)
9       3     Extension (padded with spaces)
12      1     Extent number (low byte)
13      1     Reserved
14      1     Extent number (high byte)
15      1     Record count in current extent
16      16    Disk allocation map
32      1     Current record within extent
33      3     Random record number (r0, r1, r2)
```

## Implementation Priority

### Phase 1: Console I/O (Run BASIC programs)
- Function 1: Console Input
- Function 2: Console Output
- Function 6: Direct Console I/O
- Function 9: Print String
- Function 11: Console Status

### Phase 2: Line Input (Interactive programs)
- Function 10: Read Console Buffer

### Phase 3: File System (Load programs, save data)
- Functions 15-23, 26, 33-34

## Example: Hello World in CP/M

```asm
        ORG     0100h

        LD      DE, MSG
        LD      C, 9        ; Print string function
        CALL    5           ; BDOS call

        LD      C, 0        ; System reset
        CALL    5

MSG:    DB      'Hello, World!$'
```

## Notes

1. **String Terminator**: CP/M uses '$' (24h) as string terminator, not null
2. **DMA Buffer**: Default at 0080h, 128 bytes for disk I/O
3. **FCB Location**: Default at 005Ch, command line parsed into it
4. **Case Sensitivity**: CP/M filenames are uppercase only
5. **Drive Letters**: 0=default, 1=A:, 2=B:, etc. in FCB byte 0

## References

- [CP/M 2.2 Operating System Manual](http://www.cpm.z80.de/manuals/cpm22-m.pdf)
- [CP/M BDOS Function Summary](http://www.gaby.de/cpm/manuals/archive/cpm22htm/ch5.htm)
