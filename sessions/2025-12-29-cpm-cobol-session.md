# Session Notes: 2025-12-29 - CP/M COBOL & File Sync

## Summary
Session focused on merging backup files, testing MS COBOL on CP/M emulator, and planning ABAP sync program.

## Completed Tasks

### 1. Fixed Claude Code Settings
- Removed malformed permission entries from `.claude/settings.local.json`
- Entries contained full Python scripts instead of permission patterns

### 2. Merged Backup Archive
- Compared current folder with `/mnt/safe/cpm-abap.tar.gz`
- Extracted missing files:
  - `output/` - transpiler output (~300 .mjs files)
  - `RunCPM/` - CP/M emulator reference (C source)
  - `reference-zmachine-go/` - Go Z-machine implementation
  - `reference-zmachine-py/` - Python Z-machine implementation
  - `.mcp.json` - MCP configuration
- These are gitignored (local-only files)

### 3. Commits Pushed
- `39197fe` - Add pure Python 6502 emulator and BCD test binary
- `a9f1006` - Add CP/M file write support and MS COBOL test files

### 4. CP/M Emulator Improvements (z80-python/cpm_z80.py)
Added file write support:
- `make_file()` - BDOS function 22 (create new file)
- `write_sequential()` - BDOS function 21
- `write_random()` - BDOS function 34
- Improved `open_file()` to handle read/write mode

### 5. MS COBOL 4.65 Setup
Extracted to `z80-python/cpm_disks/`:
- `COBOL.COM` + `COBOL*.OVR` - Compiler + overlays
- `L80.COM`, `M80.COM` - Microsoft linker & assembler
- `RUNCOB.COM` - COBOL runtime
- `COBLIB.REL`, `COBLBX.REL` - Libraries
- Sample programs: `SQUARO.COB`, `CRTEST.COB`

Created test programs:
- `HELLO.COB` - Minimal hello world
- `BYTEBT.COB` - Bytebeat-style pattern generator

### 6. MS COBOL Status
- Compiler loads and shows banner: "Microsoft MS-COBOL Version 4.65"
- Opens source files (.COB) successfully
- Creates output files (.REL) successfully
- **Issue**: Gets stuck during actual compilation
- Likely cause: Overlay file loading mechanism not implemented
- Simple CP/M programs (HELLO.COM) work fine

## Pending Tasks

### ZCPM_SYNC Program (ABAP)
User wants to create program similar to ZXRAY_SYNC that:
1. Takes local folder path as parameter
2. Uploads all files to ZCPM_00_BIN table (to be cloned from ZRAY_00_BIN)
3. Supports "virtual disks" (A:, B:, etc.)

**Blocked**: Need access to ZXRAY_SYNC source code and table structure.
MCP-ADT tools not available in current session.

## Technical Notes

### CP/M BDOS Functions Implemented
| Function | Name | Status |
|----------|------|--------|
| 0 | System Reset | ✓ |
| 1 | Console Input | ✓ |
| 2 | Console Output | ✓ |
| 6 | Direct I/O | ✓ |
| 9 | Print String | ✓ |
| 10 | Read Buffer | ✓ |
| 11 | Console Status | ✓ |
| 12 | Version | ✓ |
| 15 | Open File | ✓ |
| 16 | Close File | ✓ |
| 20 | Read Sequential | ✓ |
| 21 | Write Sequential | ✓ (NEW) |
| 22 | Make File | ✓ (NEW) |
| 26 | Set DMA | ✓ |
| 33 | Read Random | ✓ |
| 34 | Write Random | ✓ (NEW) |
| 35 | File Size | ✓ |

### Files Structure
```
z80-python/
├── cpm_z80.py          # CP/M emulator (modified)
├── cpm_disks/          # MS COBOL and test files (NEW)
│   ├── COBOL.COM
│   ├── COBOL*.OVR
│   ├── L80.COM
│   ├── M80.COM
│   ├── HELLO.COB
│   ├── BYTEBT.COB
│   └── ...
├── pure6502.py         # Pure Python 6502 emulator (NEW)
└── test_decimal.bin    # BCD test binary (NEW)
```

## Next Steps
1. Get ZXRAY_SYNC source code from SAP
2. Get ZRAY_00_BIN table structure
3. Create ZCPM_00_BIN table (clone)
4. Implement ZCPM_SYNC program
5. Consider fixing MS COBOL overlay loading in emulator
