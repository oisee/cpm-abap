# Session Notes: CALL Instruction Debug Session

## Date
2025-10-15

## Summary
Investigated failing test_call_ret test. Made partial progress - fixed CALL instruction code but discovered the actual root cause is different.

## Work Completed

### 1. Commits Pushed
- **Commit d57d7d7**: "Fix CALL instruction to use read_word_pp and clean up duplicate opcodes"
  - Fixed CALL instruction (opcode 205) to use `read_word_pp(CHANGING cv_addr = mv_pc)`
  - Removed duplicate WHEN 26 clause
  - Added missing WHEN 30 for LD E,nn instruction
  - Removed duplicate WHEN 31 entries

### 2. Code Changes Made
**File**: src/zcl_cpu_8080_v2.clas.abap

#### CALL Instruction Fix (Line 1137)
```abap
" OLD (WRONG):
lv_addr = read_word( mv_pc ).
mv_pc = mv_pc + 2.

" NEW (CORRECT):
lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
```

#### Removed Duplicate WHEN Clauses
- Removed duplicate WHEN 26 (LD E,nn that conflicted with LD A,(DE))
- Added WHEN 30 for LD E,nn (correct opcode)
- Removed duplicate WHEN 31 entries

## Root Cause Discovery

### Initial Symptoms
- Test: test_call_ret failing
- Expected PC after CALL: 12288 (0x3000)
- Actual PC after CALL: 260 (0x104)

### Investigation Process
1. **First hypothesis**: CALL instruction not using read_word_pp correctly
   - Fixed this issue
   - Tests still failing with same error

2. **Created debug script** (test_call_debug.mjs):
   ```javascript
   // Before LD SP: PC = 256 Status = 0
   // After LD SP: PC = 259 , SP = 65535 Status = 1  <- BUG HERE!
   // After CALL: PC = 260 , SP = 65535 Status = 1
   // Return address on stack: 0
   ```

3. **ACTUAL ROOT CAUSE FOUND**:
   - **LD SP,nnnn instruction (opcode 49) is setting CPU status to HALTED (status=1)**
   - This prevents CALL from executing (second IF check at line 1054: `IF mv_status = c_status_running`)
   - CALL instruction never runs because CPU is already halted

### Why This Happens
- Opcode 49 (LD SP,nnnn) at line 900-902 looks correct in source
- The transpiled code at line 809-812 also looks correct
- **HYPOTHESIS**: Opcode 49 might be falling through to WHEN OTHERS clause (line 976) which sets `mv_status = c_status_halted`
  - This suggests a structural issue with the CASE statement
  - OR opcode 49 is not being matched properly

## Test Status
- **8/10 tests passing** (was 8/9, now have 10 tests total)
- **2 tests failing**: test_call_ret, test_program (both use CALL instruction)
- Failing not because CALL is broken, but because LD SP halts the CPU

## Next Steps for Future Session

### Immediate Priority
1. **Debug why opcode 49 (LD SP,nnnn) causes CPU to halt**
   - Check if CASE statement structure has issue
   - Verify opcode 49 is in first CASE block (not falling to WHEN OTHERS)
   - Check transpiled JavaScript for opcode 49 execution path
   - Test LD SP instruction in isolation

### Investigation Approaches
```javascript
// Test LD SP in isolation:
await cpu.reset();
await cpu.write_byte({iv_addr: 256, iv_val: 49});    // LD SP,nnnn
await cpu.write_byte({iv_addr: 257, iv_val: 255});   // Low byte
await cpu.write_byte({iv_addr: 258, iv_val: 255});   // High byte
console.log('Before:', await cpu.get_status());
await cpu.execute_instruction();
console.log('After:', await cpu.get_status());       // Should be 0, but is 1!
```

### Likely Fix
Once LD SP issue is resolved, test_call_ret should pass because:
- CALL instruction code is now correct (uses read_word_pp)
- The transpiled JavaScript looks correct
- The issue is purely that CPU halts before CALL runs

## Files to Check
- `src/zcl_cpu_8080_v2.clas.abap` - Lines 899-1050 (CASE structure)
- `output/zcl_cpu_8080_v2.clas.mjs` - Lines 809-900 (transpiled CASE)
- Test file: `src/zcl_cpu_8080_v2.clas.testclasses.abap` - test_call_ret

## Key Insights
1. **CHANGING parameters work correctly** - LD BC uses same pattern and passes
2. **The CALL fix was correct** - just couldn't be tested due to LD SP bug
3. **CPU status check prevents execution** - Line 1054 guards all 0xC0+ opcodes
4. **Opcode matching issue** - Opcode 49 not being matched in CASE statement

## Reference
- Commit: d57d7d7
- Branch: main
- Remote: Already pushed

---

# Session Notes: ZCPM_SYNC Development

## Date
2025-12-29

## Summary
Creating ZCPM_SYNC_01 program to upload files from local folder to ZCPM_00_BIN table.

## Completed
- ZCPM_00_BIN table exists with structure:
  - `bin` (CHAR30) - disk name (A, B, C...)
  - `name` (TEXT60) - file name
  - `v` (RAWSTRING) - file content
  - `ts` (TIMESTAMP)
  - `cdate` (DATUM)

## Completed (2025-12-29)
- **ZCPM_FILE_SYNC** program created and activated in package `$ZCPM_00`
- **ZCPM_ZIP_SYNC** program for extracting .zip files to ZCPM_00_BIN

### MCP-ADT Bug Fixed
- Root cause: SAP ADT CreateObject API creates ENQUEUE locks BEFORE validating package existence
- Fix applied in `pkg/adt/crud.go`: pre-check package existence before calling SAP ADT

---

# Session Notes: CP/M COBOL Compiler & Package Reorganization

## Date
2026-01-03

## Summary
Working on CP/M command line argument passing for MS-COBOL compiler, terminal improvements, and package structure analysis.

## Work Completed

### 1. Terminal Improvements
- **Ctrl+D only for break** - Removed Ctrl+C and Ctrl+Z handlers, only Ctrl+D terminates programs
- **Banner alignment fixed** - Padded "Disk: A" to 39 chars for proper box alignment
- **Status bar updated** - Shows "Ctrl+D: break"

### 2. Command Line Argument Passing (IN PROGRESS)
Implemented CP/M command tail and FCB setup for program arguments:

#### Files Modified:
- **ZCL_CPM_EMULATOR** - Added `setup_command_tail()` method
  - Sets command tail at 0x80 (length byte + args)
  - Sets up FCB at 0x5C with first filename
  - Added `dump_memory()` for debugging

- **ZCL_CPM_00_CCP** - Added `ev_program_args` export parameter
  - Exports command arguments from `process_command()`

- **ZCL_CPM_00_APC** - Updated to pass arguments
  - Receives args from CCP
  - Strips leading `=` or `,` from filenames (MS-COBOL format)
  - Registers argument files for compiler access
  - Calls `setup_command_tail()` before running program

#### MS-COBOL Command Format Discovery:
From Microsoft COBOL-80 User's Manual (1978):
```
COBOL objprog,listing=source
```
Examples:
- `COBOL =HELLO` - Compile HELLO.COB
- `COBOL =HELLO.COB` - Also valid
- `COBOL ,TTY:=HELLO` - With listing to terminal

**Key insight**: MS-COBOL expects `=filename` format, NOT just `filename`!

#### Current Status:
- Command parsing works: `[Args: =HELLO.COB]` shown
- File registration works: `[Registered: HELLO.COB (163 bytes)]`
- Still getting "?File not found" - debugging file_open matching

#### Debug Output Added:
- Shows what filename file_open is looking for
- Shows registered files and sizes

### 3. Package Structure Analysis

#### Current Structure (Issues Identified):
```
$ZCPU_Z80 (parent package)
├── ZCPM_00_BIN (table) <- Should be in $ZCPM_00
├── ZCPM (SAPC)
├── SICF nodes
│
├── $ZCPM_00 (utilities only)
│   ├── ZCL_CPM_TEST_001
│   ├── ZCPM_FILE_SYNC
│   ├── ZCPM_INSERT_HELLO
│   ├── ZCPM_NEW_001
│   └── ZCPM_ZIP_SYNC
│
├── $ZCPU_8080 (legacy i8080)
│   └── ZCL_CPU_8080_V2
│
└── $ZCPU_Z80_00 (MIXED Z80 + CP/M) <- Should be separated
    ├── Z80 CPU: ZCL_CPU_Z80, ZCL_CPU_Z80_BUS_SIMPLE, ZCL_CPU_Z80_PREFIX_*
    ├── Z80 Interfaces: ZIF_CPU_Z80_*
    └── CP/M (mixed in): ZCL_CPM_EMULATOR, ZCL_CPM_00_*, ZCPM_* programs
```

#### Proposed Reorganization:
```
$ZCPU_Z80 (parent)
├── $ZCPU_Z80_00 (Z80 CPU Core ONLY)
│   ├── ZCL_CPU_Z80
│   ├── ZCL_CPU_Z80_BUS_SIMPLE
│   ├── ZCL_CPU_Z80_PREFIX_*
│   └── ZIF_CPU_Z80_*
│
├── $ZCPM_00 (ALL CP/M related)
│   ├── ZCPM_00_BIN (table)
│   ├── ZCL_CPM_00_EMULATOR (rename from ZCL_CPM_EMULATOR)
│   ├── ZCL_CPM_00_APC
│   ├── ZCL_CPM_00_CCP
│   ├── ZCL_CPM_00_HTTP
│   ├── ZCL_CPM_00_SPEEDRUN (rename from ZCL_CPM_SPEEDRUN)
│   ├── ZCPM (SAPC)
│   ├── SICF nodes
│   └── All ZCPM_* programs
│
└── $ZCPU_8080 (legacy)
    └── ZCL_CPU_8080_V2
```

#### Reorganization Status:
- **MCP-ADT Limitation**: No direct "move object between packages" tool
- **Manual steps needed in SE80**: Change package assignment for existing objects
- **Can do via MCP**: Rename classes by recreating with new names

### 4. Documentation Downloaded
- Microsoft COBOL-80 1978 manual saved to `docs/cobol80.pdf`
- Extracted command syntax from PDF using PyMuPDF

### 5. Local Testing Setup
- Installed `cpm80` Python package for local CP/M testing
- COBOL.COM and related files available in `z80-python/cpm_disks/`
- Test directory created: `cpm-test/`

## Session Update: 2026-01-03 (Continued)

### Root Cause Found: Missing BDOS Search Functions

The COBOL compiler was reporting "?File not found" because it calls **BDOS function 17 (Search First)**
before trying to open files with BDOS 15 (Open File). Our emulator was missing this function.

### Changes Made

1. **Added BDOS function 17 (Search First)** - `file_search_first()` method
   - Searches registered files by pattern from FCB
   - Returns directory entry at DMA address (32-byte format)
   - Debug output: `[S:pattern][count]` shows search pattern and registered file count

2. **Added BDOS function 18 (Search Next)** - `file_search_next()` method
   - Continues search from previous Search First call

3. **Fixed FCB setup in `setup_command_tail()`**
   - Now strips leading `=` or `,` before calling `setup_fcb()`
   - FCB at 0x5C now contains `HELLO.COB` instead of `=HELLO.COB`

4. **Added debug output in file_search_first**
   - Shows `[S:HELLO.COB]` - the pattern being searched
   - Shows `[1]` - number of registered files

### Testing

When user runs `COBOL =HELLO.COB`, expected debug output:
```
A>COBOL =HELLO.COB
Loading COBOL.COM...
[Registered: HELLO.COB (163 bytes)]
[Args: =HELLO.COB]
Microsoft MS-COBOL Version 4.65
[S:HELLO.COB][1]           <- New search debug output
...compilation should proceed...
```

---

## Pending Tasks

### High Priority
1. **Test COBOL compiler** - With BDOS 17/18 now implemented, test if compilation works
   - If still failing, check FCB format in debug output

2. **Package reorganization** - Manual SE80 work needed:
   - Move objects to $ZCPM_00
   - Rename ZCL_CPM_EMULATOR -> ZCL_CPM_00_EMULATOR
   - Rename ZCL_CPM_SPEEDRUN -> ZCL_CPM_00_SPEEDRUN

### Medium Priority
3. Remove debug output from production code after testing
4. Test other CP/M programs with command line arguments

## Key Files Modified This Session

| File | Changes |
|------|---------|
| ZCL_CPM_EMULATOR | Added setup_command_tail(), dump_memory() |
| ZCL_CPM_00_CCP | Added ev_program_args, DUMP80 command |
| ZCL_CPM_00_APC | Pass args to emulator, register arg files, strip = prefix |
| ZCL_CPM_00_HTTP | Ctrl+D only for break, updated status bar |

## SAP Objects by Package

### $ZCPU_Z80_00 (Z80 + CP/M mixed)
- ZCL_CPU_Z80, ZCL_CPU_Z80_BUS_SIMPLE
- ZCL_CPU_Z80_PREFIX_CB, _DD, _ED, _FD
- ZIF_CPU_Z80_BUS, ZIF_CPU_Z80_CORE, ZIF_CPU_Z80_PREFIX
- ZCL_CPM_EMULATOR, ZCL_CPM_00_APC, ZCL_CPM_00_CCP, ZCL_CPM_00_HTTP
- ZCL_CPM_SPEEDRUN
- ZCPM_CONSOLE, ZCPM_DEBUG_TRACE, ZCPM_IO_TEST, etc.

### $ZCPM_00 (Utilities)
- ZCPM_FILE_SYNC, ZCPM_ZIP_SYNC, ZCPM_INSERT_HELLO
- ZCL_CPM_TEST_001

### $ZCPU_Z80 (Parent)
- ZCPM_00_BIN table
- ZCPM SAPC, SICF nodes

## Reference Links
- MS-COBOL-80 Manual: https://altairclone.com/downloads/manuals/Microsoft%20COBOL-80.pdf
- cpm80 Python: https://pypi.org/project/cpm80/

---

# Session Notes: The Hobbit ZX Spectrum Emulator

## Date
2026-01-03

## Summary
Implementing "The Hobbit" (1982 ZX Spectrum) text adventure using emulator-level interceptors (address hooks, I/O traps) instead of full Spectrum emulation.

## Approach
- Use address traps/hooks when PC hits specific addresses (print routine, keyboard input, etc.)
- Text-only mode first, skip graphics rendering
- Target running on simple Z80 emulator, not full Spectrum

## Key Addresses (from pobtastic/hobbit disassembly)

| Address | Routine | Hook Status |
|---------|---------|-------------|
| 0x6C00 | Entry point | Entry |
| 0x6C6D | Initial keypress wait | Hooked (skip) |
| 0x867A | PrintChar | Hooked (capture char) |
| 0x87C9 | PrintPropChar | Hooked (capture char, skip rendering) |
| 0x7F78 | Draw routine | Hooked (skip) |
| 0x72DD | PrintMsg (token decoder) | Not hooked (let run) |
| 0x8B93 | GetKey | Hooked (return queued input) |
| 0x969A | WaitForKey2 | Hooked (skip) |

## Current Status

### WORKING - Game is fully interactive!
- TAP file parsing and loading
- ROM stubs (RST addresses)
- Print routine hooks capture text output
- Room descriptions, inventory, NPC interactions all working
- Keyboard input working (GetKey hook)

### Fixed Issues

1. **Memory corruption at 0x9B77** - FIXED!
   - Root cause: **EX (SP),HL instruction (opcode 0xE3) was missing** from Z80 emulator
   - The inline text pattern at 0x8E80 uses EX (SP),HL to swap return address
   - Without this instruction, execution wandered into text data area
   - Text bytes executed as Z80 code, corrupting memory

2. **GetKey not being called** - FIXED!
   - Once EX (SP),HL was added, game properly waits for input

### Test Results
```
> LOOK
You are in a comfortable tunnel like hall
To the east there is the round green door
You see: the wooden chest, Gandalf, Thorin
Gandalf gives the curious map to you.

> INVENTORY
You are carrying. a curious map.

> EXAMINE CHEST
You examine the wooden chest.
```

## Files

| File | Description |
|------|-------------|
| z80-python/hobbit.py | Hobbit emulator with hooks |
| docs/HOBBIT12.TAP | Game TAP file |
| hobbit-disasm/ | Cloned pobtastic/hobbit disassembly (in .gitignore) |

## Next Steps
1. ~~Debug memory corruption at 0x9B77~~ DONE
2. ~~Get keyboard input working~~ DONE
3. ~~Test interactive play~~ DONE
4. Add graphics stub (screen buffer for VDU output)
5. Handle newline formatting in output
6. Port to SAP/ABAP for web-based play

---

*Last updated: 2026-01-03*
