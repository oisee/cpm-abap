# Instructions for Claude (AI Assistant)

This file contains guidance for Claude Code or other AI assistants working on this project.

## Project Overview

This project has **two components**:

1. **Z-Machine Interpreter** (ACTIVE FOCUS) - Written in ABAP, runs in SAP system
   - Plays Infocom interactive fiction games (Zork, etc.)
   - Uses MCP-ADT tools for development

2. **Z80/i8080 CPU Emulator** (PAUSED) - Written in ABAP, transpiler-compatible
   - Designed to run CP/M programs
   - Local testing via Node.js transpiler

## Current Focus: Z-Machine

### Development Approach

**Use MCP-ADT (SAP ADT Tools)** - NOT the local ABAP transpiler!

```
# DO NOT USE:
npm test                    # This is for i8080 emulator

# USE INSTEAD:
MCP tools: mcp__a4h-abap-adt__*
```

### Key SAP Objects

| Object | Type | Description |
|--------|------|-------------|
| ZORK_00_CONSOLE | PROG | Main Z-Machine console (full game) |
| ZORK_00_CONSOLE_TEST | PROG | Simple input/output test |
| ZCL_ZORK_00_MEMORY | CLAS | Z-Machine memory management |
| ZCL_ZORK_00_STACK | CLAS | Z-Machine stack operations |
| ZCL_ZORK_00_DECODER | CLAS | Text decoder (ZSCII/abbreviations) |
| ZCL_ZORK_00_OBJECTS | CLAS | Object table handling |
| ZCL_ZORK_00_EXECUTOR | CLAS | Instruction executor |
| ZCL_ZORK_00_IO_HTML | CLAS | HTML-based I/O for SAP GUI |
| ZCL_ZORK_00_GAMES | CLAS | Game story file factory |
| ZIF_ZORK_00_IO | INTF | I/O interface |
| ZIF_ZORK_00_GAME | INTF | Game interface |

### Package Structure

- **$ZORK_00** - Main Z-Machine package

### Common MCP-ADT Commands

```
# Search for objects
mcp__a4h-abap-adt__SearchObject(query="ZORK*")

# Get source code
mcp__a4h-abap-adt__GetSource(object_type="PROG", name="ZORK_00_CONSOLE_TEST")
mcp__a4h-abap-adt__GetSource(object_type="CLAS", name="ZCL_ZORK_00_IO_HTML")

# Write/update source
mcp__a4h-abap-adt__WriteSource(object_type="PROG", name="...", source="...", mode="upsert")

# Edit source (surgical replacement)
mcp__a4h-abap-adt__EditSource(object_url="...", old_string="...", new_string="...")

# Activate
mcp__a4h-abap-adt__Activate(object_url="...", object_name="...")
```

### Current Development Task

**ZORK_00_CONSOLE_TEST** - Simple input/output test:
- Takes input from `gv_input` field
- Echoes to HTML console as `> YOUR_TEXT`
- Clears input field after processing
- Handles ok_codes: '', 'ENTER', 'ONLI'

### Testing Workflow

1. Edit code via MCP-ADT
2. Activate in SAP
3. Run program in SAP GUI (SE38 or transaction)
4. Verify behavior

---

## Paused: Z80/i8080 Emulator

### Files (Local - Transpiler Compatible)

- `src/zcl_cpu_8080_v2.clas.abap` - Main CPU emulator
- `src/zcl_cpu_8080_v2.clas.testclasses.abap` - Unit tests

### Testing (When Working on i8080)

```bash
npm test    # Transpile + run tests locally
```

### Status

- 86/105 i8080 opcodes implemented (82%)
- 16 unit tests passing
- See CONTEXT.md, TODO.md for details

---

## Project Files

| File | Purpose |
|------|---------|
| CLAUDE.md | AI assistant instructions (this file) |
| CONTEXT.md | i8080 project history and status |
| TODO.md | i8080 implementation plan |
| BRAINSTORM.md | Architecture decisions |
| TRANSPILER.md | ABAP transpiler notes |

---

*Last updated: 2025-12-08*
*Current focus: Z-Machine (ZORK) via MCP-ADT*
