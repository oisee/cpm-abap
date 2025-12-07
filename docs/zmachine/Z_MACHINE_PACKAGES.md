# ZORK Package Structure

Package structure for Z-machine interpreter in SAP, following conventions from `$ZLLM` and `$ZRAY`.

## Package Hierarchy

```
$ZORK                          " Root package: Z-Machine / Zork Interpreter
├── $ZORK_00                   " Core: Interpreter engine, memory, opcodes
│   └── $ZORK_00_TEST          " Test story files and test utilities
├── $ZORK_10                   " UI: SAP GUI integration, REPL
└── $ZORK_99                   " Playground/experimental
```

## Naming Conventions

| Object Type | Pattern | Example |
|-------------|---------|---------|
| **Package** | `$ZORK_nn` | `$ZORK_00`, `$ZORK_10` |
| **Class** | `ZCL_ZORK_nn_*` | `ZCL_ZORK_00_MEMORY` |
| **Interface** | `ZIF_ZORK_nn_*` | `ZIF_ZORK_00_IO` |
| **Table** | `ZORK_nn_*` | `ZORK_00_SAVE` |
| **Structure** | `ZORK_nnS_*` | `ZORK_00S_FRAME` |
| **Table Type** | `ZORK_nn_*_T` | `ZORK_00_FRAME_T` |
| **Data Element** | `ZORK_nnE_*` | `ZORK_00E_OPCODE` |
| **Domain** | `ZORK_nnD_*` | `ZORK_00D_VERSION` |
| **CDS View** | `ZORK_nn_I_*` | `ZORK_00_I_SAVES` |
| **Program** | `ZORK_nn_*` | `ZORK_00_REPL` |
| **Message Class** | `ZORK_nn` | `ZORK_00` |
| **Function Group** | `ZORK_nn` | `ZORK_00` |

## $ZORK_00 — Core Interpreter

### Classes

| Class | Description |
|-------|-------------|
| `ZCL_ZORK_00` | Main Z-machine interpreter (facade) |
| `ZCL_ZORK_00_MEMORY` | Memory management (load, read, write) |
| `ZCL_ZORK_00_DECODER` | Instruction decoder |
| `ZCL_ZORK_00_TEXT` | ZSCII text encoding/decoding |
| `ZCL_ZORK_00_OBJECTS` | Object tree operations |
| `ZCL_ZORK_00_STACK` | Call stack management |
| `ZCL_ZORK_00_RANDOM` | Random number generator |
| `ZCL_ZORK_00_IO_CONSOLE` | Console I/O (for testing) |
| `ZCL_ZORK_00_IO_SAP` | SAP GUI I/O implementation |

### Interfaces

| Interface | Description |
|-----------|-------------|
| `ZIF_ZORK_00_IO` | I/O abstraction (print, input, status) |
| `ZIF_ZORK_00_TYPES` | Common type definitions |

### Tables

| Table | Description |
|-------|-------------|
| `ZORK_00_SAVE` | Saved game state (Quetzal format) |
| `ZORK_00_STORY` | Story file storage (optional) |
| `ZORK_00_TRANSCRIPT` | Game transcript log |

### Structures

| Structure | Description |
|-----------|-------------|
| `ZORK_00S_HEADER` | Story file header (64 bytes) |
| `ZORK_00S_FRAME` | Call stack frame |
| `ZORK_00S_INSTR` | Decoded instruction |
| `ZORK_00S_OBJECT` | Object entry (v3: 9 bytes) |

### Data Elements

| Data Element | Description |
|--------------|-------------|
| `ZORK_00E_VERSION` | Z-machine version (1-8) |
| `ZORK_00E_OPCODE` | Opcode value (0-255) |
| `ZORK_00E_ADDR` | Memory address (0-65535) |
| `ZORK_00E_WORD` | 16-bit word value |
| `ZORK_00E_BYTE` | 8-bit byte value |
| `ZORK_00E_ZCHAR` | Z-character (0-31) |

### Domains

| Domain | Description |
|--------|-------------|
| `ZORK_00D_VERSION` | Z-machine version (1-8) |
| `ZORK_00D_OP_TYPE` | Operand type (0-3) |
| `ZORK_00D_FORM` | Instruction form |

## $ZORK_10 — UI Layer

### Programs

| Program | Description |
|---------|-------------|
| `ZORK_10_REPL` | Interactive game player (REPL) |
| `ZORK_10_LOAD` | Load story file utility |
| `ZORK_10_DEBUG` | Debugger/step-through |

### Classes

| Class | Description |
|-------|-------------|
| `ZCL_ZORK_10_GUI` | SAP GUI integration |
| `ZCL_ZORK_10_SCREEN` | Screen management (split windows) |

## Class Relationships

```
                    ┌─────────────────┐
                    │   ZCL_ZORK_00   │  Main Interpreter
                    │     (facade)    │
                    └────────┬────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
        ▼                    ▼                    ▼
┌───────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ZCL_ZORK_00_   │  │ ZCL_ZORK_00_    │  │ ZCL_ZORK_00_    │
│   MEMORY      │  │   DECODER       │  │   STACK         │
└───────────────┘  └─────────────────┘  └─────────────────┘
        │
        ├──────────────────┐
        ▼                  ▼
┌───────────────┐  ┌─────────────────┐
│ZCL_ZORK_00_   │  │ ZCL_ZORK_00_    │
│   TEXT        │  │   OBJECTS       │
└───────────────┘  └─────────────────┘

        ┌─────────────────┐
        │ ZIF_ZORK_00_IO  │  I/O Interface
        └────────┬────────┘
                 │
        ┌────────┴────────┐
        ▼                 ▼
┌───────────────┐  ┌─────────────────┐
│ZCL_ZORK_00_   │  │ ZCL_ZORK_00_    │
│  IO_CONSOLE   │  │   IO_SAP        │
└───────────────┘  └─────────────────┘
```

## File Mapping (for transpiler)

```
src/zmachine/
├── zcl_zork_00.clas.abap                 " Main interpreter
├── zcl_zork_00.clas.testclasses.abap     " Unit tests
├── zcl_zork_00_memory.clas.abap          " Memory
├── zcl_zork_00_decoder.clas.abap         " Decoder
├── zcl_zork_00_text.clas.abap            " ZSCII
├── zcl_zork_00_objects.clas.abap         " Objects
├── zcl_zork_00_stack.clas.abap           " Stack
├── zcl_zork_00_random.clas.abap          " RNG
├── zif_zork_00_io.intf.abap              " I/O interface
├── zcl_zork_00_io_console.clas.abap      " Console I/O
└── zif_zork_00_types.intf.abap           " Types
```

## Message Class: ZORK_00

| Msg # | Type | Text |
|-------|------|------|
| 001 | E | Invalid Z-machine version &1 (expected 1-8) |
| 002 | E | Division by zero at PC &1 |
| 003 | E | Invalid opcode &1 at PC &2 |
| 004 | E | Stack underflow |
| 005 | E | Memory access violation at &1 |
| 006 | E | Invalid object number &1 |
| 007 | E | Property &1 not found in object &2 |
| 010 | I | Game saved successfully |
| 011 | I | Game restored successfully |
| 012 | W | Save failed |
| 013 | W | Restore failed |

## Transport Strategy

**Local development:** `$TMP` or local package
**Transportable:** Create request for `$ZORK` hierarchy

## Implementation Order

1. **Create packages:**
   ```
   $ZORK        → $ZORK (root, no transport)
   $ZORK_00     → $ZORK (subpackage)
   $ZORK_10     → $ZORK (subpackage)
   ```

2. **Create types first:**
   - `ZIF_ZORK_00_TYPES` (common types)
   - `ZIF_ZORK_00_IO` (I/O interface)
   - Data elements, domains, structures

3. **Create core classes:**
   - `ZCL_ZORK_00_MEMORY`
   - `ZCL_ZORK_00_DECODER`
   - `ZCL_ZORK_00_TEXT`
   - `ZCL_ZORK_00_STACK`
   - `ZCL_ZORK_00_OBJECTS`
   - `ZCL_ZORK_00` (main, last)

4. **Create I/O implementations:**
   - `ZCL_ZORK_00_IO_CONSOLE`
   - `ZCL_ZORK_00_IO_SAP`

5. **Create UI layer:**
   - `ZORK_10_REPL` program
   - `ZCL_ZORK_10_GUI` class

## Comparison with Existing Projects

| Aspect | $ZLLM | $ZRAY | $ZORK |
|--------|-------|-------|-------|
| Purpose | LLM integration | Code analysis | Z-machine interpreter |
| Core package | $ZLLM_00 | $ZRAY_00 | $ZORK_00 |
| UI package | - | $ZRAY_10 | $ZORK_10 |
| Main class | ZCL_LLM | - | ZCL_ZORK_00 |
| Tables | ZLLM_00_* | ZRAY_00_* | ZORK_00_* |

---

*Document created for ABAP Z-Machine Interpreter project*
*Version: 1.0*
*Date: 2025-12-07*
