*&---------------------------------------------------------------------*
*& Z-Machine Interpreter - Common Types Interface
*& Version 3 compatible (Zork I, II, III)
*& Naming: TS_ = structure, TT_ = table, TY_ = scalar, TTR_ = range
*&---------------------------------------------------------------------*
INTERFACE zif_zork_00_types
  PUBLIC.

  "======================================================================
  " Header Address Constants (Z-machine specification)
  "======================================================================
  CONSTANTS:
    c_hdr_version     TYPE i VALUE 0,    " $00: Version number (1-6)
    c_hdr_flags1      TYPE i VALUE 1,    " $01: Flags 1
    c_hdr_high_mem    TYPE i VALUE 4,    " $04: Base of high memory (word)
    c_hdr_init_pc     TYPE i VALUE 6,    " $06: Initial PC (word)
    c_hdr_dictionary  TYPE i VALUE 8,    " $08: Dictionary address (word)
    c_hdr_objects     TYPE i VALUE 10,   " $0A: Object table address (word)
    c_hdr_globals     TYPE i VALUE 12,   " $0C: Global variables address (word)
    c_hdr_static_mem  TYPE i VALUE 14,   " $0E: Base of static memory (word)
    c_hdr_flags2      TYPE i VALUE 16,   " $10: Flags 2
    c_hdr_abbrev      TYPE i VALUE 24,   " $18: Abbreviations table (word)
    c_hdr_file_len    TYPE i VALUE 26,   " $1A: File length / 2 (word)
    c_hdr_checksum    TYPE i VALUE 28,   " $1C: Checksum (word)
    c_hdr_interp_num  TYPE i VALUE 30,   " $1E: Interpreter number
    c_hdr_interp_ver  TYPE i VALUE 31,   " $1F: Interpreter version
    c_hdr_scr_height  TYPE i VALUE 32,   " $20: Screen height (lines)
    c_hdr_scr_width   TYPE i VALUE 33,   " $21: Screen width (chars)
    c_hdr_std_rev_hi  TYPE i VALUE 50,   " $32: Standard revision (hi)
    c_hdr_std_rev_lo  TYPE i VALUE 51.   " $33: Standard revision (lo)

  "======================================================================
  " Instruction Form Constants
  "======================================================================
  CONSTANTS:
    c_form_short    TYPE i VALUE 0,  " Short form (1 operand or 0)
    c_form_long     TYPE i VALUE 1,  " Long form (2 operands)
    c_form_variable TYPE i VALUE 2,  " Variable form (0-4 operands)
    c_form_extended TYPE i VALUE 3.  " Extended form (v5+)

  "======================================================================
  " Operand Count Constants
  "======================================================================
  CONSTANTS:
    c_opcount_0op TYPE i VALUE 0,  " Zero operands
    c_opcount_1op TYPE i VALUE 1,  " One operand
    c_opcount_2op TYPE i VALUE 2,  " Two operands
    c_opcount_var TYPE i VALUE 3.  " Variable operands

  "======================================================================
  " Operand Type Constants
  "======================================================================
  CONSTANTS:
    c_optype_large TYPE i VALUE 0,  " Large constant (2 bytes)
    c_optype_small TYPE i VALUE 1,  " Small constant (1 byte)
    c_optype_var   TYPE i VALUE 2,  " Variable
    c_optype_omit  TYPE i VALUE 3.  " Omitted

  "======================================================================
  " Interpreter Status
  "======================================================================
  CONSTANTS:
    c_status_running TYPE i VALUE 0,
    c_status_halted  TYPE i VALUE 1,
    c_status_waiting TYPE i VALUE 2,  " Waiting for input
    c_status_quit    TYPE i VALUE 3.

  "======================================================================
  " Scalar Types (TY_)
  "======================================================================
  TYPES:
    ty_word    TYPE i,   " 16-bit Z-machine word (0-65535)
    ty_byte    TYPE i,   " 8-bit byte (0-255)
    ty_address TYPE i,   " Memory address
    ty_packed  TYPE i.   " Packed address

  "======================================================================
  " Operand Structure and Table
  "======================================================================
  TYPES:
    BEGIN OF ts_operand,
      type  TYPE i,      " Operand type (c_optype_*)
      value TYPE i,      " Operand value (resolved)
    END OF ts_operand,

    tt_operands TYPE STANDARD TABLE OF ts_operand WITH EMPTY KEY.

  "======================================================================
  " Decoded Instruction Structure
  "======================================================================
  TYPES:
    BEGIN OF ts_instruction,
      address     TYPE i,            " Memory address of instruction
      opcode      TYPE i,            " Opcode number (0-255)
      form        TYPE i,            " Instruction form (short/long/var/ext)
      op_count    TYPE i,            " Operand count type (0OP/1OP/2OP/VAR)
      operands    TYPE tt_operands,  " Operands table
      has_store   TYPE abap_bool,    " Does instruction store result?
      store_var   TYPE i,            " Variable to store result
      has_branch  TYPE abap_bool,    " Does instruction branch?
      branch_on   TYPE abap_bool,    " Branch on true or false?
      branch_off  TYPE i,            " Branch offset
      text_addr   TYPE i,            " Address of embedded text (print/print_ret)
      instr_len   TYPE i,            " Total instruction length in bytes
      next_pc     TYPE i,            " PC after this instruction
    END OF ts_instruction,

    tt_instructions TYPE STANDARD TABLE OF ts_instruction WITH KEY address.

  "======================================================================
  " Local Variable Table (for call frames)
  "======================================================================
  TYPES:
    BEGIN OF ts_local,
      index TYPE i,     " Local variable number (1-15)
      value TYPE i,     " Value (signed 16-bit: -32768 to 32767)
    END OF ts_local,

    tt_locals TYPE STANDARD TABLE OF ts_local WITH KEY index.

  "======================================================================
  " Evaluation Stack (per frame)
  "======================================================================
  TYPES:
    tt_eval_stack TYPE STANDARD TABLE OF i WITH EMPTY KEY.

  "======================================================================
  " Call Stack Frame Structure
  "======================================================================
  TYPES:
    BEGIN OF ts_frame,
      return_pc   TYPE i,             " Return address (PC after call)
      result_var  TYPE i,             " Where to store result (-1 = discard)
      num_locals  TYPE i,             " Number of local variables (0-15)
      locals      TYPE tt_locals,     " Local variables table
      eval_stack  TYPE tt_eval_stack, " Evaluation stack
      arg_count   TYPE i,             " Number of arguments supplied (v5+)
    END OF ts_frame,

    tt_frames TYPE STANDARD TABLE OF ts_frame WITH EMPTY KEY.

  "======================================================================
  " Header Information Structure
  "======================================================================
  TYPES:
    BEGIN OF ts_header,
      version     TYPE i,        " Z-machine version (1-8)
      flags1      TYPE i,        " Flags 1
      high_mem    TYPE i,        " Base of high memory
      init_pc     TYPE i,        " Initial program counter
      dictionary  TYPE i,        " Dictionary address
      objects     TYPE i,        " Object table address
      globals     TYPE i,        " Global variables address
      static_mem  TYPE i,        " Base of static memory
      flags2      TYPE i,        " Flags 2
      abbrev      TYPE i,        " Abbreviations table address
      file_len    TYPE i,        " File length (actual)
      checksum    TYPE i,        " Story file checksum
    END OF ts_header.

  "======================================================================
  " Backward compatibility aliases (deprecated - use TS_/TT_ versions)
  "======================================================================
  TYPES:
    ty_instruction TYPE ts_instruction,
    ty_frame       TYPE ts_frame,
    ty_header      TYPE ts_header.

ENDINTERFACE.
