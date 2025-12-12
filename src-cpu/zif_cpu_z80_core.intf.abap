INTERFACE zif_cpu_z80_core PUBLIC.
  " Core CPU operations interface for prefix handlers
  " Allows prefix handlers to access CPU state without tight coupling

  " Memory operations (via bus)
  METHODS read_mem IMPORTING iv_addr TYPE i RETURNING VALUE(rv_val) TYPE i.
  METHODS write_mem IMPORTING iv_addr TYPE i iv_val TYPE i.

  " I/O operations (via bus)
  METHODS read_io IMPORTING iv_port TYPE i RETURNING VALUE(rv_val) TYPE i.
  METHODS write_io IMPORTING iv_port TYPE i iv_val TYPE i.

  " Instruction fetch
  METHODS fetch_byte RETURNING VALUE(rv_byte) TYPE i.
  METHODS fetch_word RETURNING VALUE(rv_word) TYPE i.

  " 16-bit register access (as pairs: high*256+low)
  METHODS get_af RETURNING VALUE(rv_val) TYPE i.
  METHODS set_af IMPORTING iv_val TYPE i.
  METHODS get_bc RETURNING VALUE(rv_val) TYPE i.
  METHODS set_bc IMPORTING iv_val TYPE i.
  METHODS get_de RETURNING VALUE(rv_val) TYPE i.
  METHODS set_de IMPORTING iv_val TYPE i.
  METHODS get_hl RETURNING VALUE(rv_val) TYPE i.
  METHODS set_hl IMPORTING iv_val TYPE i.

  " Index registers
  METHODS get_ix RETURNING VALUE(rv_val) TYPE i.
  METHODS set_ix IMPORTING iv_val TYPE i.
  METHODS get_iy RETURNING VALUE(rv_val) TYPE i.
  METHODS set_iy IMPORTING iv_val TYPE i.

  " Stack pointer
  METHODS get_sp RETURNING VALUE(rv_val) TYPE i.
  METHODS set_sp IMPORTING iv_val TYPE i.

  " Program counter
  METHODS get_pc RETURNING VALUE(rv_val) TYPE i.
  METHODS set_pc IMPORTING iv_val TYPE i.

  " Interrupt registers
  METHODS get_i RETURNING VALUE(rv_val) TYPE i.
  METHODS set_i IMPORTING iv_val TYPE i.
  METHODS get_r RETURNING VALUE(rv_val) TYPE i.
  METHODS set_r IMPORTING iv_val TYPE i.

  " Interrupt flip-flops
  METHODS get_iff1 RETURNING VALUE(rv_val) TYPE abap_bool.
  METHODS set_iff1 IMPORTING iv_val TYPE abap_bool.
  METHODS get_iff2 RETURNING VALUE(rv_val) TYPE abap_bool.
  METHODS set_iff2 IMPORTING iv_val TYPE abap_bool.

  " Interrupt mode
  METHODS get_im RETURNING VALUE(rv_val) TYPE i.
  METHODS set_im IMPORTING iv_val TYPE i.

  " 8-bit register access (convenience)
  METHODS get_a RETURNING VALUE(rv_val) TYPE i.
  METHODS set_a IMPORTING iv_val TYPE i.
  METHODS get_f RETURNING VALUE(rv_val) TYPE i.
  METHODS set_f IMPORTING iv_val TYPE i.
  METHODS get_b RETURNING VALUE(rv_val) TYPE i.
  METHODS set_b IMPORTING iv_val TYPE i.
  METHODS get_c RETURNING VALUE(rv_val) TYPE i.
  METHODS set_c IMPORTING iv_val TYPE i.
  METHODS get_d RETURNING VALUE(rv_val) TYPE i.
  METHODS set_d IMPORTING iv_val TYPE i.
  METHODS get_e RETURNING VALUE(rv_val) TYPE i.
  METHODS set_e IMPORTING iv_val TYPE i.
  METHODS get_h RETURNING VALUE(rv_val) TYPE i.
  METHODS set_h IMPORTING iv_val TYPE i.
  METHODS get_l RETURNING VALUE(rv_val) TYPE i.
  METHODS set_l IMPORTING iv_val TYPE i.

  " Stack operations
  METHODS push IMPORTING iv_val TYPE i.
  METHODS pop RETURNING VALUE(rv_val) TYPE i.

  " Flag constants
  CONSTANTS c_flag_c TYPE i VALUE 1.    " Carry
  CONSTANTS c_flag_n TYPE i VALUE 2.    " Add/Subtract
  CONSTANTS c_flag_pv TYPE i VALUE 4.   " Parity/Overflow
  CONSTANTS c_flag_h TYPE i VALUE 16.   " Half-carry
  CONSTANTS c_flag_z TYPE i VALUE 64.   " Zero
  CONSTANTS c_flag_s TYPE i VALUE 128.  " Sign

  " Flag helpers
  METHODS set_flag IMPORTING iv_flag TYPE i iv_val TYPE abap_bool.
  METHODS get_flag IMPORTING iv_flag TYPE i RETURNING VALUE(rv_val) TYPE abap_bool.

  " ALU operations (reusable by prefix handlers)
  METHODS alu_add8 IMPORTING iv_val TYPE i iv_carry TYPE i DEFAULT 0 RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_sub8 IMPORTING iv_val TYPE i iv_carry TYPE i DEFAULT 0 RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_and8 IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_or8 IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_xor8 IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_cp8 IMPORTING iv_val TYPE i.
  METHODS alu_inc8 IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_dec8 IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.

  " Rotate/shift operations
  METHODS alu_rlc IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_rrc IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_rl IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_rr IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_sla IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_sra IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
  METHODS alu_srl IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.

  " Add cycles
  METHODS add_cycles IMPORTING iv_cycles TYPE i.

ENDINTERFACE.
