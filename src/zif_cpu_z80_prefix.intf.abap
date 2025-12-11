INTERFACE zif_cpu_z80_prefix PUBLIC.
  " Interface for Z80 prefix opcode handlers (CB, DD, ED, FD)

  " Execute the prefix opcode and return cycles consumed
  METHODS execute
    RETURNING VALUE(rv_cycles) TYPE i.

ENDINTERFACE.