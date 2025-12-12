#!/usr/bin/env python3
"""
Z80 Runtime Generator for 6502 Threaded Code Emulator

Generates Z80 machine code for:
- Handler table (16-way interleaved at $7000-$7FFF)
- HLE trap handlers ($6000-$6FFF)
- Memory access routines (READ_6502, WRITE_6502)
- Core runtime (SYNC_ZP, SYNC_STACK, NEXT)

Design A: 8 x 16KB banks at $C000-$FFFF
"""

from dataclasses import dataclass
from typing import List, Dict, Tuple, Optional
import struct

# =============================================================================
# CONSTANTS
# =============================================================================

# Memory layout
HANDLER_TABLE_BASE = 0x7000   # Handler table: $7000-$7FFF
HLE_TRAPS_BASE     = 0x6000   # HLE handlers: $6000-$6FFF
RUNTIME_BASE       = 0x8300   # Runtime code: $8300-$8FFF
ZP_LINEAR_BASE     = 0x8000   # Zero Page: $8000-$80FF
STACK_LINEAR_BASE  = 0x8100   # Stack: $8100-$81FF
BUFFER_BASE        = 0x8200   # Input buffer: $8200-$82FF
CODE_WINDOW_BASE   = 0xC000   # Banked code: $C000-$FFFF

# Sync flags
ZP_SYNCED_ADDR     = 0x8F00
STACK_SYNCED_ADDR  = 0x8F01

# Shadow stack for JSR/RTS (6502 stack is different from threading stack)
SHADOW_STACK_BASE  = 0x8E00
SHADOW_STACK_PTR   = 0x8EFE   # Points to current shadow stack position

# 6502 register storage (when not in Z80 regs)
REG_S_ADDR         = 0x8F02   # 6502 Stack pointer
REG_P_ADDR         = 0x8F03   # 6502 Processor status

# Banking
BANK_PORT          = 0x00
HANDLER_HIGH_BASE  = 0x70     # Handler addresses: $70xx-$7Fxx

# =============================================================================
# Z80 ASSEMBLER HELPERS
# =============================================================================

class Z80Asm:
    """Simple Z80 assembler for generating machine code"""

    def __init__(self, org: int = 0):
        self.code: List[int] = []
        self.org = org
        self.labels: Dict[str, int] = {}
        self.fixups: List[Tuple[int, str, str]] = []  # (offset, label, type)

    def here(self) -> int:
        return self.org + len(self.code)

    def label(self, name: str):
        self.labels[name] = self.here()

    def db(self, *bytes):
        for b in bytes:
            self.code.append(b & 0xFF)

    def dw(self, word: int):
        self.code.append(word & 0xFF)
        self.code.append((word >> 8) & 0xFF)

    def emit(self, *bytes):
        self.db(*bytes)

    # --- Single byte instructions ---
    def nop(self):       self.emit(0x00)
    def ret(self):       self.emit(0xC9)
    def exx(self):       self.emit(0xD9)
    def ex_af_af(self):  self.emit(0x08)
    def di(self):        self.emit(0xF3)
    def ei(self):        self.emit(0xFB)
    def halt(self):      self.emit(0x76)
    def ccf(self):       self.emit(0x3F)
    def scf(self):       self.emit(0x37)
    def cpl(self):       self.emit(0x2F)
    def daa(self):       self.emit(0x27)
    def rla(self):       self.emit(0x17)
    def rra(self):       self.emit(0x1F)
    def rlca(self):      self.emit(0x07)
    def rrca(self):      self.emit(0x0F)

    def ex_de_hl(self):  self.emit(0xEB)
    def ex_sp_hl(self):  self.emit(0xE3)

    def inc_a(self):     self.emit(0x3C)
    def dec_a(self):     self.emit(0x3D)
    def inc_b(self):     self.emit(0x04)
    def dec_b(self):     self.emit(0x05)
    def inc_c(self):     self.emit(0x0C)
    def dec_c(self):     self.emit(0x0D)
    def inc_d(self):     self.emit(0x14)
    def dec_d(self):     self.emit(0x15)
    def inc_e(self):     self.emit(0x1C)
    def dec_e(self):     self.emit(0x1D)
    def inc_h(self):     self.emit(0x24)
    def dec_h(self):     self.emit(0x25)
    def inc_l(self):     self.emit(0x2C)
    def dec_l(self):     self.emit(0x2D)

    def inc_bc(self):    self.emit(0x03)
    def dec_bc(self):    self.emit(0x0B)
    def inc_de(self):    self.emit(0x13)
    def dec_de(self):    self.emit(0x1B)
    def inc_hl(self):    self.emit(0x23)
    def dec_hl(self):    self.emit(0x2B)

    def inc_mem_hl(self): self.emit(0x34)
    def dec_mem_hl(self): self.emit(0x35)

    def add_hl_bc(self): self.emit(0x09)
    def add_hl_de(self): self.emit(0x19)
    def add_hl_hl(self): self.emit(0x29)

    def push_af(self):   self.emit(0xF5)
    def push_bc(self):   self.emit(0xC5)
    def push_de(self):   self.emit(0xD5)
    def push_hl(self):   self.emit(0xE5)
    def pop_af(self):    self.emit(0xF1)
    def pop_bc(self):    self.emit(0xC1)
    def pop_de(self):    self.emit(0xD1)
    def pop_hl(self):    self.emit(0xE1)

    def ld_a_b(self):    self.emit(0x78)
    def ld_a_c(self):    self.emit(0x79)
    def ld_a_d(self):    self.emit(0x7A)
    def ld_a_e(self):    self.emit(0x7B)
    def ld_a_h(self):    self.emit(0x7C)
    def ld_a_l(self):    self.emit(0x7D)
    def ld_b_a(self):    self.emit(0x47)
    def ld_c_a(self):    self.emit(0x4F)
    def ld_d_a(self):    self.emit(0x57)
    def ld_e_a(self):    self.emit(0x5F)
    def ld_h_a(self):    self.emit(0x67)
    def ld_l_a(self):    self.emit(0x6F)
    def ld_d_e(self):    self.emit(0x53)
    def ld_e_l(self):    self.emit(0x5D)
    def ld_d_h(self):    self.emit(0x54)
    def ld_e_d(self):    self.emit(0x5A)
    def ld_h_b(self):    self.emit(0x60)
    def ld_l_c(self):    self.emit(0x69)
    def ld_b_h(self):    self.emit(0x44)
    def ld_c_l(self):    self.emit(0x4D)
    def ld_h_d(self):    self.emit(0x62)
    def ld_l_e(self):    self.emit(0x6B)
    def ld_b_l(self):    self.emit(0x45)
    def ld_c_e(self):    self.emit(0x4B)
    def ld_b_e(self):    self.emit(0x43)
    def ld_c_d(self):    self.emit(0x4A)
    def ld_l_b(self):    self.emit(0x68)
    def ld_l_c(self):    self.emit(0x69)
    def ld_h_c(self):    self.emit(0x61)
    def ld_e_c(self):    self.emit(0x59)
    def ld_d_l(self):    self.emit(0x55)

    def ld_a_mem_hl(self):  self.emit(0x7E)
    def ld_mem_hl_a(self):  self.emit(0x77)
    def ld_a_mem_de(self):  self.emit(0x1A)
    def ld_mem_de_a(self):  self.emit(0x12)
    def ld_a_mem_bc(self):  self.emit(0x0A)
    def ld_mem_bc_a(self):  self.emit(0x02)
    def ld_mem_hl_b(self):  self.emit(0x70)
    def ld_mem_hl_c(self):  self.emit(0x71)
    def ld_mem_hl_d(self):  self.emit(0x72)
    def ld_mem_hl_e(self):  self.emit(0x73)
    def ld_b_mem_hl(self):  self.emit(0x46)
    def ld_c_mem_hl(self):  self.emit(0x4E)
    def ld_d_mem_hl(self):  self.emit(0x56)
    def ld_e_mem_hl(self):  self.emit(0x5E)
    def ld_h_mem_hl(self):  self.emit(0x66)
    def ld_l_mem_hl(self):  self.emit(0x6E)

    def sla_e(self):     self.emit(0xCB, 0x23)
    def sla_d(self):     self.emit(0xCB, 0x22)
    def sla_l(self):     self.emit(0xCB, 0x25)
    def sla_h(self):     self.emit(0xCB, 0x24)
    def sla_a(self):     self.emit(0xCB, 0x27)
    def srl_a(self):     self.emit(0xCB, 0x3F)
    def rl_d(self):      self.emit(0xCB, 0x12)
    def rl_e(self):      self.emit(0xCB, 0x13)
    def rr_a(self):      self.emit(0xCB, 0x1F)
    def bit_7_a(self):   self.emit(0xCB, 0x7F)
    def set_7_a(self):   self.emit(0xCB, 0xFF)
    def res_7_a(self):   self.emit(0xCB, 0xBF)

    def add_a_a(self):   self.emit(0x87)
    def add_a_b(self):   self.emit(0x80)
    def add_a_c(self):   self.emit(0x81)
    def add_a_d(self):   self.emit(0x82)
    def add_a_e(self):   self.emit(0x83)
    def add_a_h(self):   self.emit(0x84)
    def add_a_l(self):   self.emit(0x85)
    def add_a_mem_hl(self): self.emit(0x86)
    def adc_a_b(self):   self.emit(0x88)
    def adc_a_c(self):   self.emit(0x89)
    def adc_a_l(self):   self.emit(0x8D)
    def adc_a_mem_hl(self): self.emit(0x8E)
    def sub_b(self):     self.emit(0x90)
    def sub_c(self):     self.emit(0x91)
    def sub_l(self):     self.emit(0x95)
    def sub_mem_hl(self): self.emit(0x96)
    def sbc_a_a(self):   self.emit(0x9F)
    def sbc_a_b(self):   self.emit(0x98)
    def sbc_a_c(self):   self.emit(0x99)
    def sbc_a_l(self):   self.emit(0x9D)
    def sbc_a_mem_hl(self): self.emit(0x9E)
    def and_a(self):     self.emit(0xA7)
    def and_b(self):     self.emit(0xA0)
    def and_c(self):     self.emit(0xA1)
    def and_l(self):     self.emit(0xA5)
    def and_mem_hl(self): self.emit(0xA6)
    def xor_a(self):     self.emit(0xAF)
    def xor_b(self):     self.emit(0xA8)
    def xor_c(self):     self.emit(0xA9)
    def xor_l(self):     self.emit(0xAD)
    def xor_mem_hl(self): self.emit(0xAE)
    def or_a(self):      self.emit(0xB7)
    def or_b(self):      self.emit(0xB0)
    def or_c(self):      self.emit(0xB1)
    def or_l(self):      self.emit(0xB5)
    def or_d(self):      self.emit(0xB2)
    def or_mem_hl(self): self.emit(0xB6)
    def cp_b(self):      self.emit(0xB8)
    def cp_c(self):      self.emit(0xB9)
    def cp_l(self):      self.emit(0xBD)
    def cp_mem_hl(self): self.emit(0xBE)

    # --- Instructions with immediate byte ---
    def ld_a_n(self, n):   self.emit(0x3E, n & 0xFF)
    def ld_b_n(self, n):   self.emit(0x06, n & 0xFF)
    def ld_c_n(self, n):   self.emit(0x0E, n & 0xFF)
    def ld_d_n(self, n):   self.emit(0x16, n & 0xFF)
    def ld_e_n(self, n):   self.emit(0x1E, n & 0xFF)
    def ld_h_n(self, n):   self.emit(0x26, n & 0xFF)
    def ld_l_n(self, n):   self.emit(0x2E, n & 0xFF)
    def add_a_n(self, n):  self.emit(0xC6, n & 0xFF)
    def adc_a_n(self, n):  self.emit(0xCE, n & 0xFF)
    def sub_n(self, n):    self.emit(0xD6, n & 0xFF)
    def sbc_a_n(self, n):  self.emit(0xDE, n & 0xFF)
    def and_n(self, n):    self.emit(0xE6, n & 0xFF)
    def xor_n(self, n):    self.emit(0xEE, n & 0xFF)
    def or_n(self, n):     self.emit(0xF6, n & 0xFF)
    def cp_n(self, n):     self.emit(0xFE, n & 0xFF)
    def in_a_n(self, n):   self.emit(0xDB, n & 0xFF)
    def out_n_a(self, n):  self.emit(0xD3, n & 0xFF)

    # --- Instructions with 16-bit immediate ---
    def ld_bc_nn(self, nn): self.emit(0x01); self.dw(nn)
    def ld_de_nn(self, nn): self.emit(0x11); self.dw(nn)
    def ld_hl_nn(self, nn): self.emit(0x21); self.dw(nn)
    def ld_sp_nn(self, nn): self.emit(0x31); self.dw(nn)

    def ld_mem_nn_a(self, nn):  self.emit(0x32); self.dw(nn)
    def ld_a_mem_nn(self, nn):  self.emit(0x3A); self.dw(nn)
    def ld_mem_nn_hl(self, nn): self.emit(0x22); self.dw(nn)
    def ld_hl_mem_nn(self, nn): self.emit(0x2A); self.dw(nn)
    def ld_mem_nn_de(self, nn): self.emit(0xED, 0x53); self.dw(nn)
    def ld_de_mem_nn(self, nn): self.emit(0xED, 0x5B); self.dw(nn)
    def ld_mem_nn_bc(self, nn): self.emit(0xED, 0x43); self.dw(nn)
    def ld_bc_mem_nn(self, nn): self.emit(0xED, 0x4B); self.dw(nn)

    # --- Jumps and calls ---
    def jp_nn(self, nn):     self.emit(0xC3); self.dw(nn)
    def jp_hl(self):         self.emit(0xE9)
    def jp_z_nn(self, nn):   self.emit(0xCA); self.dw(nn)
    def jp_nz_nn(self, nn):  self.emit(0xC2); self.dw(nn)
    def jp_c_nn(self, nn):   self.emit(0xDA); self.dw(nn)
    def jp_nc_nn(self, nn):  self.emit(0xD2); self.dw(nn)
    def call_nn(self, nn):   self.emit(0xCD); self.dw(nn)
    def call_z_nn(self, nn): self.emit(0xCC); self.dw(nn)
    def call_nz_nn(self, nn):self.emit(0xC4); self.dw(nn)
    def call_c_nn(self, nn): self.emit(0xDC); self.dw(nn)
    def call_nc_nn(self, nn):self.emit(0xD4); self.dw(nn)

    def jr_n(self, offset):  self.emit(0x18, offset & 0xFF)
    def jr_z_n(self, offset):  self.emit(0x28, offset & 0xFF)
    def jr_nz_n(self, offset): self.emit(0x20, offset & 0xFF)
    def jr_c_n(self, offset):  self.emit(0x38, offset & 0xFF)
    def jr_nc_n(self, offset): self.emit(0x30, offset & 0xFF)
    def djnz_n(self, offset):  self.emit(0x10, offset & 0xFF)

    # --- Relative jumps to labels ---
    def jr_label(self, label: str):
        self.emit(0x18, 0x00)
        self.fixups.append((len(self.code) - 1, label, 'rel'))

    def jr_z_label(self, label: str):
        self.emit(0x28, 0x00)
        self.fixups.append((len(self.code) - 1, label, 'rel'))

    def jr_nz_label(self, label: str):
        self.emit(0x20, 0x00)
        self.fixups.append((len(self.code) - 1, label, 'rel'))

    def jr_c_label(self, label: str):
        self.emit(0x38, 0x00)
        self.fixups.append((len(self.code) - 1, label, 'rel'))

    def jr_nc_label(self, label: str):
        self.emit(0x30, 0x00)
        self.fixups.append((len(self.code) - 1, label, 'rel'))

    def djnz_label(self, label: str):
        self.emit(0x10, 0x00)
        self.fixups.append((len(self.code) - 1, label, 'rel'))

    def jp_label(self, label: str):
        self.emit(0xC3, 0x00, 0x00)
        self.fixups.append((len(self.code) - 2, label, 'abs'))

    def call_label(self, label: str):
        self.emit(0xCD, 0x00, 0x00)
        self.fixups.append((len(self.code) - 2, label, 'abs'))

    def resolve(self):
        """Resolve all label references"""
        for offset, label, fixup_type in self.fixups:
            if label not in self.labels:
                raise ValueError(f"Unknown label: {label}")
            target = self.labels[label]

            if fixup_type == 'rel':
                # Relative offset from instruction after the offset byte
                rel = target - (self.org + offset + 1)
                if rel < -128 or rel > 127:
                    raise ValueError(f"Relative jump out of range: {label} ({rel})")
                self.code[offset] = rel & 0xFF
            elif fixup_type == 'abs':
                self.code[offset] = target & 0xFF
                self.code[offset + 1] = (target >> 8) & 0xFF

    def get_bytes(self) -> bytes:
        self.resolve()
        return bytes(self.code)


# =============================================================================
# RUNTIME GENERATOR
# =============================================================================

def generate_runtime() -> Z80Asm:
    """Generate the main runtime code at $8300"""

    asm = Z80Asm(org=RUNTIME_BASE)

    # ----- READ_6502: DE = 6502 addr -> A = value -----
    asm.label('READ_6502')
    # Bank select (upper 3 bits direct to port)
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.out_n_a(BANK_PORT)
    # Address calc
    asm.ld_a_d()
    asm.and_n(0x1F)          # offset high (5 bits)
    asm.sla_e()              # x2 low
    asm.rla()                # x2 high + carry
    asm.or_n(0xC0)           # add base $C000
    asm.ld_d_a()
    # Read
    asm.ld_a_mem_de()
    asm.ret()

    # ----- WRITE_6502: A = value, DE = 6502 addr -----
    asm.label('WRITE_6502')
    asm.ex_af_af()           # save value to A'
    # Bank select
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.out_n_a(BANK_PORT)
    # Address calc
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    # Write low byte (data)
    asm.ex_af_af()           # restore value
    asm.ld_mem_de_a()
    # Write high byte (handler)
    asm.and_n(0x0F)
    asm.or_n(HANDLER_HIGH_BASE)
    asm.inc_de()
    asm.ld_mem_de_a()
    asm.ret()

    # ----- SYNC_ZP: Copy linear ZP to CODE area -----
    asm.label('SYNC_ZP')
    asm.ld_hl_nn(ZP_LINEAR_BASE)    # source
    asm.ld_de_nn(CODE_WINDOW_BASE)  # dest ($C000)
    # Select bank 0
    asm.xor_a()
    asm.out_n_a(BANK_PORT)
    asm.ld_b_n(0)            # 256 iterations
    asm.label('_sync_zp_loop')
    asm.ld_a_mem_hl()        # read ZP byte
    asm.ld_mem_de_a()        # write low byte
    asm.inc_de()
    asm.and_n(0x0F)
    asm.or_n(HANDLER_HIGH_BASE)
    asm.ld_mem_de_a()        # write high byte
    asm.inc_de()
    asm.inc_hl()
    asm.djnz_label('_sync_zp_loop')
    asm.ret()

    # ----- SYNC_STACK: Copy linear Stack to CODE area -----
    asm.label('SYNC_STACK')
    asm.ld_hl_nn(STACK_LINEAR_BASE) # source
    asm.ld_de_nn(CODE_WINDOW_BASE + 0x200)  # dest ($C200)
    # Select bank 0
    asm.xor_a()
    asm.out_n_a(BANK_PORT)
    asm.ld_b_n(0)            # 256 iterations
    asm.label('_sync_stack_loop')
    asm.ld_a_mem_hl()
    asm.ld_mem_de_a()
    asm.inc_de()
    asm.and_n(0x0F)
    asm.or_n(HANDLER_HIGH_BASE)
    asm.ld_mem_de_a()
    asm.inc_de()
    asm.inc_hl()
    asm.djnz_label('_sync_stack_loop')
    asm.ret()

    # ----- ADDR_TO_Z80: DE = 6502 addr -> DE = Z80 addr -----
    # (Also does bank select)
    asm.label('ADDR_TO_Z80')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.out_n_a(BANK_PORT)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ret()

    # ----- GET_OPERAND_BYTE: Pop from stream, return low byte in A -----
    asm.label('GET_OPERAND_BYTE')
    asm.pop_hl()             # HL = handler-encoded operand
    asm.ld_a_l()             # A = low byte = actual operand
    asm.ret()

    # ----- GET_OPERAND_WORD: Pop 2 from stream, return 6502 addr in DE -----
    asm.label('GET_OPERAND_WORD')
    asm.pop_hl()             # HL = first word (low byte = addr_lo)
    asm.pop_de()             # DE = second word (low byte = addr_hi)
    asm.ld_a_l()             # A = addr_lo
    asm.ld_d_e()             # D = addr_hi
    asm.ld_e_a()             # E = addr_lo
    # DE = 6502 address
    asm.ret()

    return asm


def generate_hle_traps() -> Z80Asm:
    """Generate HLE trap handlers at $6000"""

    asm = Z80Asm(org=HLE_TRAPS_BASE)

    # ----- TRAP_COUT ($6010): Output char from 6502 A -----
    asm.label('TRAP_COUT')
    # A = 6502 A (already in Z80 A)
    asm.out_n_a(0x01)        # VZ80 console port
    asm.ret()

    # ----- TRAP_GETLN ($6020): Read line into buffer -----
    asm.label('TRAP_GETLN')
    asm.ld_hl_nn(BUFFER_BASE)  # $8200 = 6502 $0200
    asm.label('_getln_loop')
    asm.in_a_n(0x01)         # read char
    asm.cp_n(0x0D)           # CR?
    asm.jr_z_label('_getln_done')
    asm.ld_mem_hl_a()
    asm.inc_hl()
    asm.jr_label('_getln_loop')
    asm.label('_getln_done')
    # Calculate length -> 6502 X
    asm.ld_a_l()
    asm.sub_n(BUFFER_BASE & 0xFF)
    asm.ld_b_a()             # B = 6502 X = length
    asm.ret()

    # ----- TRAP_HOME ($6030): Clear screen -----
    asm.label('TRAP_HOME')
    asm.ld_a_n(0x0C)         # Form feed
    asm.out_n_a(0x01)
    asm.ret()

    # ----- TRAP_RDKEY ($6040): Read single key -----
    asm.label('TRAP_RDKEY')
    asm.in_a_n(0x01)
    asm.ret()

    # ----- TRAP_UNIMPL ($6050): Unimplemented trap -----
    asm.label('TRAP_UNIMPL')
    asm.ld_a_n(ord('?'))
    asm.out_n_a(0x01)
    asm.halt()

    return asm


def generate_handler_stub(opcode: int, asm: Z80Asm, handler_impls: Dict[int, str]):
    """Generate a single handler stub that jumps to implementation"""

    # Handler address format: ($70 | (opcode & $0F)) << 8 | opcode
    handler_high = HANDLER_HIGH_BASE | (opcode & 0x0F)
    handler_addr = (handler_high << 8) | opcode

    if opcode in handler_impls:
        # Jump to implementation
        impl_label = handler_impls[opcode]
        asm.jp_label(impl_label)
    else:
        # Unimplemented - halt
        asm.ld_a_n(opcode)
        asm.halt()


# =============================================================================
# OPCODE HANDLERS
# =============================================================================

def generate_opcode_handlers() -> Z80Asm:
    """Generate implementations for 6502 opcodes"""

    asm = Z80Asm(org=0x6100)  # After HLE traps

    # Register usage:
    # A  = 6502 A
    # B  = 6502 X
    # C  = 6502 Y
    # DE = temp / addresses
    # HL = temp / addresses
    # A' = temp (via EX AF,AF')

    # ===== LOAD INSTRUCTIONS =====

    # LDA #imm ($A9)
    asm.label('H_LDA_IMM')
    asm.pop_hl()             # L = immediate
    asm.ld_a_l()             # A = value
    asm.or_a()               # set Z, S flags
    asm.ret()

    # LDA zp ($A5)
    asm.label('H_LDA_ZP')
    asm.pop_hl()             # L = ZP addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)  # H = $80
    asm.ld_a_mem_hl()        # A = (ZP)
    asm.or_a()
    asm.ret()

    # LDA zp,X ($B5)
    asm.label('H_LDA_ZPX')
    asm.pop_hl()             # L = base
    asm.ld_a_l()
    asm.add_a_b()            # A = base + X
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_a_mem_hl()
    asm.or_a()
    asm.ret()

    # LDA abs ($AD)
    asm.label('H_LDA_ABS')
    asm.ex_af_af()           # save 6502 A
    asm.call_label('GET_OPERAND_WORD')  # DE = addr
    asm.call_label('READ_6502')         # A = value
    asm.or_a()               # set flags
    asm.ex_af_af()           # A' = value with flags
    asm.ex_af_af()           # A = value (this is our 6502 A now)
    asm.ret()

    # LDX #imm ($A2)
    asm.label('H_LDX_IMM')
    asm.pop_hl()
    asm.ld_b_a()             # save A
    asm.ld_a_l()
    asm.or_a()               # set flags
    asm.ld_b_a()             # B = X = value
    # Restore A... tricky. We need flags from L but A preserved
    # Actually, let's just update X and set flags based on X
    asm.pop_hl()
    asm.ld_b_l()             # B = X = immediate
    asm.ld_a_b()
    asm.or_a()               # set flags for X
    asm.ret()

    # LDY #imm ($A0)
    asm.label('H_LDY_IMM')
    asm.pop_hl()
    asm.ld_c_l()             # C = Y = immediate
    asm.ld_a_c()
    asm.or_a()
    asm.ret()

    # ===== STORE INSTRUCTIONS =====

    # STA zp ($85)
    asm.label('H_STA_ZP')
    asm.pop_hl()             # L = ZP addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_mem_hl_a()
    # Clear ZP sync flag
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()
    asm.ret()

    # STA abs ($8D)
    asm.label('H_STA_ABS')
    asm.ex_af_af()           # save 6502 A
    asm.call_label('GET_OPERAND_WORD')  # DE = addr
    asm.ex_af_af()           # A = value to store
    asm.call_label('WRITE_6502')
    asm.ret()

    # STX zp ($86)
    asm.label('H_STX_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_mem_hl_b()        # store X
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()
    asm.ret()

    # STY zp ($84)
    asm.label('H_STY_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_mem_hl_c()        # store Y
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()
    asm.ret()

    # ===== TRANSFER INSTRUCTIONS =====

    # TAX ($AA)
    asm.label('H_TAX')
    asm.ld_b_a()             # X = A
    asm.or_a()
    asm.ret()

    # TAY ($A8)
    asm.label('H_TAY')
    asm.ld_c_a()             # Y = A
    asm.or_a()
    asm.ret()

    # TXA ($8A)
    asm.label('H_TXA')
    asm.ld_a_b()             # A = X
    asm.or_a()
    asm.ret()

    # TYA ($98)
    asm.label('H_TYA')
    asm.ld_a_c()             # A = Y
    asm.or_a()
    asm.ret()

    # ===== INCREMENT/DECREMENT =====

    # INX ($E8)
    asm.label('H_INX')
    asm.inc_b()
    asm.ld_a_b()
    asm.or_a()               # set flags
    asm.ret()

    # INY ($C8)
    asm.label('H_INY')
    asm.inc_c()
    asm.ld_a_c()
    asm.or_a()
    asm.ret()

    # DEX ($CA)
    asm.label('H_DEX')
    asm.dec_b()
    asm.ld_a_b()
    asm.or_a()
    asm.ret()

    # DEY ($88)
    asm.label('H_DEY')
    asm.dec_c()
    asm.ld_a_c()
    asm.or_a()
    asm.ret()

    # INC zp ($E6)
    asm.label('H_INC_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.inc_mem_hl()
    # Clear sync, set flags
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ld_a_mem_hl()
    asm.or_a()
    asm.ex_af_af()
    asm.ret()

    # DEC zp ($C6)
    asm.label('H_DEC_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.dec_mem_hl()
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ld_a_mem_hl()
    asm.or_a()
    asm.ex_af_af()
    asm.ret()

    # ===== ARITHMETIC =====

    # ADC #imm ($69)
    asm.label('H_ADC_IMM')
    asm.pop_hl()
    asm.adc_a_l()            # A = A + imm + C
    asm.ret()

    # SBC #imm ($E9)
    asm.label('H_SBC_IMM')
    asm.pop_hl()
    asm.sbc_a_l()            # A = A - imm - !C
    asm.ret()

    # ===== LOGICAL =====

    # AND #imm ($29)
    asm.label('H_AND_IMM')
    asm.pop_hl()
    asm.and_l()
    asm.ret()

    # ORA #imm ($09)
    asm.label('H_ORA_IMM')
    asm.pop_hl()
    asm.or_l()
    asm.ret()

    # EOR #imm ($49)
    asm.label('H_EOR_IMM')
    asm.pop_hl()
    asm.xor_l()
    asm.ret()

    # ===== COMPARE =====

    # CMP #imm ($C9)
    asm.label('H_CMP_IMM')
    asm.pop_hl()
    asm.cp_l()               # compare A with imm
    asm.ret()

    # CPX #imm ($E0)
    asm.label('H_CPX_IMM')
    asm.pop_hl()
    asm.ld_a_b()             # A = X
    asm.cp_l()
    asm.ret()

    # CPY #imm ($C0)
    asm.label('H_CPY_IMM')
    asm.pop_hl()
    asm.ld_a_c()             # A = Y
    asm.cp_l()
    asm.ret()

    # ===== BRANCHES =====
    # For branches, we need to handle the relative offset
    # The offset is in the threaded stream as a handler-encoded byte

    # BEQ ($F0)
    asm.label('H_BEQ')
    asm.pop_hl()             # L = offset
    asm.jr_nz_label('_beq_no')
    # Take branch: adjust SP by offset
    # This is tricky... we need to add L to SP
    asm.ld_a_l()
    asm.ld_e_a()
    # Sign extend to DE
    asm.add_a_a()            # carry = sign bit
    asm.sbc_a_a()            # A = 0 or $FF
    asm.ld_d_a()
    # DE = signed offset, need to multiply by 2 (handler expansion)
    asm.sla_e()
    asm.rl_d()
    # Add to SP... but we can't easily do that
    # Alternative: use HL
    asm.add_hl_de()          # HL = SP + offset*2 (but HL was popped value!)
    # Actually we need current SP...
    # This is getting complicated. Let's use a simpler approach:
    # Store offset, manipulate SP later
    asm.label('_beq_no')
    asm.ret()

    # BNE ($D0)
    asm.label('H_BNE')
    asm.pop_hl()
    asm.jr_z_label('_bne_no')
    # Take branch - same complexity as BEQ
    asm.label('_bne_no')
    asm.ret()

    # BCS ($B0)
    asm.label('H_BCS')
    asm.pop_hl()
    asm.jr_nc_label('_bcs_no')
    asm.label('_bcs_no')
    asm.ret()

    # BCC ($90)
    asm.label('H_BCC')
    asm.pop_hl()
    asm.jr_c_label('_bcc_no')
    asm.label('_bcc_no')
    asm.ret()

    # BMI ($30)
    asm.label('H_BMI')
    asm.pop_hl()
    # Check sign flag (bit 7 of last result)
    # Z80 S flag = bit 7, so JP M would work if flags are set
    asm.ret()

    # BPL ($10)
    asm.label('H_BPL')
    asm.pop_hl()
    asm.ret()

    # ===== JUMPS =====

    # JMP abs ($4C)
    asm.label('H_JMP_ABS')
    asm.call_label('GET_OPERAND_WORD')  # DE = target
    # Check for ZP/Stack
    asm.ld_a_d()
    asm.or_a()
    asm.jr_z_label('_jmp_zp')
    asm.cp_n(0x01)
    asm.jr_z_label('_jmp_stack')
    # Normal jump
    asm.label('_jmp_normal')
    asm.call_label('ADDR_TO_Z80')
    asm.ex_de_hl()
    asm.jp_hl()

    asm.label('_jmp_zp')
    asm.ex_af_af()
    asm.ld_a_mem_nn(ZP_SYNCED_ADDR)
    asm.or_a()
    asm.jr_nz_label('_jmp_zp_ok')
    asm.call_label('SYNC_ZP')
    asm.ld_a_n(1)
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.label('_jmp_zp_ok')
    asm.ex_af_af()
    asm.jr_label('_jmp_normal')

    asm.label('_jmp_stack')
    asm.ex_af_af()
    asm.ld_a_mem_nn(STACK_SYNCED_ADDR)
    asm.or_a()
    asm.jr_nz_label('_jmp_stack_ok')
    asm.call_label('SYNC_STACK')
    asm.ld_a_n(1)
    asm.ld_mem_nn_a(STACK_SYNCED_ADDR)
    asm.label('_jmp_stack_ok')
    asm.ex_af_af()
    asm.jr_label('_jmp_normal')

    # JSR abs ($20)
    asm.label('H_JSR')
    asm.call_label('GET_OPERAND_WORD')  # DE = target

    # Check for ROM trap ($FC00+)
    asm.ld_a_d()
    asm.cp_n(0xFC)
    asm.jr_nc_label('_jsr_trap')

    # Check for ZP/Stack
    asm.or_a()
    asm.jr_z_label('_jsr_zp')
    asm.cp_n(0x01)
    asm.jr_z_label('_jsr_stack')

    # Normal JSR - push return to shadow stack
    asm.label('_jsr_do')
    # TODO: push return address to shadow stack
    asm.call_label('ADDR_TO_Z80')
    asm.ex_de_hl()
    asm.jp_hl()

    asm.label('_jsr_trap')
    # Look up trap handler
    # Trap table at $8C00, indexed by (addr & $03FF)
    asm.ld_h_n(0x8C)
    asm.ld_a_d()
    asm.and_n(0x03)
    asm.or_n(0x8C)
    asm.ld_h_a()
    asm.ld_l_e()
    # Read trap handler address
    asm.ld_e_mem_hl()
    asm.inc_hl()
    asm.ld_d_mem_hl()
    asm.ex_de_hl()
    asm.jp_hl()

    asm.label('_jsr_zp')
    asm.ex_af_af()
    asm.ld_a_mem_nn(ZP_SYNCED_ADDR)
    asm.or_a()
    asm.jr_nz_label('_jsr_zp_ok')
    asm.call_label('SYNC_ZP')
    asm.ld_a_n(1)
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.label('_jsr_zp_ok')
    asm.ex_af_af()
    asm.jr_label('_jsr_do')

    asm.label('_jsr_stack')
    asm.ex_af_af()
    asm.ld_a_mem_nn(STACK_SYNCED_ADDR)
    asm.or_a()
    asm.jr_nz_label('_jsr_stack_ok')
    asm.call_label('SYNC_STACK')
    asm.ld_a_n(1)
    asm.ld_mem_nn_a(STACK_SYNCED_ADDR)
    asm.label('_jsr_stack_ok')
    asm.ex_af_af()
    asm.jr_label('_jsr_do')

    # RTS ($60)
    asm.label('H_RTS')
    # Pop return from shadow stack
    # TODO: implement shadow stack
    asm.halt()               # placeholder

    # ===== FLAG INSTRUCTIONS =====

    # CLC ($18)
    asm.label('H_CLC')
    asm.and_a()              # clear carry (actually OR A also works, but AND A is more explicit)
    # Wait, we need to clear carry without affecting A
    # SCF then CCF? Or just CCF if carry is set?
    asm.scf()
    asm.ccf()                # C = 0
    asm.ret()

    # SEC ($38)
    asm.label('H_SEC')
    asm.scf()                # C = 1
    asm.ret()

    # CLI ($58) - no interrupts in our emulator
    asm.label('H_CLI')
    asm.ret()

    # SEI ($78)
    asm.label('H_SEI')
    asm.ret()

    # CLD ($D8) - no decimal mode
    asm.label('H_CLD')
    asm.ret()

    # SED ($F8)
    asm.label('H_SED')
    asm.ret()

    # CLV ($B8)
    asm.label('H_CLV')
    asm.ret()

    # ===== STACK =====

    # PHA ($48)
    asm.label('H_PHA')
    # Push A to 6502 stack
    asm.ex_af_af()
    asm.ld_a_mem_nn(REG_S_ADDR)
    asm.ld_l_a()
    asm.ld_h_n(STACK_LINEAR_BASE >> 8)
    asm.ex_af_af()
    asm.ld_mem_hl_a()
    # Decrement S
    asm.ex_af_af()
    asm.dec_a()
    asm.ld_mem_nn_a(REG_S_ADDR)
    # Clear stack sync
    asm.xor_a()
    asm.ld_mem_nn_a(STACK_SYNCED_ADDR)
    asm.ex_af_af()
    asm.ret()

    # PLA ($68)
    asm.label('H_PLA')
    # Increment S first
    asm.ex_af_af()
    asm.ld_a_mem_nn(REG_S_ADDR)
    asm.inc_a()
    asm.ld_mem_nn_a(REG_S_ADDR)
    asm.ld_l_a()
    asm.ld_h_n(STACK_LINEAR_BASE >> 8)
    asm.ld_a_mem_hl()
    asm.ex_af_af()           # A' = pulled value
    asm.ex_af_af()           # A = pulled value
    asm.or_a()               # set flags
    asm.ret()

    # ===== NOP =====

    # NOP ($EA)
    asm.label('H_NOP')
    asm.ret()

    # ===== BRK =====

    # BRK ($00)
    asm.label('H_BRK')
    asm.halt()               # Stop emulation

    return asm


# =============================================================================
# HANDLER TABLE
# =============================================================================

# Mapping of 6502 opcodes to handler labels
OPCODE_HANDLERS = {
    0x00: 'H_BRK',
    0x09: 'H_ORA_IMM',
    0x10: 'H_BPL',
    0x18: 'H_CLC',
    0x20: 'H_JSR',
    0x29: 'H_AND_IMM',
    0x30: 'H_BMI',
    0x38: 'H_SEC',
    0x48: 'H_PHA',
    0x49: 'H_EOR_IMM',
    0x4C: 'H_JMP_ABS',
    0x58: 'H_CLI',
    0x60: 'H_RTS',
    0x68: 'H_PLA',
    0x69: 'H_ADC_IMM',
    0x78: 'H_SEI',
    0x84: 'H_STY_ZP',
    0x85: 'H_STA_ZP',
    0x86: 'H_STX_ZP',
    0x88: 'H_DEY',
    0x8A: 'H_TXA',
    0x8D: 'H_STA_ABS',
    0x90: 'H_BCC',
    0x98: 'H_TYA',
    0xA0: 'H_LDY_IMM',
    0xA2: 'H_LDX_IMM',
    0xA5: 'H_LDA_ZP',
    0xA8: 'H_TAY',
    0xA9: 'H_LDA_IMM',
    0xAA: 'H_TAX',
    0xAD: 'H_LDA_ABS',
    0xB0: 'H_BCS',
    0xB5: 'H_LDA_ZPX',
    0xB8: 'H_CLV',
    0xC0: 'H_CPY_IMM',
    0xC6: 'H_DEC_ZP',
    0xC8: 'H_INY',
    0xC9: 'H_CMP_IMM',
    0xCA: 'H_DEX',
    0xD0: 'H_BNE',
    0xD8: 'H_CLD',
    0xE0: 'H_CPX_IMM',
    0xE6: 'H_INC_ZP',
    0xE8: 'H_INX',
    0xE9: 'H_SBC_IMM',
    0xEA: 'H_NOP',
    0xF0: 'H_BEQ',
    0xF8: 'H_SED',
}


def generate_handler_table(handler_addrs: Dict[str, int]) -> bytes:
    """
    Generate the 16-way interleaved handler table at $7000-$7FFF

    Handler address format: ($70 | (opcode & $0F)) << 8 | opcode

    For opcode $A9: addr = $79A9
    Table entry at $79A9 contains JP to actual handler

    The table is 4KB ($7000-$7FFF). Each handler entry needs 3 bytes (JP nn).
    Opcodes are interleaved by low nibble, giving 16 bytes spacing.
    """

    table = bytearray(4096)  # $7000-$7FFF

    for opcode in range(256):
        handler_high = HANDLER_HIGH_BASE | (opcode & 0x0F)
        handler_addr = (handler_high << 8) | opcode
        table_offset = handler_addr - HANDLER_TABLE_BASE

        # Safety check - ensure we don't overflow
        if table_offset + 2 >= len(table):
            # This can happen for opcodes $xD, $xE, $xF with high values
            # Use a simpler encoding or skip
            if table_offset < len(table):
                table[table_offset] = 0x76  # HALT
            continue

        if opcode in OPCODE_HANDLERS:
            label = OPCODE_HANDLERS[opcode]
            if label in handler_addrs:
                target = handler_addrs[label]
                # JP target (3 bytes)
                table[table_offset] = 0xC3      # JP
                table[table_offset + 1] = target & 0xFF
                table[table_offset + 2] = (target >> 8) & 0xFF
            else:
                # Handler not implemented yet
                table[table_offset] = 0x76      # HALT
        else:
            # Unimplemented opcode - HALT with opcode in A for debugging
            table[table_offset] = 0x3E          # LD A, opcode
            if table_offset + 1 < len(table):
                table[table_offset + 1] = opcode
            if table_offset + 2 < len(table):
                table[table_offset + 2] = 0x76  # HALT

    return bytes(table)


# =============================================================================
# MAIN
# =============================================================================

def generate_all_code() -> Tuple[Z80Asm, Z80Asm, Z80Asm]:
    """
    Generate all code sections.
    Returns (runtime_asm, hle_asm, handlers_asm) with shared labels.
    """
    # First pass: generate to get label addresses
    runtime_asm = generate_runtime()
    hle_asm = generate_hle_traps()

    # Create a shared labels dict for handlers
    shared_labels = {}
    shared_labels.update(runtime_asm.labels)
    shared_labels.update(hle_asm.labels)

    # Generate handlers with access to runtime labels
    handlers_asm = generate_opcode_handlers()
    # Copy runtime labels into handlers_asm
    handlers_asm.labels.update(shared_labels)

    return runtime_asm, hle_asm, handlers_asm


def main():
    print("Z80 Runtime Generator for 6502 Threaded Code")
    print("=" * 50)

    # Generate all components
    runtime_asm, hle_asm, handlers_asm = generate_all_code()

    # Collect all labels
    all_labels = {}
    all_labels.update(runtime_asm.labels)
    all_labels.update(hle_asm.labels)
    all_labels.update(handlers_asm.labels)

    # Resolve references
    runtime_asm.resolve()
    hle_asm.resolve()
    handlers_asm.resolve()

    # Get bytes
    runtime_bytes = runtime_asm.get_bytes()
    hle_bytes = hle_asm.get_bytes()
    handlers_bytes = handlers_asm.get_bytes()

    # Generate handler table
    handler_table = generate_handler_table(all_labels)

    # Print summary
    print(f"\nRuntime ($8300): {len(runtime_bytes)} bytes")
    print(f"HLE Traps ($6000): {len(hle_bytes)} bytes")
    print(f"Handlers ($6100): {len(handlers_bytes)} bytes")
    print(f"Handler Table ($7000): {len(handler_table)} bytes")

    print(f"\nImplemented opcodes: {len(OPCODE_HANDLERS)}/256")

    # Print some handler addresses
    print("\nKey addresses:")
    for name in ['READ_6502', 'WRITE_6502', 'SYNC_ZP', 'TRAP_COUT', 'H_LDA_IMM', 'H_JSR']:
        if name in all_labels:
            print(f"  {name}: ${all_labels[name]:04X}")

    # Save to file
    output_file = 'runtime_6502.bin'
    with open(output_file, 'wb') as f:
        # Write each section at its proper offset
        # For now, just concatenate with markers
        f.write(b'HLE@6000')
        f.write(hle_bytes)
        f.write(b'HDL@6100')
        f.write(handlers_bytes)
        f.write(b'TBL@7000')
        f.write(handler_table)
        f.write(b'RUN@8300')
        f.write(runtime_bytes)

    print(f"\nSaved to {output_file}")

    # Also save as Python module for VZ80
    py_output = 'runtime_6502_data.py'
    with open(py_output, 'w') as f:
        f.write('# Generated Z80 runtime for 6502 emulator\n\n')
        f.write(f'RUNTIME_ORG = 0x{RUNTIME_BASE:04X}\n')
        f.write(f'HLE_ORG = 0x{HLE_TRAPS_BASE:04X}\n')
        f.write(f'HANDLERS_ORG = 0x6100\n')
        f.write(f'TABLE_ORG = 0x{HANDLER_TABLE_BASE:04X}\n\n')

        f.write('RUNTIME_CODE = bytes([\n')
        for i in range(0, len(runtime_bytes), 16):
            chunk = runtime_bytes[i:i+16]
            f.write('    ' + ', '.join(f'0x{b:02X}' for b in chunk) + ',\n')
        f.write('])\n\n')

        f.write('HLE_CODE = bytes([\n')
        for i in range(0, len(hle_bytes), 16):
            chunk = hle_bytes[i:i+16]
            f.write('    ' + ', '.join(f'0x{b:02X}' for b in chunk) + ',\n')
        f.write('])\n\n')

        f.write('HANDLERS_CODE = bytes([\n')
        for i in range(0, len(handlers_bytes), 16):
            chunk = handlers_bytes[i:i+16]
            f.write('    ' + ', '.join(f'0x{b:02X}' for b in chunk) + ',\n')
        f.write('])\n\n')

        f.write('HANDLER_TABLE = bytes([\n')
        for i in range(0, len(handler_table), 16):
            chunk = handler_table[i:i+16]
            f.write('    ' + ', '.join(f'0x{b:02X}' for b in chunk) + ',\n')
        f.write('])\n\n')

        f.write('LABELS = {\n')
        for name, addr in sorted(all_labels.items()):
            f.write(f'    {name!r}: 0x{addr:04X},\n')
        f.write('}\n')

    print(f"Saved Python module to {py_output}")


if __name__ == '__main__':
    main()
