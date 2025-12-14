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
CODE_BANK_ADDR     = 0x8F04   # Current code bank (for restore after writes)
D_FLAG_ADDR        = 0x8F05   # Decimal mode flag (non-zero = decimal mode)

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
    def add_hl_sp(self): self.emit(0x39)

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
    def sla_mem_hl(self): self.emit(0xCB, 0x26)
    def srl_a(self):     self.emit(0xCB, 0x3F)
    def srl_mem_hl(self): self.emit(0xCB, 0x3E)
    def rl_a(self):      self.emit(0xCB, 0x17)
    def rl_d(self):      self.emit(0xCB, 0x12)
    def rl_e(self):      self.emit(0xCB, 0x13)
    def rl_mem_hl(self): self.emit(0xCB, 0x16)
    def rr_a(self):      self.emit(0xCB, 0x1F)
    def rr_mem_hl(self): self.emit(0xCB, 0x1E)
    def bit_7_a(self):   self.emit(0xCB, 0x7F)
    def bit_6_mem_hl(self): self.emit(0xCB, 0x76)
    def bit_7_mem_hl(self): self.emit(0xCB, 0x7E)
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
    def sbc_a_d(self):   self.emit(0x9A)
    def sbc_a_e(self):   self.emit(0x9B)
    def sbc_a_h(self):   self.emit(0x9C)
    def adc_a_d(self):   self.emit(0x8A)
    def adc_a_e(self):   self.emit(0x8B)
    def adc_a_h(self):   self.emit(0x8C)
    def and_a(self):     self.emit(0xA7)
    def and_b(self):     self.emit(0xA0)
    def and_c(self):     self.emit(0xA1)
    def and_d(self):     self.emit(0xA2)
    def and_e(self):     self.emit(0xA3)
    def and_h(self):     self.emit(0xA4)
    def and_l(self):     self.emit(0xA5)
    def and_mem_hl(self): self.emit(0xA6)
    def xor_a(self):     self.emit(0xAF)
    def xor_b(self):     self.emit(0xA8)
    def xor_c(self):     self.emit(0xA9)
    def xor_h(self):     self.emit(0xAC)
    def xor_l(self):     self.emit(0xAD)
    def xor_mem_hl(self): self.emit(0xAE)
    def or_a(self):      self.emit(0xB7)
    def or_b(self):      self.emit(0xB0)
    def or_c(self):      self.emit(0xB1)
    def or_h(self):      self.emit(0xB4)
    def or_l(self):      self.emit(0xB5)
    def or_d(self):      self.emit(0xB2)
    def or_mem_hl(self): self.emit(0xB6)
    def cp_b(self):      self.emit(0xB8)
    def cp_c(self):      self.emit(0xB9)
    def cp_h(self):      self.emit(0xBC)
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
    def ld_sp_hl(self):     self.emit(0xF9)

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

    def jp_p_label(self, label: str):
        """JP P, nn - Jump if positive (sign flag clear)"""
        self.emit(0xF2, 0x00, 0x00)
        self.fixups.append((len(self.code) - 2, label, 'abs'))

    def jp_m_label(self, label: str):
        """JP M, nn - Jump if minus (sign flag set)"""
        self.emit(0xFA, 0x00, 0x00)
        self.fixups.append((len(self.code) - 2, label, 'abs'))

    def jp_z_label(self, label: str):
        """JP Z, nn - Jump if zero (zero flag set)"""
        self.emit(0xCA, 0x00, 0x00)
        self.fixups.append((len(self.code) - 2, label, 'abs'))

    def jp_nz_label(self, label: str):
        """JP NZ, nn - Jump if not zero (zero flag clear)"""
        self.emit(0xC2, 0x00, 0x00)
        self.fixups.append((len(self.code) - 2, label, 'abs'))

    def jp_c_label(self, label: str):
        """JP C, nn - Jump if carry (carry flag set)"""
        self.emit(0xDA, 0x00, 0x00)
        self.fixups.append((len(self.code) - 2, label, 'abs'))

    def jp_nc_label(self, label: str):
        """JP NC, nn - Jump if no carry (carry flag clear)"""
        self.emit(0xD2, 0x00, 0x00)
        self.fixups.append((len(self.code) - 2, label, 'abs'))

    def jp_pe_label(self, label: str):
        """JP PE, nn - Jump if parity even (overflow set after ADC/SBC)"""
        self.emit(0xEA, 0x00, 0x00)
        self.fixups.append((len(self.code) - 2, label, 'abs'))

    def jp_po_label(self, label: str):
        """JP PO, nn - Jump if parity odd (overflow clear after ADC/SBC)"""
        self.emit(0xE2, 0x00, 0x00)
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
    # Bank select: bank = high_byte >> 5 (gives 0-7 for 8 banks)
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()               # Rotate left 3 times to get bank 0-7
    asm.rlca()
    asm.rlca()
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
    # NOTE: Only writes data byte, not handler byte.
    # Handler bytes are only needed for code (threaded at load time).
    # Data writes (variables, buffers) don't need handler bytes.
    asm.label('WRITE_6502')
    asm.ex_af_af()           # save value to A'
    # Bank select: bank = high_byte >> 5
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(BANK_PORT)
    # Address calc
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    # Write low byte (data only)
    asm.ex_af_af()           # restore value
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
    asm.rlca()
    asm.rlca()
    asm.rlca()
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

    # ----- TRAP_STUB: Stub for ROM routines we don't need -----
    asm.label('TRAP_STUB')
    asm.ret()

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

    # LDX zp ($A6)
    asm.label('H_LDX_ZP')
    asm.pop_hl()             # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_b_mem_hl()        # B = X = value
    asm.ld_l_a()             # Preserve 6502 A in L
    asm.ld_a_b()
    asm.or_a()               # Set flags for X
    asm.ld_a_l()             # Restore 6502 A
    asm.ret()

    # LDX zp,Y ($B6)
    asm.label('H_LDX_ZPY')
    asm.pop_hl()
    asm.ld_a_l()
    asm.add_a_c()            # A = base + Y
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_b_mem_hl()        # B = X = value
    asm.ld_l_a()             # Preserve 6502 A in L
    asm.ld_a_b()
    asm.or_a()
    asm.ld_a_l()             # Restore 6502 A
    asm.ret()

    # LDY zp ($A4)
    asm.label('H_LDY_ZP')
    asm.pop_hl()             # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_c_mem_hl()        # C = Y = value
    asm.ld_l_a()             # Preserve 6502 A in L
    asm.ld_a_c()
    asm.or_a()               # Set flags for Y
    asm.ld_a_l()             # Restore 6502 A
    asm.ret()

    # LDY zp,X ($B4)
    asm.label('H_LDY_ZPX')
    asm.pop_hl()
    asm.ld_a_l()
    asm.add_a_b()            # A = base + X
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_c_mem_hl()        # C = Y = value
    asm.ld_l_a()             # Preserve 6502 A in L
    asm.ld_a_c()
    asm.or_a()
    asm.ld_a_l()             # Restore 6502 A
    asm.ret()

    # LDX abs ($AE)
    asm.label('H_LDX_ABS')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = 6502 address
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)        # bank select
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    asm.ld_a_mem_de()        # A = value
    asm.ld_b_a()             # B = X = value
    asm.or_a()               # Set Z/N flags
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # LDY abs ($AC)
    asm.label('H_LDY_ABS')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = 6502 address
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)        # bank select
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    asm.ld_a_mem_de()        # A = value
    asm.ld_c_a()             # C = Y = value
    asm.or_a()               # Set Z/N flags
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # LDA abs ($AD)
    asm.label('H_LDA_ABS')
    # Inline operand extraction (can't use CALL - corrupts threading stack!)
    asm.pop_hl()             # HL = first word (low byte = addr_lo)
    asm.pop_de()             # DE = second word (low byte = addr_hi)
    asm.ld_d_e()             # D = addr_hi
    asm.ld_e_l()             # E = addr_lo, DE = 6502 address

    # Check for memory-mapped I/O at $FFF0-$FFF3
    asm.ld_a_d()
    asm.cp_n(0xFF)           # Is high byte $FF?
    asm.jr_nz_label('_lda_abs_read')  # No, normal read
    asm.ld_a_e()
    asm.cp_n(0xF0)           # Is low byte >= $F0?
    asm.jr_c_label('_lda_abs_read')   # No, normal read
    asm.cp_n(0xF4)           # Is low byte >= $F4?
    asm.jr_nc_label('_lda_abs_read')  # Yes, normal read (only $F0-$F3 are I/O)

    # Handle I/O: $FFF1=CHARIN, $FFF2=STATUS, $FFF3=PEEK
    asm.cp_n(0xF1)
    asm.jr_z_label('_lda_io_charin')
    asm.cp_n(0xF2)
    asm.jr_z_label('_lda_io_status')
    # $FFF0 or $FFF3 - return 0
    asm.xor_a()
    asm.ret()

    asm.label('_lda_io_charin')
    asm.in_a_n(0x01)         # Read character from port 1
    # No echo - BASIC does its own echoing
    asm.or_a()               # Set flags
    asm.ret()

    asm.label('_lda_io_status')
    asm.in_a_n(0x02)         # Read input status from port 2 (0x01 if ready, 0x00 if not)
    asm.or_a()               # Set flags
    asm.ret()

    # Inline READ_6502
    asm.label('_lda_abs_read')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)        # bank select
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    asm.ld_a_mem_de()        # A = value
    asm.or_a()               # set flags
    # Restore code bank before RET
    asm.ld_d_a()             # Save A in D
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ld_a_d()             # Restore A
    asm.or_a()               # Re-set flags
    asm.ret()

    # LDA abs,X ($BD)
    asm.label('H_LDA_ABSX')
    asm.pop_hl()             # L = addr_lo
    asm.pop_de()             # E = addr_hi
    asm.ld_d_e()
    asm.ld_e_l()             # DE = base addr
    # Add X (in B register) to address
    asm.ld_a_e()
    asm.add_a_b()            # A = addr_lo + X
    asm.ld_e_a()
    asm.jr_nc_label('_lda_absx_nc')
    asm.inc_d()              # Carry to high byte
    asm.label('_lda_absx_nc')
    # Now read from DE
    asm.jr_label('_lda_abs_read')

    # LDA abs,Y ($B9)
    asm.label('H_LDA_ABSY')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = base addr
    # Add Y (in C register) to address
    asm.ld_a_e()
    asm.add_a_c()            # A = addr_lo + Y
    asm.ld_e_a()
    asm.jr_nc_label('_lda_absy_nc')
    asm.inc_d()
    asm.label('_lda_absy_nc')
    asm.jr_label('_lda_abs_read')

    # LDX #imm ($A2)
    asm.label('H_LDX_IMM')
    asm.pop_hl()             # L = immediate value
    asm.ld_b_l()             # B = X = immediate
    asm.ld_l_a()             # L = 6502 A (preserve it)
    asm.ld_a_b()
    asm.or_a()               # set Z/N flags for X
    asm.ld_a_l()             # A = 6502 A (restore)
    asm.ret()

    # LDY #imm ($A0)
    asm.label('H_LDY_IMM')
    asm.pop_hl()
    asm.ld_c_l()             # C = Y = immediate
    asm.ld_l_a()             # L = 6502 A (preserve it)
    asm.ld_a_c()
    asm.or_a()               # set Z/N flags for Y
    asm.ld_a_l()             # A = 6502 A (restore)
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

    # STA zp,X ($95)
    asm.label('H_STA_ZPX')
    asm.ex_af_af()           # Save A to A'
    asm.pop_hl()             # L = zp base
    asm.ld_a_l()
    asm.add_a_b()            # A = zp + X
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ex_af_af()           # Restore A
    asm.ld_mem_hl_a()        # Store A to ZP+X
    # Clear ZP sync
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()
    asm.ret()

    # STA abs ($8D)
    asm.label('H_STA_ABS')
    asm.ex_af_af()           # save 6502 A to A'
    # Inline operand extraction
    asm.pop_hl()             # HL = first word (low byte = addr_lo)
    asm.pop_de()             # DE = second word (low byte = addr_hi)
    asm.ld_d_e()             # D = addr_hi
    asm.ld_e_l()             # E = addr_lo, DE = 6502 address

    # Check for memory-mapped I/O at $FFF0 (character output)
    asm.ld_a_d()
    asm.cp_n(0xFF)           # Is high byte $FF?
    asm.jr_nz_label('_sta_abs_write')  # No, normal write
    asm.ld_a_e()
    asm.cp_n(0xF0)           # Is low byte $F0? (CHAROUT)
    asm.jr_nz_label('_sta_abs_write')  # No, normal write
    # Output character via port 1
    asm.ex_af_af()           # A = 6502 A (character to output)
    asm.out_n_a(0x01)        # Output to console
    asm.ret()

    # Inline WRITE_6502 (data only - no handler byte for data writes)
    asm.label('_sta_abs_write')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)        # bank select
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    asm.ex_af_af()           # A = value to store
    asm.ld_l_a()             # L = 6502 A (preserve it!)
    asm.ld_mem_de_a()        # write data byte only
    # Restore code bank before RET
    asm.ld_a_l()             # A = 6502 A (restore from L)
    asm.ex_af_af()           # Save 6502 A to A'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # STA abs,X ($9D)
    asm.label('H_STA_ABSX')
    asm.ex_af_af()           # save 6502 A
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = base addr
    # Add X
    asm.ld_a_e()
    asm.add_a_b()
    asm.ld_e_a()
    asm.jr_nc_label('_sta_absx_nc')
    asm.inc_d()
    asm.label('_sta_absx_nc')
    asm.jr_label('_sta_abs_write')

    # STA abs,Y ($99)
    asm.label('H_STA_ABSY')
    asm.ex_af_af()
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_sta_absy_nc')
    asm.inc_d()
    asm.label('_sta_absy_nc')
    asm.jr_label('_sta_abs_write')

    # STX abs ($8E)
    asm.label('H_STX_ABS')
    asm.ex_af_af()           # Save 6502 A
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = 6502 address
    # Bank select and address calc
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    asm.ld_a_b()             # A = X value
    asm.ld_mem_de_a()        # Write data byte only
    # Restore code bank before RET
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # STY abs ($8C)
    asm.label('H_STY_ABS')
    asm.ex_af_af()           # Save 6502 A
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = 6502 address
    # Bank select and address calc
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    asm.ld_a_c()             # A = Y value
    asm.ld_mem_de_a()        # Write data byte only
    # Restore code bank before RET
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # ===== INDIRECT ADDRESSING =====

    # LDA (zp),Y ($B1) - Post-indexed indirect
    # Address at (ZP) + Y -> A
    asm.label('H_LDA_INDY')
    asm.pop_hl()             # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)  # HL = $80xx (ZP address)
    # Read 16-bit address from ZP
    asm.ld_e_mem_hl()        # E = low byte of pointer
    asm.inc_l()              # Next ZP byte (wraps within ZP)
    asm.ld_d_mem_hl()        # D = high byte of pointer
    # Add Y to address
    asm.ld_a_e()
    asm.add_a_c()            # A = ptr_lo + Y
    asm.ld_e_a()
    asm.jr_nc_label('_lda_indy_nc')
    asm.inc_d()              # Carry to high byte
    asm.label('_lda_indy_nc')
    # Now DE = final 6502 address, inline READ_6502
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.or_a()
    # Restore code bank before RET
    asm.ld_d_a()             # Save A in D
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ld_a_d()             # Restore A
    asm.or_a()               # Re-set flags
    asm.ret()

    # STA (zp),Y ($91) - Post-indexed indirect
    asm.label('H_STA_INDY')
    asm.ex_af_af()           # Save 6502 A
    asm.pop_hl()             # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    # Read 16-bit address from ZP
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    # Add Y
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_sta_indy_nc')
    asm.inc_d()
    asm.label('_sta_indy_nc')
    # Inline WRITE_6502 (data only)
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_af_af()           # A = 6502 value
    asm.ld_l_a()             # L = 6502 A (preserve it!)
    asm.ld_mem_de_a()        # Write data byte only
    # Restore code bank and 6502 A
    asm.ld_a_l()             # A = 6502 A (restore from L)
    asm.ex_af_af()           # Save 6502 A to A'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # LDA (zp,X) ($A1) - Pre-indexed indirect
    # Address at (ZP + X) -> A
    asm.label('H_LDA_INDX')
    asm.pop_hl()             # L = zp addr
    # Add X to ZP address (wraps within page)
    asm.ld_a_l()
    asm.add_a_b()            # A = zp + X
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    # Read 16-bit address from ZP+X
    asm.ld_e_mem_hl()
    asm.inc_l()              # Wraps within ZP
    asm.ld_d_mem_hl()
    # DE = target address, inline READ_6502
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.or_a()
    asm.ret()

    # STA (zp,X) ($81) - Pre-indexed indirect
    asm.label('H_STA_INDX')
    asm.ex_af_af()           # Save 6502 A
    asm.pop_hl()             # L = zp addr
    # Add X
    asm.ld_a_l()
    asm.add_a_b()
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    # Read address
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    # Inline WRITE_6502 (data only)
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_af_af()           # A = 6502 value
    asm.ld_l_a()             # L = 6502 A (preserve it!)
    asm.ld_mem_de_a()        # Write data byte only
    # Restore code bank and 6502 A
    asm.ld_a_l()             # A = 6502 A (restore from L)
    asm.ex_af_af()           # Save 6502 A to A'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore 6502 A
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

    # STX zp,Y ($96)
    asm.label('H_STX_ZPY')
    asm.pop_hl()             # L = zp base
    asm.ex_af_af()
    asm.ld_a_l()
    asm.add_a_c()            # A = zp + Y
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_mem_hl_b()        # Store X
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

    # STY zp,X ($94)
    asm.label('H_STY_ZPX')
    asm.pop_hl()             # L = zp base
    asm.ex_af_af()
    asm.ld_a_l()
    asm.add_a_b()            # A = zp + X
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_mem_hl_c()        # Store Y
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

    # TXS ($9A) - Transfer X to Stack Pointer (no flags)
    asm.label('H_TXS')
    asm.ld_l_a()             # L = 6502 A (preserve it)
    asm.ld_a_b()             # A = X
    asm.ld_mem_nn_a(REG_S_ADDR)  # S = X
    asm.ld_a_l()             # A = 6502 A (restore)
    asm.ret()

    # TSX ($BA) - Transfer Stack Pointer to X (updates N,Z)
    asm.label('H_TSX')
    asm.ld_l_a()             # L = 6502 A (preserve it)
    asm.ld_a_mem_nn(REG_S_ADDR)  # A = S
    asm.ld_b_a()             # X = S
    asm.or_a()               # Update N,Z flags based on X
    asm.ld_a_l()             # A = 6502 A (restore)
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

    # INC abs ($EE)
    asm.label('H_INC_ABS')
    asm.ex_af_af()           # Save 6502 A
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = 6502 address
    # Bank select and address calc
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    asm.ld_a_mem_de()        # A = value
    asm.inc_a()              # Increment
    asm.ld_mem_de_a()        # Write data byte only
    asm.or_a()               # Set Z/N flags
    # Restore code bank before RET
    asm.ld_h_a()             # Save result in H
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ld_a_h()             # Restore result to set flags
    asm.or_a()               # Re-set Z/N flags
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # DEC abs ($CE)
    asm.label('H_DEC_ABS')
    asm.ex_af_af()           # Save 6502 A
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = 6502 address
    # Bank select and address calc
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    asm.ld_a_mem_de()        # A = value
    asm.dec_a()              # Decrement
    asm.ld_mem_de_a()        # Write data byte only
    asm.or_a()               # Set Z/N flags
    # Restore code bank before RET
    asm.ld_h_a()             # Save result in H
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ld_a_h()             # Restore result to set flags
    asm.or_a()               # Re-set Z/N flags
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # ===== ARITHMETIC =====

    # ADC #imm ($69)
    asm.label('H_ADC_IMM')
    asm.pop_hl()                     # L = immediate value
    # Check decimal mode
    asm.ex_af_af()                   # Save A,F to A',F'
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.or_a()
    asm.jr_nz_label('_adc_imm_dec')
    asm.ex_af_af()                   # Restore A,F
    asm.adc_a_l()                    # Binary add: A = A + L + C
    asm.ret()
    asm.label('_adc_imm_dec')
    asm.ex_af_af()                   # Restore A,F
    asm.adc_a_l()                    # Binary add first
    asm.daa()                        # Decimal adjust
    asm.ret()

    # ADC zp ($65)
    asm.label('H_ADC_ZP')
    asm.pop_hl()                     # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    # Check decimal mode
    asm.ex_af_af()
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.or_a()
    asm.jr_nz_label('_adc_zp_dec')
    asm.ex_af_af()
    asm.adc_a_mem_hl()               # Binary add
    asm.ret()
    asm.label('_adc_zp_dec')
    asm.ex_af_af()
    asm.adc_a_mem_hl()
    asm.daa()
    asm.ret()

    # ADC zp,X ($75)
    asm.label('H_ADC_ZPX')
    asm.pop_hl()                     # L = zp addr
    asm.ex_af_af()                   # Save A,F
    asm.ld_a_mem_nn(D_FLAG_ADDR)     # Check decimal mode
    asm.ld_d_a()                     # D = decimal flag (preserved)
    asm.ld_a_l()
    asm.add_a_b()                    # Add X (wraps within ZP)
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_a_mem_hl()                # A = memory value
    asm.ld_h_a()                     # H = memory value
    asm.ex_af_af()                   # Restore A,F
    asm.adc_a_h()                    # A = A + H + C
    # Check decimal flag (D preserved from earlier)
    asm.inc_d()
    asm.dec_d()                      # Set Z flag based on D, preserves CF
    asm.jr_z_label('_adc_zpx_done')
    asm.daa()
    asm.label('_adc_zpx_done')
    asm.ret()

    # ADC abs ($6D)
    asm.label('H_ADC_ABS')
    # Inline operand extraction
    asm.pop_hl()             # L = addr_lo
    asm.pop_de()             # E = addr_hi
    asm.ld_d_e()
    asm.ld_e_l()             # DE = 6502 address
    # Inline READ_6502 (but save 6502 A first)
    asm.ex_af_af()           # Save A to A'
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)        # bank select
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    asm.ld_a_mem_de()        # A = memory value
    asm.ld_h_a()             # H = memory value
    # Check decimal mode while we have A free
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag (preserved)
    asm.ex_af_af()           # Restore 6502 A
    asm.adc_a_h()            # A = A + H + C
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()              # Set Z based on L, preserves CF
    asm.jr_z_label('_adc_abs_nodaa')
    asm.daa()
    asm.label('_adc_abs_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ADC abs,X ($7D)
    asm.label('H_ADC_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = base addr
    # Add X
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_b()
    asm.ld_e_a()
    asm.jr_nc_label('_adc_absx_nc')
    asm.inc_d()
    asm.label('_adc_absx_nc')
    # Read and ADC
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    # Check decimal mode
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag
    asm.ex_af_af()           # Restore A
    asm.adc_a_h()
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()
    asm.jr_z_label('_adc_absx_nodaa')
    asm.daa()
    asm.label('_adc_absx_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ADC abs,Y ($79)
    asm.label('H_ADC_ABSY')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()             # DE = base addr
    # Add Y
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_adc_absy_nc')
    asm.inc_d()
    asm.label('_adc_absy_nc')
    # Read and ADC
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    # Check decimal mode
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag
    asm.ex_af_af()           # Restore A
    asm.adc_a_h()
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()
    asm.jr_z_label('_adc_absy_nodaa')
    asm.daa()
    asm.label('_adc_absy_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ADC (zp),Y ($71)
    asm.label('H_ADC_INDY')
    asm.pop_hl()             # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    # Read 16-bit pointer
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    # Add Y
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_adc_indy_nc')
    asm.inc_d()
    asm.label('_adc_indy_nc')
    # Read and ADC
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    # Check decimal mode
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag
    asm.ex_af_af()           # Restore A
    asm.adc_a_h()
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()
    asm.jr_z_label('_adc_indy_nodaa')
    asm.daa()
    asm.label('_adc_indy_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # SBC #imm ($E9)
    asm.label('H_SBC_IMM')
    asm.pop_hl()                     # L = immediate value
    # Check decimal mode
    asm.ex_af_af()                   # Save A,F to A',F'
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.or_a()
    asm.jr_nz_label('_sbc_imm_dec')
    asm.ex_af_af()                   # Restore A,F
    asm.sbc_a_l()                    # Binary subtract
    asm.ret()
    asm.label('_sbc_imm_dec')
    asm.ex_af_af()                   # Restore A,F
    asm.sbc_a_l()                    # Binary subtract first
    asm.daa()                        # Decimal adjust
    asm.ret()

    # SBC zp ($E5)
    asm.label('H_SBC_ZP')
    asm.pop_hl()                     # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    # Check decimal mode
    asm.ex_af_af()
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.or_a()
    asm.jr_nz_label('_sbc_zp_dec')
    asm.ex_af_af()
    asm.sbc_a_mem_hl()               # Binary subtract
    asm.ret()
    asm.label('_sbc_zp_dec')
    asm.ex_af_af()
    asm.sbc_a_mem_hl()
    asm.daa()
    asm.ret()

    # SBC zp,X ($F5)
    asm.label('H_SBC_ZPX')
    asm.pop_hl()                     # L = zp addr
    asm.ex_af_af()                   # Save A,F
    asm.ld_a_mem_nn(D_FLAG_ADDR)     # Check decimal mode
    asm.ld_d_a()                     # D = decimal flag (preserved)
    asm.ld_a_l()
    asm.add_a_b()                    # Add X (wraps within ZP)
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_a_mem_hl()                # A = memory value
    asm.ld_h_a()                     # H = memory value
    asm.ex_af_af()                   # Restore A,F
    asm.sbc_a_h()                    # A = A - H - !C
    # Check decimal flag (D preserved from earlier)
    asm.inc_d()
    asm.dec_d()                      # Set Z flag based on D, preserves CF
    asm.jr_z_label('_sbc_zpx_done')
    asm.daa()
    asm.label('_sbc_zpx_done')
    asm.ret()

    # SBC abs ($ED)
    asm.label('H_SBC_ABS')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()           # Save A
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()             # H = memory value
    # Check decimal mode
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag
    asm.ex_af_af()           # Restore A
    asm.sbc_a_h()            # A = A - H - !C
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()
    asm.jr_z_label('_sbc_abs_nodaa')
    asm.daa()
    asm.label('_sbc_abs_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # SBC abs,Y ($F9)
    asm.label('H_SBC_ABSY')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_sbc_absy_nc')
    asm.inc_d()
    asm.label('_sbc_absy_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    # Check decimal mode
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag
    asm.ex_af_af()
    asm.sbc_a_h()
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()
    asm.jr_z_label('_sbc_absy_nodaa')
    asm.daa()
    asm.label('_sbc_absy_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # SBC (zp),Y ($F1)
    asm.label('H_SBC_INDY')
    asm.pop_hl()             # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    # Read 16-bit pointer
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    # Add Y
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_sbc_indy_nc')
    asm.inc_d()
    asm.label('_sbc_indy_nc')
    # Read and SBC
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    # Check decimal mode
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag
    asm.ex_af_af()           # Restore A
    asm.sbc_a_h()
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()
    asm.jr_z_label('_sbc_indy_nodaa')
    asm.daa()
    asm.label('_sbc_indy_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # SBC abs,X ($FD)
    asm.label('H_SBC_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_b()            # Add X
    asm.ld_e_a()
    asm.jr_nc_label('_sbc_absx_nc')
    asm.inc_d()
    asm.label('_sbc_absx_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    # Check decimal mode
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag
    asm.ex_af_af()           # Restore A
    asm.sbc_a_h()
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()
    asm.jr_z_label('_sbc_absx_nodaa')
    asm.daa()
    asm.label('_sbc_absx_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ADC (zp,X) ($61)
    asm.label('H_ADC_INDX')
    asm.pop_hl()             # L = zp addr
    asm.ex_af_af()           # Save A
    asm.ld_a_l()
    asm.add_a_b()            # A = zp + X
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    # Read 16-bit pointer
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()        # DE = target address
    # Read from target and ADC
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    # Check decimal mode
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag
    asm.ex_af_af()
    asm.adc_a_h()
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()
    asm.jr_z_label('_adc_indx_nodaa')
    asm.daa()
    asm.label('_adc_indx_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # SBC (zp,X) ($E1)
    asm.label('H_SBC_INDX')
    asm.pop_hl()
    asm.ex_af_af()
    asm.ld_a_l()
    asm.add_a_b()
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    # Check decimal mode
    asm.ld_a_mem_nn(D_FLAG_ADDR)
    asm.ld_l_a()             # L = decimal flag
    asm.ex_af_af()
    asm.sbc_a_h()
    # Apply DAA if decimal mode
    asm.inc_l()
    asm.dec_l()
    asm.jr_z_label('_sbc_indx_nodaa')
    asm.daa()
    asm.label('_sbc_indx_nodaa')
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ===== LOGICAL =====

    # AND #imm ($29)
    asm.label('H_AND_IMM')
    asm.pop_hl()
    asm.and_l()
    asm.ret()

    # AND zp ($25)
    asm.label('H_AND_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.and_mem_hl()
    asm.ret()

    # AND zp,X ($35)
    asm.label('H_AND_ZPX')
    asm.pop_hl()
    asm.ex_af_af()           # Save A
    asm.ld_a_l()
    asm.add_a_b()            # Add X (wraps within ZP)
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_a_mem_hl()
    asm.ld_h_a()             # H = memory value
    asm.ex_af_af()           # Restore A
    asm.and_h()
    asm.ret()

    # AND abs ($2D)
    asm.label('H_AND_ABS')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.and_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # AND abs,Y ($39)
    asm.label('H_AND_ABSY')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_and_absy_nc')
    asm.inc_d()
    asm.label('_and_absy_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.and_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # AND abs,X ($3D)
    asm.label('H_AND_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_b()            # Add X (not Y!)
    asm.ld_e_a()
    asm.jr_nc_label('_and_absx_nc')
    asm.inc_d()
    asm.label('_and_absx_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.and_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # AND (zp,X) ($21)
    asm.label('H_AND_INDX')
    asm.pop_hl()
    asm.ex_af_af()
    asm.ld_a_l()
    asm.add_a_b()
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.and_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # AND (zp),Y ($31)
    asm.label('H_AND_INDY')
    asm.pop_hl()             # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_and_indy_nc')
    asm.inc_d()
    asm.label('_and_indy_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()           # Restore A
    asm.and_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ORA (zp,X) ($01)
    asm.label('H_ORA_INDX')
    asm.pop_hl()
    asm.ex_af_af()
    asm.ld_a_l()
    asm.add_a_b()
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.or_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ORA (zp),Y ($11)
    asm.label('H_ORA_INDY')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_ora_indy_nc')
    asm.inc_d()
    asm.label('_ora_indy_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.or_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # EOR (zp,X) ($41)
    asm.label('H_EOR_INDX')
    asm.pop_hl()
    asm.ex_af_af()
    asm.ld_a_l()
    asm.add_a_b()
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.xor_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # EOR (zp),Y ($51)
    asm.label('H_EOR_INDY')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_eor_indy_nc')
    asm.inc_d()
    asm.label('_eor_indy_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.xor_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ORA #imm ($09)
    asm.label('H_ORA_IMM')
    asm.pop_hl()
    asm.or_l()
    asm.ret()

    # ORA zp ($05)
    asm.label('H_ORA_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.or_mem_hl()
    asm.ret()

    # ORA zp,X ($15)
    asm.label('H_ORA_ZPX')
    asm.pop_hl()
    asm.ex_af_af()           # Save A
    asm.ld_a_l()
    asm.add_a_b()            # Add X (wraps within ZP)
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_a_mem_hl()
    asm.ld_h_a()             # H = memory value
    asm.ex_af_af()           # Restore A
    asm.or_h()
    asm.ret()

    # ORA abs ($0D)
    asm.label('H_ORA_ABS')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.or_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ORA abs,Y ($19)
    asm.label('H_ORA_ABSY')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_ora_absy_nc')
    asm.inc_d()
    asm.label('_ora_absy_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.or_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ORA abs,X ($1D)
    asm.label('H_ORA_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_b()
    asm.ld_e_a()
    asm.jr_nc_label('_ora_absx_nc')
    asm.inc_d()
    asm.label('_ora_absx_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.or_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # EOR #imm ($49)
    asm.label('H_EOR_IMM')
    asm.pop_hl()
    asm.xor_l()
    asm.ret()

    # EOR zp ($45)
    asm.label('H_EOR_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.xor_mem_hl()
    asm.ret()

    # EOR zp,X ($55)
    asm.label('H_EOR_ZPX')
    asm.pop_hl()
    asm.ex_af_af()           # Save A
    asm.ld_a_l()
    asm.add_a_b()            # Add X (wraps within ZP)
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_a_mem_hl()
    asm.ld_h_a()             # H = memory value
    asm.ex_af_af()           # Restore A
    asm.xor_h()
    asm.ret()

    # EOR abs ($4D)
    asm.label('H_EOR_ABS')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.xor_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # EOR abs,Y ($59)
    asm.label('H_EOR_ABSY')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_eor_absy_nc')
    asm.inc_d()
    asm.label('_eor_absy_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.xor_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # EOR abs,X ($5D)
    asm.label('H_EOR_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_b()
    asm.ld_e_a()
    asm.jr_nc_label('_eor_absx_nc')
    asm.inc_d()
    asm.label('_eor_absx_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.xor_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ===== SHIFT AND ROTATE =====

    # ASL A ($0A) - Arithmetic Shift Left accumulator
    asm.label('H_ASL_A')
    asm.sla_a()              # Z80 SLA is same as 6502 ASL
    asm.ret()

    # ASL zp ($06)
    asm.label('H_ASL_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ex_af_af()           # Save 6502 A
    asm.sla_mem_hl()         # Shift memory
    asm.ex_af_af()           # Restore 6502 A (flags from SLA are correct)
    # Clear ZP sync
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()
    asm.ret()

    # ASL zp,X ($16)
    asm.label('H_ASL_ZPX')
    asm.pop_hl()
    asm.ex_af_af()           # Save 6502 A
    asm.ld_a_l()
    asm.add_a_b()            # Add X (wraps within ZP)
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.sla_mem_hl()         # Shift memory
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # ASL abs ($0E)
    asm.label('H_ASL_ABS')
    asm.ex_af_af()
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    # Bank select and address calc
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_de_hl()           # HL = Z80 addr
    asm.sla_mem_hl()         # Shift in place
    # Update handler byte
    asm.ld_a_mem_hl()
    asm.and_n(0x0F)
    asm.or_n(0x70)
    asm.inc_hl()
    asm.ld_mem_hl_a()
    asm.ex_af_af()
    asm.ret()

    # LSR A ($4A) - Logical Shift Right accumulator
    asm.label('H_LSR_A')
    asm.srl_a()              # Z80 SRL is same as 6502 LSR
    asm.ret()

    # LSR zp ($46)
    asm.label('H_LSR_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ex_af_af()
    asm.srl_mem_hl()
    asm.ex_af_af()
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()
    asm.ret()

    # LSR zp,X ($56)
    asm.label('H_LSR_ZPX')
    asm.pop_hl()
    asm.ex_af_af()           # Save 6502 A
    asm.ld_a_l()
    asm.add_a_b()            # Add X (wraps within ZP)
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.srl_mem_hl()         # Shift memory
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # LSR abs ($4E)
    asm.label('H_LSR_ABS')
    asm.ex_af_af()
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_de_hl()
    asm.srl_mem_hl()
    asm.ld_a_mem_hl()
    asm.and_n(0x0F)
    asm.or_n(0x70)
    asm.inc_hl()
    asm.ld_mem_hl_a()
    asm.ex_af_af()
    asm.ret()

    # ROL A ($2A) - Rotate Left through carry
    asm.label('H_ROL_A')
    asm.rl_a()               # Z80 RL is same as 6502 ROL
    asm.ret()

    # ROL zp ($26)
    asm.label('H_ROL_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ex_af_af()
    asm.rl_mem_hl()
    asm.ex_af_af()
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()
    asm.ret()

    # ROL zp,X ($36)
    asm.label('H_ROL_ZPX')
    asm.pop_hl()
    asm.ex_af_af()           # Save 6502 A
    asm.ld_a_l()
    asm.add_a_b()            # Add X (wraps within ZP)
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.rl_mem_hl()          # Rotate memory
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # ROL abs ($2E)
    asm.label('H_ROL_ABS')
    asm.ex_af_af()
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_de_hl()
    asm.rl_mem_hl()
    asm.ld_a_mem_hl()
    asm.and_n(0x0F)
    asm.or_n(0x70)
    asm.inc_hl()
    asm.ld_mem_hl_a()
    asm.ex_af_af()
    asm.ret()

    # ROR A ($6A) - Rotate Right through carry
    asm.label('H_ROR_A')
    asm.rr_a()               # Z80 RR is same as 6502 ROR
    asm.ret()

    # ROR zp ($66)
    asm.label('H_ROR_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ex_af_af()
    asm.rr_mem_hl()
    asm.ex_af_af()
    asm.ex_af_af()
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()
    asm.ret()

    # ROR zp,X ($76)
    asm.label('H_ROR_ZPX')
    asm.pop_hl()
    asm.ex_af_af()           # Save 6502 A
    asm.ld_a_l()
    asm.add_a_b()            # Add X (wraps within ZP)
    asm.ld_l_a()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.rr_mem_hl()          # Rotate memory
    asm.xor_a()
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

    # ROR abs ($6E)
    asm.label('H_ROR_ABS')
    asm.ex_af_af()
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_de_hl()
    asm.rr_mem_hl()
    asm.ld_a_mem_hl()
    asm.and_n(0x0F)
    asm.or_n(0x70)
    asm.inc_hl()
    asm.ld_mem_hl_a()
    asm.ex_af_af()
    asm.ret()

    # ASL abs,X ($1E)
    asm.label('H_ASL_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_b()            # Add X to low byte
    asm.ld_e_a()
    asm.jr_nc_label('_asl_absx_nc')
    asm.inc_d()
    asm.label('_asl_absx_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_de_hl()
    asm.sla_mem_hl()         # Shift in place
    asm.ld_a_mem_hl()
    asm.and_n(0x0F)
    asm.or_n(0x70)
    asm.inc_hl()
    asm.ld_mem_hl_a()
    # Restore code bank before RET
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()
    asm.ret()

    # LSR abs,X ($5E)
    asm.label('H_LSR_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_b()            # Add X to low byte
    asm.ld_e_a()
    asm.jr_nc_label('_lsr_absx_nc')
    asm.inc_d()
    asm.label('_lsr_absx_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_de_hl()
    asm.srl_mem_hl()         # Shift in place
    asm.ld_a_mem_hl()
    asm.and_n(0x0F)
    asm.or_n(0x70)
    asm.inc_hl()
    asm.ld_mem_hl_a()
    # Restore code bank before RET
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()
    asm.ret()

    # ROL abs,X ($3E)
    asm.label('H_ROL_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_b()            # Add X to low byte
    asm.ld_e_a()
    asm.jr_nc_label('_rol_absx_nc')
    asm.inc_d()
    asm.label('_rol_absx_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_de_hl()
    asm.rl_mem_hl()          # Rotate in place
    asm.ld_a_mem_hl()
    asm.and_n(0x0F)
    asm.or_n(0x70)
    asm.inc_hl()
    asm.ld_mem_hl_a()
    # Restore code bank before RET
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()
    asm.ret()

    # ROR abs,X ($7E)
    asm.label('H_ROR_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()           # Save A
    asm.ld_a_e()
    asm.add_a_b()            # Add X to low byte
    asm.ld_e_a()
    asm.jr_nc_label('_ror_absx_nc')
    asm.inc_d()
    asm.label('_ror_absx_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ex_de_hl()
    asm.rr_mem_hl()          # Rotate in place
    asm.ld_a_mem_hl()
    asm.and_n(0x0F)
    asm.or_n(0x70)
    asm.inc_hl()
    asm.ld_mem_hl_a()
    # Restore code bank before RET
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()
    asm.ret()

    # ===== BIT TEST =====

    # BIT zp ($24) - test bits with memory
    # Sets Z from A AND M (A is unchanged, LD doesn't affect flags)
    asm.label('H_BIT_ZP')
    asm.pop_hl()             # L = zp address
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_e_a()             # E = save original A (no flag change)
    asm.ld_a_mem_hl()        # A = memory value (no flag change)
    asm.and_e()              # A AND original_A - sets Z correctly
    asm.ld_a_e()             # Restore A (no flag change, Z preserved!)
    asm.ret()

    # BIT abs ($2C)
    asm.label('H_BIT_ABS')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    # Save original A
    asm.ld_l_a()             # L = original A (no flag change)
    # Bank select and address calc
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()        # A = memory value (no flag change)
    asm.and_l()              # A AND original_A - sets Z correctly
    asm.ld_a_l()             # Restore A (no flag change, Z preserved!)
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # ===== COMPARE =====

    # CMP #imm ($C9)
    asm.label('H_CMP_IMM')
    asm.pop_hl()
    asm.cp_l()               # compare A with imm
    asm.ret()

    # CMP zp ($C5)
    asm.label('H_CMP_ZP')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.cp_mem_hl()
    asm.ret()

    # CMP abs ($CD)
    asm.label('H_CMP_ABS')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.cp_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # CMP abs,X ($DD)
    asm.label('H_CMP_ABSX')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_b()
    asm.ld_e_a()
    asm.jr_nc_label('_cmp_absx_nc')
    asm.inc_d()
    asm.label('_cmp_absx_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.cp_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # CMP abs,Y ($D9)
    asm.label('H_CMP_ABSY')
    asm.pop_hl()
    asm.pop_de()
    asm.ld_d_e()
    asm.ld_e_l()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_cmp_absy_nc')
    asm.inc_d()
    asm.label('_cmp_absy_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.cp_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # CMP (zp),Y ($D1)
    asm.label('H_CMP_INDY')
    asm.pop_hl()
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_e_mem_hl()
    asm.inc_l()
    asm.ld_d_mem_hl()
    asm.ex_af_af()
    asm.ld_a_e()
    asm.add_a_c()
    asm.ld_e_a()
    asm.jr_nc_label('_cmp_indy_nc')
    asm.inc_d()
    asm.label('_cmp_indy_nc')
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()
    asm.ld_a_mem_de()
    asm.ld_h_a()
    asm.ex_af_af()
    asm.cp_h()
    # Restore code bank before RET (preserve flags with EX AF,AF')
    asm.ex_af_af()           # Save A and flags to A'F'
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.out_n_a(0x00)
    asm.ex_af_af()           # Restore A and flags
    asm.ret()

    # CPX #imm ($E0)
    asm.label('H_CPX_IMM')
    asm.pop_hl()
    asm.ld_a_b()             # A = X
    asm.cp_l()
    asm.ret()

    # CPX zp ($E4)
    asm.label('H_CPX_ZP')
    asm.pop_hl()             # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_l_mem_hl()        # L = value from ZP
    asm.ld_a_b()             # A = X
    asm.cp_l()               # Compare X - value
    asm.ret()

    # NOP #imm ($82) - Unofficial opcode, 2-byte NOP
    asm.label('H_NOP_IMM')
    asm.pop_hl()             # Skip the immediate byte
    asm.ret()

    # CPY #imm ($C0)
    asm.label('H_CPY_IMM')
    asm.pop_hl()
    asm.ld_a_c()             # A = Y
    asm.cp_l()
    asm.ret()

    # CPY zp ($C4)
    asm.label('H_CPY_ZP')
    asm.pop_hl()             # L = zp addr
    asm.ld_h_n(ZP_LINEAR_BASE >> 8)
    asm.ld_l_mem_hl()        # L = value from ZP
    asm.ld_a_c()             # A = Y
    asm.cp_l()               # Compare Y - value
    asm.ret()

    # ===== BRANCHES =====
    # For branches, we need to handle the relative offset
    # The offset is in the threaded stream as a handler-encoded byte
    # Offset is relative to instruction AFTER branch, SP already points there after POP
    # We need: SP = SP + (signed_offset * 2)

    # Helper: emit branch-taken code (call after conditional JR that skips branch)
    # NOTE: 6502 A must be saved in A' before calling this!
    def emit_branch_taken(asm):
        """Emit code to take branch: add signed offset*2 to SP"""
        asm.ld_a_l()             # A = offset (L from popped value)
        asm.ld_e_a()             # E = offset
        asm.add_a_a()            # Sign bit into carry
        asm.sbc_a_a()            # A = $00 (positive) or $FF (negative)
        asm.ld_d_a()             # DE = sign-extended offset
        asm.sla_e()              # *2 low byte
        asm.rl_d()               # *2 high byte (DE = offset * 2)
        asm.ex_de_hl()           # HL = offset*2
        asm.add_hl_sp()          # HL = SP + offset*2
        asm.ld_sp_hl()           # SP = new PC
        asm.ex_af_af()           # Restore 6502 A from A'

    # BEQ ($F0) - Branch if Equal (Z=1)
    asm.label('H_BEQ')
    asm.ex_af_af()           # Save 6502 A to A' (also saves flags)
    asm.pop_hl()             # L = offset
    asm.ex_af_af()           # Restore flags for branch test
    asm.jr_nz_label('_beq_no')
    asm.ex_af_af()           # Save 6502 A again before branch calc
    emit_branch_taken(asm)
    asm.label('_beq_no')
    asm.ret()

    # BNE ($D0) - Branch if Not Equal (Z=0)
    asm.label('H_BNE')
    asm.ex_af_af()           # Save 6502 A
    asm.pop_hl()
    asm.ex_af_af()           # Restore flags
    asm.jr_z_label('_bne_no')
    asm.ex_af_af()           # Save A for branch calc
    emit_branch_taken(asm)
    asm.label('_bne_no')
    asm.ret()

    # BCS ($B0) - Branch if Carry Set (6502 C=1)
    # Note: Z80 carry is OPPOSITE of 6502 for CMP
    # 6502 CMP sets C=1 for A >= M, Z80 CP sets C=1 for A < M
    # So 6502 C=1 ↔ Z80 C=0
    asm.label('H_BCS')
    asm.ex_af_af()
    asm.pop_hl()
    asm.ex_af_af()
    asm.jr_c_label('_bcs_no')    # Skip branch when Z80 C=1 (= 6502 C=0)
    asm.ex_af_af()
    emit_branch_taken(asm)
    asm.label('_bcs_no')
    asm.ret()

    # BCC ($90) - Branch if Carry Clear (6502 C=0)
    # 6502 C=0 ↔ Z80 C=1
    asm.label('H_BCC')
    asm.ex_af_af()
    asm.pop_hl()
    asm.ex_af_af()
    asm.jr_nc_label('_bcc_no')   # Skip branch when Z80 C=0 (= 6502 C=1)
    asm.ex_af_af()
    emit_branch_taken(asm)
    asm.label('_bcc_no')
    asm.ret()

    # BMI ($30) - Branch if Minus (N=1)
    # Z80 Sign flag after CMP/math ops reflects bit 7
    asm.label('H_BMI')
    asm.ex_af_af()
    asm.pop_hl()
    asm.ex_af_af()
    asm.jp_p_label('_bmi_no')    # JP P = jump if positive (sign flag clear)
    asm.ex_af_af()
    emit_branch_taken(asm)
    asm.label('_bmi_no')
    asm.ret()

    # BPL ($10) - Branch if Plus (N=0)
    asm.label('H_BPL')
    asm.ex_af_af()
    asm.pop_hl()
    asm.ex_af_af()
    asm.jp_m_label('_bpl_no')    # JP M = jump if minus (sign flag set)
    asm.ex_af_af()
    emit_branch_taken(asm)
    asm.label('_bpl_no')
    asm.ret()

    # BVS ($70) - Branch if Overflow Set (V=1)
    # Z80 P/V flag after ADC/SBC reflects signed overflow
    asm.label('H_BVS')
    asm.ex_af_af()
    asm.pop_hl()
    asm.ex_af_af()
    asm.jp_po_label('_bvs_no')   # JP PO = jump if parity odd (V=0)
    asm.ex_af_af()
    emit_branch_taken(asm)
    asm.label('_bvs_no')
    asm.ret()

    # BVC ($50) - Branch if Overflow Clear (V=0)
    asm.label('H_BVC')
    asm.ex_af_af()
    asm.pop_hl()
    asm.ex_af_af()
    asm.jp_pe_label('_bvc_no')   # JP PE = jump if parity even (V=1)
    asm.ex_af_af()
    emit_branch_taken(asm)
    asm.label('_bvc_no')
    asm.ret()

    # ===== JUMPS =====

    # JMP abs ($4C)
    asm.label('H_JMP_ABS')
    asm.ex_af_af()           # Save 6502 A to A'
    # Inline operand extraction
    asm.pop_hl()             # HL = first word (low byte = addr_lo)
    asm.pop_de()             # DE = second word (low byte = addr_hi)
    asm.ld_d_e()             # D = addr_hi
    asm.ld_e_l()             # E = addr_lo, DE = 6502 address
    # Jump to common code
    asm.label('_jmp_to_de')
    # Inline ADDR_TO_Z80
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)        # bank select
    asm.ld_mem_nn_a(CODE_BANK_ADDR)  # Save code bank
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr
    # Set SP to new address and RET to start execution
    asm.ex_de_hl()
    asm.ld_sp_hl()           # SP = target addr
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()                # RET will pop next handler

    # JMP (abs) ($6C) - Indirect jump
    asm.label('H_JMP_IND')
    asm.ex_af_af()           # Save 6502 A to A'
    # Get the operand address
    asm.pop_hl()             # L = addr_lo
    asm.pop_de()             # E = addr_hi
    asm.ld_d_e()
    asm.ld_e_l()             # DE = address that contains the target
    # Read 16-bit target from this address
    # First select bank and convert address
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)        # bank select
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr of pointer
    # Read low byte of target
    asm.ld_a_mem_de()        # A = target_lo
    asm.ld_l_a()             # L = target_lo (save it)
    # Read high byte (DE+2 in threaded code, but original 6502 addr+1)
    asm.inc_de()
    asm.inc_de()             # Skip to next byte (2 bytes per 6502 byte)
    asm.ld_a_mem_de()        # A = target_hi
    asm.ld_d_a()             # D = target_hi
    asm.ld_e_l()             # E = target_lo, DE = target address
    # Now jump to this target
    asm.jr_label('_jmp_to_de')

    # JSR abs ($20)
    asm.label('H_JSR')
    asm.ex_af_af()           # Save 6502 A to A' !!!

    # Inline operand extraction
    asm.pop_hl()             # HL = first word (low byte = addr_lo)
    asm.pop_de()             # DE = second word (low byte = addr_hi)
    asm.ld_d_e()             # D = addr_hi
    asm.ld_e_l()             # E = addr_lo, DE = 6502 target address

    # Check if target is in zero page ($00xx) - need to sync ZP first
    asm.ld_a_d()
    asm.or_a()               # Is D == 0?
    asm.jr_nz_label('_jsr_not_zp')
    # Target is in ZP! Check if ZP needs sync
    asm.push_de()            # Save target
    asm.ld_a_mem_nn(ZP_SYNCED_ADDR)
    asm.or_a()               # Is ZP synced?
    asm.jr_nz_label('_jsr_zp_synced')
    # Need to sync ZP - copy linear ZP to threaded format in bank 0
    # Save registers
    asm.push_bc()
    asm.push_hl()
    # Call SYNC_ZP inline
    asm.ld_hl_nn(ZP_LINEAR_BASE)    # source
    asm.ld_de_nn(CODE_WINDOW_BASE)  # dest ($C000)
    asm.xor_a()
    asm.out_n_a(BANK_PORT)          # Select bank 0
    # Copy 256 bytes, converting each to threaded format
    asm.ld_b_n(0)                   # 256 iterations
    asm.label('_jsr_sync_loop')
    asm.ld_a_mem_hl()               # Get byte from linear ZP
    asm.ld_mem_de_a()               # Store as low byte
    asm.and_n(0x0F)
    asm.or_n(0x70)                  # Handler high byte
    asm.inc_de()
    asm.ld_mem_de_a()               # Store handler high
    asm.inc_de()
    asm.inc_hl()
    asm.djnz_label('_jsr_sync_loop')
    # Mark ZP as synced
    asm.ld_a_n(1)
    asm.ld_mem_nn_a(ZP_SYNCED_ADDR)
    asm.pop_hl()
    asm.pop_bc()
    asm.label('_jsr_zp_synced')
    asm.pop_de()             # Restore target
    asm.ld_a_d()             # Restore A = D for next check

    asm.label('_jsr_not_zp')
    # Check for ROM trap ($FC00+ uses trap table)
    asm.cp_n(0xFC)
    asm.jr_nc_label('_jsr_trap')

    # Check for specific ROM routines in $F800-$FBFF range
    asm.cp_n(0xF8)
    asm.jr_c_label('_jsr_do')    # Below $F800, normal JSR

    # D is $F8, $F9, $FA, or $FB - check for known addresses
    asm.cp_n(0xFB)
    asm.jr_nz_label('_check_f8')

    # D=$FB, check E for specific addresses
    asm.ld_a_e()
    asm.cp_n(0xFD)               # $FBFD = VIDOUT
    asm.jp_z_label('TRAP_COUT')  # VIDOUT is same as COUT
    asm.cp_n(0xC1)               # $FBC1 = BASCALC
    asm.jp_z_label('TRAP_STUB')  # Just return, we don't need it
    asm.jr_label('_jsr_do')      # Unknown $FBxx, normal JSR

    asm.label('_check_f8')
    asm.cp_n(0xF8)
    asm.jr_nz_label('_check_f6')   # Not $F8xx, check $F6xx

    asm.ld_a_e()
    asm.cp_n(0x01)               # $F801 = PLOT
    asm.jp_z_label('TRAP_STUB')  # Just return for graphics
    asm.jr_label('_jsr_do')      # Unknown $F8xx, normal JSR

    asm.label('_check_f6')
    asm.cp_n(0xF6)
    asm.jr_nz_label('_jsr_do')   # Not $F6xx, normal JSR

    asm.ld_a_e()
    asm.cp_n(0xF0)               # $F6F0 = unknown ROM routine
    asm.jp_z_label('TRAP_STUB')  # Just return
    # Fall through to normal JSR for unknown $F6xx

    # Normal JSR - push return addr to shadow stack, then jump
    asm.label('_jsr_do')
    # Step 1: Save target address (DE) temporarily
    # Use $8F10 (outside shadow stack area $8E00-$8EFF)
    asm.ld_mem_nn_de(0x8F10)  # TEMP_ADDR = DE (target)

    # Step 2: Get current SP (this is our return address!)
    asm.ld_hl_nn(0)
    asm.add_hl_sp()          # HL = SP = return addr

    # Step 3: Push return addr AND code bank to shadow stack (3 bytes)
    asm.ld_de_mem_nn(SHADOW_STACK_PTR)  # DE = shadow ptr
    asm.dec_de()
    asm.dec_de()
    asm.dec_de()             # Make room for 3 bytes (addr + bank)
    asm.ld_mem_nn_de(SHADOW_STACK_PTR)  # Update shadow ptr
    # Store return addr (HL) at shadow stack (DE)
    asm.ex_de_hl()           # HL = shadow ptr, DE = return addr
    asm.ld_mem_hl_e()        # low byte of addr
    asm.inc_hl()
    asm.ld_mem_hl_d()        # high byte of addr
    asm.inc_hl()
    # Store current code bank
    asm.ld_a_mem_nn(CODE_BANK_ADDR)
    asm.ld_mem_hl_a()        # bank byte

    # Step 4: Restore target and convert to Z80 address
    asm.ld_de_mem_nn(0x8F10) # DE = target 6502 addr
    # Inline ADDR_TO_Z80
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)        # bank select
    asm.ld_mem_nn_a(CODE_BANK_ADDR)  # Save code bank
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 addr

    # Step 5: Jump to target
    asm.ex_de_hl()
    asm.ld_sp_hl()           # SP = target
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()                # Start executing at target

    asm.label('_jsr_trap')
    # Look up trap handler in table at $8C00
    # Index by (addr & $03FF)
    asm.ld_h_n(0x8C)
    asm.ld_a_d()
    asm.and_n(0x03)
    asm.or_n(0x8C)
    asm.ld_h_a()
    asm.ld_l_e()             # HL = $8Cxx or $8Dxx etc.
    # Read trap handler address
    asm.ld_e_mem_hl()
    asm.inc_hl()
    asm.ld_d_mem_hl()        # DE = trap handler addr
    asm.ex_de_hl()
    asm.ex_af_af()           # Restore 6502 A before trap call !!!
    asm.jp_hl()              # Jump to trap handler (will RET back to threading)

    # RTS ($60)
    asm.label('H_RTS')
    asm.ex_af_af()           # Save 6502 A and flags to A'
    # Pop return address AND code bank from shadow stack (3 bytes)
    asm.ld_hl_mem_nn(SHADOW_STACK_PTR)  # HL = shadow ptr
    # Read return addr from (HL)
    asm.ld_e_mem_hl()        # E = low byte of addr
    asm.inc_hl()
    asm.ld_d_mem_hl()        # D = high byte of addr
    asm.inc_hl()
    asm.ld_a_mem_hl()        # A = code bank
    asm.inc_hl()
    asm.ld_mem_nn_hl(SHADOW_STACK_PTR)  # Update shadow ptr (popped 3 bytes)
    # Restore code bank
    asm.out_n_a(0x00)        # Switch to return address's bank
    asm.ld_mem_nn_a(CODE_BANK_ADDR)  # Update CODE_BANK_ADDR
    # Set SP = return address and continue
    asm.ex_de_hl()           # HL = return addr
    asm.ld_sp_hl()           # SP = return addr
    asm.ex_af_af()           # Restore 6502 A and flags
    asm.ret()                # Continue execution

    # RTI ($40) - Return from Interrupt
    # Pops P, PCL, PCH from 6502 stack and jumps to address
    asm.label('H_RTI')
    asm.ex_af_af()           # Save 6502 A
    # Increment S and read P (we'll mostly ignore flags)
    asm.ld_a_mem_nn(REG_S_ADDR)
    asm.inc_a()
    asm.ld_mem_nn_a(REG_S_ADDR)
    asm.ld_l_a()
    asm.ld_h_n(STACK_LINEAR_BASE >> 8)
    asm.ld_e_mem_hl()        # E = P (processor status, mostly ignore)
    # Increment S and read PCL
    asm.ld_a_mem_nn(REG_S_ADDR)
    asm.inc_a()
    asm.ld_mem_nn_a(REG_S_ADDR)
    asm.ld_l_a()
    asm.ld_d_mem_hl()        # D = PCL
    # Increment S and read PCH
    asm.ld_a_mem_nn(REG_S_ADDR)
    asm.inc_a()
    asm.ld_mem_nn_a(REG_S_ADDR)
    asm.ld_l_a()
    asm.ld_e_mem_hl()        # E = PCH, now DE = PCH:PCL (swapped!)
    # Swap to get correct order: D=PCH, E=PCL
    asm.ld_a_d()
    asm.ld_d_e()
    asm.ld_e_a()             # Now DE = 6502 PC
    # Convert 6502 address to Z80 threaded code address
    asm.ld_a_d()
    asm.and_n(0xE0)
    asm.rlca()
    asm.rlca()
    asm.rlca()
    asm.out_n_a(0x00)        # Bank select
    asm.ld_a_d()
    asm.and_n(0x1F)
    asm.sla_e()
    asm.rla()
    asm.or_n(0xC0)
    asm.ld_d_a()             # DE = Z80 address
    asm.ex_de_hl()
    asm.ld_sp_hl()           # SP = threaded code address
    asm.ex_af_af()           # Restore 6502 A
    asm.ret()

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

    # CLD ($D8) - clear decimal mode
    asm.label('H_CLD')
    asm.ex_af_af()              # Save A
    asm.xor_a()                 # A = 0
    asm.ld_mem_nn_a(D_FLAG_ADDR)
    asm.ex_af_af()              # Restore A
    asm.ret()

    # SED ($F8) - set decimal mode
    asm.label('H_SED')
    asm.ex_af_af()              # Save A
    asm.ld_a_n(1)
    asm.ld_mem_nn_a(D_FLAG_ADDR)
    asm.ex_af_af()              # Restore A
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

    # ===== PHP/PLP (Processor Status) =====

    # PHP ($08) - Push Processor Status
    # We'll push the Z80 flags as a rough approximation
    asm.label('H_PHP')
    asm.ex_af_af()           # Get flags into F
    asm.push_af()            # Push to Z80 stack temporarily
    # Read and store in 6502 stack
    asm.ld_hl_nn(0)
    asm.add_hl_sp()
    asm.ld_a_mem_hl()        # A = F register (flags)
    asm.pop_af()             # Restore A (discard F)
    # Now push to 6502 stack
    asm.ld_hl_mem_nn(REG_S_ADDR)
    asm.ld_l_a()             # L = flags
    asm.ld_h_n(STACK_LINEAR_BASE >> 8)
    asm.ld_a_mem_nn(REG_S_ADDR)
    asm.ld_h_n(STACK_LINEAR_BASE >> 8)
    # Actually let's simplify - just push 0 for now
    asm.ld_a_mem_nn(REG_S_ADDR)
    asm.ld_l_a()
    asm.ld_h_n(STACK_LINEAR_BASE >> 8)
    asm.ld_a_n(0x30)         # Default: B and unused bits set
    asm.ld_mem_hl_a()
    # Decrement S
    asm.ld_a_mem_nn(REG_S_ADDR)
    asm.dec_a()
    asm.ld_mem_nn_a(REG_S_ADDR)
    asm.ex_af_af()
    asm.ret()

    # PLP ($28) - Pull Processor Status
    # Restore flags - we'll just pop and mostly ignore
    asm.label('H_PLP')
    asm.ex_af_af()
    asm.ld_a_mem_nn(REG_S_ADDR)
    asm.inc_a()
    asm.ld_mem_nn_a(REG_S_ADDR)
    asm.ld_l_a()
    asm.ld_h_n(STACK_LINEAR_BASE >> 8)
    asm.ld_a_mem_hl()        # A = pulled flags (ignored mostly)
    asm.ex_af_af()
    asm.ret()

    return asm


# =============================================================================
# HANDLER TABLE
# =============================================================================

# Mapping of 6502 opcodes to handler labels
OPCODE_HANDLERS = {
    0x00: 'H_BRK',
    0x01: 'H_ORA_INDX',
    0x05: 'H_ORA_ZP',
    0x06: 'H_ASL_ZP',
    0x08: 'H_PHP',
    0x09: 'H_ORA_IMM',
    0x0A: 'H_ASL_A',
    0x0D: 'H_ORA_ABS',
    0x0E: 'H_ASL_ABS',
    0x10: 'H_BPL',
    0x11: 'H_ORA_INDY',
    0x15: 'H_ORA_ZPX',
    0x16: 'H_ASL_ZPX',
    0x18: 'H_CLC',
    0x19: 'H_ORA_ABSY',
    0x1D: 'H_ORA_ABSX',
    0x1E: 'H_ASL_ABSX',
    0x20: 'H_JSR',
    0x21: 'H_AND_INDX',
    0x24: 'H_BIT_ZP',
    0x25: 'H_AND_ZP',
    0x26: 'H_ROL_ZP',
    0x28: 'H_PLP',
    0x29: 'H_AND_IMM',
    0x2A: 'H_ROL_A',
    0x2C: 'H_BIT_ABS',
    0x2D: 'H_AND_ABS',
    0x2E: 'H_ROL_ABS',
    0x30: 'H_BMI',
    0x31: 'H_AND_INDY',
    0x35: 'H_AND_ZPX',
    0x36: 'H_ROL_ZPX',
    0x38: 'H_SEC',
    0x39: 'H_AND_ABSY',
    0x3D: 'H_AND_ABSX',
    0x3E: 'H_ROL_ABSX',
    0x40: 'H_RTI',
    0x41: 'H_EOR_INDX',
    0x45: 'H_EOR_ZP',
    0x46: 'H_LSR_ZP',
    0x48: 'H_PHA',
    0x49: 'H_EOR_IMM',
    0x4A: 'H_LSR_A',
    0x4C: 'H_JMP_ABS',
    0x4D: 'H_EOR_ABS',
    0x4E: 'H_LSR_ABS',
    0x50: 'H_BVC',
    0x51: 'H_EOR_INDY',
    0x55: 'H_EOR_ZPX',
    0x56: 'H_LSR_ZPX',
    0x58: 'H_CLI',
    0x59: 'H_EOR_ABSY',
    0x5D: 'H_EOR_ABSX',
    0x5E: 'H_LSR_ABSX',
    0x60: 'H_RTS',
    0x61: 'H_ADC_INDX',
    0x65: 'H_ADC_ZP',
    0x66: 'H_ROR_ZP',
    0x68: 'H_PLA',
    0x69: 'H_ADC_IMM',
    0x6A: 'H_ROR_A',
    0x6C: 'H_JMP_IND',
    0x6D: 'H_ADC_ABS',
    0x6E: 'H_ROR_ABS',
    0x70: 'H_BVS',
    0x71: 'H_ADC_INDY',
    0x75: 'H_ADC_ZPX',
    0x76: 'H_ROR_ZPX',
    0x78: 'H_SEI',
    0x79: 'H_ADC_ABSY',
    0x7D: 'H_ADC_ABSX',
    0x7E: 'H_ROR_ABSX',
    0x81: 'H_STA_INDX',
    0x84: 'H_STY_ZP',
    0x85: 'H_STA_ZP',
    0x86: 'H_STX_ZP',
    0x88: 'H_DEY',
    0x8A: 'H_TXA',
    0x8C: 'H_STY_ABS',
    0x8D: 'H_STA_ABS',
    0x8E: 'H_STX_ABS',
    0x90: 'H_BCC',
    0x91: 'H_STA_INDY',
    0x94: 'H_STY_ZPX',
    0x95: 'H_STA_ZPX',
    0x96: 'H_STX_ZPY',
    0x98: 'H_TYA',
    0x99: 'H_STA_ABSY',
    0x9A: 'H_TXS',
    0x9D: 'H_STA_ABSX',
    0xA0: 'H_LDY_IMM',
    0xA1: 'H_LDA_INDX',
    0xA2: 'H_LDX_IMM',
    0xA4: 'H_LDY_ZP',
    0xA5: 'H_LDA_ZP',
    0xA6: 'H_LDX_ZP',
    0xA8: 'H_TAY',
    0xA9: 'H_LDA_IMM',
    0xAA: 'H_TAX',
    0xAC: 'H_LDY_ABS',
    0xAD: 'H_LDA_ABS',
    0xAE: 'H_LDX_ABS',
    0xB0: 'H_BCS',
    0xB1: 'H_LDA_INDY',
    0xB4: 'H_LDY_ZPX',
    0xB5: 'H_LDA_ZPX',
    0xB6: 'H_LDX_ZPY',
    0xB8: 'H_CLV',
    0xB9: 'H_LDA_ABSY',
    0xBA: 'H_TSX',
    0xBD: 'H_LDA_ABSX',
    0xC0: 'H_CPY_IMM',
    0xC4: 'H_CPY_ZP',
    0xC5: 'H_CMP_ZP',
    0xC6: 'H_DEC_ZP',
    0xC8: 'H_INY',
    0xC9: 'H_CMP_IMM',
    0xCA: 'H_DEX',
    0xCD: 'H_CMP_ABS',
    0xCE: 'H_DEC_ABS',
    0xD0: 'H_BNE',
    0xD1: 'H_CMP_INDY',
    0xD8: 'H_CLD',
    0xD9: 'H_CMP_ABSY',
    0xDD: 'H_CMP_ABSX',
    0x82: 'H_NOP_IMM',  # Unofficial 2-byte NOP
    0xC2: 'H_NOP_IMM',  # Unofficial 2-byte NOP
    0xE2: 'H_NOP_IMM',  # Unofficial 2-byte NOP
    0x89: 'H_NOP_IMM',  # Unofficial 2-byte NOP (or BIT #imm on 65C02)
    0xE0: 'H_CPX_IMM',
    0xE1: 'H_SBC_INDX',
    0xE4: 'H_CPX_ZP',
    0xE5: 'H_SBC_ZP',
    0xE6: 'H_INC_ZP',
    0xE8: 'H_INX',
    0xEE: 'H_INC_ABS',
    0xE9: 'H_SBC_IMM',
    0xEA: 'H_NOP',
    0xED: 'H_SBC_ABS',
    0xF0: 'H_BEQ',
    0xF1: 'H_SBC_INDY',
    0xF5: 'H_SBC_ZPX',
    0xF8: 'H_SED',
    0xF9: 'H_SBC_ABSY',
    0xFD: 'H_SBC_ABSX',
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
