"""
Unit tests for Z80 CPU Emulator
Comprehensive test suite matching ABAP unit tests
"""

import unittest
from z80 import Z80, Bus, FLAG_Z, FLAG_C, FLAG_S, FLAG_N, FLAG_PV, FLAG_H


class TestZ80Basic(unittest.TestCase):
    """Basic Z80 instruction tests"""

    def setUp(self):
        self.bus = Bus()
        self.cpu = Z80(self.bus)
        self.cpu.reset()

    def load_and_run(self, program: bytes, start: int = 0):
        """Load program and run until halted"""
        self.bus.load(start, program)
        self.cpu.pc = start
        self.cpu.run(max_cycles=10000)

    # ==================== LD r,n tests ====================
    def test_ld_a_n(self):
        """LD A,n"""
        self.load_and_run(bytes([0x3E, 0x42, 0x76]))  # LD A,42; HALT
        self.assertEqual(self.cpu.a, 0x42)

    def test_ld_b_n(self):
        """LD B,n"""
        self.load_and_run(bytes([0x06, 0x55, 0x76]))  # LD B,55; HALT
        self.assertEqual(self.cpu.b, 0x55)

    def test_ld_c_n(self):
        """LD C,n"""
        self.load_and_run(bytes([0x0E, 0xAA, 0x76]))  # LD C,AA; HALT
        self.assertEqual(self.cpu.c, 0xAA)

    def test_ld_d_n(self):
        """LD D,n"""
        self.load_and_run(bytes([0x16, 0x12, 0x76]))  # LD D,12; HALT
        self.assertEqual(self.cpu.d, 0x12)

    def test_ld_e_n(self):
        """LD E,n"""
        self.load_and_run(bytes([0x1E, 0x34, 0x76]))  # LD E,34; HALT
        self.assertEqual(self.cpu.e, 0x34)

    def test_ld_h_n(self):
        """LD H,n"""
        self.load_and_run(bytes([0x26, 0x56, 0x76]))  # LD H,56; HALT
        self.assertEqual(self.cpu.h, 0x56)

    def test_ld_l_n(self):
        """LD L,n"""
        self.load_and_run(bytes([0x2E, 0x78, 0x76]))  # LD L,78; HALT
        self.assertEqual(self.cpu.l, 0x78)

    # ==================== LD rr,nn tests ====================
    def test_ld_bc_nn(self):
        """LD BC,nn"""
        self.load_and_run(bytes([0x01, 0x34, 0x12, 0x76]))  # LD BC,1234; HALT
        self.assertEqual(self.cpu.bc, 0x1234)

    def test_ld_de_nn(self):
        """LD DE,nn"""
        self.load_and_run(bytes([0x11, 0x78, 0x56, 0x76]))  # LD DE,5678; HALT
        self.assertEqual(self.cpu.de, 0x5678)

    def test_ld_hl_nn(self):
        """LD HL,nn"""
        self.load_and_run(bytes([0x21, 0xBC, 0x9A, 0x76]))  # LD HL,9ABC; HALT
        self.assertEqual(self.cpu.hl, 0x9ABC)

    def test_ld_sp_nn(self):
        """LD SP,nn"""
        self.load_and_run(bytes([0x31, 0x00, 0x80, 0x76]))  # LD SP,8000; HALT
        self.assertEqual(self.cpu.sp, 0x8000)

    # ==================== LD r,r tests ====================
    def test_ld_r_r(self):
        """LD B,A"""
        self.load_and_run(bytes([0x3E, 0x42, 0x47, 0x76]))  # LD A,42; LD B,A; HALT
        self.assertEqual(self.cpu.a, 0x42)
        self.assertEqual(self.cpu.b, 0x42)

    # ==================== Memory operations ====================
    def test_ld_hl_mem_a(self):
        """LD (HL),A"""
        # LD A,42; LD HL,0x100; LD (HL),A; HALT
        self.load_and_run(bytes([0x3E, 0x42, 0x21, 0x00, 0x01, 0x77, 0x76]))
        self.assertEqual(self.bus.read_mem(0x100), 0x42)

    def test_ld_a_mem_hl(self):
        """LD A,(HL)"""
        self.bus.write_mem(0x100, 0x55)
        # LD HL,0x100; LD A,(HL); HALT
        self.load_and_run(bytes([0x21, 0x00, 0x01, 0x7E, 0x76]))
        self.assertEqual(self.cpu.a, 0x55)

    def test_ld_bc_mem_a(self):
        """LD (BC),A"""
        # LD A,0x99; LD BC,0x200; LD (BC),A; HALT
        self.load_and_run(bytes([0x3E, 0x99, 0x01, 0x00, 0x02, 0x02, 0x76]))
        self.assertEqual(self.bus.read_mem(0x200), 0x99)

    def test_ld_a_mem_bc(self):
        """LD A,(BC)"""
        self.bus.write_mem(0x200, 0x77)
        # LD BC,0x200; LD A,(BC); HALT
        self.load_and_run(bytes([0x01, 0x00, 0x02, 0x0A, 0x76]))
        self.assertEqual(self.cpu.a, 0x77)

    # ==================== INC/DEC tests ====================
    def test_inc_a(self):
        """INC A"""
        self.load_and_run(bytes([0x3E, 0x41, 0x3C, 0x76]))  # LD A,41; INC A; HALT
        self.assertEqual(self.cpu.a, 0x42)

    def test_dec_a(self):
        """DEC A"""
        self.load_and_run(bytes([0x3E, 0x43, 0x3D, 0x76]))  # LD A,43; DEC A; HALT
        self.assertEqual(self.cpu.a, 0x42)

    def test_inc_bc(self):
        """INC BC"""
        self.load_and_run(bytes([0x01, 0xFF, 0x00, 0x03, 0x76]))  # LD BC,00FF; INC BC; HALT
        self.assertEqual(self.cpu.bc, 0x0100)

    def test_dec_bc(self):
        """DEC BC"""
        self.load_and_run(bytes([0x01, 0x00, 0x01, 0x0B, 0x76]))  # LD BC,0100; DEC BC; HALT
        self.assertEqual(self.cpu.bc, 0x00FF)

    # ==================== ALU tests ====================
    def test_add_a_b(self):
        """ADD A,B"""
        self.load_and_run(bytes([0x3E, 0x10, 0x06, 0x20, 0x80, 0x76]))  # LD A,10; LD B,20; ADD A,B; HALT
        self.assertEqual(self.cpu.a, 0x30)

    def test_add_a_n(self):
        """ADD A,n"""
        self.load_and_run(bytes([0x3E, 0x10, 0xC6, 0x05, 0x76]))  # LD A,10; ADD A,5; HALT
        self.assertEqual(self.cpu.a, 0x15)

    def test_sub_a_b(self):
        """SUB A,B"""
        self.load_and_run(bytes([0x3E, 0x30, 0x06, 0x10, 0x90, 0x76]))  # LD A,30; LD B,10; SUB B; HALT
        self.assertEqual(self.cpu.a, 0x20)
        self.assertTrue(self.cpu.f & FLAG_N)  # N flag set for subtraction

    def test_and_a_b(self):
        """AND A,B"""
        self.load_and_run(bytes([0x3E, 0xFF, 0x06, 0x0F, 0xA0, 0x76]))  # LD A,FF; LD B,0F; AND B; HALT
        self.assertEqual(self.cpu.a, 0x0F)

    def test_or_a_b(self):
        """OR A,B"""
        self.load_and_run(bytes([0x3E, 0xF0, 0x06, 0x0F, 0xB0, 0x76]))  # LD A,F0; LD B,0F; OR B; HALT
        self.assertEqual(self.cpu.a, 0xFF)

    def test_xor_a_b(self):
        """XOR A,B"""
        self.load_and_run(bytes([0x3E, 0xFF, 0x06, 0xFF, 0xA8, 0x76]))  # LD A,FF; LD B,FF; XOR B; HALT
        self.assertEqual(self.cpu.a, 0x00)
        self.assertTrue(self.cpu.f & FLAG_Z)  # Z flag set

    def test_cp_a_b_equal(self):
        """CP A,B (equal)"""
        self.load_and_run(bytes([0x3E, 0x42, 0x06, 0x42, 0xB8, 0x76]))  # LD A,42; LD B,42; CP B; HALT
        self.assertTrue(self.cpu.f & FLAG_Z)  # Z flag set
        self.assertEqual(self.cpu.a, 0x42)  # A unchanged

    def test_cp_a_b_less(self):
        """CP A,B (A < B)"""
        self.load_and_run(bytes([0x3E, 0x10, 0x06, 0x20, 0xB8, 0x76]))  # LD A,10; LD B,20; CP B; HALT
        self.assertTrue(self.cpu.f & FLAG_C)  # C flag set

    # ==================== Jump tests ====================
    def test_jp_nn(self):
        """JP nn"""
        # JP 0x10; HALT at 0x10
        program = bytes([0xC3, 0x10, 0x00])  # JP 0010
        self.bus.load(0, program)
        self.bus.write_mem(0x10, 0x3E)  # LD A,0x42
        self.bus.write_mem(0x11, 0x42)
        self.bus.write_mem(0x12, 0x76)  # HALT
        self.cpu.pc = 0
        self.cpu.run()
        self.assertEqual(self.cpu.a, 0x42)

    def test_jr_e(self):
        """JR e (relative jump)"""
        # JR +2; NOP; NOP; LD A,42; HALT
        self.load_and_run(bytes([0x18, 0x02, 0x00, 0x00, 0x3E, 0x42, 0x76]))
        self.assertEqual(self.cpu.a, 0x42)

    def test_jr_z_taken(self):
        """JR Z,e (taken)"""
        # XOR A; JR Z,+2; NOP; NOP; LD A,55; HALT
        self.load_and_run(bytes([0xAF, 0x28, 0x02, 0x00, 0x00, 0x3E, 0x55, 0x76]))
        self.assertEqual(self.cpu.a, 0x55)

    def test_jr_z_not_taken(self):
        """JR Z,e (not taken)"""
        # LD A,1; OR A; JR Z,+3; LD A,42; HALT; LD A,99; HALT
        self.load_and_run(bytes([0x3E, 0x01, 0xB7, 0x28, 0x03, 0x3E, 0x42, 0x76, 0x3E, 0x99, 0x76]))
        self.assertEqual(self.cpu.a, 0x42)

    # ==================== Call/Return tests ====================
    def test_call_ret(self):
        """CALL nn / RET"""
        # At 0x00: LD A,0x10; CALL 0x10; ADD A,0x20; HALT
        # At 0x10: ADD A,0x05; RET
        program = bytes([
            0x3E, 0x10,       # LD A,0x10
            0xCD, 0x10, 0x00, # CALL 0x0010
            0xC6, 0x20,       # ADD A,0x20
            0x76              # HALT
        ])
        self.bus.load(0, program)
        self.bus.write_mem(0x10, 0xC6)  # ADD A,0x05
        self.bus.write_mem(0x11, 0x05)
        self.bus.write_mem(0x12, 0xC9)  # RET
        self.cpu.pc = 0
        self.cpu.run()
        self.assertEqual(self.cpu.a, 0x35)  # 0x10 + 0x05 + 0x20

    # ==================== Stack tests ====================
    def test_push_pop(self):
        """PUSH/POP"""
        # LD BC,0x1234; PUSH BC; POP DE; HALT
        self.load_and_run(bytes([0x01, 0x34, 0x12, 0xC5, 0xD1, 0x76]))
        self.assertEqual(self.cpu.de, 0x1234)

    def test_push_af_pop_af(self):
        """PUSH AF / POP AF"""
        # LD A,0x42; XOR A (set Z flag); PUSH AF; LD A,0xFF; POP AF; HALT
        self.load_and_run(bytes([0x3E, 0x00, 0xB7, 0xF5, 0x3E, 0xFF, 0xF1, 0x76]))
        self.assertEqual(self.cpu.a, 0x00)

    # ==================== Rotate tests ====================
    def test_rlca(self):
        """RLCA"""
        self.load_and_run(bytes([0x3E, 0x80, 0x07, 0x76]))  # LD A,0x80; RLCA; HALT
        self.assertEqual(self.cpu.a, 0x01)
        self.assertTrue(self.cpu.f & FLAG_C)

    def test_rrca(self):
        """RRCA"""
        self.load_and_run(bytes([0x3E, 0x01, 0x0F, 0x76]))  # LD A,0x01; RRCA; HALT
        self.assertEqual(self.cpu.a, 0x80)
        self.assertTrue(self.cpu.f & FLAG_C)

    # ==================== CB prefix tests ====================
    def test_cb_rlc_b(self):
        """CB RLC B"""
        self.load_and_run(bytes([0x06, 0x80, 0xCB, 0x00, 0x76]))  # LD B,0x80; RLC B; HALT
        self.assertEqual(self.cpu.b, 0x01)
        self.assertTrue(self.cpu.f & FLAG_C)

    def test_cb_bit_0_a(self):
        """CB BIT 0,A"""
        self.load_and_run(bytes([0x3E, 0x01, 0xCB, 0x47, 0x76]))  # LD A,0x01; BIT 0,A; HALT
        self.assertFalse(self.cpu.f & FLAG_Z)  # Bit 0 is set

    def test_cb_bit_0_a_zero(self):
        """CB BIT 0,A (zero)"""
        self.load_and_run(bytes([0x3E, 0x02, 0xCB, 0x47, 0x76]))  # LD A,0x02; BIT 0,A; HALT
        self.assertTrue(self.cpu.f & FLAG_Z)  # Bit 0 is not set

    def test_cb_set_3_b(self):
        """CB SET 3,B"""
        self.load_and_run(bytes([0x06, 0x00, 0xCB, 0xD8, 0x76]))  # LD B,0x00; SET 3,B; HALT
        self.assertEqual(self.cpu.b, 0x08)

    def test_cb_res_3_b(self):
        """CB RES 3,B"""
        self.load_and_run(bytes([0x06, 0xFF, 0xCB, 0x98, 0x76]))  # LD B,0xFF; RES 3,B; HALT
        self.assertEqual(self.cpu.b, 0xF7)

    def test_cb_sla_a(self):
        """CB SLA A"""
        self.load_and_run(bytes([0x3E, 0x40, 0xCB, 0x27, 0x76]))  # LD A,0x40; SLA A; HALT
        self.assertEqual(self.cpu.a, 0x80)

    def test_cb_sra_a(self):
        """CB SRA A (preserve sign)"""
        self.load_and_run(bytes([0x3E, 0x80, 0xCB, 0x2F, 0x76]))  # LD A,0x80; SRA A; HALT
        self.assertEqual(self.cpu.a, 0xC0)  # Sign preserved

    def test_cb_srl_a(self):
        """CB SRL A"""
        self.load_and_run(bytes([0x3E, 0x80, 0xCB, 0x3F, 0x76]))  # LD A,0x80; SRL A; HALT
        self.assertEqual(self.cpu.a, 0x40)

    # ==================== Exchange tests ====================
    def test_ex_de_hl(self):
        """EX DE,HL"""
        self.load_and_run(bytes([0x21, 0x34, 0x12, 0x11, 0x78, 0x56, 0xEB, 0x76]))
        # LD HL,1234; LD DE,5678; EX DE,HL; HALT
        self.assertEqual(self.cpu.hl, 0x5678)
        self.assertEqual(self.cpu.de, 0x1234)

    def test_ex_af_af(self):
        """EX AF,AF'"""
        # LD A,0x42; EX AF,AF'; LD A,0x00; EX AF,AF'; HALT
        self.load_and_run(bytes([0x3E, 0x42, 0x08, 0x3E, 0x00, 0x08, 0x76]))
        self.assertEqual(self.cpu.a, 0x42)

    def test_exx(self):
        """EXX"""
        # LD BC,0x1111; LD DE,0x2222; LD HL,0x3333; EXX;
        # LD BC,0x0000; LD DE,0x0000; LD HL,0x0000; EXX; HALT
        self.load_and_run(bytes([
            0x01, 0x11, 0x11,  # LD BC,1111
            0x11, 0x22, 0x22,  # LD DE,2222
            0x21, 0x33, 0x33,  # LD HL,3333
            0xD9,              # EXX
            0x01, 0x00, 0x00,  # LD BC,0000
            0x11, 0x00, 0x00,  # LD DE,0000
            0x21, 0x00, 0x00,  # LD HL,0000
            0xD9,              # EXX
            0x76               # HALT
        ]))
        self.assertEqual(self.cpu.bc, 0x1111)
        self.assertEqual(self.cpu.de, 0x2222)
        self.assertEqual(self.cpu.hl, 0x3333)

    # ==================== Miscellaneous tests ====================
    def test_cpl(self):
        """CPL"""
        self.load_and_run(bytes([0x3E, 0xAA, 0x2F, 0x76]))  # LD A,0xAA; CPL; HALT
        self.assertEqual(self.cpu.a, 0x55)

    def test_scf(self):
        """SCF"""
        self.load_and_run(bytes([0x37, 0x76]))  # SCF; HALT
        self.assertTrue(self.cpu.f & FLAG_C)

    def test_ccf(self):
        """CCF"""
        self.load_and_run(bytes([0x37, 0x3F, 0x76]))  # SCF; CCF; HALT
        self.assertFalse(self.cpu.f & FLAG_C)

    def test_di_ei(self):
        """DI/EI"""
        self.load_and_run(bytes([0xFB, 0xF3, 0x76]))  # EI; DI; HALT
        self.assertFalse(self.cpu.iff1)
        self.assertFalse(self.cpu.iff2)

    # ==================== I/O tests ====================
    def test_out_in(self):
        """OUT (n),A / IN A,(n)"""
        # LD A,0x42; OUT (0x10),A; LD A,0x00; IN A,(0x10); HALT
        self.load_and_run(bytes([0x3E, 0x42, 0xD3, 0x10, 0x3E, 0x00, 0xDB, 0x10, 0x76]))
        self.assertEqual(self.cpu.a, 0x42)

    # ==================== Flag tests ====================
    def test_zero_flag(self):
        """Zero flag"""
        self.load_and_run(bytes([0x3E, 0x01, 0x3D, 0x76]))  # LD A,1; DEC A; HALT
        self.assertTrue(self.cpu.f & FLAG_Z)

    def test_carry_flag_add(self):
        """Carry flag on ADD"""
        self.load_and_run(bytes([0x3E, 0xFF, 0xC6, 0x02, 0x76]))  # LD A,0xFF; ADD A,2; HALT
        self.assertTrue(self.cpu.f & FLAG_C)
        self.assertEqual(self.cpu.a, 0x01)

    def test_sign_flag(self):
        """Sign flag"""
        self.load_and_run(bytes([0x3E, 0x7F, 0x3C, 0x76]))  # LD A,0x7F; INC A; HALT
        self.assertTrue(self.cpu.f & FLAG_S)
        self.assertEqual(self.cpu.a, 0x80)


class TestZ80AddHL(unittest.TestCase):
    """ADD HL,rr tests"""

    def setUp(self):
        self.bus = Bus()
        self.cpu = Z80(self.bus)
        self.cpu.reset()

    def load_and_run(self, program: bytes):
        self.bus.load(0, program)
        self.cpu.pc = 0
        self.cpu.run(max_cycles=10000)

    def test_add_hl_bc(self):
        """ADD HL,BC"""
        # LD HL,0x1000; LD BC,0x0234; ADD HL,BC; HALT
        self.load_and_run(bytes([0x21, 0x00, 0x10, 0x01, 0x34, 0x02, 0x09, 0x76]))
        self.assertEqual(self.cpu.hl, 0x1234)

    def test_add_hl_de(self):
        """ADD HL,DE"""
        # LD HL,0x8000; LD DE,0x8000; ADD HL,DE; HALT
        self.load_and_run(bytes([0x21, 0x00, 0x80, 0x11, 0x00, 0x80, 0x19, 0x76]))
        self.assertEqual(self.cpu.hl, 0x0000)
        self.assertTrue(self.cpu.f & FLAG_C)  # Carry from overflow


class TestZ80RST(unittest.TestCase):
    """RST n tests"""

    def setUp(self):
        self.bus = Bus()
        self.cpu = Z80(self.bus)
        self.cpu.reset()

    def test_rst_08(self):
        """RST 08"""
        # At 0x00: RST 08
        # At 0x08: LD A,0x42; RET
        self.bus.load(0x00, bytes([0xCF]))  # RST 08
        self.bus.load(0x01, bytes([0x76]))  # HALT (after return)
        self.bus.load(0x08, bytes([0x3E, 0x42, 0xC9]))  # LD A,42; RET
        self.cpu.pc = 0
        self.cpu.run()
        self.assertEqual(self.cpu.a, 0x42)


if __name__ == "__main__":
    unittest.main(verbosity=2)
