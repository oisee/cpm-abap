# Pure Python 6502 emulator for testing

class Pure6502:
    def __init__(self):
        self.mem = bytearray(65536)
        self.A = 0
        self.X = 0
        self.Y = 0
        self.SP = 0xFF
        self.PC = 0
        self.N = False
        self.Z = False
        self.C = False
        self.V = False
        self.D = False
        self.I = False
        self.halted = False
        self.output = []
        self.input_buffer = ""
        self.key_data = 0
        self.key_ready = False
        self.trap_calls = {}

    def load(self, data, org):
        for i, b in enumerate(data):
            self.mem[org + i] = b
        self.PC = org

    def read(self, addr):
        addr = addr & 0xFFFF
        if addr == 0xC000:
            if not self.key_ready and self.input_buffer:
                ch = ord(self.input_buffer[0])
                self.input_buffer = self.input_buffer[1:]
                self.key_data = ch | 0x80
                self.key_ready = True
            return self.key_data if self.key_ready else 0
        elif addr == 0xC010:
            self.key_ready = False
            return self.key_data & 0x7F
        return self.mem[addr]

    def write(self, addr, val):
        self.mem[addr & 0xFFFF] = val & 0xFF

    def push(self, val):
        self.mem[0x100 + self.SP] = val & 0xFF
        self.SP = (self.SP - 1) & 0xFF

    def pop(self):
        self.SP = (self.SP + 1) & 0xFF
        return self.mem[0x100 + self.SP]

    def set_nz(self, val):
        self.N = (val & 0x80) != 0
        self.Z = (val & 0xFF) == 0
        return val & 0xFF

    def get_flags(self):
        return ((1 if self.N else 0) << 7 |
                (1 if self.V else 0) << 6 | 1 << 5 |
                (1 if self.D else 0) << 3 |
                (1 if self.I else 0) << 2 |
                (1 if self.Z else 0) << 1 |
                (1 if self.C else 0))

    def set_flags(self, val):
        self.N = (val & 0x80) != 0
        self.V = (val & 0x40) != 0
        self.D = (val & 0x08) != 0
        self.I = (val & 0x04) != 0
        self.Z = (val & 0x02) != 0
        self.C = (val & 0x01) != 0

    def trap(self, addr):
        self.trap_calls[addr] = self.trap_calls.get(addr, 0) + 1
        if addr == 0xFDED or addr == 0xFBFD:
            ch = self.A & 0x7F
            self.output.append(chr(ch) if 32 <= ch < 127 or ch in (10, 13) else '.')
            return True
        elif addr in (0xFC58, 0xFC10, 0xFC62):
            return True
        elif addr == 0xFD0C:
            if self.input_buffer:
                ch = ord(self.input_buffer[0])
                self.input_buffer = self.input_buffer[1:]
                self.A = ch | 0x80
            else:
                self.A = 0x8D
            return True
        return False

    def step(self):
        if self.halted: return False
        pc = self.PC
        op = self.mem[pc]

        if pc >= 0xF800:
            if self.trap(pc):
                lo = self.pop(); hi = self.pop()
                self.PC = ((hi << 8) | lo) + 1
                return True

        if op == 0x00: self.halted = True; return False
        elif op == 0x01:
            zp = (self.mem[pc+1] + self.X) & 0xFF
            addr = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.A = self.set_nz(self.A | self.read(addr)); self.PC = pc + 2
        elif op == 0x05: self.A = self.set_nz(self.A | self.read(self.mem[pc+1])); self.PC = pc + 2
        elif op == 0x06:
            addr = self.mem[pc+1]; val = self.read(addr)
            self.C = (val & 0x80) != 0; val = self.set_nz((val << 1) & 0xFF)
            self.write(addr, val); self.PC = pc + 2
        elif op == 0x08: self.push(self.get_flags() | 0x10); self.PC = pc + 1
        elif op == 0x09: self.A = self.set_nz(self.A | self.mem[pc+1]); self.PC = pc + 2
        elif op == 0x0A:
            self.C = (self.A & 0x80) != 0; self.A = self.set_nz((self.A << 1) & 0xFF); self.PC = pc + 1
        elif op == 0x0D:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8)
            self.A = self.set_nz(self.A | self.read(addr)); self.PC = pc + 3
        elif op == 0x0E:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            self.C = (val & 0x80) != 0; val = self.set_nz((val << 1) & 0xFF)
            self.write(addr, val); self.PC = pc + 3
        elif op == 0x10:
            offset = self.mem[pc+1]
            if offset >= 0x80: offset -= 256
            self.PC = pc + 2
            if not self.N: self.PC = (self.PC + offset) & 0xFFFF
        elif op == 0x11:
            zp = self.mem[pc+1]; base = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.A = self.set_nz(self.A | self.read((base + self.Y) & 0xFFFF)); self.PC = pc + 2
        elif op == 0x15:
            addr = (self.mem[pc+1] + self.X) & 0xFF
            self.A = self.set_nz(self.A | self.read(addr)); self.PC = pc + 2
        elif op == 0x16:
            addr = (self.mem[pc+1] + self.X) & 0xFF; val = self.read(addr)
            self.C = (val & 0x80) != 0; val = self.set_nz((val << 1) & 0xFF)
            self.write(addr, val); self.PC = pc + 2
        elif op == 0x18: self.C = False; self.PC = pc + 1
        elif op == 0x19:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.Y
            self.A = self.set_nz(self.A | self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0x1D:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X
            self.A = self.set_nz(self.A | self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0x20:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); ret = pc + 2
            self.push((ret >> 8) & 0xFF); self.push(ret & 0xFF); self.PC = addr
        elif op == 0x21:
            zp = (self.mem[pc+1] + self.X) & 0xFF
            addr = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.A = self.set_nz(self.A & self.read(addr)); self.PC = pc + 2
        elif op == 0x24:
            val = self.read(self.mem[pc+1])
            self.N = (val & 0x80) != 0; self.V = (val & 0x40) != 0
            self.Z = (self.A & val) == 0; self.PC = pc + 2
        elif op == 0x25: self.A = self.set_nz(self.A & self.read(self.mem[pc+1])); self.PC = pc + 2
        elif op == 0x26:
            addr = self.mem[pc+1]; val = self.read(addr)
            c = 1 if self.C else 0; self.C = (val & 0x80) != 0
            val = self.set_nz(((val << 1) | c) & 0xFF); self.write(addr, val); self.PC = pc + 2
        elif op == 0x28: self.set_flags(self.pop()); self.PC = pc + 1
        elif op == 0x29: self.A = self.set_nz(self.A & self.mem[pc+1]); self.PC = pc + 2
        elif op == 0x2A:
            c = 1 if self.C else 0; self.C = (self.A & 0x80) != 0
            self.A = self.set_nz(((self.A << 1) | c) & 0xFF); self.PC = pc + 1
        elif op == 0x2C:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            self.N = (val & 0x80) != 0; self.V = (val & 0x40) != 0
            self.Z = (self.A & val) == 0; self.PC = pc + 3
        elif op == 0x2D:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8)
            self.A = self.set_nz(self.A & self.read(addr)); self.PC = pc + 3
        elif op == 0x2E:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            c = 1 if self.C else 0; self.C = (val & 0x80) != 0
            val = self.set_nz(((val << 1) | c) & 0xFF); self.write(addr, val); self.PC = pc + 3
        elif op == 0x30:
            offset = self.mem[pc+1]
            if offset >= 0x80: offset -= 256
            self.PC = pc + 2
            if self.N: self.PC = (self.PC + offset) & 0xFFFF
        elif op == 0x31:
            zp = self.mem[pc+1]; base = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.A = self.set_nz(self.A & self.read((base + self.Y) & 0xFFFF)); self.PC = pc + 2
        elif op == 0x35:
            addr = (self.mem[pc+1] + self.X) & 0xFF
            self.A = self.set_nz(self.A & self.read(addr)); self.PC = pc + 2
        elif op == 0x38: self.C = True; self.PC = pc + 1
        elif op == 0x39:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.Y
            self.A = self.set_nz(self.A & self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0x3D:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X
            self.A = self.set_nz(self.A & self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0x40:
            self.set_flags(self.pop()); lo = self.pop(); hi = self.pop()
            self.PC = (hi << 8) | lo
        elif op == 0x41:
            zp = (self.mem[pc+1] + self.X) & 0xFF
            addr = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.A = self.set_nz(self.A ^ self.read(addr)); self.PC = pc + 2
        elif op == 0x45: self.A = self.set_nz(self.A ^ self.read(self.mem[pc+1])); self.PC = pc + 2
        elif op == 0x46:
            addr = self.mem[pc+1]; val = self.read(addr)
            self.C = (val & 1) != 0; val = self.set_nz(val >> 1); self.write(addr, val); self.PC = pc + 2
        elif op == 0x48: self.push(self.A); self.PC = pc + 1
        elif op == 0x49: self.A = self.set_nz(self.A ^ self.mem[pc+1]); self.PC = pc + 2
        elif op == 0x4A: self.C = (self.A & 1) != 0; self.A = self.set_nz(self.A >> 1); self.PC = pc + 1
        elif op == 0x4C: self.PC = self.mem[pc+1] | (self.mem[pc+2] << 8)
        elif op == 0x4D:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8)
            self.A = self.set_nz(self.A ^ self.read(addr)); self.PC = pc + 3
        elif op == 0x4E:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            self.C = (val & 1) != 0; val = self.set_nz(val >> 1); self.write(addr, val); self.PC = pc + 3
        elif op == 0x50:
            offset = self.mem[pc+1]
            if offset >= 0x80: offset -= 256
            self.PC = pc + 2
            if not self.V: self.PC = (self.PC + offset) & 0xFFFF
        elif op == 0x51:
            zp = self.mem[pc+1]; base = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.A = self.set_nz(self.A ^ self.read((base + self.Y) & 0xFFFF)); self.PC = pc + 2
        elif op == 0x55:
            addr = (self.mem[pc+1] + self.X) & 0xFF
            self.A = self.set_nz(self.A ^ self.read(addr)); self.PC = pc + 2
        elif op == 0x59:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.Y
            self.A = self.set_nz(self.A ^ self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0x5D:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X
            self.A = self.set_nz(self.A ^ self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0x60:
            lo = self.pop(); hi = self.pop(); self.PC = ((hi << 8) | lo) + 1
        elif op == 0x61:
            zp = (self.mem[pc+1] + self.X) & 0xFF
            addr = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            val = self.read(addr); result = self.A + val + (1 if self.C else 0)
            self.C = result > 255; self.V = ((self.A ^ result) & (val ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0x65:
            val = self.read(self.mem[pc+1]); result = self.A + val + (1 if self.C else 0)
            self.C = result > 255; self.V = ((self.A ^ result) & (val ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0x66:
            addr = self.mem[pc+1]; val = self.read(addr)
            c = 0x80 if self.C else 0; self.C = (val & 1) != 0
            val = self.set_nz((val >> 1) | c); self.write(addr, val); self.PC = pc + 2
        elif op == 0x68: self.A = self.set_nz(self.pop()); self.PC = pc + 1
        elif op == 0x69:
            val = self.mem[pc+1]; result = self.A + val + (1 if self.C else 0)
            self.C = result > 255; self.V = ((self.A ^ result) & (val ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0x6A:
            c = 0x80 if self.C else 0; self.C = (self.A & 1) != 0
            self.A = self.set_nz((self.A >> 1) | c); self.PC = pc + 1
        elif op == 0x6C:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8)
            lo = self.read(addr); hi = self.read((addr & 0xFF00) | ((addr + 1) & 0xFF))
            self.PC = lo | (hi << 8)
        elif op == 0x6D:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            result = self.A + val + (1 if self.C else 0)
            self.C = result > 255; self.V = ((self.A ^ result) & (val ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 3
        elif op == 0x6E:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            c = 0x80 if self.C else 0; self.C = (val & 1) != 0
            val = self.set_nz((val >> 1) | c); self.write(addr, val); self.PC = pc + 3
        elif op == 0x70:
            offset = self.mem[pc+1]
            if offset >= 0x80: offset -= 256
            self.PC = pc + 2
            if self.V: self.PC = (self.PC + offset) & 0xFFFF
        elif op == 0x71:
            zp = self.mem[pc+1]; base = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            val = self.read((base + self.Y) & 0xFFFF); result = self.A + val + (1 if self.C else 0)
            self.C = result > 255; self.V = ((self.A ^ result) & (val ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0x75:
            addr = (self.mem[pc+1] + self.X) & 0xFF; val = self.read(addr)
            result = self.A + val + (1 if self.C else 0)
            self.C = result > 255; self.V = ((self.A ^ result) & (val ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0x78: self.I = True; self.PC = pc + 1
        elif op == 0x79:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.Y; val = self.read(addr & 0xFFFF)
            result = self.A + val + (1 if self.C else 0)
            self.C = result > 255; self.V = ((self.A ^ result) & (val ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 3
        elif op == 0x7D:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X; val = self.read(addr & 0xFFFF)
            result = self.A + val + (1 if self.C else 0)
            self.C = result > 255; self.V = ((self.A ^ result) & (val ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 3
        elif op == 0x81:
            zp = (self.mem[pc+1] + self.X) & 0xFF
            addr = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.write(addr, self.A); self.PC = pc + 2
        elif op == 0x84: self.write(self.mem[pc+1], self.Y); self.PC = pc + 2
        elif op == 0x85: self.write(self.mem[pc+1], self.A); self.PC = pc + 2
        elif op == 0x86: self.write(self.mem[pc+1], self.X); self.PC = pc + 2
        elif op == 0x88: self.Y = self.set_nz((self.Y - 1) & 0xFF); self.PC = pc + 1
        elif op == 0x8A: self.A = self.set_nz(self.X); self.PC = pc + 1
        elif op == 0x8C:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); self.write(addr, self.Y); self.PC = pc + 3
        elif op == 0x8D:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); self.write(addr, self.A); self.PC = pc + 3
        elif op == 0x8E:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); self.write(addr, self.X); self.PC = pc + 3
        elif op == 0x90:
            offset = self.mem[pc+1]
            if offset >= 0x80: offset -= 256
            self.PC = pc + 2
            if not self.C: self.PC = (self.PC + offset) & 0xFFFF
        elif op == 0x91:
            zp = self.mem[pc+1]; base = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.write((base + self.Y) & 0xFFFF, self.A); self.PC = pc + 2
        elif op == 0x94:
            addr = (self.mem[pc+1] + self.X) & 0xFF; self.write(addr, self.Y); self.PC = pc + 2
        elif op == 0x95:
            addr = (self.mem[pc+1] + self.X) & 0xFF; self.write(addr, self.A); self.PC = pc + 2
        elif op == 0x96:
            addr = (self.mem[pc+1] + self.Y) & 0xFF; self.write(addr, self.X); self.PC = pc + 2
        elif op == 0x98: self.A = self.set_nz(self.Y); self.PC = pc + 1
        elif op == 0x99:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.Y
            self.write(addr & 0xFFFF, self.A); self.PC = pc + 3
        elif op == 0x9A: self.SP = self.X; self.PC = pc + 1
        elif op == 0x9D:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X
            self.write(addr & 0xFFFF, self.A); self.PC = pc + 3
        elif op == 0xA0: self.Y = self.set_nz(self.mem[pc+1]); self.PC = pc + 2
        elif op == 0xA1:
            zp = (self.mem[pc+1] + self.X) & 0xFF
            addr = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.A = self.set_nz(self.read(addr)); self.PC = pc + 2
        elif op == 0xA2: self.X = self.set_nz(self.mem[pc+1]); self.PC = pc + 2
        elif op == 0xA4: self.Y = self.set_nz(self.read(self.mem[pc+1])); self.PC = pc + 2
        elif op == 0xA5: self.A = self.set_nz(self.read(self.mem[pc+1])); self.PC = pc + 2
        elif op == 0xA6: self.X = self.set_nz(self.read(self.mem[pc+1])); self.PC = pc + 2
        elif op == 0xA8: self.Y = self.set_nz(self.A); self.PC = pc + 1
        elif op == 0xA9: self.A = self.set_nz(self.mem[pc+1]); self.PC = pc + 2
        elif op == 0xAA: self.X = self.set_nz(self.A); self.PC = pc + 1
        elif op == 0xAC:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8)
            self.Y = self.set_nz(self.read(addr)); self.PC = pc + 3
        elif op == 0xAD:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8)
            self.A = self.set_nz(self.read(addr)); self.PC = pc + 3
        elif op == 0xAE:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8)
            self.X = self.set_nz(self.read(addr)); self.PC = pc + 3
        elif op == 0xB0:
            offset = self.mem[pc+1]
            if offset >= 0x80: offset -= 256
            self.PC = pc + 2
            if self.C: self.PC = (self.PC + offset) & 0xFFFF
        elif op == 0xB1:
            zp = self.mem[pc+1]; base = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            self.A = self.set_nz(self.read((base + self.Y) & 0xFFFF)); self.PC = pc + 2
        elif op == 0xB4:
            addr = (self.mem[pc+1] + self.X) & 0xFF
            self.Y = self.set_nz(self.read(addr)); self.PC = pc + 2
        elif op == 0xB5:
            addr = (self.mem[pc+1] + self.X) & 0xFF
            self.A = self.set_nz(self.read(addr)); self.PC = pc + 2
        elif op == 0xB6:
            addr = (self.mem[pc+1] + self.Y) & 0xFF
            self.X = self.set_nz(self.read(addr)); self.PC = pc + 2
        elif op == 0xB8: self.V = False; self.PC = pc + 1
        elif op == 0xB9:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.Y
            self.A = self.set_nz(self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0xBA: self.X = self.set_nz(self.SP); self.PC = pc + 1
        elif op == 0xBC:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X
            self.Y = self.set_nz(self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0xBD:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X
            self.A = self.set_nz(self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0xBE:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.Y
            self.X = self.set_nz(self.read(addr & 0xFFFF)); self.PC = pc + 3
        elif op == 0xC0:
            val = self.mem[pc+1]; result = self.Y - val; self.C = self.Y >= val
            self.set_nz(result); self.PC = pc + 2
        elif op == 0xC1:
            zp = (self.mem[pc+1] + self.X) & 0xFF
            addr = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            val = self.read(addr); result = self.A - val; self.C = self.A >= val
            self.set_nz(result); self.PC = pc + 2
        elif op == 0xC4:
            val = self.read(self.mem[pc+1]); result = self.Y - val; self.C = self.Y >= val
            self.set_nz(result); self.PC = pc + 2
        elif op == 0xC5:
            val = self.read(self.mem[pc+1]); result = self.A - val; self.C = self.A >= val
            self.set_nz(result); self.PC = pc + 2
        elif op == 0xC6:
            addr = self.mem[pc+1]; val = self.set_nz((self.read(addr) - 1) & 0xFF)
            self.write(addr, val); self.PC = pc + 2
        elif op == 0xC8: self.Y = self.set_nz((self.Y + 1) & 0xFF); self.PC = pc + 1
        elif op == 0xC9:
            val = self.mem[pc+1]; result = self.A - val; self.C = self.A >= val
            self.set_nz(result); self.PC = pc + 2
        elif op == 0xCA: self.X = self.set_nz((self.X - 1) & 0xFF); self.PC = pc + 1
        elif op == 0xCC:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            result = self.Y - val; self.C = self.Y >= val; self.set_nz(result); self.PC = pc + 3
        elif op == 0xCD:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            result = self.A - val; self.C = self.A >= val; self.set_nz(result); self.PC = pc + 3
        elif op == 0xCE:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8)
            val = self.set_nz((self.read(addr) - 1) & 0xFF); self.write(addr, val); self.PC = pc + 3
        elif op == 0xD0:
            offset = self.mem[pc+1]
            if offset >= 0x80: offset -= 256
            self.PC = pc + 2
            if not self.Z: self.PC = (self.PC + offset) & 0xFFFF
        elif op == 0xD1:
            zp = self.mem[pc+1]; base = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            val = self.read((base + self.Y) & 0xFFFF); result = self.A - val
            self.C = self.A >= val; self.set_nz(result); self.PC = pc + 2
        elif op == 0xD5:
            addr = (self.mem[pc+1] + self.X) & 0xFF; val = self.read(addr)
            result = self.A - val; self.C = self.A >= val; self.set_nz(result); self.PC = pc + 2
        elif op == 0xD8: self.D = False; self.PC = pc + 1
        elif op == 0xD9:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.Y; val = self.read(addr & 0xFFFF)
            result = self.A - val; self.C = self.A >= val; self.set_nz(result); self.PC = pc + 3
        elif op == 0xDD:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X; val = self.read(addr & 0xFFFF)
            result = self.A - val; self.C = self.A >= val; self.set_nz(result); self.PC = pc + 3
        elif op == 0xDE:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X
            val = self.set_nz((self.read(addr & 0xFFFF) - 1) & 0xFF)
            self.write(addr & 0xFFFF, val); self.PC = pc + 3
        elif op == 0xE0:
            val = self.mem[pc+1]; result = self.X - val; self.C = self.X >= val
            self.set_nz(result); self.PC = pc + 2
        elif op == 0xE1:
            zp = (self.mem[pc+1] + self.X) & 0xFF
            addr = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            val = self.read(addr); result = self.A - val - (0 if self.C else 1)
            self.C = result >= 0; self.V = ((self.A ^ result) & ((255 - val) ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0xE4:
            val = self.read(self.mem[pc+1]); result = self.X - val; self.C = self.X >= val
            self.set_nz(result); self.PC = pc + 2
        elif op == 0xE5:
            val = self.read(self.mem[pc+1]); result = self.A - val - (0 if self.C else 1)
            self.C = result >= 0; self.V = ((self.A ^ result) & ((255 - val) ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0xE6:
            addr = self.mem[pc+1]; val = self.set_nz((self.read(addr) + 1) & 0xFF)
            self.write(addr, val); self.PC = pc + 2
        elif op == 0xE8: self.X = self.set_nz((self.X + 1) & 0xFF); self.PC = pc + 1
        elif op == 0xE9:
            val = self.mem[pc+1]; result = self.A - val - (0 if self.C else 1)
            self.C = result >= 0; self.V = ((self.A ^ result) & ((255 - val) ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0xEA: self.PC = pc + 1
        elif op == 0xEC:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            result = self.X - val; self.C = self.X >= val; self.set_nz(result); self.PC = pc + 3
        elif op == 0xED:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8); val = self.read(addr)
            result = self.A - val - (0 if self.C else 1)
            self.C = result >= 0; self.V = ((self.A ^ result) & ((255 - val) ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 3
        elif op == 0xEE:
            addr = self.mem[pc+1] | (self.mem[pc+2] << 8)
            val = self.set_nz((self.read(addr) + 1) & 0xFF); self.write(addr, val); self.PC = pc + 3
        elif op == 0xF0:
            offset = self.mem[pc+1]
            if offset >= 0x80: offset -= 256
            self.PC = pc + 2
            if self.Z: self.PC = (self.PC + offset) & 0xFFFF
        elif op == 0xF1:
            zp = self.mem[pc+1]; base = self.read(zp) | (self.read((zp+1) & 0xFF) << 8)
            val = self.read((base + self.Y) & 0xFFFF)
            result = self.A - val - (0 if self.C else 1)
            self.C = result >= 0; self.V = ((self.A ^ result) & ((255 - val) ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0xF5:
            addr = (self.mem[pc+1] + self.X) & 0xFF; val = self.read(addr)
            result = self.A - val - (0 if self.C else 1)
            self.C = result >= 0; self.V = ((self.A ^ result) & ((255 - val) ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 2
        elif op == 0xF8: self.D = True; self.PC = pc + 1
        elif op == 0xF9:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.Y; val = self.read(addr & 0xFFFF)
            result = self.A - val - (0 if self.C else 1)
            self.C = result >= 0; self.V = ((self.A ^ result) & ((255 - val) ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 3
        elif op == 0xFD:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X; val = self.read(addr & 0xFFFF)
            result = self.A - val - (0 if self.C else 1)
            self.C = result >= 0; self.V = ((self.A ^ result) & ((255 - val) ^ result) & 0x80) != 0
            self.A = self.set_nz(result); self.PC = pc + 3
        elif op == 0xFE:
            addr = (self.mem[pc+1] | (self.mem[pc+2] << 8)) + self.X
            val = self.set_nz((self.read(addr & 0xFFFF) + 1) & 0xFF)
            self.write(addr & 0xFFFF, val); self.PC = pc + 3
        else:
            print(f"UNIMPLEMENTED opcode ${op:02X} at ${pc:04X}")
            self.halted = True
            return False
        return True

    def run(self, max_steps=1000000):
        for i in range(max_steps):
            if not self.step(): return i
        return max_steps


if __name__ == "__main__":
    # Test with Adventureland
    with open('scott_adams/adventureland.bin', 'rb') as f:
        code = f.read()

    emu = Pure6502()
    emu.load(code, 0x0800)

    # Full interaction: N for no restore, Return to continue, then LOOK command
    emu.input_buffer = "N\r\rLOOK\r"

    steps = emu.run(max_steps=2000000)

    print(f"Steps: {steps}")
    print(f"\nGame output ({len(emu.output)} chars):")
    out = ''.join(emu.output)
    lines = out.replace('\r', '\n').split('\n')
    for line in lines:
        print(line)
    print()
    print(f"Trap calls: {emu.trap_calls}")
    print(f"Final PC: ${emu.PC:04X}")
