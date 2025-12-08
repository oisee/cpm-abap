#!/usr/bin/env python3
"""
Minimal Z-Machine Version 3 Interpreter
========================================
Extracted from xyppy for clarity and ABAP porting.

Structure (maps to ABAP classes):
- Memory: ZCL_ZORK_00_MEMORY
- Stack: ZCL_ZORK_00_STACK
- Objects: ZCL_ZORK_00_OBJECTS
- Text: ZCL_ZORK_00_DECODER
- Opcodes: ZCL_ZORK_00_EXECUTOR

Usage: python3 z3_minimal.py story.z3
"""

import sys
from array import array

# =============================================================================
# CONSTANTS - Z-Machine Version 3
# =============================================================================

# Default alphabets for text encoding/decoding
# These are 26 characters each, indexed by (zchar - 6) for zchars 6-31
A0 = 'abcdefghijklmnopqrstuvwxyz'       # zchar 6='a', 7='b', ... 31='z'
A1 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'       # shift 4 -> uppercase
A2 = ' \n0123456789.,!?_#\'"/\\-:()'    # shift 5 -> symbols (26 chars)

# Object table constants for V3
OBJ_ATTR_BYTES = 4      # 4 bytes = 32 attributes
OBJ_ENTRY_SIZE = 9      # parent(1) + sibling(1) + child(1) + props(2) = 9 bytes for V3
OBJ_MAX_OBJECTS = 255   # V3 uses 1-byte object numbers
PROP_DEFAULTS_SIZE = 31 # 31 words (62 bytes) of property defaults

# =============================================================================
# MEMORY & HEADER
# =============================================================================

class Memory:
    """Z-Machine memory management - maps to ZCL_ZORK_00_MEMORY"""

    def __init__(self, data: bytes):
        self.mem = array('B', data)
        self.size = len(data)

        # Parse header (first 64 bytes)
        self.version = self.mem[0x00]
        if self.version != 3:
            raise ValueError(f"Only Z3 supported, got version {self.version}")

        self.flags1 = self.mem[0x01]
        self.high_mem = self.u16(0x04)
        self.initial_pc = self.u16(0x06)
        self.dict_addr = self.u16(0x08)
        self.obj_addr = self.u16(0x0A)
        self.globals_addr = self.u16(0x0C)
        self.static_mem = self.u16(0x0E)
        self.abbrev_addr = self.u16(0x18)
        self.file_len = self.u16(0x1A) * 2  # V3: multiply by 2
        self.checksum = self.u16(0x1C)

        # Serial number (6 ASCII chars at 0x12-0x17)
        self.serial = bytes(self.mem[0x12:0x18]).decode('ascii', errors='replace')

    def u8(self, addr: int) -> int:
        """Read unsigned byte"""
        return self.mem[addr]

    def u16(self, addr: int) -> int:
        """Read unsigned 16-bit word (big-endian)"""
        return (self.mem[addr] << 8) | self.mem[addr + 1]

    def w8(self, addr: int, val: int):
        """Write unsigned byte"""
        self.mem[addr] = val & 0xFF

    def w16(self, addr: int, val: int):
        """Write unsigned 16-bit word (big-endian)"""
        self.mem[addr] = (val >> 8) & 0xFF
        self.mem[addr + 1] = val & 0xFF

    def get_global(self, var: int) -> int:
        """Get global variable (16-239 -> globals table)"""
        addr = self.globals_addr + (var - 16) * 2
        return self.u16(addr)

    def set_global(self, var: int, val: int):
        """Set global variable"""
        addr = self.globals_addr + (var - 16) * 2
        self.w16(addr, val & 0xFFFF)

# =============================================================================
# STACK & CALL FRAMES
# =============================================================================

class Frame:
    """Call stack frame - one per routine call"""
    def __init__(self, return_pc: int, return_var: int, num_locals: int):
        self.return_pc = return_pc      # PC to return to
        self.return_var = return_var    # Variable to store result (or None)
        self.locals = [0] * num_locals  # Local variables (1-15)
        self.stack = []                 # Evaluation stack for this frame

class Stack:
    """Z-Machine stack management - maps to ZCL_ZORK_00_STACK"""

    def __init__(self):
        self.frames = [Frame(0, 0, 0)]  # Initial frame

    @property
    def current(self) -> Frame:
        return self.frames[-1]

    def push(self, val: int):
        """Push value onto evaluation stack"""
        self.current.stack.append(val & 0xFFFF)

    def pop(self) -> int:
        """Pop value from evaluation stack"""
        if not self.current.stack:
            raise RuntimeError("Stack underflow")
        return self.current.stack.pop()

    def peek(self) -> int:
        """Peek at top of stack without popping"""
        if not self.current.stack:
            raise RuntimeError("Stack underflow")
        return self.current.stack[-1]

    def get_local(self, var: int) -> int:
        """Get local variable (1-15)"""
        return self.current.locals[var - 1]

    def set_local(self, var: int, val: int):
        """Set local variable (1-15)"""
        self.current.locals[var - 1] = val & 0xFFFF

    def call(self, return_pc: int, return_var: int, num_locals: int) -> Frame:
        """Create new call frame"""
        frame = Frame(return_pc, return_var, num_locals)
        self.frames.append(frame)
        return frame

    def ret(self, val: int) -> tuple:
        """Return from routine, returns (return_pc, return_var, value)"""
        frame = self.frames.pop()
        return (frame.return_pc, frame.return_var, val)

# =============================================================================
# OBJECT TABLE
# =============================================================================

class Objects:
    """Z-Machine V3 object table - maps to ZCL_ZORK_00_OBJECTS"""

    def __init__(self, mem: Memory):
        self.mem = mem
        self.base = mem.obj_addr
        # Property defaults: 31 words before object entries
        self.prop_defaults_addr = self.base
        # First object at base + 62 (31 * 2 bytes)
        self.first_obj_addr = self.base + PROP_DEFAULTS_SIZE * 2

    def _obj_addr(self, obj: int) -> int:
        """Get address of object entry (1-based object number)"""
        if obj < 1 or obj > OBJ_MAX_OBJECTS:
            raise ValueError(f"Invalid object {obj}")
        return self.first_obj_addr + (obj - 1) * OBJ_ENTRY_SIZE

    def get_attr(self, obj: int, attr: int) -> bool:
        """Test attribute (0-31)"""
        if attr < 0 or attr > 31:
            return False
        addr = self._obj_addr(obj)
        byte_idx = attr // 8
        bit_idx = 7 - (attr % 8)  # Big-endian bit order!
        return bool(self.mem.u8(addr + byte_idx) & (1 << bit_idx))

    def set_attr(self, obj: int, attr: int):
        """Set attribute"""
        addr = self._obj_addr(obj)
        byte_idx = attr // 8
        bit_idx = 7 - (attr % 8)
        self.mem.w8(addr + byte_idx, self.mem.u8(addr + byte_idx) | (1 << bit_idx))

    def clear_attr(self, obj: int, attr: int):
        """Clear attribute"""
        addr = self._obj_addr(obj)
        byte_idx = attr // 8
        bit_idx = 7 - (attr % 8)
        self.mem.w8(addr + byte_idx, self.mem.u8(addr + byte_idx) & ~(1 << bit_idx))

    def get_parent(self, obj: int) -> int:
        """Get parent object number"""
        return self.mem.u8(self._obj_addr(obj) + OBJ_ATTR_BYTES)

    def get_sibling(self, obj: int) -> int:
        """Get sibling object number"""
        return self.mem.u8(self._obj_addr(obj) + OBJ_ATTR_BYTES + 1)

    def get_child(self, obj: int) -> int:
        """Get first child object number"""
        return self.mem.u8(self._obj_addr(obj) + OBJ_ATTR_BYTES + 2)

    def set_parent(self, obj: int, parent: int):
        self.mem.w8(self._obj_addr(obj) + OBJ_ATTR_BYTES, parent)

    def set_sibling(self, obj: int, sibling: int):
        self.mem.w8(self._obj_addr(obj) + OBJ_ATTR_BYTES + 1, sibling)

    def set_child(self, obj: int, child: int):
        self.mem.w8(self._obj_addr(obj) + OBJ_ATTR_BYTES + 2, child)

    def get_prop_addr(self, obj: int) -> int:
        """Get address of property table for object"""
        return self.mem.u16(self._obj_addr(obj) + OBJ_ATTR_BYTES + 3)

    def get_name(self, obj: int) -> int:
        """Get address of object short name (Z-string after length byte)"""
        prop_addr = self.get_prop_addr(obj)
        name_len = self.mem.u8(prop_addr)  # Length in words
        if name_len == 0:
            return 0
        return prop_addr + 1  # String starts after length byte

    def get_prop(self, obj: int, prop: int) -> int:
        """Get property value (returns default if not found)"""
        prop_addr = self.get_prop_addr(obj)
        name_len = self.mem.u8(prop_addr)
        addr = prop_addr + 1 + name_len * 2  # Skip name

        while True:
            size_byte = self.mem.u8(addr)
            if size_byte == 0:
                break  # End of property list
            prop_num = size_byte & 0x1F
            prop_len = (size_byte >> 5) + 1
            if prop_num == prop:
                if prop_len == 1:
                    return self.mem.u8(addr + 1)
                else:
                    return self.mem.u16(addr + 1)
            addr += 1 + prop_len
            if prop_num < prop:
                break  # Properties are in descending order

        # Return default
        return self.mem.u16(self.prop_defaults_addr + (prop - 1) * 2)

    def get_prop_len(self, prop_data_addr: int) -> int:
        """Get length of property from its data address"""
        if prop_data_addr == 0:
            return 0
        size_byte = self.mem.u8(prop_data_addr - 1)
        return (size_byte >> 5) + 1

    def get_next_prop(self, obj: int, prop: int) -> int:
        """Get next property number after given property (0 = first)"""
        prop_addr = self.get_prop_addr(obj)
        name_len = self.mem.u8(prop_addr)
        addr = prop_addr + 1 + name_len * 2

        if prop == 0:
            # Return first property
            size_byte = self.mem.u8(addr)
            return size_byte & 0x1F if size_byte else 0

        while True:
            size_byte = self.mem.u8(addr)
            if size_byte == 0:
                return 0
            prop_num = size_byte & 0x1F
            prop_len = (size_byte >> 5) + 1
            if prop_num == prop:
                # Return next property
                addr += 1 + prop_len
                size_byte = self.mem.u8(addr)
                return size_byte & 0x1F if size_byte else 0
            addr += 1 + prop_len

    def put_prop(self, obj: int, prop: int, val: int):
        """Set property value"""
        prop_addr = self.get_prop_addr(obj)
        name_len = self.mem.u8(prop_addr)
        addr = prop_addr + 1 + name_len * 2

        while True:
            size_byte = self.mem.u8(addr)
            if size_byte == 0:
                raise RuntimeError(f"Property {prop} not found on object {obj}")
            prop_num = size_byte & 0x1F
            prop_len = (size_byte >> 5) + 1
            if prop_num == prop:
                if prop_len == 1:
                    self.mem.w8(addr + 1, val & 0xFF)
                else:
                    self.mem.w16(addr + 1, val & 0xFFFF)
                return
            addr += 1 + prop_len

    def get_prop_addr_by_num(self, obj: int, prop: int) -> int:
        """Get address of property data (0 if not found)"""
        prop_addr = self.get_prop_addr(obj)
        name_len = self.mem.u8(prop_addr)
        addr = prop_addr + 1 + name_len * 2

        while True:
            size_byte = self.mem.u8(addr)
            if size_byte == 0:
                return 0
            prop_num = size_byte & 0x1F
            prop_len = (size_byte >> 5) + 1
            if prop_num == prop:
                return addr + 1  # Data starts after size byte
            addr += 1 + prop_len
            if prop_num < prop:
                return 0
        return 0

    def remove_obj(self, obj: int):
        """Remove object from its parent"""
        parent = self.get_parent(obj)
        if parent == 0:
            return

        # Find and unlink from parent's child list
        child = self.get_child(parent)
        if child == obj:
            # First child - update parent's child pointer
            self.set_child(parent, self.get_sibling(obj))
        else:
            # Find previous sibling
            while child != 0:
                next_sib = self.get_sibling(child)
                if next_sib == obj:
                    self.set_sibling(child, self.get_sibling(obj))
                    break
                child = next_sib

        self.set_parent(obj, 0)
        self.set_sibling(obj, 0)

    def insert_obj(self, obj: int, dest: int):
        """Insert object as first child of destination"""
        self.remove_obj(obj)
        self.set_sibling(obj, self.get_child(dest))
        self.set_child(dest, obj)
        self.set_parent(obj, dest)

# =============================================================================
# TEXT ENCODING/DECODING
# =============================================================================

class Text:
    """Z-Machine text handling - maps to ZCL_ZORK_00_DECODER"""

    def __init__(self, mem: Memory):
        self.mem = mem

    def decode(self, addr: int) -> tuple:
        """Decode Z-string at address, returns (text, bytes_consumed)"""
        result = []
        alphabet = 0  # 0=A0, 1=A1, 2=A2
        abbrev_mode = 0
        zscii_mode = 0
        zscii_char = 0
        start_addr = addr

        while True:
            word = self.mem.u16(addr)
            addr += 2

            # Extract 3 5-bit Z-characters
            zchars = [
                (word >> 10) & 0x1F,
                (word >> 5) & 0x1F,
                word & 0x1F
            ]

            for zc in zchars:
                if abbrev_mode:
                    # Abbreviation: lookup and expand
                    abbrev_num = (abbrev_mode - 1) * 32 + zc
                    abbrev_addr = self.mem.u16(self.mem.abbrev_addr + abbrev_num * 2) * 2
                    text, _ = self.decode(abbrev_addr)
                    result.append(text)
                    abbrev_mode = 0
                elif zscii_mode == 1:
                    # First half of 10-bit ZSCII
                    zscii_char = zc << 5
                    zscii_mode = 2
                elif zscii_mode == 2:
                    # Second half of 10-bit ZSCII
                    zscii_char |= zc
                    result.append(chr(zscii_char))
                    zscii_mode = 0
                elif zc == 0:
                    result.append(' ')
                elif zc in (1, 2, 3):
                    # Abbreviation marker
                    abbrev_mode = zc
                elif zc == 4:
                    # Shift to A1
                    alphabet = 1
                elif zc == 5:
                    # Shift to A2
                    alphabet = 2
                elif zc == 6 and alphabet == 2:
                    # 10-bit ZSCII follows
                    zscii_mode = 1
                    alphabet = 0
                else:
                    # Regular character (zchars 6-31 -> index 0-25)
                    idx = zc - 6
                    if idx >= 0 and idx < 26:
                        if alphabet == 0:
                            result.append(A0[idx])
                        elif alphabet == 1:
                            result.append(A1[idx])
                        else:
                            result.append(A2[idx])
                    alphabet = 0  # Reset shift

            if word & 0x8000:  # End bit set
                break

        return (''.join(result), addr - start_addr)

    def encode_for_dict(self, text: str) -> list:
        """Encode text for dictionary lookup (V3: 6 chars -> 2 words)"""
        text = text.lower()[:6]  # V3: max 6 chars

        zchars = []
        for ch in text:
            if ch in A0:
                zchars.append(A0.index(ch) + 6)  # Index 0-25 -> zchar 6-31
            elif ch in A2:
                zchars.append(5)  # Shift to A2
                zchars.append(A2.index(ch) + 6)  # Index 0-25 -> zchar 6-31
            else:
                # 10-bit ZSCII (rare)
                zchars.append(5)
                zchars.append(6)
                zchars.append(ord(ch) >> 5)
                zchars.append(ord(ch) & 0x1F)

        # Pad to 6 Z-characters with 5 (pad character)
        while len(zchars) < 6:
            zchars.append(5)
        zchars = zchars[:6]  # Truncate if needed

        # Pack into 2 words
        words = []
        for i in range(0, 6, 3):
            word = (zchars[i] << 10) | (zchars[i+1] << 5) | zchars[i+2]
            words.append(word)
        words[-1] |= 0x8000  # Set end bit

        return words

# =============================================================================
# DICTIONARY
# =============================================================================

class Dictionary:
    """Z-Machine dictionary - used for tokenization"""

    def __init__(self, mem: Memory, text: Text):
        self.mem = mem
        self.text = text
        self.base = mem.dict_addr

        # Parse dictionary header
        self.num_separators = mem.u8(self.base)
        self.separators = [mem.u8(self.base + 1 + i) for i in range(self.num_separators)]

        header_size = 1 + self.num_separators
        self.entry_length = mem.u8(self.base + header_size)
        self.num_entries = mem.u16(self.base + header_size + 1)
        self.entries_addr = self.base + header_size + 3

    def lookup(self, word: str) -> int:
        """Find word in dictionary, returns address or 0"""
        encoded = self.text.encode_for_dict(word)

        # Binary search (dictionary is sorted)
        lo, hi = 0, abs(self.num_entries) - 1
        while lo <= hi:
            mid = (lo + hi) // 2
            addr = self.entries_addr + mid * self.entry_length

            # Compare encoded words
            entry = [self.mem.u16(addr), self.mem.u16(addr + 2)]

            if encoded == entry:
                return addr
            elif encoded < entry:
                hi = mid - 1
            else:
                lo = mid + 1

        return 0

    def tokenize(self, text: str, text_buf: int, parse_buf: int):
        """Tokenize input text into parse buffer"""
        max_words = self.mem.u8(parse_buf)
        words = []

        # Split on spaces and separators
        i = 0
        while i < len(text) and len(words) < max_words:
            # Skip spaces
            while i < len(text) and text[i] == ' ':
                i += 1
            if i >= len(text):
                break

            # Check for separator
            if ord(text[i]) in self.separators:
                words.append((text[i], i, 1))
                i += 1
                continue

            # Collect word
            start = i
            while i < len(text) and text[i] != ' ' and ord(text[i]) not in self.separators:
                i += 1
            words.append((text[start:i], start, i - start))

        # Write parse buffer
        # Byte 0: max words (already there)
        # Byte 1: actual word count
        # Bytes 2+: 4 bytes per word (addr:2, len:1, pos:1)
        self.mem.w8(parse_buf + 1, len(words))

        for idx, (word, pos, length) in enumerate(words):
            addr = self.lookup(word)
            entry_addr = parse_buf + 2 + idx * 4
            self.mem.w16(entry_addr, addr)
            self.mem.w8(entry_addr + 2, length)
            self.mem.w8(entry_addr + 3, pos + 1)  # 1-based position in V3

# =============================================================================
# Z-MACHINE INTERPRETER
# =============================================================================

class ZMachine:
    """Main Z-Machine interpreter"""

    def __init__(self, data: bytes):
        self.mem = Memory(data)
        self.stack = Stack()
        self.objects = Objects(self.mem)
        self.text = Text(self.mem)
        self.dictionary = Dictionary(self.mem, self.text)

        self.pc = self.mem.initial_pc
        self.running = True
        self.output_buffer = ""

    # -------------------------------------------------------------------------
    # Variable access
    # -------------------------------------------------------------------------

    def get_var(self, var: int) -> int:
        """Get variable value (0=stack, 1-15=locals, 16-255=globals)"""
        if var == 0:
            return self.stack.pop()
        elif var < 16:
            return self.stack.get_local(var)
        else:
            return self.mem.get_global(var)

    def set_var(self, var: int, val: int):
        """Set variable value"""
        val = val & 0xFFFF
        if var == 0:
            self.stack.push(val)
        elif var < 16:
            self.stack.set_local(var, val)
        else:
            self.mem.set_global(var, val)

    def peek_var(self, var: int) -> int:
        """Get variable without popping stack"""
        if var == 0:
            return self.stack.peek()
        elif var < 16:
            return self.stack.get_local(var)
        else:
            return self.mem.get_global(var)

    # -------------------------------------------------------------------------
    # Instruction decoding
    # -------------------------------------------------------------------------

    def read_byte(self) -> int:
        val = self.mem.u8(self.pc)
        self.pc += 1
        return val

    def read_word(self) -> int:
        val = self.mem.u16(self.pc)
        self.pc += 2
        return val

    def decode_operands(self, opcode: int, form: str) -> list:
        """Decode instruction operands based on form"""
        operands = []

        if form == 'short':
            op_type = (opcode >> 4) & 0x03
            if op_type == 0:  # Large constant
                operands.append(self.read_word())
            elif op_type == 1:  # Small constant
                operands.append(self.read_byte())
            elif op_type == 2:  # Variable
                operands.append(self.get_var(self.read_byte()))
            # op_type == 3 means 0OP

        elif form == 'long':
            # Two operands, types encoded in bits 6 and 5
            if opcode & 0x40:
                operands.append(self.get_var(self.read_byte()))
            else:
                operands.append(self.read_byte())
            if opcode & 0x20:
                operands.append(self.get_var(self.read_byte()))
            else:
                operands.append(self.read_byte())

        elif form == 'variable':
            # Operand types in next byte(s)
            type_byte = self.read_byte()
            for shift in [6, 4, 2, 0]:
                op_type = (type_byte >> shift) & 0x03
                if op_type == 0:
                    operands.append(self.read_word())
                elif op_type == 1:
                    operands.append(self.read_byte())
                elif op_type == 2:
                    operands.append(self.get_var(self.read_byte()))
                else:  # 3 = omitted
                    break

        return operands

    def read_branch(self) -> tuple:
        """Read branch offset, returns (branch_on_true, offset)"""
        byte1 = self.read_byte()
        branch_on = bool(byte1 & 0x80)

        if byte1 & 0x40:  # Single byte offset
            offset = byte1 & 0x3F
        else:  # Two byte offset (signed)
            byte2 = self.read_byte()
            offset = ((byte1 & 0x3F) << 8) | byte2
            if offset & 0x2000:  # Sign extend
                offset = offset - 0x4000

        return (branch_on, offset)

    def do_branch(self, condition: bool, branch_on: bool, offset: int):
        """Execute branch if condition matches"""
        if condition == branch_on:
            if offset == 0:
                self.do_return(0)
            elif offset == 1:
                self.do_return(1)
            else:
                self.pc = self.pc + offset - 2

    def do_return(self, val: int):
        """Return from current routine"""
        return_pc, return_var, _ = self.stack.ret(val)
        self.pc = return_pc
        if return_var is not None:
            self.set_var(return_var, val)

    # -------------------------------------------------------------------------
    # Output
    # -------------------------------------------------------------------------

    def print_text(self, text: str):
        """Print text to output"""
        for ch in text:
            if ch == '\n':
                print(self.output_buffer)
                self.output_buffer = ""
            else:
                self.output_buffer += ch

    def flush_output(self):
        """Flush output buffer"""
        if self.output_buffer:
            print(self.output_buffer, end='')
            self.output_buffer = ""

    # -------------------------------------------------------------------------
    # Main execution loop
    # -------------------------------------------------------------------------

    def step(self):
        """Execute one instruction"""
        opcode = self.read_byte()

        # Determine form and operation
        if opcode == 0xBE:  # Extended (V5+, not in V3)
            raise RuntimeError("Extended opcodes not supported in V3")
        elif opcode & 0x80 == 0:  # Long form (2OP)
            form = 'long'
            op_num = opcode & 0x1F
            operands = self.decode_operands(opcode, form)
            self.execute_2op(op_num, operands)
        elif opcode & 0xC0 == 0x80:  # Short form
            form = 'short'
            op_type = (opcode >> 4) & 0x03
            op_num = opcode & 0x0F
            if op_type == 3:  # 0OP
                self.execute_0op(op_num)
            else:  # 1OP
                operands = self.decode_operands(opcode, form)
                self.execute_1op(op_num, operands)
        else:  # Variable form
            form = 'variable'
            if opcode & 0x20:  # VAR
                op_num = opcode & 0x1F
                operands = self.decode_operands(opcode, form)
                self.execute_var(op_num, operands)
            else:  # 2OP in variable form
                op_num = opcode & 0x1F
                operands = self.decode_operands(opcode, form)
                self.execute_2op(op_num, operands)

    def run(self):
        """Main execution loop"""
        while self.running:
            self.step()

    # -------------------------------------------------------------------------
    # Opcode implementations
    # -------------------------------------------------------------------------

    def execute_0op(self, op: int):
        """Execute 0OP instruction"""
        if op == 0x00:  # rtrue
            self.do_return(1)
        elif op == 0x01:  # rfalse
            self.do_return(0)
        elif op == 0x02:  # print
            text, length = self.text.decode(self.pc)
            self.pc += length
            self.print_text(text)
        elif op == 0x03:  # print_ret
            text, length = self.text.decode(self.pc)
            self.pc += length
            self.print_text(text + '\n')
            self.do_return(1)
        elif op == 0x04:  # nop
            pass
        elif op == 0x05:  # save (V3: branch)
            branch_on, offset = self.read_branch()
            # Simplified: always fail in this minimal implementation
            self.do_branch(False, branch_on, offset)
        elif op == 0x06:  # restore (V3: branch)
            branch_on, offset = self.read_branch()
            # Simplified: always fail
            self.do_branch(False, branch_on, offset)
        elif op == 0x07:  # restart
            raise RuntimeError("RESTART not implemented")
        elif op == 0x08:  # ret_popped
            self.do_return(self.stack.pop())
        elif op == 0x09:  # pop (V3) / catch (V5+)
            self.stack.pop()
        elif op == 0x0A:  # quit
            self.flush_output()
            self.running = False
        elif op == 0x0B:  # new_line
            self.print_text('\n')
        elif op == 0x0C:  # show_status
            pass  # Status line display (V3)
        elif op == 0x0D:  # verify
            branch_on, offset = self.read_branch()
            # Simplified: always pass
            self.do_branch(True, branch_on, offset)
        else:
            raise RuntimeError(f"Unknown 0OP: {op:02X}")

    def execute_1op(self, op: int, operands: list):
        """Execute 1OP instruction"""
        a = operands[0]

        if op == 0x00:  # jz
            branch_on, offset = self.read_branch()
            self.do_branch(a == 0, branch_on, offset)
        elif op == 0x01:  # get_sibling
            store = self.read_byte()
            sibling = self.objects.get_sibling(a)
            self.set_var(store, sibling)
            branch_on, offset = self.read_branch()
            self.do_branch(sibling != 0, branch_on, offset)
        elif op == 0x02:  # get_child
            store = self.read_byte()
            child = self.objects.get_child(a)
            self.set_var(store, child)
            branch_on, offset = self.read_branch()
            self.do_branch(child != 0, branch_on, offset)
        elif op == 0x03:  # get_parent
            store = self.read_byte()
            self.set_var(store, self.objects.get_parent(a))
        elif op == 0x04:  # get_prop_len
            store = self.read_byte()
            self.set_var(store, self.objects.get_prop_len(a))
        elif op == 0x05:  # inc
            var = a
            val = to_signed(self.peek_var(var)) + 1
            self.set_var(var, val)
        elif op == 0x06:  # dec
            var = a
            val = to_signed(self.peek_var(var)) - 1
            self.set_var(var, val)
        elif op == 0x07:  # print_addr
            text, _ = self.text.decode(a)
            self.print_text(text)
        elif op == 0x08:  # call_1s (V4+, but handle for compat)
            raise RuntimeError("CALL_1S not in V3")
        elif op == 0x09:  # remove_obj
            self.objects.remove_obj(a)
        elif op == 0x0A:  # print_obj
            name_addr = self.objects.get_name(a)
            if name_addr:
                text, _ = self.text.decode(name_addr)
                self.print_text(text)
        elif op == 0x0B:  # ret
            self.do_return(a)
        elif op == 0x0C:  # jump
            offset = to_signed(a)
            self.pc = self.pc + offset - 2
        elif op == 0x0D:  # print_paddr
            addr = a * 2  # V3: multiply by 2
            text, _ = self.text.decode(addr)
            self.print_text(text)
        elif op == 0x0E:  # load
            store = self.read_byte()
            if a == 0:
                self.set_var(store, self.stack.peek())
            else:
                self.set_var(store, self.peek_var(a))
        elif op == 0x0F:  # not (V3) / call_1n (V5+)
            store = self.read_byte()
            self.set_var(store, (~a) & 0xFFFF)
        else:
            raise RuntimeError(f"Unknown 1OP: {op:02X}")

    def execute_2op(self, op: int, operands: list):
        """Execute 2OP instruction"""
        if len(operands) < 2:
            operands.append(0)  # Handle missing operand
        a, b = operands[0], operands[1]

        if op == 0x01:  # je
            branch_on, offset = self.read_branch()
            result = a == b
            if len(operands) > 2:
                result = result or a == operands[2]
            if len(operands) > 3:
                result = result or a == operands[3]
            self.do_branch(result, branch_on, offset)
        elif op == 0x02:  # jl
            branch_on, offset = self.read_branch()
            self.do_branch(to_signed(a) < to_signed(b), branch_on, offset)
        elif op == 0x03:  # jg
            branch_on, offset = self.read_branch()
            self.do_branch(to_signed(a) > to_signed(b), branch_on, offset)
        elif op == 0x04:  # dec_chk
            var = a
            val = to_signed(self.peek_var(var)) - 1
            self.set_var(var, val)
            branch_on, offset = self.read_branch()
            self.do_branch(val < to_signed(b), branch_on, offset)
        elif op == 0x05:  # inc_chk
            var = a
            val = to_signed(self.peek_var(var)) + 1
            self.set_var(var, val)
            branch_on, offset = self.read_branch()
            self.do_branch(val > to_signed(b), branch_on, offset)
        elif op == 0x06:  # jin
            branch_on, offset = self.read_branch()
            self.do_branch(self.objects.get_parent(a) == b, branch_on, offset)
        elif op == 0x07:  # test
            branch_on, offset = self.read_branch()
            self.do_branch((a & b) == b, branch_on, offset)
        elif op == 0x08:  # or
            store = self.read_byte()
            self.set_var(store, a | b)
        elif op == 0x09:  # and
            store = self.read_byte()
            self.set_var(store, a & b)
        elif op == 0x0A:  # test_attr
            branch_on, offset = self.read_branch()
            self.do_branch(self.objects.get_attr(a, b), branch_on, offset)
        elif op == 0x0B:  # set_attr
            self.objects.set_attr(a, b)
        elif op == 0x0C:  # clear_attr
            self.objects.clear_attr(a, b)
        elif op == 0x0D:  # store
            self.set_var(a, b)
        elif op == 0x0E:  # insert_obj
            self.objects.insert_obj(a, b)
        elif op == 0x0F:  # loadw
            store = self.read_byte()
            addr = (a + b * 2) & 0xFFFF
            self.set_var(store, self.mem.u16(addr))
        elif op == 0x10:  # loadb
            store = self.read_byte()
            addr = (a + b) & 0xFFFF
            self.set_var(store, self.mem.u8(addr))
        elif op == 0x11:  # get_prop
            store = self.read_byte()
            self.set_var(store, self.objects.get_prop(a, b))
        elif op == 0x12:  # get_prop_addr
            store = self.read_byte()
            self.set_var(store, self.objects.get_prop_addr_by_num(a, b))
        elif op == 0x13:  # get_next_prop
            store = self.read_byte()
            self.set_var(store, self.objects.get_next_prop(a, b))
        elif op == 0x14:  # add
            store = self.read_byte()
            self.set_var(store, (to_signed(a) + to_signed(b)) & 0xFFFF)
        elif op == 0x15:  # sub
            store = self.read_byte()
            self.set_var(store, (to_signed(a) - to_signed(b)) & 0xFFFF)
        elif op == 0x16:  # mul
            store = self.read_byte()
            self.set_var(store, (to_signed(a) * to_signed(b)) & 0xFFFF)
        elif op == 0x17:  # div
            store = self.read_byte()
            if b == 0:
                raise RuntimeError("Division by zero")
            result = int(to_signed(a) / to_signed(b))
            self.set_var(store, result & 0xFFFF)
        elif op == 0x18:  # mod
            store = self.read_byte()
            if b == 0:
                raise RuntimeError("Division by zero")
            # Python's % follows floored division, Z-machine uses truncated
            sa, sb = to_signed(a), to_signed(b)
            result = sa - int(sa / sb) * sb
            self.set_var(store, result & 0xFFFF)
        else:
            raise RuntimeError(f"Unknown 2OP: {op:02X}")

    def execute_var(self, op: int, operands: list):
        """Execute VAR instruction"""
        if op == 0x00:  # call
            if operands[0] == 0:
                store = self.read_byte()
                self.set_var(store, 0)
                return

            routine_addr = operands[0] * 2  # V3: multiply by 2
            store = self.read_byte()

            # Read routine header
            num_locals = self.mem.u8(routine_addr)
            routine_addr += 1

            # Create new frame
            frame = self.stack.call(self.pc, store, num_locals)

            # Initialize locals with defaults (V3: 2 bytes each)
            for i in range(num_locals):
                frame.locals[i] = self.mem.u16(routine_addr)
                routine_addr += 2

            # Override with passed arguments
            for i, arg in enumerate(operands[1:]):
                if i < num_locals:
                    frame.locals[i] = arg

            self.pc = routine_addr

        elif op == 0x01:  # storew
            addr = (operands[0] + operands[1] * 2) & 0xFFFF
            self.mem.w16(addr, operands[2])

        elif op == 0x02:  # storeb
            addr = (operands[0] + operands[1]) & 0xFFFF
            self.mem.w8(addr, operands[2])

        elif op == 0x03:  # put_prop
            self.objects.put_prop(operands[0], operands[1], operands[2])

        elif op == 0x04:  # sread (V3: read)
            text_buf = operands[0]
            parse_buf = operands[1]

            # Flush output before reading
            self.flush_output()
            print('>', end=' ')

            try:
                line = input().strip().lower()
            except EOFError:
                self.running = False
                return

            # Write to text buffer
            max_len = self.mem.u8(text_buf)
            line = line[:max_len]
            for i, ch in enumerate(line):
                self.mem.w8(text_buf + 1 + i, ord(ch))
            self.mem.w8(text_buf + 1 + len(line), 0)  # Null terminator

            # Tokenize
            self.dictionary.tokenize(line, text_buf, parse_buf)

        elif op == 0x05:  # print_char
            self.print_text(chr(operands[0]))

        elif op == 0x06:  # print_num
            self.print_text(str(to_signed(operands[0])))

        elif op == 0x07:  # random
            store = self.read_byte()
            import random
            if to_signed(operands[0]) <= 0:
                random.seed(operands[0])
                self.set_var(store, 0)
            else:
                self.set_var(store, random.randint(1, operands[0]))

        elif op == 0x08:  # push
            self.stack.push(operands[0])

        elif op == 0x09:  # pull
            self.set_var(operands[0], self.stack.pop())

        elif op == 0x0A:  # split_window
            pass  # V3 screen splitting

        elif op == 0x0B:  # set_window
            pass  # V3 window selection

        elif op == 0x13:  # output_stream
            pass  # Stream selection

        elif op == 0x14:  # input_stream
            pass  # Input stream selection

        elif op == 0x15:  # sound_effect
            pass  # Sound (not implemented)

        else:
            raise RuntimeError(f"Unknown VAR: {op:02X}")

# =============================================================================
# UTILITIES
# =============================================================================

def to_signed(val: int) -> int:
    """Convert 16-bit unsigned to signed"""
    if val >= 0x8000:
        return val - 0x10000
    return val

# =============================================================================
# MAIN
# =============================================================================

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 z3_minimal.py story.z3")
        sys.exit(1)

    with open(sys.argv[1], 'rb') as f:
        data = f.read()

    print(f"Loaded {len(data)} bytes")

    zm = ZMachine(data)
    print(f"Z-Machine version {zm.mem.version}")
    print(f"Serial: {zm.mem.serial}")
    print(f"Initial PC: 0x{zm.mem.initial_pc:04X}")
    print()

    try:
        zm.run()
    except KeyboardInterrupt:
        print("\n\n[Interrupted]")
    except Exception as e:
        print(f"\n\nError at PC=0x{zm.pc:04X}: {e}")
        raise

if __name__ == '__main__':
    main()
