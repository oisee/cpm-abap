"""
The Hobbit (1982) ZX Spectrum emulator
Text-only mode - intercepts print routines and outputs to console

Based on:
- Melbourne House "The Hobbit" (1982) for ZX Spectrum 48K
- TAP file format from Sinclair Wiki
- Routine addresses from icemark.com/robin/hobbit/

Key intercept points:
  0x867A - Print Character (outputs character in A register)
  0x7F78 - Drawing Routine (vector graphics - skip in text mode)
  0x8B81 - Get Key (keyboard input)
"""

import struct
import sys
import os
from typing import List, Optional, Callable, Tuple
from dataclasses import dataclass

# Import the Z80 emulator
from z80 import Z80, Bus, FLAG_C, FLAG_Z


@dataclass
class TAPBlock:
    """A single block from a TAP file"""
    flag: int           # 0x00 = header, 0xFF = data
    data: bytes         # Block data (without flag and checksum)
    block_type: str     # Description
    name: str           # Block name (from header)
    param1: int         # First parameter
    param2: int         # Second parameter


def parse_tap_file(filepath: str) -> List[TAPBlock]:
    """
    Parse a ZX Spectrum TAP file

    TAP format:
    - 2 bytes: block length (little endian)
    - 1 byte: flag (0x00 = header, 0xFF = data)
    - N-2 bytes: data
    - 1 byte: checksum (XOR of flag and data)

    Header format (17 bytes of data):
    - Byte 0: type (0=program, 1=num array, 2=char array, 3=code)
    - Bytes 1-10: filename (padded with spaces)
    - Bytes 11-12: data length
    - Bytes 13-14: param1 (autostart line for BASIC, start addr for code)
    - Bytes 15-16: param2 (variable offset for BASIC, unused for code)
    """
    blocks = []

    with open(filepath, 'rb') as f:
        data = f.read()

    offset = 0
    block_num = 0
    pending_header = None

    while offset < len(data) - 2:
        # Read block length
        block_len = struct.unpack_from('<H', data, offset)[0]
        offset += 2

        if block_len == 0 or offset + block_len > len(data):
            break

        # Read block
        block_data = data[offset:offset + block_len]
        offset += block_len

        if len(block_data) < 2:
            continue

        flag = block_data[0]
        payload = block_data[1:-1]  # Exclude flag and checksum
        checksum = block_data[-1]

        # Verify checksum
        calc_check = flag
        for b in payload:
            calc_check ^= b

        block_type = "unknown"
        name = ""
        param1 = 0
        param2 = 0

        if flag == 0x00 and len(payload) >= 17:
            # Header block
            header_type = payload[0]
            name = bytes(payload[1:11]).decode('ascii', errors='replace').strip()
            data_len = struct.unpack_from('<H', payload, 11)[0]
            param1 = struct.unpack_from('<H', payload, 13)[0]
            param2 = struct.unpack_from('<H', payload, 15)[0]

            type_names = {0: "Program", 1: "Number array", 2: "Character array", 3: "Code"}
            block_type = f"Header: {type_names.get(header_type, 'Unknown')} '{name}'"
            pending_header = (header_type, name, data_len, param1, param2)

        elif flag == 0xFF:
            # Data block
            if pending_header:
                h_type, h_name, h_len, h_p1, h_p2 = pending_header
                name = h_name
                param1 = h_p1
                param2 = h_p2
                block_type = f"Data for '{h_name}' ({len(payload)} bytes)"
                pending_header = None
            else:
                block_type = f"Data block ({len(payload)} bytes)"

        blocks.append(TAPBlock(
            flag=flag,
            data=payload,
            block_type=block_type,
            name=name,
            param1=param1,
            param2=param2
        ))
        block_num += 1

    return blocks


class VirtualScreen:
    """
    Virtual framebuffer for rendering Hobbit graphics
    Spectrum resolution: 256x176 pixels
    Output: Unicode block characters
    """
    WIDTH = 256
    HEIGHT = 176

    # Unicode block characters for 2x2 pixel blocks
    # Each char represents a 2x2 pixel pattern
    BLOCKS = {
        0b0000: ' ',   # Empty
        0b0001: '▗',   # Bottom-right
        0b0010: '▖',   # Bottom-left
        0b0011: '▄',   # Bottom half
        0b0100: '▝',   # Top-right
        0b0101: '▐',   # Right half
        0b0110: '▞',   # Diagonal
        0b0111: '▟',   # All but top-left
        0b1000: '▘',   # Top-left
        0b1001: '▚',   # Diagonal
        0b1010: '▌',   # Left half
        0b1011: '▙',   # All but top-right
        0b1100: '▀',   # Top half
        0b1101: '▜',   # All but bottom-left
        0b1110: '▛',   # All but bottom-right
        0b1111: '█',   # Full block
    }

    def __init__(self):
        # 1-bit framebuffer (black/white)
        self.pixels = [[0] * self.WIDTH for _ in range(self.HEIGHT)]
        self.pen_x = 0
        self.pen_y = 0

    def clear(self):
        """Clear framebuffer"""
        for y in range(self.HEIGHT):
            for x in range(self.WIDTH):
                self.pixels[y][x] = 0

    def set_pixel(self, x: int, y: int, color: int = 1):
        """Set a pixel"""
        if 0 <= x < self.WIDTH and 0 <= y < self.HEIGHT:
            self.pixels[y][x] = color

    def draw_line(self, x1: int, y1: int, x2: int, y2: int):
        """Draw a line using Bresenham's algorithm"""
        dx = abs(x2 - x1)
        dy = abs(y2 - y1)
        sx = 1 if x1 < x2 else -1
        sy = 1 if y1 < y2 else -1
        err = dx - dy

        while True:
            self.set_pixel(x1, y1)
            if x1 == x2 and y1 == y2:
                break
            e2 = 2 * err
            if e2 > -dy:
                err -= dy
                x1 += sx
            if e2 < dx:
                err += dx
                y1 += sy

    def move_to(self, x: int, y: int):
        """Move pen without drawing"""
        self.pen_x = x
        self.pen_y = y

    def line_to(self, x: int, y: int):
        """Draw line from current position"""
        self.draw_line(self.pen_x, self.pen_y, x, y)
        self.pen_x = x
        self.pen_y = y

    def render_to_text(self, scale: int = 2) -> str:
        """
        Render framebuffer to Unicode block characters
        scale=2 means 2x2 pixels per character (128x88 chars)
        scale=4 means 4x4 pixels per character (64x44 chars)
        """
        lines = []
        char_height = self.HEIGHT // scale
        char_width = self.WIDTH // scale

        for cy in range(0, self.HEIGHT, scale):
            line = ""
            for cx in range(0, self.WIDTH, scale):
                if scale == 2:
                    # 2x2 pixel block
                    tl = self.pixels[cy][cx] if cy < self.HEIGHT and cx < self.WIDTH else 0
                    tr = self.pixels[cy][cx+1] if cy < self.HEIGHT and cx+1 < self.WIDTH else 0
                    bl = self.pixels[cy+1][cx] if cy+1 < self.HEIGHT and cx < self.WIDTH else 0
                    br = self.pixels[cy+1][cx+1] if cy+1 < self.HEIGHT and cx+1 < self.WIDTH else 0
                    pattern = (tl << 3) | (tr << 2) | (bl << 1) | br
                    line += self.BLOCKS.get(pattern, ' ')
                else:
                    # Larger scale - check if any pixel is set
                    has_pixel = False
                    for dy in range(scale):
                        for dx in range(scale):
                            py, px = cy + dy, cx + dx
                            if py < self.HEIGHT and px < self.WIDTH and self.pixels[py][px]:
                                has_pixel = True
                                break
                        if has_pixel:
                            break
                    line += '█' if has_pixel else ' '
            lines.append(line.rstrip())

        # Remove trailing empty lines
        while lines and not lines[-1]:
            lines.pop()

        return '\n'.join(lines)

    def capture_from_spectrum_memory(self, memory: bytearray):
        """
        Capture graphics from ZX Spectrum screen memory (0x4000-0x57FF)
        Spectrum uses a complex address layout:
        Address = 0x4000 + (y7y6)*2048 + (y2y1y0)*256 + (y5y4y3)*32 + x_byte
        """
        self.clear()
        for y in range(min(192, self.HEIGHT)):
            y7y6 = (y >> 6) & 0x03
            y5y4y3 = (y >> 3) & 0x07
            y2y1y0 = y & 0x07
            for x_byte in range(32):
                addr = 0x4000 + (y7y6 * 2048) + (y2y1y0 * 256) + (y5y4y3 * 32) + x_byte
                byte = memory[addr]
                for bit in range(8):
                    if byte & (0x80 >> bit):
                        self.set_pixel(x_byte * 8 + bit, y)


class SpectrumBus(Bus):
    """
    ZX Spectrum 48K memory map:
    0x0000 - 0x3FFF: ROM (16KB)
    0x4000 - 0x57FF: Screen memory (6KB)
    0x5800 - 0x5AFF: Attributes (768 bytes)
    0x5B00 - 0xFFFF: RAM (41KB)

    I/O Ports:
    0xFE: Keyboard/border - we hook at GetKey level instead
    """

    def __init__(self):
        super().__init__()
        self.rom = bytearray(16384)  # 16KB ROM
        self.rom_loaded = False
        self.watchpoints = set()  # Memory write watchpoints
        self.watch_callback = None  # Callback for watchpoint hits

    def load_rom(self, data: bytes):
        """Load Spectrum ROM at 0x0000"""
        for i, b in enumerate(data[:16384]):
            self.rom[i] = b
        self.rom_loaded = True

    def read_mem(self, addr: int) -> int:
        addr = addr & 0xFFFF
        if addr < 0x4000 and self.rom_loaded:
            return self.rom[addr]
        return self.memory[addr]

    def write_mem(self, addr: int, val: int):
        addr = addr & 0xFFFF
        # Check for watchpoints
        if addr in self.watchpoints and self.watch_callback:
            old_val = self.memory[addr]
            self.watch_callback(addr, old_val, val & 0xFF)
        # ROM is read-only
        if addr >= 0x4000:
            self.memory[addr] = val & 0xFF

    def read_io(self, port: int) -> int:
        """Read from I/O port - keyboard handled by GetKey hook"""
        low_port = port & 0xFF
        if low_port == 0xFE:
            # Keyboard port - return "no key pressed" (all bits high)
            # Actual input is handled by GetKey hook at 0x8B93
            return 0xFF
        return 0xFF

    def write_io(self, port: int, val: int):
        """Write to I/O port"""
        low_port = port & 0xFF
        if low_port == 0xFE:
            # Border color (bits 0-2) - ignore for text mode
            pass


class HobbitEmulator:
    """
    The Hobbit game emulator with routine interception

    Key addresses (from The Hobbit disassembly - pobtastic/hobbit):
    - 0x867A: Print character routine (char in A)
    - 0x7F78: Drawing/graphics routine
    - 0x8B93: GetKey - main keyboard input (returns char in A, 0 if none)
    - 0x969A: WaitForKey2 - wait for any key press
    - 0x6C00: Game entry point (from RANDOMIZE USR 27648)
    - 0x5CB0: Game state variables start
    """

    # Hobbit-specific addresses (from disassembly - pobtastic/hobbit)
    ENTRY_POINT = 0x6C00   # Game entry (RANDOMIZE USR 27648)
    PRINT_CHAR = 0x867A    # Print character (char in A)
    DRAW_ROUTINE = 0x7F78  # Graphics routine
    PRINT_PROP_CHAR = 0x87C9  # PrintPropChar - graphical font rendering
    PRINT_NEWLINE = 0x8583 # Print newline routine (sends 0x0D)
    PRINT_MSG = 0x72DD     # PrintMsg - main text message routine
    GET_KEY = 0x8B93       # GetKey - returns ASCII in A (0 if no key)
    WAIT_FOR_KEY = 0x969A  # WaitForKey2 - busy wait for any keypress
    RESTART_KEYPRESS = 0x6C6D  # Initial "press any key" wait at startup

    # Inline text pattern (EX (SP),HL / CALL 72DD / RET)
    INLINE_TEXT_CALL = 0x8E81  # CALL $72DD in inline text pattern
    INLINE_TEXT_RET = 0x8E84   # RET after inline text

    def __init__(self):
        self.bus = SpectrumBus()
        self.cpu = Z80(self.bus)

        # Output buffer for text
        self.output_buffer = ""
        self.text_buffer = []

        # Input handling - queue commands here
        self.input_queue = ""
        self.waiting_for_input = False
        self.auto_commands = []  # Pre-queued commands to execute

        # Execution control
        self.running = True
        self.debug = False
        self.trace = False  # Instruction tracing

        # Hook table: address -> handler function
        self.hooks = {}
        self._setup_default_hooks()

        # Statistics
        self.instruction_count = 0
        self.print_count = 0

        # Text output tracking
        self.current_column = 0  # Track column for stripping leading spaces
        self.skip_leading_spaces = True  # Skip spaces at start of line
        self.leading_spaces = 0  # Count leading spaces
        self.last_was_newline = True  # Track consecutive newlines

        # Graphics mode
        self.show_graphics_placeholder = False  # Show "[Graphics: X]" for locations
        self.render_graphics = False  # Render actual graphics
        self.screen = VirtualScreen()  # Virtual framebuffer
        self.graphics_scale = 2  # Scale for text rendering (2=128x88, 4=64x44)

        # Debug tracking for memory corruption
        self.last_pc_history = []  # Last N PC values

    def add_watchpoint(self, addr: int):
        """Add memory write watchpoint"""
        self.bus.watchpoints.add(addr)
        self.bus.watch_callback = self._on_watchpoint

    def _on_watchpoint(self, addr: int, old_val: int, new_val: int):
        """Called when a watchpoint is hit"""
        pc = self.cpu.pc
        print(f"\n[WATCHPOINT] 0x{addr:04X}: 0x{old_val:02X} -> 0x{new_val:02X} at PC=0x{pc:04X}")
        print(f"  Recent PCs: {[hex(p) for p in self.last_pc_history[-10:]]}")
        print(f"  HL=0x{self.cpu.hl:04X} BC=0x{self.cpu.bc:04X} DE=0x{self.cpu.de:04X}")
        print(f"  A=0x{self.cpu.a:02X} SP=0x{self.cpu.sp:04X}")

    def _setup_default_hooks(self):
        """Set up default routine hooks"""
        self.hooks[self.PRINT_CHAR] = self._hook_print_char
        self.hooks[self.DRAW_ROUTINE] = self._hook_draw_routine
        self.hooks[self.PRINT_PROP_CHAR] = self._hook_print_prop_char
        self.hooks[self.PRINT_NEWLINE] = self._hook_print_newline
        self.hooks[self.PRINT_MSG] = self._hook_print_msg
        self.hooks[self.GET_KEY] = self._hook_get_key
        self.hooks[self.WAIT_FOR_KEY] = self._hook_wait_for_key
        self.hooks[self.RESTART_KEYPRESS] = self._hook_restart_keypress

    def _hook_print_char(self) -> bool:
        """
        Hook for 0x867A - Print Character
        Character to print is in A register
        Returns True if handled (skip original code)
        """
        char = self.cpu.a
        self.print_count += 1

        if self.debug:
            print(f"[CHR:{char:02X}='{chr(char) if 32<=char<127 else '?'}']", end='', flush=True)

        # Handle control characters
        if char == 13 or char == 10:  # CR/LF
            self.output_buffer += '\n'
            print()
            self.current_column = 0
            self.skip_leading_spaces = True
            self.leading_spaces = 0
        elif char == 8:  # Backspace
            if self.output_buffer:
                self.output_buffer = self.output_buffer[:-1]
                print('\b \b', end='', flush=True)
            if self.current_column > 0:
                self.current_column -= 1
        elif char == 22:  # AT control (Spectrum specific) - next 2 bytes are row,col
            # Skip this - it's a positioning command
            pass
        elif char == 23:  # TAB control
            pass
        elif char == 32:  # Space - use same logic as PrintPropChar
            self.leading_spaces += 1
            if self.skip_leading_spaces:
                pass  # Skip leading spaces
            elif self.leading_spaces >= 8 and self.current_column > 0:
                # Column positioning - convert to newline
                self.output_buffer += '\n'
                print(flush=True)
                self.current_column = 0
                self.leading_spaces = 0
                self.skip_leading_spaces = True
            elif self.leading_spaces == 1:
                pass  # Wait to see if more spaces follow
            # else: accumulating spaces
        elif char > 32 and char < 127:
            # Non-space printable - output any pending single spaces first
            if self.leading_spaces >= 1 and self.leading_spaces < 8 and not self.skip_leading_spaces:
                self.output_buffer += ' '
                print(' ', end='', flush=True)
                self.current_column += 1
            self.leading_spaces = 0
            self.skip_leading_spaces = False
            # Filter out "+" which appears to be a formatting artifact
            if char != 43:  # Skip "+"
                self.output_buffer += chr(char)
                print(chr(char), end='', flush=True)
                self.current_column += 1
        elif char == 127:  # Delete / copyright symbol
            self.leading_spaces = 0
            self.output_buffer += '(C)'
            print('(C)', end='', flush=True)
            self.current_column += 3
        elif char >= 128 and char <= 143:
            # Spectrum block graphics - show as block
            self.leading_spaces = 0
            self.output_buffer += '#'
            print('#', end='', flush=True)
            self.current_column += 1
        elif char >= 144:
            # UDGs - show placeholder
            self.leading_spaces = 0
            self.output_buffer += '?'
            print('?', end='', flush=True)
            self.current_column += 1
        else:
            # Other control chars - ignore
            if self.debug:
                print(f"[CTRL:{char}]", end='', flush=True)

        sys.stdout.flush()

        # Execute RET to return from routine
        self._do_ret()
        return True

    def _hook_draw_routine(self) -> bool:
        """
        Hook for 0x7F78 - Drawing Routine
        In text-only mode, we skip graphics entirely
        With render_graphics, we capture screen memory after drawing
        Returns True if handled (skip original code)
        """
        # Check if graphics are enabled (B707 != 0)
        if self.bus.read_mem(0xB707) != 0:
            # Location ID is in A register
            loc_id = self.cpu.a

            if self.render_graphics:
                # Let the routine run to draw graphics to screen memory
                # Then capture and render after it returns
                # For now, we can't easily do this without running the routine
                # So just show placeholder and skip
                if self.show_graphics_placeholder:
                    print(f"\n[Graphics: Location {loc_id}]", flush=True)
            elif self.show_graphics_placeholder:
                print(f"\n[Graphics: Location {loc_id}]", flush=True)

        # Skip graphics, just return
        self._do_ret()
        return True

    def capture_screen(self) -> str:
        """Capture current Spectrum screen memory and render to text"""
        self.screen.capture_from_spectrum_memory(self.bus.memory)
        return self.screen.render_to_text(scale=self.graphics_scale)

    def _hook_print_prop_char(self) -> bool:
        """
        Hook for 0x87C9 - PrintPropChar
        Graphical proportional font rendering
        Character to print is in A register

        We capture the character for text output and skip the graphical rendering.
        Returns True if handled (skip original code)
        """
        char = self.cpu.a
        self.print_count += 1

        # Handle printable characters
        if char >= 32 and char < 127:
            if char == 32:
                # Track spaces
                self.leading_spaces += 1

                if self.skip_leading_spaces:
                    # Already skipping spaces (after newline or column positioning)
                    # Skip all leading spaces silently
                    pass
                elif self.leading_spaces >= 8 and self.current_column > 0:
                    # Lots of spaces after text = column positioning, convert to newline
                    self.output_buffer += '\n'
                    print(flush=True)
                    self.current_column = 0
                    self.leading_spaces = 0  # Reset space counter
                    self.skip_leading_spaces = True  # Skip remaining spaces
                    self.last_was_newline = True
                elif self.leading_spaces == 1:
                    # First space - wait to see if more follow
                    pass
                # else: accumulating spaces, keep waiting
            else:
                # Non-space character
                # First output any accumulated single spaces (1-7 spaces between words)
                if self.leading_spaces >= 1 and self.leading_spaces < 8 and not self.skip_leading_spaces:
                    # Small gap between words - output single space
                    self.output_buffer += ' '
                    print(' ', end='', flush=True)
                    self.current_column += 1
                self.leading_spaces = 0
                self.skip_leading_spaces = False  # Found non-space, stop skipping
                self.output_buffer += chr(char)
                print(chr(char), end='', flush=True)
                self.current_column += 1
                self.last_was_newline = False
        elif char == 127:  # Copyright
            self.leading_spaces = 0
            self.output_buffer += '(C)'
            print('(C)', end='', flush=True)
            self.current_column += 3
            self.last_was_newline = False

        # Skip graphical rendering, just return
        self._do_ret()
        return True

    def _hook_print_newline(self) -> bool:
        """
        Hook for 0x8583 - Print Newline routine
        This sends 0x0D to the print system and resets cursor position.

        We output a newline and reset our column counter.
        Skip consecutive blank lines.
        """
        # Only print newline if we printed something on this line
        if self.current_column > 0 or not self.last_was_newline:
            self.output_buffer += '\n'
            print(flush=True)  # Print newline

        self.current_column = 0  # Reset column for next line
        self.skip_leading_spaces = True  # Reset for next line
        self.leading_spaces = 0  # Reset leading space counter
        self.last_was_newline = True  # Track consecutive newlines

        # Let original code run to update internal screen position
        return False

    def _hook_print_msg(self) -> bool:
        """
        Hook for 0x72DD - PrintMsg routine

        DON'T hook this - let the real token decoder run.
        Individual characters will be captured by 0x867A hook.

        Return False to let original code execute.
        """
        # Let the original code run - it will call 867A for each character
        return False

    def _hook_get_key(self) -> bool:
        """
        Hook for 0x8B93 - GetKey routine
        Returns ASCII character in A (0 if no key pressed)

        In HALT mode: blocks until user provides input
        This prevents NPCs from acting while waiting for player input.

        Note: The game has its own ">" prompt, so we don't print one.
        """
        # If we have queued input, return next character
        if self.input_queue:
            char = ord(self.input_queue[0])
            self.input_queue = self.input_queue[1:]
            self.cpu.a = char
            if self.debug:
                ch = chr(char) if 32 <= char < 127 else f'0x{char:02X}'
                print(f"[INPUT:{ch}]", end='', flush=True)
        else:
            # No input queued - block and wait for user input
            if self.auto_commands:
                # Use auto-command
                cmd = self.auto_commands.pop(0)
                print(cmd)  # Game already printed ">"
                self.input_queue = cmd + '\r'
                char = ord(self.input_queue[0])
                self.input_queue = self.input_queue[1:]
                self.cpu.a = char
            else:
                # Wait for user input (blocking)
                # Game prints its own ">" prompt, so we just wait
                try:
                    user_input = input()
                    if user_input:
                        self.input_queue = user_input + '\r'
                        char = ord(self.input_queue[0])
                        self.input_queue = self.input_queue[1:]
                        self.cpu.a = char
                    else:
                        self.cpu.a = 13  # Just Enter
                except (EOFError, KeyboardInterrupt):
                    self.running = False
                    self.cpu.a = 0

        # Execute RET to return from routine
        self._do_ret()
        return True

    def _hook_wait_for_key(self) -> bool:
        """
        Hook for 0x969A - WaitForKey2 routine
        Original code busy-waits until any key is pressed
        We just return immediately (skip the wait)
        Returns True if handled (skip original code)
        """
        if self.debug:
            print("[WAIT_KEY:skip]", end='', flush=True)
        # Just return - no need to wait
        self._do_ret()
        return True

    def _hook_restart_keypress(self) -> bool:
        """
        Hook for 0x6C6D - Initial "press any key" wait at startup
        Original code loops at 0x6C6D-0x6C74 waiting for any key
        We skip directly to 0x6C76 (after the wait)
        Returns True if handled (skip original code)
        """
        if self.debug:
            print("[RESTART:skip_keypress]", end='', flush=True)
        # Skip to after the wait loop
        self.cpu.pc = 0x6C76
        return True

    def _do_ret(self):
        """Execute a RET instruction (pop PC from stack)"""
        self.cpu.pc = self.cpu.pop16()

    def load_tap(self, filepath: str):
        """Load The Hobbit from a TAP file"""
        print(f"Loading TAP file: {filepath}")
        blocks = parse_tap_file(filepath)

        for i, block in enumerate(blocks):
            print(f"  Block {i}: {block.block_type}")
            if block.flag == 0xFF and len(block.data) > 100:
                # This is a data block - check if it's code
                # For The Hobbit, code blocks load at various addresses
                if block.param1 > 0:
                    load_addr = block.param1
                    print(f"    Loading {len(block.data)} bytes at 0x{load_addr:04X}")
                    self.bus.load(load_addr, block.data)

    def load_snapshot(self, addr: int, data: bytes):
        """Load binary data directly at specified address"""
        print(f"Loading {len(data)} bytes at 0x{addr:04X}")
        self.bus.load(addr, data)

    def setup_spectrum_rom_stubs(self):
        """
        Create minimal ROM stubs for essential routines
        The Hobbit doesn't use much ROM, but we need some basics
        """
        # RST 0x00 (0x0000) - Reset
        self.bus.memory[0x0000] = 0xC9  # RET

        # RST 0x08 (0x0008) - Error handler
        self.bus.memory[0x0008] = 0xC9  # RET

        # RST 0x10 (0x0010) - Print character
        # Just return - we hook the game's own print routine
        self.bus.memory[0x0010] = 0xC9  # RET

        # RST 0x18 (0x0018) - Get character
        self.bus.memory[0x0018] = 0xC9  # RET

        # RST 0x20 (0x0020) - Next character
        self.bus.memory[0x0020] = 0xC9  # RET

        # RST 0x28 (0x0028) - Calculator - The Hobbit uses this!
        # Just return (skip calculations)
        self.bus.memory[0x0028] = 0xC9  # RET

        # RST 0x30 (0x0030) - Make BC spaces
        self.bus.memory[0x0030] = 0xC9  # RET

        # RST 0x38 (0x0038) - Maskable interrupt handler (IM 1)
        self.bus.memory[0x0038] = 0xFB  # EI
        self.bus.memory[0x0039] = 0xC9  # RET

        # NMI handler (0x0066)
        self.bus.memory[0x0066] = 0xC9  # RET (NMI handler)

    def step(self) -> int:
        """Execute one instruction with hook checking"""
        pc = self.cpu.pc
        self.instruction_count += 1

        # Track PC history for debugging
        self.last_pc_history.append(pc)
        if len(self.last_pc_history) > 100:
            self.last_pc_history.pop(0)

        # Trace output
        if self.trace:
            op = self.bus.read_mem(pc)
            print(f"[{self.instruction_count:08d}] {pc:04X}: {op:02X}  A={self.cpu.a:02X} HL={self.cpu.hl:04X} SP={self.cpu.sp:04X}")

        # Check for hooks
        if pc in self.hooks:
            if self.debug:
                print(f"[HOOK] 0x{pc:04X}")
            handler = self.hooks[pc]
            if handler():
                return 4  # Handled by hook

        # Normal execution
        return self.cpu.step()

    def queue_command(self, cmd: str):
        """Queue a command to be executed"""
        self.auto_commands.append(cmd)

    def run(self, start_addr: int = None, max_instructions: int = 10000000):
        """Run The Hobbit with blocking input at GetKey"""
        if start_addr is not None:
            self.cpu.pc = start_addr

        self.cpu.sp = 0xFF00  # Set up stack

        print("\n=== The Hobbit (Text Mode) ===")
        print("Type commands and press Enter. Ctrl+C to quit.\n")

        instructions = 0

        try:
            while self.running and not self.cpu.halted and instructions < max_instructions:
                self.step()
                instructions += 1

        except KeyboardInterrupt:
            print("\n[Interrupted]")

        print(f"\n[Executed {instructions} instructions]")
        print(f"[Final PC: 0x{self.cpu.pc:04X}]")
        return instructions

    def disassemble_at(self, addr: int, count: int = 10):
        """Simple disassembly for debugging"""
        mnemonics = {
            0x00: "NOP", 0x01: "LD BC,nn", 0x06: "LD B,n", 0x0E: "LD C,n",
            0x11: "LD DE,nn", 0x16: "LD D,n", 0x1E: "LD E,n",
            0x21: "LD HL,nn", 0x26: "LD H,n", 0x2E: "LD L,n",
            0x31: "LD SP,nn", 0x3E: "LD A,n",
            0x76: "HALT", 0xC3: "JP nn", 0xC9: "RET", 0xCD: "CALL nn",
        }

        for _ in range(count):
            op = self.bus.read_mem(addr)
            mnem = mnemonics.get(op, f"??? ({op:02X})")
            print(f"  0x{addr:04X}: {mnem}")
            addr += 1


def extract_hobbit_from_tap(filepath: str) -> Tuple[dict, bytes]:
    """
    Extract The Hobbit game data from TAP file
    Returns: (info_dict, combined_data)
    """
    blocks = parse_tap_file(filepath)

    info = {
        'filename': os.path.basename(filepath),
        'blocks': len(blocks),
        'code_blocks': []
    }

    # Find code blocks
    i = 0
    while i < len(blocks):
        block = blocks[i]

        if block.flag == 0x00 and 'Code' in block.block_type:
            # Found a code header, next block should be data
            if i + 1 < len(blocks) and blocks[i+1].flag == 0xFF:
                code_info = {
                    'name': block.name,
                    'load_addr': block.param1,
                    'size': len(blocks[i+1].data),
                    'data': blocks[i+1].data
                }
                info['code_blocks'].append(code_info)
                print(f"Found code: '{block.name}' at 0x{block.param1:04X} ({len(blocks[i+1].data)} bytes)")
                i += 2
                continue

        i += 1

    return info, b''


def main():
    """Main entry point"""
    import argparse
    parser = argparse.ArgumentParser(description='The Hobbit ZX Spectrum Emulator (Text Mode)')
    parser.add_argument('tap_file', nargs='?', help='TAP file path')
    parser.add_argument('-d', '--debug', action='store_true', help='Enable debug output')
    parser.add_argument('-t', '--trace', action='store_true', help='Enable instruction trace')
    parser.add_argument('-a', '--analyze', action='store_true', help='Analyze TAP only, do not run')
    parser.add_argument('-g', '--graphics', action='store_true', help='Show graphics placeholders')
    parser.add_argument('-r', '--render', action='store_true', help='Render graphics to Unicode blocks')
    parser.add_argument('-s', '--scale', type=int, default=4, choices=[2, 4], help='Graphics scale (2=128x88, 4=64x44)')
    parser.add_argument('-m', '--max', type=int, default=10000000, help='Max instructions')
    parser.add_argument('-c', '--command', action='append', help='Auto-execute command(s)')
    args = parser.parse_args()

    if not args.tap_file:
        # Try default location
        tap_path = os.path.join(os.path.dirname(__file__), '..', 'docs', 'HOBBIT12.TAP')
        if not os.path.exists(tap_path):
            print("Usage: python hobbit.py <HOBBIT.TAP>")
            print("\nThe Hobbit ZX Spectrum Emulator (Text Mode)")
            print("\nThis emulator intercepts the game's print routines")
            print("and outputs text to the console, skipping graphics.")
            sys.exit(1)
    else:
        tap_path = args.tap_file

    # Analyze TAP file first
    print("=== The Hobbit ZX Spectrum Emulator (Text Mode) ===\n")
    print(f"Loading: {tap_path}\n")

    blocks = parse_tap_file(tap_path)

    if args.analyze:
        print("TAP file structure:\n")
        for i, block in enumerate(blocks):
            print(f"Block {i}: {block.block_type}")
            if block.flag == 0x00:
                print(f"  Name: '{block.name}'")
                print(f"  Param1: 0x{block.param1:04X} ({block.param1})")
                print(f"  Param2: 0x{block.param2:04X} ({block.param2})")
            elif block.flag == 0xFF:
                print(f"  Data size: {len(block.data)} bytes")
                if block.param1 > 0:
                    print(f"  Load address: 0x{block.param1:04X}")
        return

    # Create emulator
    emu = HobbitEmulator()
    emu.debug = args.debug
    emu.trace = args.trace
    emu.show_graphics_placeholder = args.graphics
    emu.render_graphics = args.render
    emu.graphics_scale = args.scale

    # Queue auto-commands (if provided via -c)
    if args.command:
        for cmd in args.command:
            emu.queue_command(cmd)

    # Set up ROM stubs
    emu.setup_spectrum_rom_stubs()

    # Load the TAP file blocks
    for block in blocks:
        if block.flag == 0xFF and len(block.data) > 10:
            # Data block - load at address from previous header
            load_addr = block.param1
            if load_addr > 0:
                print(f"  Loading '{block.name}' ({len(block.data)} bytes) at 0x{load_addr:04X}")
                emu.bus.load(load_addr, block.data)

    # Entry point is 0x6C00 (from RANDOMIZE USR 27648 in BASIC loader)
    entry = HobbitEmulator.ENTRY_POINT
    print(f"\nEntry point: 0x{entry:04X}")

    if args.debug:
        print("\nDisassembly at entry:")
        emu.disassemble_at(entry, 20)

    print("\n" + "="*50)
    print("Starting game... Type commands and press Enter.")
    print("Press Ctrl+C to quit.")
    print("="*50 + "\n")

    # Run the game
    try:
        emu.run(start_addr=entry, max_instructions=args.max)
    except Exception as e:
        print(f"\n[Error: {e}]")
        if args.debug:
            import traceback
            traceback.print_exc()

    print(f"\n[Statistics: {emu.instruction_count} instructions, {emu.print_count} print calls]")


if __name__ == "__main__":
    main()
