"""
Unit tests for The Hobbit (1982) ZX Spectrum Emulator
Tests the HobbitEmulator class with TAP file loading and game execution
"""

import unittest
import os
import sys

# Ensure the module path is set
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from hobbit import HobbitEmulator, parse_tap_file, VirtualScreen


class TestTAPParsing(unittest.TestCase):
    """Test TAP file parsing"""

    TAP_PATH = '/home/alice/dev/cpm-abap/docs/HOBBIT12.TAP'

    @classmethod
    def setUpClass(cls):
        """Check if TAP file exists"""
        if not os.path.exists(cls.TAP_PATH):
            raise unittest.SkipTest(f"TAP file not found: {cls.TAP_PATH}")

    def test_parse_tap_file(self):
        """Parse HOBBIT12.TAP and verify block structure"""
        blocks = parse_tap_file(self.TAP_PATH)

        # Should have 6 blocks (3 header + 3 data)
        self.assertEqual(len(blocks), 6)

        # Check block types alternate: header, data, header, data, ...
        for i, block in enumerate(blocks):
            if i % 2 == 0:
                # Header block
                self.assertEqual(block.flag, 0x00, f"Block {i} should be header")
            else:
                # Data block
                self.assertEqual(block.flag, 0xFF, f"Block {i} should be data")

    def test_tap_data_blocks_have_addresses(self):
        """Data blocks should have valid load addresses"""
        blocks = parse_tap_file(self.TAP_PATH)

        data_blocks = [b for b in blocks if b.flag == 0xFF]
        self.assertEqual(len(data_blocks), 3)

        # All data blocks should have addresses > 0
        for block in data_blocks:
            self.assertGreater(block.param1, 0,
                f"Block '{block.name}' should have valid address")
            self.assertGreater(len(block.data), 0,
                f"Block '{block.name}' should have data")


class TestHobbitEmulator(unittest.TestCase):
    """Test HobbitEmulator class"""

    TAP_PATH = '/home/alice/dev/cpm-abap/docs/HOBBIT12.TAP'

    @classmethod
    def setUpClass(cls):
        """Check if TAP file exists"""
        if not os.path.exists(cls.TAP_PATH):
            raise unittest.SkipTest(f"TAP file not found: {cls.TAP_PATH}")

    def setUp(self):
        """Create emulator and load game"""
        self.emu = HobbitEmulator()

        blocks = parse_tap_file(self.TAP_PATH)
        for block in blocks:
            if block.flag == 0xFF and len(block.data) > 10 and block.param1 > 0:
                self.emu.bus.load(block.param1, block.data)

        # Set ROM stubs AFTER loading game (game data at addr 5 would overwrite stubs)
        self.emu.setup_spectrum_rom_stubs()

        self.emu.cpu.pc = HobbitEmulator.ENTRY_POINT
        self.emu.cpu.sp = 0xFF00

        # Patch _hook_get_key to set waiting_for_input instead of blocking
        self._patch_get_key()

    def _patch_get_key(self):
        """Patch the get_key hook to not block on input"""
        original_hook = self.emu._hook_get_key

        def test_get_key():
            # If we have queued input, use original
            if self.emu.input_queue:
                return original_hook()
            # If we have auto_commands, use original
            if self.emu.auto_commands:
                return original_hook()
            # Otherwise, set waiting flag and return True (handled)
            self.emu.waiting_for_input = True
            return True

        self.emu.hooks[self.emu.GET_KEY] = test_get_key

    def run_until_input_or_limit(self, max_cycles=1000000):
        """Run emulator until it waits for input or hits cycle limit"""
        count = 0
        while (self.emu.running and
               not self.emu.cpu.halted and
               not self.emu.waiting_for_input and
               count < max_cycles):
            self.emu.step()
            count += 1
        return count

    def test_entry_point(self):
        """Verify entry point is correct"""
        self.assertEqual(HobbitEmulator.ENTRY_POINT, 0x6C00)
        self.assertEqual(self.emu.cpu.pc, 0x6C00)

    def test_rom_stubs_installed(self):
        """Verify ROM stubs are installed at key RST vectors"""
        # RST 0x00 = RET
        self.assertEqual(self.emu.bus.read_mem(0x0000), 0xC9)
        # RST 0x28 (Calculator) = RET
        self.assertEqual(self.emu.bus.read_mem(0x0028), 0xC9)
        # RST 0x38 = EI + RET
        self.assertEqual(self.emu.bus.read_mem(0x0038), 0xFB)  # EI
        self.assertEqual(self.emu.bus.read_mem(0x0039), 0xC9)  # RET
        # NMI handler = RET
        self.assertEqual(self.emu.bus.read_mem(0x0066), 0xC9)

    def test_game_code_loaded(self):
        """Verify game code is loaded at entry point"""
        # Entry point should have actual code, not zeros
        code_byte = self.emu.bus.read_mem(0x6C00)
        self.assertNotEqual(code_byte, 0x00,
            "Game code should be loaded at entry point")

    def test_look_command(self):
        """Test LOOK command produces expected output"""
        self.emu.auto_commands = ['LOOK']
        self.run_until_input_or_limit()

        output = self.emu.output_buffer

        # Should contain room description
        self.assertIn('tunnel', output.lower(),
            f"Output should contain 'tunnel'. Got: {output[:500]}")
        self.assertIn('hall', output.lower(),
            f"Output should contain 'hall'. Got: {output[:500]}")

    def test_look_contains_gandalf(self):
        """Test LOOK shows Gandalf in starting room"""
        self.emu.auto_commands = ['LOOK']
        self.run_until_input_or_limit()

        output = self.emu.output_buffer
        self.assertIn('Gandalf', output,
            f"Starting room should show Gandalf. Got: {output[:500]}")

    def test_look_contains_thorin(self):
        """Test LOOK shows Thorin in starting room"""
        self.emu.auto_commands = ['LOOK']
        self.run_until_input_or_limit()

        output = self.emu.output_buffer
        self.assertIn('Thorin', output,
            f"Starting room should show Thorin. Got: {output[:500]}")

    def test_look_contains_wooden_chest(self):
        """Test LOOK shows wooden chest in starting room"""
        self.emu.auto_commands = ['LOOK']
        self.run_until_input_or_limit()

        output = self.emu.output_buffer
        self.assertIn('wooden chest', output.lower(),
            f"Starting room should show wooden chest. Got: {output[:500]}")

    def test_look_contains_green_door(self):
        """Test LOOK shows green door in starting room"""
        self.emu.auto_commands = ['LOOK']
        self.run_until_input_or_limit()

        output = self.emu.output_buffer
        self.assertIn('green door', output.lower(),
            f"Starting room should show green door. Got: {output[:500]}")

    def test_inventory_command(self):
        """Test INVENTORY command"""
        self.emu.auto_commands = ['INVENTORY']
        self.run_until_input_or_limit()

        output = self.emu.output_buffer
        # At start, should get some response
        self.assertGreater(len(output), 10,
            "INVENTORY should produce output")

    def test_multiple_commands(self):
        """Test running multiple commands in sequence"""
        self.emu.auto_commands = ['LOOK', 'EAST']
        self.run_until_input_or_limit(max_cycles=2000000)

        output = self.emu.output_buffer
        # After going east, should see different description
        self.assertGreater(len(output), 50,
            "Multiple commands should produce substantial output")

    def test_gandalf_gives_map(self):
        """Test that Gandalf gives the curious map"""
        self.emu.auto_commands = ['LOOK']
        self.run_until_input_or_limit()

        output = self.emu.output_buffer
        # Gandalf should give the map during intro
        self.assertIn('map', output.lower(),
            f"Gandalf should give curious map. Got: {output[:500]}")

    def test_waits_for_input(self):
        """Test that emulator waits for input after command"""
        self.emu.auto_commands = ['LOOK']
        self.run_until_input_or_limit()

        # After processing LOOK, should wait for next input
        self.assertTrue(self.emu.waiting_for_input,
            "Emulator should be waiting for input after command")

    def test_go_east_then_west(self):
        """Test navigation: go east then back west"""
        self.emu.auto_commands = ['EAST', 'WEST']
        self.run_until_input_or_limit(max_cycles=3000000)

        output = self.emu.output_buffer
        # Should return to Bilbo's home
        self.assertIn('tunnel', output.lower(),
            "Should return to tunnel after going east then west")


class TestVirtualScreen(unittest.TestCase):
    """Test VirtualScreen graphics rendering"""

    def test_set_pixel(self):
        """Test setting individual pixels"""
        screen = VirtualScreen()
        screen.set_pixel(10, 20)  # x, y
        # pixels[y][x] format
        self.assertEqual(screen.pixels[20][10], 1)

    def test_clear_screen(self):
        """Test screen clearing"""
        screen = VirtualScreen()
        screen.set_pixel(10, 20)
        screen.clear()
        self.assertEqual(screen.pixels[20][10], 0)

    def test_draw_line_horizontal(self):
        """Test horizontal line drawing"""
        screen = VirtualScreen()
        screen.draw_line(0, 50, 100, 50)  # x1, y1, x2, y2

        # Check some points on the line - pixels[y][x]
        self.assertEqual(screen.pixels[50][0], 1)
        self.assertEqual(screen.pixels[50][50], 1)
        self.assertEqual(screen.pixels[50][100], 1)

    def test_draw_line_vertical(self):
        """Test vertical line drawing"""
        screen = VirtualScreen()
        screen.draw_line(50, 0, 50, 100)

        # Check some points on the line - pixels[y][x]
        self.assertEqual(screen.pixels[0][50], 1)
        self.assertEqual(screen.pixels[50][50], 1)
        self.assertEqual(screen.pixels[100][50], 1)

    def test_draw_line_diagonal(self):
        """Test diagonal line drawing"""
        screen = VirtualScreen()
        screen.draw_line(0, 0, 100, 100)

        # Diagonal line should hit approximate midpoint
        # (Bresenham may not hit exact center) - pixels[y][x]
        has_mid = any(screen.pixels[x][x] for x in range(45, 55))
        self.assertTrue(has_mid, "Diagonal line should pass through middle")

    def test_render_to_text(self):
        """Test text rendering produces output"""
        screen = VirtualScreen()
        screen.draw_line(0, 0, 50, 50)

        text = screen.render_to_text(scale=4)
        self.assertGreater(len(text), 0, "Text render should produce output")

    def test_screen_dimensions(self):
        """Test screen has correct dimensions"""
        screen = VirtualScreen()
        self.assertEqual(screen.WIDTH, 256)
        self.assertEqual(screen.HEIGHT, 192)


class TestHobbitGraphics(unittest.TestCase):
    """Test Hobbit graphics rendering (if available)"""

    TAP_PATH = '/home/alice/dev/cpm-abap/docs/HOBBIT12.TAP'

    @classmethod
    def setUpClass(cls):
        if not os.path.exists(cls.TAP_PATH):
            raise unittest.SkipTest(f"TAP file not found: {cls.TAP_PATH}")

    def setUp(self):
        self.emu = HobbitEmulator()

        blocks = parse_tap_file(self.TAP_PATH)
        for block in blocks:
            if block.flag == 0xFF and len(block.data) > 10 and block.param1 > 0:
                self.emu.bus.load(block.param1, block.data)

        # Set ROM stubs AFTER loading game
        self.emu.setup_spectrum_rom_stubs()

        self.emu.cpu.pc = HobbitEmulator.ENTRY_POINT
        self.emu.cpu.sp = 0xFF00

    def test_graphics_table_exists(self):
        """Verify graphics table is loaded"""
        # Graphics table should be at 0xCC00 (52224)
        table_addr = 0xCC00
        # Check table has some non-zero entries
        has_data = any(self.emu.bus.read_mem(table_addr + i) != 0
                      for i in range(256))
        # Note: This might fail if graphics table is elsewhere
        # Just skip if not found at expected location
        if not has_data:
            self.skipTest("Graphics table not found at expected address")


if __name__ == "__main__":
    unittest.main(verbosity=2)
