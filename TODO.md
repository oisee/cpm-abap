# TODO: i8080 CPU Emulator Implementation

## Current Status (2025-10-19)

✅ **Completed: 86 / ~105 core i8080 opcodes (82%)**

- ✅ All register operations (LD, INC, DEC)
- ✅ All ALU operations (ADD, SUB, AND, OR, XOR, CP)
- ✅ All control flow (JP, CALL, RET - conditional and unconditional)
- ✅ Stack operations (PUSH, POP)
- ✅ Rotate operations (RLCA, RRCA, RLA, RRA)
- ✅ Miscellaneous (NOP, HALT, DAA, CPL, STC, CMC)
- ✅ 16 comprehensive tests, all passing
- ✅ Local TDD workflow (transpiler working)

## Missing i8080 Opcodes (~19 opcodes)

### Priority 1: Critical for Most Programs (10 opcodes)

**16-bit Arithmetic** - Used extensively in real programs
- [ ] `0x09` - ADD HL,BC (DAD B in i8080)
- [ ] `0x19` - ADD HL,DE (DAD D in i8080)
- [ ] `0x29` - ADD HL,HL (DAD H in i8080)
- [ ] `0x39` - ADD HL,SP (DAD SP in i8080)

**Exchange Operations** - Very common
- [ ] `0xEB` - EX DE,HL (XCHG in i8080) - Exchange DE and HL
- [ ] `0xE9` - JP (HL) (PCHL in i8080) - Jump to address in HL
- [ ] `0xF9` - LD SP,HL (SPHL in i8080) - Load SP from HL
- [ ] `0xE3` - EX (SP),HL (XTHL in i8080) - Exchange top of stack with HL

**Interrupt Control** - Required by CP/M (can be NOPs in emulator)
- [ ] `0xF3` - DI - Disable interrupts
- [ ] `0xFB` - EI - Enable interrupts

**Estimated time:** 2-3 hours

### Priority 2: RST Instructions (8 opcodes)

Used for system calls and debugging. CP/M uses RST 7 for warm boot.

- [ ] `0xC7` - RST 0 (CALL 0x0000)
- [ ] `0xCF` - RST 1 (CALL 0x0008)
- [ ] `0xD7` - RST 2 (CALL 0x0010)
- [ ] `0xDF` - RST 3 (CALL 0x0018)
- [ ] `0xE7` - RST 4 (CALL 0x0020)
- [ ] `0xEF` - RST 5 (CALL 0x0028)
- [ ] `0xF7` - RST 6 (CALL 0x0030)
- [ ] `0xFF` - RST 7 (CALL 0x0038) - **CP/M warm boot**

**Estimated time:** 1-2 hours (all very similar)

### Priority 3: I/O Operations (2 opcodes)

Required for CP/M BIOS. Can be stubbed initially.

- [ ] `0xDB` - IN A,(nn) - Read from I/O port
- [ ] `0xD3` - OUT (nn),A - Write to I/O port

**Estimated time:** 1 hour

## Testing Strategy

### Phase 1: Industry Standard Test Suite ⭐

**8080 Exerciser by Ian Bartholomew** - Gold standard for i8080 validation

- [ ] Download test files:
  - `8080EXM.COM` - Full instruction exerciser
  - `8080PRE.COM` - Preliminary test
  - Available at: http://www.retroarchive.org/cpm/cdrom/SIMTEL/CPMUG/

- [ ] Run 8080EXM.COM through emulator
  - Tests all instructions systematically
  - Computes CRC checksums
  - Prints "PASS" or shows failures

**What the exerciser tests:**
```
dad <b,d,h,sp>................  PASS
aluop nn......................  PASS
aluop <b,c,d,e,h,l,m,a>.......  PASS
<daa,cma,stc,cmc>.............  PASS
<inr,dcr> a...................  PASS
<inr,dcr> b...................  PASS
... (continues for all instructions)
```

### Phase 2: Additional Test Suites (Optional)

- [ ] ZEXDOC/ZEXALL by Frank D. Cringle (Z80, includes i8080 subset)
- [ ] TST8080.COM - Microcosm Associates (simpler validation)

### Phase 3: Real CP/M Programs

Once all opcodes implemented:
- [ ] HELLO.COM - Simple "Hello World"
- [ ] DIR.COM - Directory listing
- [ ] PIP.COM - File copy utility
- [ ] MBASIC.COM - Microsoft BASIC
- [ ] Text adventures (ZORK, Colossal Cave)

## CP/M BDOS Emulation (After i8080 Complete)

### Minimal Console I/O (1-2 days)

- [ ] Function 2 - Console output
- [ ] Function 9 - Print string
- [ ] Function 10 - Read console buffer
- [ ] Function 11 - Console status
- [ ] .COM file loader

### File I/O (3-5 days)

- [ ] Function 15 - Open file
- [ ] Function 16 - Close file
- [ ] Function 19 - Delete file
- [ ] Function 20 - Read sequential
- [ ] Function 21 - Write sequential
- [ ] Simulated file system (ABAP internal tables or browser LocalStorage)

## Implementation Approaches

### Option 1: Complete i8080 First (Recommended)

**Timeline:** 4-6 hours

1. Implement all 19 missing opcodes
2. Run 8080 Exerciser to validate
3. Fix any bugs found
4. Then move to BDOS emulation

**Pros:**
- Clean, complete implementation
- Validated by industry-standard test
- Confidence in correctness

**Cons:**
- Can't run real programs immediately

### Option 2: Pragmatic Incremental

**Timeline:** Spread over 1-2 weeks

1. Implement Priority 1 opcodes (2-3 hours)
2. Add minimal BDOS console I/O (1 day)
3. Try to run HELLO.COM
4. Add missing opcodes as needed
5. Eventually run full 8080 Exerciser

**Pros:**
- See real programs running sooner
- Motivating to see progress
- Learn which opcodes matter most

**Cons:**
- May miss subtle bugs
- Piecemeal implementation

### Option 3: Test-Driven (TDD Approach)

**Timeline:** 1 week

1. Download 8080 Exerciser
2. Try to run it
3. Implement opcodes as failures reveal them
4. Iterate until all tests pass

**Pros:**
- Test-first methodology
- Only implement what's tested
- Automatic validation

**Cons:**
- Need BDOS stub to run .COM files
- Might be harder to debug

## Recommended Next Steps

### Immediate (This Session)

1. **Add missing Priority 1 opcodes** (2-3 hours)
   - Implement ADD HL,rr family (4 opcodes)
   - Implement exchange operations (4 opcodes)
   - Implement DI/EI as NOPs (2 opcodes)
   - Write tests for each

2. **Add RST instructions** (1-2 hours)
   - All 8 RST opcodes (very similar code)
   - Write test for RST

3. **Add I/O operations** (1 hour)
   - IN/OUT as stubs initially
   - Can enhance later

### Short Term (Next Session)

4. **Download 8080 Exerciser**
   - Get 8080EXM.COM and 8080PRE.COM
   - Set up .COM file loader

5. **Minimal BDOS stub**
   - Just enough to run test programs
   - Console output (function 2)
   - Program exit (function 0)

6. **Run 8080 Exerciser**
   - Execute tests
   - Fix any failures
   - Celebrate when all tests pass! 🎉

### Medium Term (Following Weeks)

7. **Full BDOS implementation**
   - Complete console I/O
   - File I/O operations
   - Disk simulation

8. **Run real CP/M programs**
   - Text adventures
   - BASIC interpreter
   - Utilities

## Resources

### Test Suites
- 8080 Exerciser: http://www.retroarchive.org/cpm/cdrom/SIMTEL/CPMUG/
- ZEXDOC/ZEXALL: https://github.com/anotherlin/z80emu
- i8080 test suite: https://github.com/begoon/i8080-core

### Documentation
- i8080 Opcode Reference: http://www.emulator101.com/reference/8080-by-opcode.html
- i8080 Opcode Table: https://pastraiser.com/cpu/i8080/i8080_opcodes.html
- CP/M 2.2 BDOS Reference: http://www.gaby.de/cpm/manuals/archive/cpm22htm/

### Example Emulators
- RunCPM (C++): https://github.com/MockbaTheBorg/RunCPM
- i8080-core (C): https://github.com/begoon/i8080-core
- 8080.js (JavaScript): https://github.com/amensch/8080.js

## Progress Tracking

Last updated: 2025-10-19

**Next milestone:** Complete all i8080 opcodes (19 remaining)
**After that:** Run 8080 Exerciser test suite
**Final goal:** Run real CP/M programs

---

*Note: Check CONTEXT.md for detailed project status and history*
*See CLAUDE.md for AI assistant instructions*
