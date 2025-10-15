# Session Notes: CALL Instruction Debug Session

## Date
2025-10-15

## Summary
Investigated failing test_call_ret test. Made partial progress - fixed CALL instruction code but discovered the actual root cause is different.

## Work Completed

### 1. Commits Pushed
- **Commit d57d7d7**: "Fix CALL instruction to use read_word_pp and clean up duplicate opcodes"
  - Fixed CALL instruction (opcode 205) to use `read_word_pp(CHANGING cv_addr = mv_pc)`
  - Removed duplicate WHEN 26 clause
  - Added missing WHEN 30 for LD E,nn instruction
  - Removed duplicate WHEN 31 entries

### 2. Code Changes Made
**File**: src/zcl_cpu_8080_v2.clas.abap

#### CALL Instruction Fix (Line 1137)
```abap
" OLD (WRONG):
lv_addr = read_word( mv_pc ).
mv_pc = mv_pc + 2.

" NEW (CORRECT):
lv_addr = read_word_pp( CHANGING cv_addr = mv_pc ).
```

#### Removed Duplicate WHEN Clauses
- Removed duplicate WHEN 26 (LD E,nn that conflicted with LD A,(DE))
- Added WHEN 30 for LD E,nn (correct opcode)
- Removed duplicate WHEN 31 entries

## Root Cause Discovery

### Initial Symptoms
- Test: test_call_ret failing
- Expected PC after CALL: 12288 (0x3000)
- Actual PC after CALL: 260 (0x104)

### Investigation Process
1. **First hypothesis**: CALL instruction not using read_word_pp correctly
   - Fixed this issue
   - Tests still failing with same error

2. **Created debug script** (test_call_debug.mjs):
   ```javascript
   // Before LD SP: PC = 256 Status = 0
   // After LD SP: PC = 259 , SP = 65535 Status = 1  ← BUG HERE!
   // After CALL: PC = 260 , SP = 65535 Status = 1
   // Return address on stack: 0
   ```

3. **ACTUAL ROOT CAUSE FOUND**:
   - **LD SP,nnnn instruction (opcode 49) is setting CPU status to HALTED (status=1)**
   - This prevents CALL from executing (second IF check at line 1054: `IF mv_status = c_status_running`)
   - CALL instruction never runs because CPU is already halted

### Why This Happens
- Opcode 49 (LD SP,nnnn) at line 900-902 looks correct in source
- The transpiled code at line 809-812 also looks correct
- **HYPOTHESIS**: Opcode 49 might be falling through to WHEN OTHERS clause (line 976) which sets `mv_status = c_status_halted`
  - This suggests a structural issue with the CASE statement
  - OR opcode 49 is not being matched properly

## Test Status
- **8/10 tests passing** (was 8/9, now have 10 tests total)
- **2 tests failing**: test_call_ret, test_program (both use CALL instruction)
- Failing not because CALL is broken, but because LD SP halts the CPU

## Next Steps for Future Session

### Immediate Priority
1. **Debug why opcode 49 (LD SP,nnnn) causes CPU to halt**
   - Check if CASE statement structure has issue
   - Verify opcode 49 is in first CASE block (not falling to WHEN OTHERS)
   - Check transpiled JavaScript for opcode 49 execution path
   - Test LD SP instruction in isolation

### Investigation Approaches
```javascript
// Test LD SP in isolation:
await cpu.reset();
await cpu.write_byte({iv_addr: 256, iv_val: 49});    // LD SP,nnnn
await cpu.write_byte({iv_addr: 257, iv_val: 255});   // Low byte
await cpu.write_byte({iv_addr: 258, iv_val: 255});   // High byte
console.log('Before:', await cpu.get_status());
await cpu.execute_instruction();
console.log('After:', await cpu.get_status());       // Should be 0, but is 1!
```

### Likely Fix
Once LD SP issue is resolved, test_call_ret should pass because:
- CALL instruction code is now correct (uses read_word_pp)
- The transpiled JavaScript looks correct
- The issue is purely that CPU halts before CALL runs

## Files to Check
- `src/zcl_cpu_8080_v2.clas.abap` - Lines 899-1050 (CASE structure)
- `output/zcl_cpu_8080_v2.clas.mjs` - Lines 809-900 (transpiled CASE)
- Test file: `src/zcl_cpu_8080_v2.clas.testclasses.abap` - test_call_ret

## Key Insights
1. **CHANGING parameters work correctly** - LD BC uses same pattern and passes
2. **The CALL fix was correct** - just couldn't be tested due to LD SP bug
3. **CPU status check prevents execution** - Line 1054 guards all 0xC0+ opcodes
4. **Opcode matching issue** - Opcode 49 not being matched in CASE statement

## Reference
- Commit: d57d7d7
- Branch: main
- Remote: Already pushed
