# ABAP Transpiler Integration

## Summary

**YES, we can use the ABAP transpiler for local TDD!**

The `@abaplint/transpiler-cli` package can convert ABAP code to JavaScript and run unit tests locally without an SAP server. This enables true Test-Driven Development with instant feedback.

## What We Discovered

### ✓ What Works

1. **Installation** - Easy setup with npm
   ```bash
   npm install @abaplint/transpiler-cli @abaplint/runtime
   ```

2. **Auto-detection** - Transpiler automatically finds `.abap` files in `src/` directory

3. **Class structure** - Basic ABAP OO syntax is supported
   - CLASS...DEFINITION / IMPLEMENTATION
   - PUBLIC/PRIVATE sections
   - METHODS with parameters
   - REF TO objects

4. **Unit test framework** - Supports ABAP unit testing
   - `FOR TESTING` classes
   - `cl_abap_unit_assert` assertions
   - TEST methods

5. **Runtime library** - Includes open-abap-core with standard ABAP functionality
   - 536 files cloned automatically
   - Basic ABAP statements and functions

### ✗ Current Limitations (Our Code)

1. **APPEND to standard tables** - Our initialization code fails:
   ```abap
   " ✗ Doesn't work in transpiler (yet)
   TYPES: ty_memory TYPE STANDARD TABLE OF x LENGTH 1 WITH EMPTY KEY.
   DATA: mt_memory TYPE ty_memory.
   APPEND '00' TO mt_memory.  " Error: target not a table type
   ```

2. **BIT operations** - Limited support:
   ```abap
   " ✗ Some BIT operations not supported
   lv_result = lv_val BIT-AND 168.  " Error: only valid for XSTRING or HEX
   ```

3. **Some helper methods** - Method signatures need adjustment

4. **DEFINE macros** - Not supported (expected, these are preprocessor directives)

## What This Means for TDD

### Option 1: Fix Compatibility Issues (Recommended)

Adjust our code to work with transpiler limitations:

**Memory as XSTRING instead of table:**
```abap
" Instead of: STANDARD TABLE OF x LENGTH 1
DATA: mv_memory TYPE xstring.

" Access byte at offset
METHOD read_byte.
  DATA(lv_offset) = iv_addr * 2.  " 2 hex chars per byte
  rv_val = mv_memory+lv_offset(2).
ENDMETHOD.
```

**BIT operations on proper hex types:**
```abap
" Cast to xstring for BIT operations
DATA(lv_hex) = CONV xstring( lv_val ).
lv_result = lv_hex BIT-AND '00A8'.
```

### Option 2: Hybrid Approach

- Keep full-featured ABAP for SAP deployment
- Create transpiler-compatible subset for local testing
- Use conditional compilation or separate test files

### Option 3: Wait for Transpiler Updates

The transpiler is actively developed (v2.12.3 from 3 days ago). Many features are being added. Current limitations may be temporary.

## How to Use (Once Fixed)

### 1. Transpile Code

```bash
npx abap_transpile
```

Output structure:
```
output/
├── zcl_cpu_8080.clas.js         # Transpiled class
├── zcl_cpu_8080_test.clas.js   # Transpiled tests
└── index.js                      # Entry point
```

### 2. Run Tests

```bash
node output/index.js
```

### 3. TDD Workflow

```bash
# Watch mode (with nodemon)
npm install --save-dev nodemon
npx nodemon --watch src --exec "npx abap_transpile && node output/index.js"
```

This gives instant feedback:
1. Edit ABAP code
2. Save file
3. Auto-transpile
4. Auto-run tests
5. See results in < 1 second

## Real-World Examples

The transpiler is used successfully in production projects:

- **abapGit** - Git client in ABAP (runs tests locally)
- **abap2UI5** - SAPUI5 framework in ABAP
- **abap-file-formats-tools** - File format parsers

## Configuration

Create `abaplint.json`:
```json
{
  "global": {
    "files": "/src/**/*.clas.abap",
    "exclude": []
  },
  "syntax": {
    "version": "v702",
    "errorNamespace": "^(Z|Y)"
  }
}
```

## Current Status

**2 files added from source:**
- zcl_cpu_8080.clas.abap (CPU emulator)
- zcl_cpu_8080_test.clas.abap (unit tests)

**536 files from open-abap-core library**

**Errors to fix:**
- Table APPEND syntax (6 locations)
- BIT operations on wrong types (15 locations)
- Some method visibility issues

**Estimated effort to fix:** 2-4 hours to make fully transpiler-compatible

## Recommendation

**YES, adopt TDD with transpiler**, but:

1. **Fix compatibility issues first** (2-4 hours work)
   - Change memory representation to XSTRING
   - Adjust BIT operations to use hex types
   - Test incrementally

2. **Set up watch mode** for instant feedback
   ```json
   // package.json
   {
     "scripts": {
       "test": "abap_transpile && node output/index.js",
       "watch": "nodemon --watch src --exec npm test"
     }
   }
   ```

3. **Keep both versions**
   - Full ABAP for SAP deployment (with tables, full syntax)
   - Transpiler-compatible subset for local TDD
   - 90% code reuse possible

## Benefits of Local TDD

1. **Speed** - Tests run in milliseconds, not minutes
2. **No SAP server needed** - Develop anywhere, anytime
3. **CI/CD integration** - Run tests in GitHub Actions, GitLab CI
4. **Modern tooling** - Use VS Code, git workflows, etc.
5. **Faster iteration** - Instant feedback loop

## Next Steps

1. Fix memory table issue (use XSTRING)
2. Fix BIT operations (proper hex types)
3. Re-transpile and verify
4. Create npm test script
5. Document TDD workflow

## Resources

- Transpiler: https://github.com/abaplint/transpiler
- Playground: https://transpiler.abaplint.org
- Examples: https://github.com/larshp/abap-advent-2020

---

**Bottom line:** The transpiler works and TDD is achievable. We need minor code adjustments to fix compatibility, then we'll have instant local testing without an SAP server!
