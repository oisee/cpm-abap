# CP/M Test Programs

Binary CP/M programs (.COM files) for testing the ZCPM_CONSOLE emulator.

## HELLO.COM (30 bytes)

Simple "Hello World" program using BDOS function 9 (print string).

**Expected output:**
```
Hello, CP/M World!
```

**To use:**
1. Upload to SMW0 as binary data (transaction SMW0 → Binary data)
2. Or use the "File" option in ZCPM_CONSOLE selection screen
3. Or just use "Built-in Test" which has HELLO.COM embedded

## Creating More Programs

CP/M .COM files are raw Z80 machine code loaded at address 0x0100.

### BDOS Functions (call via CALL 0005H)

| C reg | Function | Parameters |
|-------|----------|------------|
| 0 | System Reset | - |
| 1 | Console Input | Returns char in A |
| 2 | Console Output | E = character |
| 6 | Direct Console I/O | E = char (output) or E = 0xFF (input) |
| 9 | Print String | DE = address of $-terminated string |
| 10 | Read Console Buffer | DE = buffer address |
| 11 | Console Status | Returns A = 0xFF if char ready |
| 12 | Return Version | Returns HL = version (0x0022 for CP/M 2.2) |

### Assembly Template

```asm
        ORG     0100H

        ; Your code here
        ; Use CALL 0005H for BDOS calls
        ; Use RET or JP 0 to exit

        END
```
