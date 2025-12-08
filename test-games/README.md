# Z-Machine Test Games

## Available Story Files

| File | Size | Version | Description |
|------|------|---------|-------------|
| minizork.z3 | 52,216 bytes | Z3 | Mini-Zork (reduced version of Zork I) |
| sampler1_R55.z3 | 126,902 bytes | Z3 | Infocom Sampler (demo compilation) |
| strictz.z5 | 4,096 bytes | Z5 | Strict Z-Machine test suite |

## Verification Hashes (SHA256)

```
c74f01a232e8df4b05d7ebcba14870143f49b3c9a25f194f7a7d2c69e31ea4a6  minizork.z3
7a2e5b7698f42a83e73dab5d9c5fbead294cf7b407f6daa8fe57caa429bd35bf  sampler1_R55.z3
```

## MiniZork Header

```
Version: 3
Serial: 871124
High memory: 0x3709
Init PC: 0x37d9
Dictionary: 0x285a
Objects: 0x03c6
Globals: 0x02b4
```

## Testing with Reference Interpreter

### Option 1: Online (Parchment)
Visit https://iplayif.com/ and upload the .z3 file

### Option 2: Install Frotz (Linux)
```bash
sudo apt-get install frotz
dfrotz minizork.z3
```

### Option 3: Download portable Frotz
```bash
# From https://gitlab.com/DavidGriffith/frotz/-/releases
wget https://gitlab.com/DavidGriffith/frotz/-/archive/v2.54/frotz-v2.54.tar.gz
tar xzf frotz-v2.54.tar.gz
cd frotz-v2.54
make dumb
./dfrotz ../minizork.z3
```

## Known Issues Found in Testing

### 1. Missing Light Attribute (Attribute 3)

In MiniZork, outdoor rooms should have attribute 3 set for "naturally lit".
Room 46 (West of House) lacks this attribute, causing immediate grue death.

**Workaround in ABAP:**
```abap
lo_obj->set_attr( iv_object = 46 iv_attr = 3 ).
```

### 2. Tokenization Issue

After fixing the light issue, commands like LOOK, INVENTORY return
"You can't go that way" - indicating parser/tokenization problems.

## ABAP Verification

Run `ZORK_00_OBJ_DUMP` in SE38 to verify:
- Story file loads correctly
- Header values match expected
- Checksum matches

## File Formats

- `.z3` - Z-Machine version 3 story file (binary)
- `.b64` - Base64 encoded version (for ABAP embedding)
- `.hex` - Hex string version (for debugging)
