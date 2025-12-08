#!/bin/bash
# Verify Z-Machine story files

echo "=== Z-Machine Story File Verification ==="
echo ""

cd "$(dirname "$0")"

echo "Local file hashes (SHA256):"
echo "----------------------------"
sha256sum *.z3 *.z5 2>/dev/null

echo ""
echo "MiniZork header (first 64 bytes):"
echo "----------------------------------"
xxd -l 64 minizork.z3

echo ""
echo "Story file details:"
echo "-------------------"
for f in *.z3 *.z5 2>/dev/null; do
    if [ -f "$f" ]; then
        size=$(stat -c%s "$f")
        version=$(xxd -l 1 -p "$f")
        echo "$f: ${size} bytes, Z-Machine v${version}"
    fi
done

echo ""
echo "To test with dfrotz (if installed):"
echo "  dfrotz -p minizork.z3"
echo ""
echo "Expected MiniZork hash:"
echo "  c74f01a232e8df4b05d7ebcba14870143f49b3c9a25f194f7a7d2c69e31ea4a6"
