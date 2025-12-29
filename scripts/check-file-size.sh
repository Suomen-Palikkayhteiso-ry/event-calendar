#!/usr/bin/env sh
# Check maximum file size for Elm files
# Usage: ./scripts/check-file-size.sh [max_lines]

MAX_LINES=${1:-400}
EXIT_CODE=0

echo "Checking Elm file sizes (max $MAX_LINES lines)..."

while IFS= read -r file; do
    lines=$(wc -l < "$file")
    if [ "$lines" -gt "$MAX_LINES" ]; then
        echo "❌ $file: $lines lines (exceeds $MAX_LINES)"
        EXIT_CODE=1
    else
        echo "✅ $file: $lines lines"
    fi
done < <(find src -name "*.elm" -type f)

if [ $EXIT_CODE -eq 0 ]; then
    echo "All Elm files are within size limits!"
else
    echo "Some Elm files exceed the maximum size limit of $MAX_LINES lines."
    echo "Consider splitting large files into smaller modules."
    exit 1
fi