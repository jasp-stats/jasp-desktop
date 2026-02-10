#!/bin/bash
# fix-markdown.sh
# Post-processing script for Pandoc-converted QML documentation.
# Fixes broken HTML artifacts, dead links, and leftover div tags.
#
# Usage: ./fix-markdown.sh [md_out_directory]

MD_DIR="${1:-QMLComponents/doc/md_out}"

if [ ! -d "$MD_DIR" ]; then
  echo "Error: Directory '$MD_DIR' not found."
  exit 1
fi

echo "Post-processing Markdown files in $MD_DIR..."

for f in "$MD_DIR"/*.md; do
  echo "  Fixing $(basename "$f")..."

  # 1. Fix internal links: .html -> .md
  sed -i 's/\.html)/\.md)/g; s/\.html#/\.md#/g; s/\.html"/\.md"/g' "$f"

  # 2. Convert <a href="..." translate="no">Text</a> to [Text](...)
  #    Handles multi-line <a> tags by joining them first
  perl -0777 -i -pe 's/<a\s+href="([^"]+)"\s*\n?\s*translate="no">([^<]+)<\/a>/[$2]($1)/g' "$f"

  # 3. Remove [More...](#details) lines
  sed -i '/^\[More\.\.\.\](#details)$/d' "$f"

  # 4. Remove sidebar div blocks
  perl -0777 -i -pe 's/<div class="sidebar">.*?<\/div>\s*<\/div>//gs' "$f"

  # 5. Remove remaining <div ...> and </div> tags
  sed -i '/<div[^>]*>/d; /<\/div>/d' "$f"

  # 6. Remove empty HTML comments <!-- -->
  sed -i '/^<!-- -->/d' "$f"

  # 7. Clean up excessive blank lines (3+ consecutive -> 2)
  perl -0777 -i -pe 's/\n{4,}/\n\n\n/g' "$f"

done

# 8. Replace index.md with the module page content (includes table with descriptions)
if [ -f "$MD_DIR/jasp-controls-qmlmodule.md" ]; then
  echo "  Building index.md from module page..."
  cat > "$MD_DIR/index.md" << 'HEADER'
# JASP QML Controls

## Introduction

Welcome to the JASP QML Controls reference documentation. This library contains the custom controls used to build JASP analysis modules.

## Getting Started

To use these controls, import the module in your QML file:

```qml
import JASP.Controls 1.0
```

## All Controls

HEADER
  # Extract the table from the module page (lines starting with |)
  grep '^|' "$MD_DIR/jasp-controls-qmlmodule.md" >> "$MD_DIR/index.md"
fi

echo "Done."
