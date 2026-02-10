# JASP QML Controls Documentation Guide

This directory contains the configuration and resources for generating documentation for JASP's QML controls. The documentation is generated from QDoc comments (`/*! ... */`) embedded in the QML source files.

## Prerequisites

To generate the documentation, you need:

1.  **Qt QDoc Tool (`qdoc`)**
    - Part of the Qt installation (Qt 5.15 or later recommended).
    - Ensure `qdoc` is in your system `PATH` or know the absolute path to the executable.
    - *Note:* Standard `qdoc` in Qt 5.15 only supports HTML output.

2.  **Pandoc** (Optional, for Markdown generation)
    - Required if you want to convert the generated HTML to Markdown.
    - Download from [pandoc.org](https://pandoc.org/).

## Configuration

The main configuration file is **`jasp_qml.qdocconf`**. It defines:
- **Source Directories**: Where to look for `.qml` files (`../components/JASP/Controls`).
- **Output Directory**: Defaults to `./html_out`.
- **File Extensions**: `*.qml`, `*.qdoc`.

## Generating Documentation

### 1. Generate HTML (Primary)

Run `qdoc` from the `jasp-desktop` root directory (or adjust paths accordingly).

**Command:**
```powershell
# From c:\JASP-Packages\jasp-desktop
qdoc QMLComponents/doc/jasp_qml.qdocconf
```

**Output:**
- The generated HTML files will be in `QMLComponents/doc/html_out`.
- The main index file is `QMLComponents/doc/html_out/jasp-controls-qmlmodule.html`.

### 2. Generate Markdown (Secondary)

Since `qdoc` (Qt 5) does not natively support Markdown output, we use `pandoc` to convert the generated HTML files, then run a post-processing script to fix artifacts.

**Step 1 — Convert HTML to Markdown (PowerShell):**
```powershell
# Create output directory
New-Item -ItemType Directory -Force -Path "QMLComponents/doc/md_out"

# Convert all HTML files to GitHub Flavored Markdown
Get-ChildItem "QMLComponents/doc/html_out/*.html" | ForEach-Object { 
    Write-Host "Converting $($_.Name)..."
    pandoc $_.FullName -o ("QMLComponents/doc/md_out/" + $_.BaseName + ".md") -t gfm 
}
```

**Step 2 — Post-process (Bash / Git Bash / WSL):**
```bash
bash QMLComponents/doc/fix-markdown.sh QMLComponents/doc/md_out
```

The `fix-markdown.sh` script handles:
- Fixing internal links (`.html` → `.md`)
- Converting `<a translate="no">` tags to proper Markdown links
- Removing dead `[More...]` anchors
- Stripping leftover HTML `<div>` wrappers
- Rebuilding `index.md` with the full component table

**Output:**
- The generated Markdown files will be in `QMLComponents/doc/md_out`.

## Troubleshooting

- **"Unable to parse QML snippet"**:
    - QDoc's QML parser mimics the QML engine and requires valid QML syntax in `\qml ... \endqml` blocks.
    - If you have multiple top-level items in an example (e.g., two `DropDown` controls), wrap them in a `Column {}` or `Item {}` to create a single root element.

- **"Unknown base" warnings**:
    - You may see warnings like `Unknown base 'JASPControl'`.
    - This happens because the C++ base classes are not documented in this QDoc pass. These warnings can be ignored if the QML API documentation itself renders correctly.

## CI Automation

Documentation is **automatically regenerated** via GitHub Actions when QML source files or QDoc configuration files are pushed to the `development` branch.

- **Workflow file**: `.github/workflows/generate-qml-docs.yml`
- **Trigger**: Push to `development` that modifies files in `QMLComponents/components/JASP/Controls/` or `QMLComponents/doc/`
- **What it does**: Runs `qdoc` → `pandoc` → link fix → commits updated `html_out/` and `md_out/` back to `development`
- **Manual trigger**: The workflow can also be run manually via the GitHub Actions UI (`workflow_dispatch`)
