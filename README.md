# FancyPolice - Unicode Gremlin Detector

A Haskell command-line tool that scans text for problematic Unicode characters that can cause issues in code, documentation, or data processing.

## What It Does

**FancyPolice** detects two types of "Unicode gremlins":

1. **Invisible/Confusing Characters** - Zero-width spaces, non-breaking spaces, lookalike letters (Cyrillic А vs Latin A), fancy quotes, em dashes, etc.
2. **Emoji Sequences** - Complex emoji like `👨‍👩‍👧‍👦` (family), `🧑‍🚀` (astronaut), or keycap sequences like `1️⃣`

## Key Features

- **Smart Emoji Detection** - Handles ZWJ sequences, skin tones, regional flags, and keycap numbers
- **Configurable Reporting** - Shows details when thresholds are exceeded or with `--pedantic` flag
- **Helpful Suggestions** - Recommends ASCII alternatives for each problematic character
- **Density Analysis** - Reports both absolute counts and character density percentages

## Use Cases

- **Code Cleanup** - Remove problematic Unicode from source files
- **Documentation Sanitization** - Ensure docs display correctly across systems
- **AI Detection** - High density of fancy quotes, em dashes, and unusual spaces can indicate AI-generated text on social media
- **Data Processing** - Clean datasets that need ASCII-only content

## Installation

This tool requires an installation of **GHC** and **Stack**.
The easiest way to do that is by using the **Hashell Toolchain Installer** (GHCUP).

https://www.haskell.org/ghcup/

## Usage

```bash
# Just install the tool
stack install

# Scan stdin for Unicode gremlins
echo "fancy "quotes"" | fancy-quotes

# Show all violations regardless of severity  
cat myfile.txt | fancy-quotes --pedantic
```
