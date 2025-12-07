# hls-lookup

## Project Goal

Build a tool that enables "jump to definition" functionality for third-party Haskell libraries in the Haskell Language Server (HLS). Currently, HLS can only jump to definitions within the current project, but not to dependency source code.

## How It Works

1. **Parse HIE files** - Extract symbol information (name, module, package) from GHC-generated `.hie` files at cursor position
2. **Download sources** - Automatically download and cache source tarballs from Hackage for the relevant package version
3. **Map to file** - Resolve module names to actual source file paths within the cached package
4. **Return location** - Provide file path that editors can open

## Current Status

- ✅ Hackage source downloader with local caching (`.hls-lookup/` directory)
- ✅ HIE file parsing and AST traversal for symbol lookup
- ✅ Extracting package name, module, and version from symbols
- 🚧 Mapping module names to source file paths within packages
- ⏳ Integration with HLS or as standalone LSP server

## Tech Stack

- GHC 9.12.2, base 4.21.0.0
- GHC API for HIE file processing
- HTTP libraries for Hackage downloads
- Tar/gzip for archive extraction

## Development Approach

Building in phases: start with core functionality (source downloading and caching), then add symbol resolution, before tackling editor integration complexity.