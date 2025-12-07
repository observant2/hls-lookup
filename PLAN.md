# HLS Lookup Implementation Plan

## Goal
Enable "go to definition" for external Hackage packages by:
1. Finding the symbol at cursor position
2. Downloading the package source from Hackage
3. Locating the module file within the package
4. Finding the definition within that file
5. Opening the file at the correct location

## Example Workflow
```
User has cursor on: httpLBS (from http-conduit-2.3.9.1)
                     ↓
Symbol lookup finds: httpLBS in Network.HTTP.Simple
                     ↓
Download package: http-conduit-2.3.9.1 from Hackage
                     ↓
Find module file: src/Network/HTTP/Simple.hs
                     ↓
Find definition: Line 142: httpLBS :: ...
                     ↓
Open file: $EDITOR +142 ~/.hls-lookup/haskell-sources/http-conduit-2.3.9.1/src/Network/HTTP/Simple.hs
```

## Current State ✓

### HieReader Module (Refactored)
- **HieReader.Types**: `SymbolInfo` data type
- **HieReader.Parser**: Parse package names/versions from UnitId strings
- **HieReader.SymbolLookup**: Extract symbols from HIE AST at cursor position
- **HieReader**: Main API (loadHieFile, getSymbolsAtPosition, etc.)

### Main.hs
- **Package downloading**: Download and cache packages from Hackage
- **Commands**:
  - `download <pkg> <ver>`: Download a package
  - `inspect-hie <file>`: Show HIE file contents
  - `find-symbol <file> <line> <col>`: Find symbol at position

## TODO

### 1. Extract Package Download Logic ⏳
**Status**: In Progress
**Module**: `PackageDownload.hs`

Extract from Main.hs:
- `cacheDir :: IO FilePath`
- `hackageUrl :: String -> String -> String`
- `downloadPackage :: String -> String -> IO FilePath`

### 2. Module Lookup
**Status**: Not Started
**Module**: `ModuleLookup.hs`

Map module name to file path within downloaded package:
- Input: Package directory + module name (e.g., "Network.HTTP.Simple")
- Output: File path (e.g., "src/Network/HTTP/Simple.hs")

**Strategy**:
1. Try common patterns:
   - `src/Network/HTTP/Simple.hs`
   - `lib/Network/HTTP/Simple.hs`
   - `Network/HTTP/Simple.hs`
2. Optional: Parse `.cabal` file to find `hs-source-dirs`

### 3. Definition Finder
**Status**: Not Started
**Module**: `DefinitionFinder.hs`

Text-based search for function/type definitions:
- Input: File path + symbol name
- Output: Line number (and maybe column)

**Search Patterns**:
- Type signature: `httpLBS :: `
- Function definition: `httpLBS = ` or `httpLBS x y = `
- Data constructor: `data Foo = ... | HttpLBS ...`
- Type definition: `type HttpLBS = ...` or `newtype HttpLBS = ...`

**Approach**: Simple text/regex search (no HIE files for downloaded packages)

### 4. Goto Definition Command
**Status**: Not Started
**Module**: `Main.hs` update

New command:
```bash
hls-lookup goto-definition <source-file> <line> <col>
```

**Workflow**:
1. Load HIE file for source file
2. Find symbol at cursor position → `SymbolInfo`
3. Download package (if not cached)
4. Find module file in package
5. Find definition line in module file
6. Open file at definition

### 5. File Opening
**Status**: Not Started
**Module**: `FileOpener.hs` or in Main.hs

**Current approach**: Open directly using `$EDITOR`
```haskell
openFile :: FilePath -> Int -> IO ()
```

**Future approach**: Output for HLS integration
```
filepath:line:col
```

### 6. Tests
**Status**: Not Started
**Module**: `test/` directory

Add tests for:
- `HieReader.Parser.parseUnitId` - various package ID formats
- `HieReader.SymbolLookup` - symbol filtering, boot libraries
- `ModuleLookup` - module name to file path mapping
- `DefinitionFinder` - finding definitions with various patterns
- Integration test: end-to-end workflow

## Design Decisions

1. **Definition Search**: Text-based (not HIE files)
   - Reason: Recompiling every package with HIE info is expensive
   - Trade-off: Less accurate, but good enough for most cases

2. **Package Cache**: `~/.hls-lookup/haskell-sources/`
   - Already implemented
   - Good enough for now

3. **File Opening**:
   - Now: Open directly with `$EDITOR`
   - Later: Output location for HLS integration

## Future Enhancements

- Parse `.cabal` files for accurate source directories
- Better definition finding (handle more edge cases)
- Support for multiple definitions (re-exports, instances)
- Jump to definition within downloaded packages (recursive lookup)
- Clean up old cached packages
- Progress indicators for downloads
- Better error handling and user messages
