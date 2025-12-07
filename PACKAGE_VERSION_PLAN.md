# Package Version Extraction - Implementation Plan

## Current State

**File**: `app/HieReader.hs`

**Current Issue** (lines 56-60):
```haskell
packageInfo = case modMaybe of
    Nothing -> Nothing
    Just mod ->
        let unit = moduleUnit mod
        in Just $ showSDocUnsafe (ppr unit)  -- This shows package info
```

This uses `ppr unit` which gives inconsistent string representations. We need separate package name and version fields.

## Proposed Changes

### Data Structure Update

Update `SymbolInfo` to have separate fields (lines 45-50):
```haskell
data SymbolInfo = SymbolInfo
    { symName :: String
    , symModule :: Maybe String
    , symPackageName :: Maybe String    -- NEW: separate package name
    , symPackageVersion :: Maybe String -- NEW: separate version
    , symSpan :: RealSrcSpan
    } deriving (Show)
```

### Two Implementation Options

#### Option 1: Parse UnitId String (RECOMMENDED for POC)

**Pros**: Simple, works with current code structure, no GHC session needed
**Cons**: Requires string parsing (but format is stable)

**Implementation**:
1. Add imports:
   ```haskell
   import GHC.Unit.Types (toUnitId, unitIdString)
   ```

2. Update `nameToSymbolInfo` function:
   ```haskell
   let unit = moduleUnit mod
       unitId = toUnitId unit
       unitStr = unitIdString unitId  -- Format: "pkgname-1.2.3:libname+hash"
   ```

3. Parse the UnitId string format:
   - Split on `:` to get base part (before `:` is "pkgname-version")
   - Parse "pkgname-version" by finding last `-` followed by version numbers
   - Version format: digits separated by dots (e.g., "1.2.3.4")
   - Everything before the version separator is package name

4. Example parsing:
   - `"base-4.21.0.0"` → name: `"base"`, version: `"4.21.0.0"`
   - `"aeson-2.2.3.0:lib+abc123"` → name: `"aeson"`, version: `"2.2.3.0"`

#### Option 2: Use GHC UnitState API (BETTER for production)

**Pros**: No parsing, uses official API, more robust
**Cons**: Requires initializing GHC session with package database

**Implementation**:
1. Add imports:
   ```haskell
   import GHC (runGhc, getSessionDynFlags)
   import GHC.Driver.Env (hsc_unit_env)
   import GHC.Unit.Env (ue_units)
   import GHC.Unit.State (lookupUnit)
   import GHC.Unit.Info (unitPackageNameString, unitPackageVersion)
   ```

2. Initialize GHC session in `loadHieFile` or as a separate context
3. Get UnitState: `hsc_unit_env hscEnv` → `ue_units unitEnv`
4. Look up unit: `lookupUnit unitState unit` → `Maybe UnitInfo`
5. Extract fields: `unitPackageNameString` and `unitPackageVersion`

## Recommended Next Steps

1. **For POC**: Implement Option 1 (parse UnitId string)
   - Quick to implement
   - Good enough to test end-to-end functionality
   - Can verify the approach works

2. **Once POC works**: Upgrade to Option 2 (GHC UnitState API)
   - More robust
   - Handles edge cases better
   - Production-ready

## Testing

After implementing, test with:
```bash
cabal build
cabal run hls-lookup -- find-symbol app/Main.hs 5 10
```

Verify output shows:
- `Package Name: <name>`
- `Package Version: <version>`
- Can be passed to `downloadPackage name version`

## Integration with Main.hs

Once `SymbolInfo` has separate name/version fields, update the output in `findSymbolAt` (lines 115-118):
```haskell
printSymbol sym = do
    putStrLn $ "  Name: " ++ sym.symName
    putStrLn $ "  Module: " ++ show sym.symModule
    putStrLn $ "  Package: " ++ show sym.symPackageName
    putStrLn $ "  Version: " ++ show sym.symPackageVersion
    putStrLn ""
```

Then can call:
```haskell
case (sym.symPackageName, sym.symPackageVersion) of
    (Just name, Just version) -> downloadPackage name version
    _ -> putStrLn "No package info available"
```

## End-to-End Goal

Complete flow:
1. User positions cursor on a symbol (e.g., `Data.Aeson.encode`)
2. HIE file parsed → extracts package name `"aeson"` and version `"2.2.3.0"`
3. Download source: `downloadPackage "aeson" "2.2.3.0"`
4. Map module `Data.Aeson` to file path in downloaded source
5. Return file location for jump-to-definition
