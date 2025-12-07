# VSCode Integration Plan

## Goal
Enable Ctrl+Click "Go to Definition" in VSCode that falls back to `hls-lookup` when HLS cannot find the definition (i.e., for external Hackage packages).

## User Experience

```
User Ctrl+Clicks on `httpLBS` in their code
    ↓
VSCode calls all definition providers (HLS + hls-lookup)
    ↓
hls-lookup checks: is this symbol from an external package import?
    ↓
Yes → VSCode shows: "Searching in Hackage packages..."
    ↓
hls-lookup downloads http-conduit (if needed)
    ↓
Progress bar: "Downloading http-conduit-2.3.9.1... 45%"
    ↓
hls-lookup finds definition
    ↓
VSCode opens: ~/.hls-lookup/haskell-sources/http-conduit-2.3.9.1/Network/HTTP/Simple.hs:141
```

**Note:** Both HLS and hls-lookup run in parallel. If HLS finds the definition (e.g., for local symbols or standard library), it will be used. If only hls-lookup finds it (external packages), that result is used. The smart filtering ensures hls-lookup doesn't run unnecessarily for local symbols.

## Architecture

### Component 1: Enhanced hls-lookup CLI
- Add JSON output mode for machine consumption
- Add progress reporting (for VSCode to display)
- Add configuration file support
- Keep human-readable mode as default

### Component 2: VSCode Extension
- TypeScript extension that registers a DefinitionProvider
- Runs in parallel with HLS, contributing results for external package symbols
- Only invokes hls-lookup for imported symbols (smart filtering)
- Shows progress notifications
- Manages settings

**Important:** VS Code calls all registered definition providers and merges results. We cannot "intercept" or check HLS first without causing infinite recursion. Instead, we use smart filtering to only invoke hls-lookup for symbols that are likely from external packages.

---

## Implementation Steps

### Phase 1: Make hls-lookup LSP-Compatible

#### 1.1 Add JSON Output Mode

**Add to `exe/Main.hs`:**
```haskell
-- Add --json flag to commands
["goto-json", srcFile, lineStr, colStr] ->
    gotoDefinitionJson srcFile (read lineStr) (read colStr)

-- JSON response type
data GotoResponse = GotoResponse
  { success :: Bool
  , file :: Maybe FilePath
  , line :: Maybe Int
  , column :: Maybe Int
  , symbolName :: Maybe String
  , packageName :: Maybe String
  , message :: String
  } deriving (Generic, ToJSON)

-- Output JSON instead of human-readable
gotoDefinitionJson :: FilePath -> Int -> Int -> IO ()
gotoDefinitionJson srcFile line col = do
    -- Run same logic as gotoDefinition
    -- But capture result and output JSON
    result <- runGoto srcFile line col
    ByteString.putStrLn $ encode result
```

**Dependencies needed:**
- `aeson` - JSON encoding
- `bytestring` - For JSON output

#### 1.2 Add Progress Reporting

**Add progress events:**
```haskell
-- With --json flag, emit progress on stderr
data ProgressEvent = ProgressEvent
  { stage :: String  -- "downloading", "extracting", "searching"
  , message :: String
  , percentage :: Maybe Int
  } deriving (Generic, ToJSON)

-- Example:
emitProgress $ ProgressEvent "downloading" "http-conduit-2.3.9.1" (Just 45)
```

#### 1.3 Add Configuration Support

**Create `~/.hls-lookup/config.json`:**
```json
{
  "cache_dir": "~/.hls-lookup/haskell-sources",
  "source_dirs": [
    "src",
    "lib",
    "source"
  ],
  "editor": "code",
  "download_timeout": 300
}
```

**Dependencies needed:**
- `aeson` - JSON parsing (already needed for JSON output)
- `directory` - Already have it

---

### Phase 2: Create VSCode Extension

#### 2.1 Extension Scaffolding

**Create directory structure:**
```
vscode-hls-lookup/
├── package.json           # Extension metadata
├── tsconfig.json         # TypeScript config
├── src/
│   ├── extension.ts      # Main extension entry point
│   ├── definitionProvider.ts
│   └── hlsLookup.ts     # Calls hls-lookup binary
└── README.md
```

**Initialize:**
```bash
npm install -g yo generator-code
yo code  # Choose "New Extension (TypeScript)"
```

#### 2.2 Implement Definition Provider

**`src/definitionProvider.ts`:**
```typescript
export class HlsLookupDefinitionProvider implements vscode.DefinitionProvider {
  async provideDefinition(
    document: vscode.TextDocument,
    position: vscode.Position,
    token: vscode.CancellationToken
  ): Promise<vscode.Location | undefined> {

    // Get the symbol at the cursor position
    const wordRange = document.getWordRangeAtPosition(position);
    if (!wordRange) {
      return undefined;
    }

    const symbol = document.getText(wordRange);

    // Only invoke hls-lookup for symbols that are likely from external packages
    // This avoids unnecessary calls when HLS can handle it
    if (!this.shouldTryHlsLookup(document, symbol, position)) {
      return undefined;
    }

    // Try hls-lookup for external package symbols
    return await this.tryHlsLookup(document, position);
  }

  private shouldTryHlsLookup(
    document: vscode.TextDocument,
    symbol: string,
    position: vscode.Position
  ): boolean {
    // Check if symbol is imported from an external package
    // by looking at the import statements in the file
    const text = document.getText();
    const imports = this.extractImports(text);

    // If symbol appears in an import from an external package, try hls-lookup
    for (const imp of imports) {
      if (imp.symbols.includes(symbol) && this.isExternalPackage(imp.moduleName)) {
        return true;
      }
    }

    // Also try if it's a qualified name (e.g., HTTP.httpLBS)
    if (symbol.includes('.')) {
      return true;
    }

    return false;
  }

  private extractImports(text: string): Array<{moduleName: string, symbols: string[]}> {
    const imports: Array<{moduleName: string, symbols: string[]}> = [];
    const importRegex = /import\s+(?:qualified\s+)?([A-Z][A-Za-z0-9.]*)\s*(?:\((.*?)\))?/g;

    let match;
    while ((match = importRegex.exec(text)) !== null) {
      const moduleName = match[1];
      const symbolList = match[2] ? match[2].split(',').map(s => s.trim()) : [];
      imports.push({ moduleName, symbols: symbolList });
    }

    return imports;
  }

  private isExternalPackage(moduleName: string): boolean {
    // Common standard library modules that HLS handles well
    const stdLibModules = [
      'Prelude', 'Data.List', 'Data.Maybe', 'Data.Either',
      'Control.Monad', 'Control.Applicative', 'System.IO',
      'Data.Map', 'Data.Set', 'Data.Text', 'Data.ByteString'
    ];

    // If it starts with a common stdlib prefix, let HLS handle it
    return !stdLibModules.some(std => moduleName.startsWith(std));
  }

  private async tryHlsLookup(
    document: vscode.TextDocument,
    position: vscode.Position
  ): Promise<vscode.Location | undefined> {

    return await vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Notification,
        title: "Searching in Hackage packages...",
        cancellable: true
      },
      async (progress, token) => {
        // Call hls-lookup binary with --json flag
        const result = await executeHlsLookup(
          document.fileName,
          position.line + 1,  // Convert to 1-indexed
          position.character + 1,
          progress,
          token
        );

        if (result.success && result.file) {
          const uri = vscode.Uri.file(result.file);
          const pos = new vscode.Position(
            result.line! - 1,  // Convert to 0-indexed
            result.column! - 1
          );
          return new vscode.Location(uri, pos);
        }

        return undefined;
      }
    );
  }
}
```

**`src/hlsLookup.ts`:**
```typescript
import { spawn } from 'child_process';

interface GotoResponse {
  success: boolean;
  file?: string;
  line?: number;
  column?: number;
  symbolName?: string;
  packageName?: string;
  message: string;
}

export async function executeHlsLookup(
  file: string,
  line: number,
  column: number,
  progress: vscode.Progress<{ message?: string; increment?: number }>,
  token: vscode.CancellationToken
): Promise<GotoResponse> {

  return new Promise((resolve, reject) => {
    const proc = spawn('hls-lookup', [
      'goto-json',
      file,
      line.toString(),
      column.toString()
    ]);

    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    proc.stderr.on('data', (data) => {
      stderr += data.toString();

      // Parse progress events from stderr
      try {
        const progressEvent = JSON.parse(stderr);
        if (progressEvent.stage && progressEvent.message) {
          progress.report({
            message: progressEvent.message,
            increment: progressEvent.percentage
          });
        }
      } catch (e) {
        // Not JSON, ignore
      }
    });

    proc.on('close', (code) => {
      if (code === 0) {
        try {
          const result = JSON.parse(stdout);
          resolve(result);
        } catch (e) {
          reject(new Error('Failed to parse hls-lookup output'));
        }
      } else {
        reject(new Error(`hls-lookup failed with code ${code}`));
      }
    });

    // Handle cancellation
    token.onCancellationRequested(() => {
      proc.kill();
      reject(new Error('Cancelled'));
    });
  });
}
```

#### 2.3 Extension Settings

**`package.json`:**
```json
{
  "name": "hls-lookup",
  "displayName": "HLS Lookup",
  "description": "Go to definition for Hackage packages",
  "version": "0.1.0",
  "engines": {
    "vscode": "^1.80.0"
  },
  "activationEvents": [
    "onLanguage:haskell"
  ],
  "main": "./out/extension.js",
  "contributes": {
    "configuration": {
      "title": "HLS Lookup",
      "properties": {
        "hlsLookup.enabled": {
          "type": "boolean",
          "default": true,
          "description": "Enable HLS Lookup for Hackage packages"
        },
        "hlsLookup.binaryPath": {
          "type": "string",
          "default": "hls-lookup",
          "description": "Path to hls-lookup binary"
        },
        "hlsLookup.cacheDir": {
          "type": "string",
          "default": "~/.hls-lookup/haskell-sources",
          "description": "Directory for caching Hackage packages"
        },
        "hlsLookup.showProgress": {
          "type": "boolean",
          "default": true,
          "description": "Show progress notifications when downloading"
        }
      }
    }
  }
}
```

---

## Design Decisions

### Why Not "Intercept" HLS Directly?

**The Problem:**
The initial approach attempted to check if HLS found a definition by calling `vscode.executeDefinitionProvider` from within our own provider. This creates **infinite recursion** because that command calls ALL providers, including ours.

**The Solution:**
Instead, we use a **parallel provider** approach with smart filtering:

1. **Register as another DefinitionProvider** alongside HLS
2. **Smart filtering**: Only invoke hls-lookup for symbols that appear to be from external packages
3. **Let VS Code merge results**: Both providers run independently, VS Code combines results

**How it works:**
- When user Ctrl+Clicks, VS Code calls both HLS and hls-lookup providers
- hls-lookup checks if the symbol is from an import (heuristic)
- If yes → invoke hls-lookup binary and return result
- If no → return undefined (let HLS handle it)
- If both find results, user can choose; if only one finds it, that's what opens

**Benefits:**
- No recursion issues
- Simple, robust implementation
- Works with any language server
- Minimal performance impact due to smart filtering

### Alternative: Command Override Approach

If you want **true fallback behavior** (hls-lookup NEVER runs if HLS succeeds), you could override the "Go to Definition" command instead:

```typescript
vscode.commands.registerCommand(
  'editor.action.revealDefinition',
  async () => {
    // Custom logic to check HLS then fallback
  },
  this
);
```

**Why we didn't choose this:**
- More complex and fragile
- Conflicts with other extensions doing similar things
- Command registration order matters
- Harder to maintain

The parallel provider approach is simpler and more aligned with VS Code's architecture.

---

## Testing Strategy

### 1. Test hls-lookup JSON mode
```bash
# Should output valid JSON
hls-lookup goto-json app/PackageDownload.hs 46 24

# Expected output:
{
  "success": true,
  "file": "/home/user/.hls-lookup/haskell-sources/http-conduit-2.3.9.1/Network/HTTP/Simple.hs",
  "line": 141,
  "column": 1,
  "symbolName": "httpLBS",
  "packageName": "http-conduit",
  "message": "Found definition in http-conduit-2.3.9.1"
}
```

### 2. Test VSCode Extension
```
1. Open a Haskell file
2. Ctrl+Click on a symbol from an external package
3. Should see progress: "Searching in Hackage packages..."
4. Should open the definition in the cached package
```

### 3. Test Error Cases
```
- Symbol not found → Show error message
- hls-lookup not installed → Show helpful error
- Network failure → Show retry option
```

---

## Installation Instructions (Future)

### For Users:

```bash
# 1. Install hls-lookup globally
cabal install hls-lookup

# 2. Install VSCode extension
# Option A: From marketplace
code --install-extension hls-lookup

# Option B: From .vsix file
code --install-extension hls-lookup-0.1.0.vsix

# 3. Configure (optional)
# Edit ~/.hls-lookup/config.json
```

---

## Future Enhancements

1. **Improved Smart Filtering**
   - Use HIE files to definitively know if symbol is local or external
   - Cache import analysis results per file
   - Learn from user behavior (if hls-lookup is cancelled, don't try that module again)
   - Integration with cabal/stack to know which packages are dependencies

2. **Cache Management UI**
   - VSCode command: "HLS Lookup: Clear Cache"
   - Show cache size in status bar

3. **Symbol Preview**
   - Show definition in hover tooltip before jumping
   - Include type signature

4. **Multiple Definition Handling**
   - When symbol is ambiguous, show quick pick menu
   - Let user choose which definition to jump to

5. **Build on Demand** (Advanced)
   - Prompt to build package with HIE for full navigation
   - "Build http-conduit for better navigation? (y/n)"

6. **Hoogle Integration**
   - Query Hoogle API for additional context
   - Show documentation in hover

7. **Performance Optimization**
   - Debounce hls-lookup calls during rapid navigation
   - Pre-fetch common packages in the background
   - Use worker threads for import analysis

---

## Open Questions

1. **How to improve the external package heuristic?**
   - Could we query HLS's import data via LSP?
   - Should we parse .cabal or stack.yaml for dependencies?
   - What about transitive dependencies?

2. **How to handle multiple workspace folders?**
   - Look for HIE files in each workspace root
   - Share cache across workspaces?

3. **What if cabal is not in PATH?**
   - Extension setting to specify cabal path

4. **Should we cache negative results?**
   - Don't repeatedly try symbols that don't exist
   - How long to cache? Per session or persistent?

5. **Rate limiting for Hackage downloads?**
   - Respect Hackage's rate limits
   - Show warning if hitting limits

6. **What if both HLS and hls-lookup return results?**
   - Current behavior: VS Code shows both, user chooses
   - Should we deduplicate if they point to the same location?
   - Should we prioritize one over the other?

7. **How to handle symbols with no imports (orphan instances)?**
   - Some symbols are in scope without explicit imports
   - Should we be more aggressive and try hls-lookup anyway?

---

## Timeline Estimate

- **Phase 1 (hls-lookup enhancements):** 2-3 days
  - JSON output: 4 hours
  - Progress reporting: 2 hours
  - Configuration: 2 hours
  - Testing: 2 hours

- **Phase 2 (VSCode extension):** 3-4 days
  - Scaffolding: 1 hour
  - Definition provider: 4 hours
  - Progress integration: 2 hours
  - Settings: 2 hours
  - Testing: 4 hours
  - Documentation: 2 hours

**Total: 5-7 days** of focused work

---

## Success Criteria

✅ Ctrl+Click on external package symbol opens definition
✅ Progress bar shows download status
✅ Works seamlessly with existing HLS
✅ Configurable via VSCode settings
✅ No performance impact when HLS can handle definition
✅ Clear error messages when things go wrong
