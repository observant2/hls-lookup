import * as vscode from 'vscode';
import { executeHlsLookup, GotoResponse } from './hlsLookup';

export class HlsLookupDefinitionProvider implements vscode.DefinitionProvider {
  async provideDefinition(
    document: vscode.TextDocument,
    position: vscode.Position,
    token: vscode.CancellationToken
  ): Promise<vscode.Location | undefined> {

    // Get the symbol at the cursor position
    // Use custom regex to capture both simple names and qualified names
    // Matches: httpLBS, Tar.unpack, Network.HTTP.Simple.httpLBS
    const qualifiedNameRegex = /[a-zA-Z][a-zA-Z0-9_']*(?:\.[a-zA-Z0-9_']+)*/;
    const wordRange = document.getWordRangeAtPosition(position, qualifiedNameRegex);
    if (!wordRange) {
      return undefined;
    }

    const symbol = document.getText(wordRange);

    // Only invoke hls-lookup for symbols that are likely from external packages
    // This avoids unnecessary calls when HLS can handle it
    if (!this.shouldTryHlsLookup(document, symbol)) {
      return undefined;
    }

    // Try hls-lookup for external package symbols
    return await this.tryHlsLookup(document, position, token);
  }

  private shouldTryHlsLookup(
    document: vscode.TextDocument,
    symbol: string
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

    // Match: import [qualified] ModuleName [(symbol1, symbol2, ...)]
    const importRegex = /import\s+(?:qualified\s+)?([A-Z][A-Za-z0-9.]*)\s*(?:\((.*?)\))?/g;

    let match;
    while ((match = importRegex.exec(text)) !== null) {
      const moduleName = match[1];
      const symbolList = match[2]
        ? match[2].split(',').map(s => s.trim().replace(/\(.*?\)/, '').trim())
        : [];
      imports.push({ moduleName, symbols: symbolList });
    }

    return imports;
  }

  private isExternalPackage(moduleName: string): boolean {
    // Common standard library modules that HLS handles well
    const stdLibModules = [
      'Prelude', 'Data.List', 'Data.Maybe', 'Data.Either',
      'Control.Monad', 'Control.Applicative', 'System.IO',
      'Data.Map', 'Data.Set', 'Data.Text', 'Data.ByteString',
      'Control.Exception', 'System.Environment', 'Data.Char',
      'Data.Bool', 'Data.Function', 'Data.Tuple', 'Text.Read',
      'Text.Show', 'Data.Ord', 'Data.Eq', 'Data.Foldable',
      'Data.Traversable', 'Control.Arrow', 'Data.Monoid'
    ];

    // If it starts with a common stdlib prefix, let HLS handle it
    return !stdLibModules.some(std => moduleName.startsWith(std));
  }

  private async tryHlsLookup(
    document: vscode.TextDocument,
    position: vscode.Position,
    token: vscode.CancellationToken
  ): Promise<vscode.Location | undefined> {

    // Check if hls-lookup is enabled
    const config = vscode.workspace.getConfiguration('hlsLookup');
    const enabled = config.get<boolean>('enabled', true);
    if (!enabled) {
      return undefined;
    }

    const showProgress = config.get<boolean>('showProgress', true);

    if (showProgress) {
      return await vscode.window.withProgress(
        {
          location: vscode.ProgressLocation.Notification,
          title: "Searching in Hackage packages...",
          cancellable: true
        },
        async (progress, progressToken) => {
          return await this.callHlsLookup(document, position, progress, progressToken);
        }
      );
    } else {
      // Call without progress notification
      return await this.callHlsLookup(document, position, undefined, token);
    }
  }

  private async callHlsLookup(
    document: vscode.TextDocument,
    position: vscode.Position,
    progress?: vscode.Progress<{ message?: string; increment?: number }>,
    token?: vscode.CancellationToken
  ): Promise<vscode.Location | undefined> {
    try {
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
          (result.line ?? 1) - 1,  // Convert to 0-indexed
          (result.column ?? 1) - 1
        );
        return new vscode.Location(uri, pos);
      }

      return undefined;
    } catch (error) {
      // Silently fail - this allows HLS to handle the definition if hls-lookup fails
      console.error('hls-lookup error:', error);
      return undefined;
    }
  }
}
