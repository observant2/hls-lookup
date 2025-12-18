import * as vscode from 'vscode';
import { executeHlsLookup } from './hlsLookup';

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

    // Call hls-lookup for all symbols
    // The CLI will determine if it's a local/boot library (skip) or external package (download)
    return await this.tryHlsLookup(document, position, token);
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

    try {
      const result = await executeHlsLookup(
        document.fileName,
        position.line + 1,  // Convert to 1-indexed
        position.character + 1,
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

      vscode.window.showWarningMessage(result.message);

      return undefined;
    } catch (error) {
      // Silently fail - this allows HLS to handle the definition if hls-lookup fails
      console.error('hls-lookup error:', error);
      return undefined;
    }
  }
}
