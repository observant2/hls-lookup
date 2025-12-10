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

      vscode.window.showWarningMessage(result.message);

      return undefined;
    } catch (error) {
      // Silently fail - this allows HLS to handle the definition if hls-lookup fails
      console.error('hls-lookup error:', error);
      return undefined;
    }
  }
}
