import { spawn } from 'child_process';
import * as vscode from 'vscode';

export interface GotoResponse {
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
  progress?: vscode.Progress<{ message?: string; increment?: number }>,
  token?: vscode.CancellationToken
): Promise<GotoResponse> {

  const config = vscode.workspace.getConfiguration('hlsLookup');
  const binaryPath = config.get<string>('binaryPath', 'hls-lookup');

  return new Promise((resolve, reject) => {
    // Determine the working directory - use the workspace folder containing the file
    const workspaceFolder = vscode.workspace.getWorkspaceFolder(vscode.Uri.file(file));
    const cwd = workspaceFolder?.uri.fsPath;

    const proc = spawn(binaryPath, [
      'goto-json',
      file,
      line.toString(),
      column.toString()
    ], {
      cwd: cwd  // Set working directory to the workspace folder
    });

    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    proc.stderr.on('data', (data) => {
      stderr += data.toString();

      // Parse progress events from stderr
      // The CLI outputs progress as JSON on stderr
      try {
        const lines = stderr.split('\n').filter(l => l.trim());
        for (const line of lines) {
          try {
            const progressEvent = JSON.parse(line);
            if (progressEvent.stage && progressEvent.message && progress) {
              progress.report({
                message: progressEvent.message,
                increment: progressEvent.percentage
              });
            }
          } catch {
            // Not JSON, ignore (it's regular log output)
          }
        }
      } catch {
        // Ignore parsing errors
      }
    });

    proc.on('close', (code) => {
      if (code === 0) {
        try {
          const result = JSON.parse(stdout) as GotoResponse;
          resolve(result);
        } catch (e) {
          reject(new Error(`Failed to parse hls-lookup output: ${e}`));
        }
      } else {
        reject(new Error(`hls-lookup failed with code ${code}: ${stderr}`));
      }
    });

    proc.on('error', (err) => {
      reject(new Error(`Failed to execute hls-lookup: ${err.message}. Make sure hls-lookup is installed and in PATH.`));
    });

    // Handle cancellation
    if (token) {
      token.onCancellationRequested(() => {
        proc.kill();
        reject(new Error('Cancelled by user'));
      });
    }
  });
}
