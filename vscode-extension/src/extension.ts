import * as vscode from 'vscode';
import { HlsLookupDefinitionProvider } from './definitionProvider';

export function activate(context: vscode.ExtensionContext) {
	console.log('HLS Lookup extension is now active!');

	// Register the definition provider for Haskell files
	const provider = new HlsLookupDefinitionProvider();
	const disposable = vscode.languages.registerDefinitionProvider(
		{ language: 'haskell', scheme: 'file' },
		provider
	);

	context.subscriptions.push(disposable);
}

export function deactivate() {}
