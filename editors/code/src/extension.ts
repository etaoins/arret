import * as vscode from 'vscode';

export const activate = (_context: vscode.ExtensionContext): void => {
  console.log('Congratulations, your extension "arret" is now active!');
};

// this method is called when your extension is deactivated
export const deactivate = (): void => {
  console.log('Arret is deactivating');
};
