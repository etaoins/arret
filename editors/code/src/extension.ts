import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  Trace,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export const activate = (_context: vscode.ExtensionContext): void => {
  const command = 'arret-lsp-server';

  const serverOptions: ServerOptions = {
    run: { command },
    debug: { command },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: ['arret'],
  };

  client = new LanguageClient('arret', serverOptions, clientOptions);
  client.trace = Trace.Verbose;

  client.start();
};

// this method is called when your extension is deactivated
export const deactivate = (): Thenable<void> | undefined => {
  if (!client) {
    return;
  }

  return client.stop();
};
