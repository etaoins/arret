import { downloadAndUnzipVSCode } from 'vscode-test';

import { VSCODE_VERSION } from './vsCodeVersion';

async function main(): Promise<void> {
  try {
    await downloadAndUnzipVSCode(VSCODE_VERSION);
  } catch (err) {
    console.error('Failed to download VS Code');
    process.exit(1);
  }
}

main();
