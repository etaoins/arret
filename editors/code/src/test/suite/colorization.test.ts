import * as assert from 'assert';
import * as fs from 'fs';
import { join, basename, dirname } from 'path';

import { commands, Uri } from 'vscode';

const hasThemeChange = (
  d: { [key: string]: unknown },
  p: { [key: string]: unknown },
): boolean => {
  const keys = Object.keys(d);
  for (const key of keys) {
    if (d[key] !== p[key]) {
      return true;
    }
  }
  return false;
};

const assertUnchangedTokens = (
  testFixurePath: string,
  done: (err?: unknown) => void,
): Thenable<void> => {
  const fileName = basename(testFixurePath);

  return commands
    .executeCommand('_workbench.captureSyntaxTokens', Uri.file(testFixurePath))
    .then(data => {
      try {
        const resultsFolderPath = join(
          dirname(dirname(testFixurePath)),
          'colorize-results',
        );
        if (!fs.existsSync(resultsFolderPath)) {
          fs.mkdirSync(resultsFolderPath);
        }
        const resultPath = join(
          resultsFolderPath,
          fileName.replace('.', '_') + '.json',
        );
        if (fs.existsSync(resultPath)) {
          const previousData = JSON.parse(
            fs.readFileSync(resultPath).toString(),
          );
          try {
            assert.deepEqual(data, previousData);
          } catch (e) {
            fs.writeFileSync(resultPath, JSON.stringify(data, null, '\t'), {
              flag: 'w',
            });
            if (
              Array.isArray(data) &&
              Array.isArray(previousData) &&
              data.length === previousData.length
            ) {
              for (let i = 0; i < data.length; i++) {
                const d = data[i];
                const p = previousData[i];
                if (d.c !== p.c || hasThemeChange(d.r, p.r)) {
                  throw e;
                }
              }
              // different but no tokenization ot color change: no failure
            } else {
              throw e;
            }
          }
        } else {
          fs.writeFileSync(resultPath, JSON.stringify(data, null, '\t'));
        }
        done();
      } catch (e) {
        done(e);
      }
    }, done);
};

suite('colorization', () => {
  const extensionColorizeFixturePath = join(
    __dirname,
    '../../../src/test/colorize-fixtures',
  );
  if (fs.existsSync(extensionColorizeFixturePath)) {
    const fixturesFiles = fs.readdirSync(extensionColorizeFixturePath);
    fixturesFiles.forEach(fixturesFile => {
      // define a test for each fixture
      test(fixturesFile, function(done) {
        assertUnchangedTokens(
          join(extensionColorizeFixturePath, fixturesFile),
          done,
        );
      });
    });
  }
});
