'use strict';
const fs = require('fs');

const log = (x, label) => {
  const line = [new Date(), JSON.stringify(x)];
  if (label) {
    line.splice(1, 0, label);
  }
  fs.appendFileSync('server.log', line.join(': ').concat('\n'));
};

const handleError = error => {
  log(error, 'FATAL');
  process.exit(error.code || 1);
};

const isInstalled = dependency => {
  try {
    require.resolve(dependency);
    return true;
  } catch (e) {
    return false;
  }
};

const ensureDependencies = () => {
  const {dependencies} = require('./package.json');
  if (Object.keys(dependencies).every(isInstalled)) {
    return Promise.resolve();
  } else {
    const execFileAsync = require('util').promisify(require('child_process').execFile);
    return execFileAsync('npm', ['install']);
  }
};

const normalize = results => results.map(result => {
  if (result.error) {
    // keep only the top of the stacktrace
    result.error = result.error[0].split('\n')[1];
    log(result);
  }

  // Emacs buffer lines are zero-indexed
  result.line--;

  result.filename = result.fileName;
  delete result.fileName;

  return result;
});

if (require.main === module) {
  process.on('unhandledRejection', handleError);

  (async () => {
    try {
      await ensureDependencies();

      const {importCost, cleanup} = require('import-cost');
      const {startServer} = require('elrpc');

      const server = await startServer();
      const emitters = {};

      server.defineMethod('calculate', (filename, contents, language) => new Promise(resolve => {
        if (filename in emitters) {
          emitters[filename].removeAllListeners();
        }

        emitters[filename] = importCost(filename, contents, language);
        emitters[filename].on('done', results => resolve(normalize(results)));
      }));

      server.defineMethod('disconnect', filename => {
        if (filename in emitters) {
          delete emitters[filename];
        }

        if (!Object.keys(emitters).length) {
          cleanup();
        }
      });

      server.wait();
    } catch (e) {
      handleError(e);
    }
  })();
}
