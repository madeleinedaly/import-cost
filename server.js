'use strict';
const fs = require('fs');

const log = x => {
  const line = [new Date(), JSON.stringify(x)].join(': ').concat('\n');
  fs.appendFileSync('server.log', line);
};

const handleError = error => {
  log(error);
};

const isInstalled = dependency => {
  try {
    require.resolve(dependency);
    return true;
  } catch (e) {
    return false;
  }
};

const ensureDepdenencies = () => {
  const {dependencies} = require('./package.json');
  if (Object.keys(dependencies).every(isInstalled)) {
    return Promise.resolve();
  } else {
    const {promisify} = require('util');
    const {execFile} = require('child_process');
    const execFileAsync = promisify(execFile);
    return execFileAsync('npm', ['install']);
  }
};

const startServer = () => {
  const epc = require('elrpc');
  return epc.startServer();
};

const normalize = result => {
  if (result.error) {
    log(result);

    // don't send the entire stacktrace, otherwise Emacs will lag from rendering long lines.
    result.error = result.error[0].split('\n')[1];
  }

  // Emacs buffer lines are zero-indexed.
  result.line--;

  // fix property key
  result.filename = result.fileName;
  delete result.fileName;

  return result;
};

const defineMethods = server => {
  const {importCost, cleanup} = require('import-cost');
  const emitters = {};

  server.defineMethod('calculate', (filename, contents, language) => new Promise(resolve => {
    if (filename in emitters) {
      emitters[filename].removeAllListeners();
    }

    emitters[filename] = importCost(filename, contents, language);
    emitters[filename].on('done', results => resolve(results.map(normalize)));
  }));

  server.defineMethod('disconnect', filename => {
    if (filename in emitters) {
      delete emitters[filename];
    }

    if (!Object.keys(emitters).length) {
      cleanup();
    }
  });

  return server;
};

if (require.main === module) {
  process.on('unhandledRejection', handleError);

  ensureDepdenencies()
    .then(startServer)
    .then(defineMethods)
    .then(server => server.wait())
    .catch(handleError);
}
