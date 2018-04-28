'use strict';

const handleError = error => {
  const {appendFileSync} = require('fs');
  const loggedError = [new Date(), error].join(': ').concat('\n');
  appendFileSync('errors.log', loggedError);
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

const defineMethods = server => {
  const {importCost, cleanup} = require('import-cost');
  const emitters = {};

  server.defineMethod('calculate', (filename, contents, language) => new Promise(resolve => {
    if (filename in emitters) {
      emitters[filename].removeAllListeners();
    }

    emitters[filename] = importCost(filename, contents, language);
    emitters[filename].on('done', results => {
      const response = results.map(result => result.error ? {
        ...result,
        error: result.error[0].split('\n')[1]
      } : result);

      resolve(response);
    });
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
