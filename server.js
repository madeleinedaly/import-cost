'use strict';
const fs = require('fs');
const os = require('os');

const handleError = error => {
  fs.appendFileSync('errors.log', [new Date(), error].join(': '));
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

const ensureInstall = () => {
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

const configureServer = server => {
  const {importCost, cleanup} = require('import-cost');
  const _ = require('lodash');
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

process.on('unhandledRejection', handleError);

// epc:start-epc expects this
const [,,port] = process.argv;
process.stdout.write(port.concat(os.EOL));

ensureInstall()
  .then(startServer)
  .then(configureServer)
  .then(server => server.wait())
  .catch(handleError);

/*

  (import-cost--deactivate)
  (import-cost--activate)
  (import-cost--process-active-buffer)


  (save-excursion
    (goto-char (point-min))
    (search-forward "require('lodash')" nil t)
    (end-of-line)
    (setq my-overlay (ov-create (point) (point)))
    (ov-set my-overlay 'after-string
                       (propertize (format "  %dKB" (/ 72269 1024))
                                   'font-lock-face '(:foreground "#d44e40"))))

*/
