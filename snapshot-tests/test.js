const Nightmare = require('nightmare');
const Niffy = require('niffy');
const assert = require('assert');
const httpServer = require('http-server');
const path = require('path');
const exec = require('pshell');
const fs = require('fs');

const headServer = {
  port: '9395',
  root: path.resolve(__dirname, '../example'),
};
const masterServer = {
  port: '9394',
  root: path.resolve(__dirname, '_master/example'),
};

var servers = [];

function startServer(config) {
  return new Promise(function(resolve, reject) {
    var options = {
      root: config.root
    };
    const server = httpServer.createServer(options);
    server.listen(config.port, 'localhost', function () {
      console.log("INFO: started server " + server.root);
      resolve();
    });
    servers.push(server);
  });
}

function build(config) {
  const root = path.resolve(config.root, '..');
  return exec("npm run-script build-example", {cwd: root}).then(() => console.log(`INFO: Built ${config.root}`));
}

function cloneMaster() {
  const root = path.resolve(masterServer.root, '..');
  if (fs.existsSync(root)) {
    console.log("INFO: don't need to clone");
    return new Promise(resolve => resolve());
  } else {
    return exec("git clone ../ _master", {cwd: path.resolve(root, '..')});
  }
}

function checkoutMaster() {
  return exec("git fetch", { cwd: masterServer.root})
    .then(() => exec("git reset --hard", { cwd: masterServer.root}))
    .then(() => exec("git checkout origin/master", { cwd: masterServer.root}));
}

before(() => {
  const buildMaster =
    cloneMaster().then(() => checkoutMaster()).then(() => build(masterServer));

  return Promise.all([
    build(headServer),
    buildMaster,
    startServer(headServer),
    startServer(masterServer),
  ]);
});

after(() => {
  servers.forEach(s => s.close());
});

describe('Load a Page', function() {
  let niffy = null;
  let headHost = `http://localhost:${headServer.port}`;
  let masterHost = `http://localhost:${masterServer.port}`;
  beforeEach(() => {
    niffy = new Niffy(masterHost, headHost, { show: false });
  });

  afterEach(function* () {
    yield niffy.end();
  });

  it('Pricing demo (editable)', function* () {
    yield niffy.test('/');
  });

  it('Pricing demo (static)', function* () {
    yield niffy.test('/', function* (nightmare) {
      yield nightmare.click('body > div:nth-child(3) > button:nth-child(1)');
    });
  });
});
