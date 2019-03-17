const puppeteer = require('puppeteer');
const httpServer = require('http-server');
const path = require('path');
const exec = require('pshell');
const fs = require('fs');
const compareImages = require("resemblejs/compareImages");

const referenceRef = process.env.COMPARE_WITH || "origin/master";


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
    .then(() => exec(`git checkout ${referenceRef}`, { cwd: masterServer.root}));
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

describe(`Comparing screenshots against ${referenceRef}`, function() {
  let browser = null;
  let headHost = `http://localhost:${headServer.port}`;
  let masterHost = `http://localhost:${masterServer.port}`;

  before(async () => {
    browser = await puppeteer.launch();
  });

  after(async () => {
    await browser.close();
  });

  function escapeXpathString(str) {
    const splitedQuotes = str.replace(/'/g, `', "'", '`);
    return `concat('${splitedQuotes}', '')`;
  }
  async function clickButton(page, text) {
    const escapedText = escapeXpathString(text);
    const linkHandlers = await page.$x(`//button[contains(text(), ${escapedText})]`);
    if (linkHandlers.length == 1) {
      await linkHandlers[0].click();
    } else if (linkHandlers.length > 1) {
      throw new Error("Multiple buttons found");
    } else {
      throw new Error("Button not found");
    }
  }

  async function test(name, actions) {
    const page = await browser.newPage();
    page.setViewport({width: 1400, height: 978});

    await page.goto(headHost);
    await actions(page);
    image1 = await page.screenshot();

    await page.goto(masterHost);
    await actions(page).catch(() => null);
    image2 = await page.screenshot();

    const result = await compareImages(image1, image2);

    const filename = `./${name}.png`;
    if (result.rawMisMatchPercentage == 0) {
      if (fs.existsSync(filename)) {
        fs.unlinkSync(filename);
      }
    } else {
      fs.writeFileSync(filename, result.getBuffer());
    }

    if (!result.isSameDimensions) {
      throw new Error(`Image dimensions don't match: ${result}`);
    } else if (result.rawMisMatchPercentage > 0.02) {
      throw new Error(`Images don't match: ${JSON.stringify(result)}`);
    }
  }

  it('Pricing demo (editable)', () => {
    return test('Pricing demo (editable)', async page => {});
  });

  it('Pricing demo (focused comments)', () => {
    return test('Pricing demo (focused comments)', async page => {
      const linkHandlers = await page.$x("//aside[contains(.,'Quest')]");
      await linkHandlers[0].click();
    });
  });

  it('Pricing demo (static)', () => {
    return test('Pricing demo (static)', async page => {
      await clickButton(page, 'Static');
    });
  });
});
