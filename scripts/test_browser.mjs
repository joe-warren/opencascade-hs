// Headless browser test for the waterfall-cad playground.
// Loads the playground, types Haskell code, clicks Run, checks output.

import { createRequire } from 'module';
import fs from 'node:fs';
const require = createRequire('/opt/puppeteer/');
const puppeteer = require('puppeteer');

const TIMEOUT = 120_000;
const URL = 'http://localhost:8080';

// Test cases: [name, code, expected stdout substring, should_succeed]
const TESTS = [
  ['hello', 'main = putStrLn "hello from playground"', 'hello from playground', true],
  ['unitcube_diagram', `import Waterfall.Solids (unitCube)
import Waterfall.Diagram (solidDiagram)
import Linear (V3(..))
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  let solid = unitCube
      diagram = solidDiagram (V3 2 3 1) solid
  diagram \`seq\` putStrLn "diagram created!" >> hFlush stdout`, 'diagram created!', true],
  ['volume_booleans', `import Waterfall.Solids (unitCube, unitSphere, volume)
import Waterfall.Booleans (difference)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn ("cube: " ++ show (volume unitCube)) >> hFlush stdout
  putStrLn ("sphere: " ++ show (volume unitSphere)) >> hFlush stdout
  putStrLn ("diff: " ++ show (volume (difference unitCube unitSphere))) >> hFlush stdout`, 'cube:', true],
  ['makebox_shell', `import OpenCascade.GP.Pnt as Pnt
import OpenCascade.BRepPrimAPI.MakeBox as MakeBox
import Data.Acquire (withAcquire)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)

main :: IO ()
main = withAcquire go $ \\_ -> pure ()
  where
    go = do
      p1 <- Pnt.new 0 0 0
      p2 <- Pnt.new 1 1 1
      box <- MakeBox.fromPnts p1 p2
      liftIO $ putStrLn "about to call shell" >> hFlush stdout
      sh <- MakeBox.shell box
      liftIO $ putStrLn "shell extracted!" >> hFlush stdout`, 'shell extracted!', true],
  ['glb_viewer', `import Waterfall.Solids (centeredCube, unitSphere)
import Waterfall.Booleans (difference)
import Waterfall.Transforms (uScale)
import Waterfall.IO (writeGLB)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  writeGLB 0.01 "/out.glb" (difference centeredCube (uScale 0.65 unitSphere))
  putStrLn "glb written" >> hFlush stdout`, 'glb written', true, true],
];

function glbSignedVolume(glb) {
  const jlen = glb.readUInt32LE(12);
  const doc = JSON.parse(glb.subarray(20, 20 + jlen).toString());
  const binOff = 20 + jlen + 8;
  const bin = glb.subarray(binOff);
  const compSize = { 5126: 4, 5123: 2, 5125: 4 };
  const readAcc = (idx) => {
    const acc = doc.accessors[idx];
    const bv = doc.bufferViews[acc.bufferView];
    const start = (bv.byteOffset || 0) + (acc.byteOffset || 0);
    const per = acc.type === 'VEC3' ? 3 : 1;
    const size = compSize[acc.componentType];
    const out = [];
    for (let i = 0; i < acc.count; i++) {
      const o = start + i * per * size;
      const v = [];
      for (let k = 0; k < per; k++) {
        v.push(
          acc.componentType === 5126
            ? bin.readFloatLE(o + k * size)
            : acc.componentType === 5123
              ? bin.readUInt16LE(o + k * size)
              : bin.readUInt32LE(o + k * size)
        );
      }
      out.push(v);
    }
    return out;
  };
  let vol = 0;
  for (const m of doc.meshes || []) {
    for (const pr of m.primitives) {
      const pos = readAcc(pr.attributes.POSITION);
      const idx = readAcc(pr.indices).map((x) => x[0]);
      for (let t = 0; t < idx.length; t += 3) {
        const [a, b, c] = [pos[idx[t]], pos[idx[t + 1]], pos[idx[t + 2]]];
        vol +=
          (a[0] * (b[1] * c[2] - b[2] * c[1]) -
            a[1] * (b[0] * c[2] - b[2] * c[0]) +
            a[2] * (b[0] * c[1] - b[1] * c[0])) /
          6;
      }
    }
  }
  return vol;
}

async function runTest(page, name, code, expectedStdout, shouldSucceed, checkModel) {
  console.log(`\n=== Test: ${name} ===`);

  // Wait for the editor and status to be ready
  await page.waitForSelector('#status', { timeout: TIMEOUT });

  // Wait for "Ready!" status
  await page.waitForFunction(
    () => document.getElementById('status')?.textContent === 'Ready!',
    { timeout: TIMEOUT }
  );
  console.log('  Playground ready');

  // Clear stdout/stderr
  await page.evaluate(() => {
    document.getElementById('stdout').value = '';
    document.getElementById('stderr').value = '';
  });

  // Set the editor content
  await page.evaluate((c) => {
    window.editor.dispatch({ changes: { from: 0, to: window.editor.state.doc.length, insert: c } });
  }, code);
  console.log('  Code entered');

  // Click Run
  await page.click('#runBtn');
  console.log('  Run clicked');

  // Wait for the run to complete (button re-enables or status changes)
  await page.waitForFunction(
    () => {
      const status = document.getElementById('status')?.textContent || '';
      return status.startsWith('Done') || status.startsWith('Error') || status.startsWith('Ready');
    },
    { timeout: TIMEOUT }
  );

  // Small delay for output to flush
  await new Promise(r => setTimeout(r, 500));

  const stdout = await page.evaluate(() => document.getElementById('stdout').value);
  const stderr = await page.evaluate(() => document.getElementById('stderr').value);
  const status = await page.evaluate(() => document.getElementById('status').textContent);

  console.log(`  Status: ${status}`);
  if (stdout.trim()) console.log(`  Stdout: ${stdout.trim().substring(0, 200)}`);
  if (stderr.trim()) console.log(`  Stderr: ${stderr.trim().substring(0, 200)}`);

  // Check console errors
  const consoleErrors = await page.evaluate(() => window.__testErrors || []);
  if (consoleErrors.length > 0) {
    console.log(`  Console errors: ${consoleErrors.length}`);
    consoleErrors.slice(0, 3).forEach(e => console.log(`    ${e.substring(0, 120)}`));
  }

  if (shouldSucceed) {
    if (!stdout.includes(expectedStdout)) {
      console.log(`  FAIL ✗ - expected stdout to contain "${expectedStdout}"`);
      return false;
    }
    if (checkModel) {
      // The viewer should pick up the .glb from the wasm filesystem and
      // model-viewer should actually parse and load it.
      await page.waitForFunction(
        () =>
          window.__lastModel &&
          document.getElementById('viewer')?.loaded === true,
        { timeout: 60000 }
      );
      const modelPath = await page.evaluate(() => window.__lastModel);
      const b64 = await page.evaluate(async () => {
        const v = document.getElementById('viewer');
        const buf = await fetch(v.dataset.url).then((r) => r.arrayBuffer());
        const bytes = new Uint8Array(buf);
        let s = '';
        for (let i = 0; i < bytes.length; i++) s += String.fromCharCode(bytes[i]);
        return btoa(s);
      });
      const glb = Buffer.from(b64, 'base64');
      fs.writeFileSync('/tmp/extracted-model.glb', glb);
      const magic = glb.subarray(0, 4).toString();
      console.log(`  Model: ${modelPath} (${glb.length} bytes, magic=${magic})`);
      if (magic !== 'glTF' || glb.length < 1000) {
        console.log('  FAIL ✗ - extracted glb looks invalid');
        return false;
      }
      // Signed mesh volume sanity check: catches mangled triangulation
      // (e.g. boolean-cut holes meshed as filled faces) that structural
      // checks miss. Expected analytic volume of the test CSG is ~0.11.
      const vol = glbSignedVolume(glb);
      console.log(`  Mesh volume: ${vol.toFixed(4)} (expected ~0.11)`);
      if (!(Math.abs(vol - 0.11) < 0.03)) {
        console.log('  FAIL ✗ - mesh volume far from analytic volume, triangulation is mangled');
        return false;
      }
    }
    console.log(`  PASS ✓`);
    return true;
  } else {
    console.log(`  (expected failure)`);
    return true;
  }
}

async function main() {
  const browser = await puppeteer.launch({
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox', '--disable-gpu'],
  });

  const page = await browser.newPage();

  // Capture console errors
  await page.evaluateOnNewDocument(() => {
    window.__testErrors = [];
    const origError = console.error;
    console.error = (...args) => {
      window.__testErrors.push(args.map(String).join(' '));
      origError.apply(console, args);
    };
  });

  // Also capture page crashes
  page.on('pageerror', err => {
    console.log(`  PAGE ERROR: ${err.message.substring(0, 200)}`);
  });

  console.log(`Loading ${URL}...`);
  await page.goto(URL, { waitUntil: 'domcontentloaded', timeout: TIMEOUT });

  let passed = 0;
  let failed = 0;

  for (const [name, code, expected, shouldSucceed, checkModel] of TESTS) {
    try {
      const ok = await runTest(page, name, code, expected, shouldSucceed, checkModel);
      if (ok) passed++; else failed++;
    } catch (e) {
      console.log(`  ERROR: ${e.message.substring(0, 200)}`);
      failed++;
    }

    // Reload for next test (clean state)
    if (TESTS.indexOf(TESTS.find(t => t[0] === name)) < TESTS.length - 1) {
      await page.reload({ waitUntil: 'domcontentloaded', timeout: TIMEOUT });
    }
  }

  await browser.close();

  console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
  process.exit(failed > 0 ? 1 : 0);
}

main().catch(e => {
  console.error('Fatal:', e.message);
  process.exit(1);
});
