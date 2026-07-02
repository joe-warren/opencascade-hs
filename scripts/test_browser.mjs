// Headless browser test for the waterfall-cad playground.
// Loads the playground, types Haskell code, clicks Run, checks the result.
//
// The playground compiles the module and renders its top-level Solid
// bindings (it does not run `main`), so tests assert on status, the solid
// picker, and the rendered glb (validated by signed mesh volume), rather
// than stdout.

import { createRequire } from 'module';
import fs from 'node:fs';
const require = createRequire('/opt/puppeteer/');
const puppeteer = require('puppeteer');

const TIMEOUT = 120_000;
const URL = 'http://localhost:8080';

// Test cases: [name, code, opts]
// opts.status  — exact expected status, or RegExp
// opts.solids  — expected entries of the solid picker, in order
// opts.volume  — expected signed mesh volume of the rendered glb (±opts.tol)
const TESTS = [
  ['csg_solid', `import Waterfall.Solids (Solid, centeredCube, unitSphere)
import Waterfall.Booleans (difference)
import Waterfall.Transforms (uScale)

solid :: Solid
solid = difference centeredCube (uScale 0.65 unitSphere)`,
    { status: 'Done!', solids: ['solid'], volume: 0.11, tol: 0.03 }],

  // Wire construction makes OCCT throw and catch Standard_NoSuchObject
  // internally (BRep_Tool::Parameter inside BRepLib_MakeWire::Add) as part
  // of normal control flow, so this test fails hard if wasm C++ exception
  // handling is broken (e.g. missing EH libc++abi - see WASM_BUILD_NOTES.md).
  ['wire_prism_exceptions', `import Waterfall.Solids (Solid, prism)
import Waterfall.TwoD.Shape (makeShape)
import Waterfall.TwoD.Path2D (pathFrom2D, lineTo2D)
import Linear (V2 (..))

triangle :: Solid
triangle = prism 1 (makeShape (pathFrom2D (V2 0 0) [lineTo2D (V2 1 0), lineTo2D (V2 1 1), lineTo2D (V2 0 0)]))`,
    { status: 'Done!', solids: ['triangle'], volume: 0.5, tol: 0.02 }],

  ['multiple_solids', `import Waterfall.Solids (Solid, unitCube, unitSphere)

cube :: Solid
cube = unitCube

sphere :: Solid
sphere = unitSphere`,
    { status: 'Done!', solids: ['cube', 'sphere'], volume: 4.19, tol: 0.15 }],

  ['no_solids', `main :: IO ()
main = pure ()`,
    { status: 'No top-level values of type Solid found.' }],

  ['compile_error', `solid = thisIsNotInScope`,
    { status: /^Error/ }],
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

// A status is terminal when neither initialisation nor a run is in flight.
const TERMINAL_STATUS = `(() => {
  const t = document.getElementById('status')?.textContent || '';
  return t === 'Ready!' || t === 'Done!' || t.startsWith('Error') ||
    t.startsWith('No top-level') || t.startsWith('Failed');
})()`;

async function runTest(page, name, code, opts) {
  console.log(`\n=== Test: ${name} ===`);

  // Wait for initialisation (and the startup auto-run of the default
  // example) to settle.
  await page.waitForSelector('#status', { timeout: TIMEOUT });
  await page.waitForFunction(TERMINAL_STATUS, { timeout: TIMEOUT });
  console.log('  Playground settled');

  // Reset status to a sentinel so we can detect this run's completion,
  // and clear the output panes.
  await page.evaluate(() => {
    document.getElementById('status').textContent = '(test pending)';
    document.getElementById('stdout').value = '';
    document.getElementById('stderr').value = '';
  });

  // Set the editor content and run
  await page.evaluate((c) => {
    window.editor.dispatch({ changes: { from: 0, to: window.editor.state.doc.length, insert: c } });
  }, code);
  await page.click('#runBtn');
  console.log('  Run clicked');

  await page.waitForFunction(TERMINAL_STATUS, { timeout: TIMEOUT });
  await new Promise(r => setTimeout(r, 500));

  const status = await page.evaluate(() => document.getElementById('status').textContent);
  const stdout = await page.evaluate(() => document.getElementById('stdout').value);
  const stderr = await page.evaluate(() => document.getElementById('stderr').value);
  console.log(`  Status: ${status}`);
  if (stdout.trim()) console.log(`  Stdout: ${stdout.trim().substring(0, 200)}`);
  if (stderr.trim()) console.log(`  Stderr: ${stderr.trim().substring(0, 200)}`);

  const statusOk = opts.status instanceof RegExp
    ? opts.status.test(status)
    : status === opts.status;
  if (!statusOk) {
    console.log(`  FAIL ✗ - expected status ${opts.status}, got "${status}"`);
    return false;
  }

  if (opts.solids) {
    const solids = await page.evaluate(() =>
      Array.from(document.getElementById('solidSelect').options).map(o => o.value));
    if (JSON.stringify(solids) !== JSON.stringify(opts.solids)) {
      console.log(`  FAIL ✗ - expected solids [${opts.solids}], got [${solids}]`);
      return false;
    }
    console.log(`  Solids: [${solids}]`);
  }

  if (opts.volume !== undefined) {
    // The viewer should pick up the rendered glb and model-viewer should
    // actually parse and load it.
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
    fs.writeFileSync(`/tmp/extracted-${name}.glb`, glb);
    const magic = glb.subarray(0, 4).toString();
    console.log(`  Model: ${modelPath} (${glb.length} bytes, magic=${magic})`);
    if (magic !== 'glTF' || glb.length < 1000) {
      console.log('  FAIL ✗ - extracted glb looks invalid');
      return false;
    }
    // Signed mesh volume sanity check: catches mangled triangulation
    // (e.g. boolean-cut holes meshed as filled faces) that structural
    // checks miss.
    const vol = glbSignedVolume(glb);
    console.log(`  Mesh volume: ${vol.toFixed(4)} (expected ~${opts.volume})`);
    if (!(Math.abs(vol - opts.volume) < opts.tol)) {
      console.log('  FAIL ✗ - mesh volume far from analytic volume');
      return false;
    }
  }

  console.log('  PASS ✓');
  return true;
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

  for (let i = 0; i < TESTS.length; i++) {
    const [name, code, opts] = TESTS[i];
    try {
      const ok = await runTest(page, name, code, opts);
      if (ok) passed++; else failed++;
    } catch (e) {
      console.log(`  ERROR: ${e.message.substring(0, 200)}`);
      failed++;
    }

    // Reload for next test (clean state)
    if (i < TESTS.length - 1) {
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
