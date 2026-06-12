// Headless browser test for the waterfall-cad playground.
// Loads the playground, types Haskell code, clicks Run, checks output.

import { createRequire } from 'module';
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
];

async function runTest(page, name, code, expectedStdout, shouldSucceed) {
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
    window.editor.setValue(c);
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
    if (stdout.includes(expectedStdout)) {
      console.log(`  PASS ✓`);
      return true;
    } else {
      console.log(`  FAIL ✗ - expected stdout to contain "${expectedStdout}"`);
      return false;
    }
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

  for (const [name, code, expected, shouldSucceed] of TESTS) {
    try {
      const ok = await runTest(page, name, code, expected, shouldSucceed);
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
