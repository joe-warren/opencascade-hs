// Run a Haskell snippet in the playground and extract the produced /out.glb.
// Usage: node extract_glb.mjs <code-file.hs> <output.glb>
import { createRequire } from 'module';
import fs from 'node:fs';
const require = createRequire('/opt/puppeteer/');
const puppeteer = require('puppeteer');

const TIMEOUT = 180_000;
const URL = 'http://localhost:8080';
const code = fs.readFileSync(process.argv[2], 'utf8');
const outPath = process.argv[3];

const browser = await puppeteer.launch({
  headless: true,
  args: ['--no-sandbox', '--disable-setuid-sandbox', '--disable-gpu'],
});
const page = await browser.newPage();
page.on('pageerror', (err) => console.log(`PAGE ERROR: ${err.message.substring(0, 200)}`));
await page.goto(URL, { waitUntil: 'domcontentloaded', timeout: TIMEOUT });
await page.waitForFunction(
  () => document.getElementById('status')?.textContent === 'Ready!',
  { timeout: TIMEOUT }
);
await page.evaluate((c) => window.editor.dispatch({ changes: { from: 0, to: window.editor.state.doc.length, insert: c } }), code);
await page.click('#runBtn');
await page.waitForFunction(
  () => {
    const s = document.getElementById('status')?.textContent || '';
    return s.startsWith('Done') || s.startsWith('Error');
  },
  { timeout: TIMEOUT }
);
const status = await page.evaluate(() => document.getElementById('status').textContent);
const stdoutTxt = await page.evaluate(() => document.getElementById('stdout').value);
const stderr = await page.evaluate(() => document.getElementById('stderr').value);
console.log('status:', status);
if (stdoutTxt.trim()) console.log('stdout:', stdoutTxt.trim().substring(0, 500));
if (stderr.trim()) console.log('stderr:', stderr.trim().substring(0, 500));
await page.waitForFunction(() => window.__lastModel, { timeout: 30_000 });
const b64 = await page.evaluate(async () => {
  const v = document.getElementById('viewer');
  const buf = await fetch(v.dataset.url).then((r) => r.arrayBuffer());
  const bytes = new Uint8Array(buf);
  let s = '';
  for (let i = 0; i < bytes.length; i++) s += String.fromCharCode(bytes[i]);
  return btoa(s);
});
fs.writeFileSync(outPath, Buffer.from(b64, 'base64'));
console.log('wrote', outPath, Buffer.from(b64, 'base64').length, 'bytes');
await browser.close();
