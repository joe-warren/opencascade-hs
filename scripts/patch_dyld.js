// Patch dyld.mjs to handle WebAssembly.Tag imports for wasm exception handling.
// The __cpp_exception tag must be shared across all modules for throw/catch to work.
const fs = require('fs');
const src = process.argv[2];
const dst = process.argv[3] || src;
let c = fs.readFileSync(src, 'utf8');

const old = `        throw new Error(
          \`cannot handle import \${module}.\${name} with kind \${kind}\`
        );`;
const rep = `        if (kind === "tag") {
          if (!this._cppExceptionTag) {
            this._cppExceptionTag = new WebAssembly.Tag({ parameters: ["i32"] });
          }
          if (!import_obj[module]) import_obj[module] = {};
          import_obj[module][name] = this._cppExceptionTag;
          continue;
        }
        throw new Error(
          \`cannot handle import \${module}.\${name} with kind \${kind}\`
        );`;

if (c.includes('_cppExceptionTag')) {
  console.log('Already patched');
} else {
  c = c.replace(old, rep);
  fs.writeFileSync(dst, c);
  console.log('Patched:', c.includes('_cppExceptionTag'));
}
