// Patch dyld.mjs for wasm C++ exception handling support.
//
// Patch 1: handle WebAssembly.Tag imports.
// The __cpp_exception tag must be shared across all modules for throw/catch
// to work.
//
// Patch 2: defer __wasm_apply_data_relocs + _initialize until every module
// in a loadDLLs plan has been instantiated and its exports processed.
// Without this, a module's data relocations are applied against GOT entries
// that are still poisoned because the defining module loads later in the
// same plan; the GOT itself is healed afterwards, but the already-written
// data words keep the poison value. This corrupts C++ typeinfo objects
// (vtable/base-class pointers), which makes catching OCCT exception types
// crash inside scan_eh_tab. Modules that export aligned_alloc (i.e. libc.so)
// are still initialised eagerly, because dyld itself calls aligned_alloc
// while loading subsequent modules.
const fs = require('fs');
const src = process.argv[2];
const dst = process.argv[3] || src;
let c = fs.readFileSync(src, 'utf8');

// --- Patch 1: exception tag imports ---
const tagOld = `        throw new Error(
          \`cannot handle import \${module}.\${name} with kind \${kind}\`
        );`;
const tagRep = `        if (kind === "tag") {
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
  console.log('Already patched (tag)');
} else if (c.includes(tagOld)) {
  c = c.replace(tagOld, tagRep);
  console.log('Patched (tag):', c.includes('_cppExceptionTag'));
} else {
  console.error('ERROR: tag patch anchor not found');
  process.exit(1);
}

// --- Patch 2: defer data relocations until the whole plan is loaded ---
const loopHeadOld = `    for (const {
      memSize,
      memP2Align,
      tableSize,
      tableP2Align,
      modp,
      soname,
    } of plan) {`;
const loopHeadNew = `    const deferredInits = [];
` + loopHeadOld;

const initOld = `      if (instance.exports.__wasm_apply_data_relocs) {
        instance.exports.__wasm_apply_data_relocs();
      }

      instance.exports._initialize();
    }
  }`;
const initNew = `      const initModule = () => {
        if (instance.exports.__wasm_apply_data_relocs) {
          instance.exports.__wasm_apply_data_relocs();
        }
        instance.exports._initialize();
      };
      if (instance.exports.aligned_alloc) {
        // libc: dyld needs aligned_alloc while loading later modules
        initModule();
      } else {
        deferredInits.push(initModule);
      }
    }

    // Apply data relocations only after every module in this plan has been
    // instantiated and its exports processed, so GOT entries satisfied by
    // later modules are no longer poisoned when relocations are written.
    for (const init of deferredInits) {
      init();
    }
  }`;

if (c.includes('deferredInits')) {
  console.log('Already patched (defer)');
} else if (c.includes(loopHeadOld) && c.includes(initOld)) {
  c = c.replace(loopHeadOld, loopHeadNew);
  c = c.replace(initOld, initNew);
  console.log('Patched (defer):', c.includes('deferredInits'));
} else {
  console.error('ERROR: defer patch anchors not found');
  process.exit(1);
}

fs.writeFileSync(dst, c);
