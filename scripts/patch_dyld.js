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
//
// Patch 3: enlarge the C shadow stack from one page (64KB) to STACK_PAGES
// pages. dyld lays linear memory out as [C stack | libc data | heap] and
// hardcodes a single page of stack; the stack-size passed when linking a .so
// is ignored under dynamic linking. 64KB is far too small for OCCT: e.g.
// filleting a B-spline surface (any non-uniformly scaled solid) nests
// BSplSLib evaluations whose stack frames are ~22KB each, overflowing the
// stack and trapping with "memory access out of bounds" / "index out of
// bounds". 128 pages = 8MB, matching the default main-thread stack on
// native platforms, where the same code runs fine.
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

// --- Patch 3: enlarge the C shadow stack ---
const STACK_PAGES = 128; // 8MB

const memoryOld = `#memory = new WebAssembly.Memory({ initial: 1 });`;
const memoryNew = `#memory = new WebAssembly.Memory({ initial: ${STACK_PAGES} });`;

const spOld = `  #sp = new WebAssembly.Global(
    {
      value: "i32",
      mutable: true,
    },
    DyLD.#pageSize
  );`;
const spNew = `  #sp = new WebAssembly.Global(
    {
      value: "i32",
      mutable: true,
    },
    // patched: STACK_PAGES pages of C stack, not one
    DyLD.#pageSize * ${STACK_PAGES}
  );`;

const memBaseOld = `        memory_base = DyLD.#pageSize;`;
const memBaseNew = `        memory_base = DyLD.#pageSize * ${STACK_PAGES};`;

const heapBaseOld = `DyLD.#pageSize * (1 + data_pages)`;
const heapBaseNew = `DyLD.#pageSize * (${STACK_PAGES} + data_pages)`;

const heapEndOld = `DyLD.#pageSize * (1 + data_pages + 1)`;
const heapEndNew = `DyLD.#pageSize * (${STACK_PAGES} + data_pages + 1)`;

if (c.includes(memoryNew)) {
  console.log('Already patched (stack)');
} else if (
  c.includes(memoryOld) && c.includes(spOld) &&
  c.includes(memBaseOld) && c.includes(heapBaseOld) && c.includes(heapEndOld)
) {
  c = c.replace(memoryOld, memoryNew);
  c = c.replace(spOld, spNew);
  c = c.replace(memBaseOld, memBaseNew);
  // heapEnd first: heapBase is a prefix of heapEnd's anchor
  c = c.replace(heapEndOld, heapEndNew);
  c = c.replace(heapBaseOld, heapBaseNew);
  console.log('Patched (stack):', c.includes(memoryNew));
} else {
  console.error('ERROR: stack patch anchors not found');
  process.exit(1);
}

fs.writeFileSync(dst, c);
