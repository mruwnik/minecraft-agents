// Why JavaScript: it loads a slice of the shadow-cljs node-test bundle before any ClojureScript runs.
// Runs engine.single-child/main from the bundle at argv[2] (the test bundle, out/test.cjs) while loading only the files
// that namespace needs, not every test namespace (the whole bundle takes seconds to load).
'use strict'
const fs = require('fs')
const Module = require('module')
const path = require('path')

const bundle = path.resolve(process.argv[2])
const lines = fs.readFileSync(bundle, 'utf8').split('\n')
const isImport = l => l.startsWith('SHADOW_IMPORT("')
const needed = /^SHADOW_IMPORT\("(goog\.|cljs\.core\.js|shadow\.js\.js|shadow\.js\.shim\.module\$(fs|path|crypto|net)\.js|engine\.single\.js|engine\.single_child\.js)/
const first = lines.findIndex(isImport)
const last = lines.length - lines.slice().reverse().findIndex(isImport)
const source = [...lines.slice(0, first), ...lines.filter(l => isImport(l) && needed.test(l)), 'engine.single_child.main();', ...lines.slice(last)].join('\n')

const m = new Module(bundle, null)
m.filename = bundle
m.paths = Module._nodeModulePaths(path.dirname(bundle))
m._compile(source, bundle)
