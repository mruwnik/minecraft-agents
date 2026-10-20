// Why JavaScript: bundles the body (cljs release bundle + engine/js ESM + Mineflayer) with esbuild and builds a V8 startup snapshot from it (node --build-snapshot is a Node CLI feature).
// Output: engine/out/body-snapshot.blob. A body started from it skips loading ~15 MB of code (~0.7 s CPU). Start it with:
//   node --snapshot-blob engine/out/body-snapshot.blob <flags> body.cjs --agent ...   (argv[1] is a dummy script name)
import * as esbuild from 'esbuild'
import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import fs from 'node:fs'
import http from 'node:http'
import { createRequire } from 'node:module'
import path from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const out = path.join(root, 'engine', 'out')
const work = path.join(out, 'snap-build')
const jsDir = path.join(root, 'engine', 'js')
const KEEP_DATA = new Set(['26.1', '1.20', '1.20.2', '1.20.3', '1.16.1', 'latest', 'common'])
const PKG_JSON = ['protodef', 'minecraft-protocol', 'prismarine-nbt', 'minecraft-data', 'prismarine-block', 'prismarine-registry']

// Only the engine/js modules the cljs side loads by file name (createRequire(root)("./js/x.mjs")).
const cljsJsModules = () => {
  const found = new Set()
  const walk = d => fs.readdirSync(d, { withFileTypes: true }).forEach(e => {
    const p = path.join(d, e.name)
    if (e.isDirectory()) return walk(p)
    if (!e.name.endsWith('.cljs')) return
    for (const m of fs.readFileSync(p, 'utf8').matchAll(/"\.\/js\/([a-z-]+\.mjs)"/g)) found.add(m[1])
  })
  walk(path.join(root, 'engine', 'src'))
  return [...found].sort()
}

const stockPhysicsRequires = () => {
  const file = path.join(root, 'node_modules/mineflayer/lib/plugins/physics.js')
  const dir = path.dirname(file)
  const ids = [...fs.readFileSync(file, 'utf8').matchAll(/require\('([^']+)'\)/g)].map(m => m[1]).filter(id => id !== 'perf_hooks')
  return [...new Set(ids)].map(id => `${JSON.stringify(id)}: require(${JSON.stringify(id.startsWith('.') ? path.resolve(dir, id) : id)})`).join(',\n')
}

const lazyBuiltin = name => `const statics = ${JSON.stringify(name === 'http' || name === 'https' ? { STATUS_CODES: http.STATUS_CODES, METHODS: http.METHODS } : {})}
let m
const g = () => m || (m = process.getBuiltinModule(${JSON.stringify(name)}))
module.exports = new Proxy(function () {}, {
  get: (_, k) => k in statics ? statics[k] : g()[k],
  set: (_, k, v) => { g()[k] = v; return true },
  has: (_, k) => k in g(),
  construct: (_, args) => new (g())(...args),
  apply: (_, t, args) => g().apply(t, args)
})`

const plugin = {
  name: 'body-snapshot',
  setup (b) {
    // Net, TLS and HTTP hold native handles a snapshot cannot serialise: load them on first use.
    b.onResolve({ filter: /^(node:)?(http|https|net|tls|dns|http2)$/ }, a => ({ path: a.path.replace('node:', ''), namespace: 'lazy-builtin' }))
    b.onLoad({ filter: /.*/, namespace: 'lazy-builtin' }, a => ({ contents: lazyBuiltin(a.path), loader: 'js' }))
    // Data for the one Minecraft version the body plays (and the older versions its tables point at).
    b.onLoad({ filter: /minecraft-data\/minecraft-data\/data\/(pc|bedrock)\/[^/]+\/[^/]+\.json$/ }, a => {
      const m = a.path.match(/data\/(pc|bedrock)\/([^/]+)\//)
      if (KEEP_DATA.has(m[2]) && (m[1] === 'pc' || m[2] === 'common')) return null
      return { contents: '{}', loader: 'json' }
    })
    // The start-up patches of lazy-requires only defer work a snapshot does at build time.
    b.onLoad({ filter: /engine\/js\/lazy-requires\.mjs$/ }, () => ({ contents: '', loader: 'js' }))
    // The cljs release bundle runs main at load; the snapshot calls it on restore.
    b.onLoad({ filter: /engine\/out\/body-release\.cjs$/ }, a => {
      const src = fs.readFileSync(a.path, 'utf8')
      const tail = 'cljs.core.apply.cljs$core$IFn$_invoke$arity$2(engine.main.main,process.argv.slice(2));'
      if (!src.includes(tail)) throw new Error('body-release.cjs no longer ends with the main call; update tools/build-body-snapshot.mjs')
      const preload = 'require(require("path").join(__dirname,"..","js","lazy-requires.mjs"));'
      return { contents: src.replace(preload, '').replace(tail, 'globalThis.__bodyMain=function(){' + tail + '};'), loader: 'js' }
    })
    // lazy-chunk swaps prismarine-chunk's export through require.cache; the bundle has none, so blocks.js reads the swap from a global.
    b.onLoad({ filter: /mineflayer\/lib\/plugins\/blocks\.js$/ }, a => {
      const call = "require('prismarine-chunk')(bot.registry)"
      const src = fs.readFileSync(a.path, 'utf8')
      if (!src.includes(call)) throw new Error('mineflayer blocks.js changed: update tools/build-body-snapshot.mjs')
      return { contents: src.replace(call, "(globalThis.__lazyChunk || require('prismarine-chunk'))(bot.registry)"), loader: 'js', resolveDir: path.dirname(a.path) }
    })
    b.onLoad({ filter: /engine\/js\/[^/]+(\/[^/]+)?\.mjs$/ }, a => {
      let src = fs.readFileSync(a.path, 'utf8')
      const url = JSON.stringify(pathToFileURL(a.path).href)
      src = src.replace(/^const require = createRequire\(import\.meta\.url\)\s*$/m, '')
        .replace(/createRequire\(import\.meta\.url\)/g, 'require')
        .replace(/require\(`\$\{(\w+)\}\/package\.json`\)/g, '__pj[$1]')
        .replace(/import\.meta\.url/g, url)
        .replace(/import\.meta\.dirname/g, JSON.stringify(path.dirname(a.path)))
      if (src.includes('__pj[')) src = `const __pj = {${PKG_JSON.map(p => `${JSON.stringify(p)}: require(${JSON.stringify(p + '/package.json')})`).join(', ')}}\n` + src
      if (a.path.endsWith('protocol-cache.mjs')) {
        // On inside the snapshot (the compiled protocol is the costly part of a start), but with no pending code-cache timer: a timer holds a vm.Script, which a snapshot cannot hold.
        const timer = 'if (!cachedData || script.cachedDataRejected) setTimeout('
        if (!src.includes(timer) || !src.includes('\ninstall()')) throw new Error('protocol-cache.mjs changed: update tools/build-body-snapshot.mjs')
        src = src.replace(timer, 'if (false) setTimeout(').replace('\ninstall()', '\ninstall(true)')
      }
      if (a.path.endsWith('lazy-chunk.mjs')) {
        src = src.replace("const path = require.resolve('prismarine-chunk')\n", '').replace('require.cache[path].exports = ', 'globalThis.__lazyChunk = ')
        const chunkRequire = createRequire(path.join(root, 'node_modules/prismarine-chunk/'))
        src = src.replace("createRequire(require.resolve('prismarine-chunk'))", '(id => require(__SMART__))').replace('__SMART__', JSON.stringify(chunkRequire.resolve('smart-buffer')))
      }
      if (a.path.endsWith('tick-rate-physics.mjs')) {
        src = src.replace('const STOCK_PATH = require.resolve(\'mineflayer/lib/plugins/physics.js\')', `const STOCK_PATH = ${JSON.stringify(path.join(root, 'node_modules/mineflayer/lib/plugins/physics.js'))}`)
          .replace('createRequire(STOCK_PATH)', `(id => ({ ${stockPhysicsRequires()} })[id])`)
          .replace('require(STOCK_PATH)(bot, options)', 'throw err')
      }
      return { contents: src, loader: 'js', resolveDir: path.dirname(a.path) }
    })
  }
}

const listFiles = d => fs.existsSync(d) ? fs.readdirSync(d, { withFileTypes: true }).flatMap(e => e.isDirectory() ? listFiles(path.join(d, e.name)) : [path.join(d, e.name)]) : []
const keyOfInputs = () => {
  const h = createHash('sha1').update(process.version)
  const files = [path.join(out, 'body-release.cjs'), fileURLToPath(import.meta.url), path.join(root, 'package-lock.json'), path.join(root, 'tools/patch-deps.mjs'),
    ...listFiles(jsDir).filter(f => f.endsWith('.mjs') && !f.endsWith('.test.mjs')), ...listFiles(path.join(root, 'patches')), ...listFiles(path.join(root, 'engine/src/jobs/lib')).filter(f => f.endsWith('.txt'))]
  files.sort().forEach(f => h.update(f).update(fs.readFileSync(f)))
  return h.digest('hex')
}
const keyFile = path.join(out, 'body-snapshot.key')
const blobFile = path.join(out, 'body-snapshot.blob')
const key = keyOfInputs()
if (fs.existsSync(blobFile) && fs.existsSync(keyFile) && fs.readFileSync(keyFile, 'utf8') === key) {
  const now = new Date()
  fs.utimesSync(blobFile, now, now)
  console.log('body-snapshot: current')
  process.exit(0)
}

fs.mkdirSync(work, { recursive: true })
const mods = cljsJsModules()
fs.writeFileSync(path.join(work, 'entry.cjs'), `const v8 = require('v8')
const Module = require('module')
const bundled = {
${mods.map(m => `  ${JSON.stringify('./js/' + m)}: () => require(${JSON.stringify(path.join(jsDir, m))})`).join(',\n')}
}
const realCreateRequire = Module.createRequire
Module.createRequire = base => {
  const real = realCreateRequire(base)
  return id => (bundled[id] ? bundled[id]() : real(id))
}
const loaded = {}
for (const k of Object.keys(bundled)) { const f = bundled[k]; bundled[k] = () => loaded[k] ??= f() }
for (const k of Object.keys(bundled)) bundled[k]()
require(${JSON.stringify(path.join(out, 'body-release.cjs'))})
v8.startupSnapshot.setDeserializeMainFunction(() => globalThis.__bodyMain())
`)
const bundle = path.join(work, 'bundle.cjs')
await esbuild.build({ entryPoints: [path.join(work, 'entry.cjs')], bundle: true, platform: 'node', format: 'cjs', outfile: bundle, plugins: [plugin], logLevel: 'error', absWorkingDir: root, legalComments: 'none' })
const blob = path.join(out, 'body-snapshot.blob')
execFileSync(process.execPath, ['--snapshot-blob', blob + '.new', '--build-snapshot', bundle], { stdio: ['ignore', 'inherit', 'inherit'], cwd: path.join(root, 'engine'), env: { ...process.env, MC_COMPILE_CACHE: 'off' } })
fs.renameSync(blob + '.new', blob)
fs.writeFileSync(keyFile, key)
console.log(`body-snapshot: ${blob} (${(fs.statSync(blob).size / 1e6).toFixed(1)} MB)`)
