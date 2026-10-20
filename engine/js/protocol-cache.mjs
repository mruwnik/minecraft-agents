// Why JavaScript: patches protodef (a CommonJS dependency) at its load boundary; must run before minecraft-protocol builds its serializers.
// Stores the generated protocol reader/writer source under the compile-cache dir, so the next body start loads it instead of
// regenerating and eval-ing ~1 MB of code. Content-addressed, so a changed protocol or protodef version simply misses.
// Next to the source goes V8's code cache taken some seconds after the first start, once the readers in use have been compiled:
// node's own compile cache is written at exit, after V8 has flushed the bytecode of the factory it ran once. MC_COMPILE_CACHE=off disables it.
import { createHash } from 'node:crypto'
import { createRequire } from 'node:module'
import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from 'node:fs'
import path from 'node:path'
import vm from 'node:vm'
import { cacheDir } from './compile-cache.mjs'
import './lazy-requires.mjs'

const require = createRequire(import.meta.url)

const { ProtoDefCompiler } = require('protodef').Compiler
const versions = ['protodef', 'minecraft-protocol', 'prismarine-nbt', 'minecraft-data'].map(p => require(`${p}/package.json`).version).join()
const dir = path.join(cacheDir, 'protocol')
const parts = ['sizeOf', 'write', 'read']

function keyOf (c) {
  const h = createHash('sha1').update(versions)
  for (const p of parts) {
    const k = c[`${p}Compiler`]
    h.update(JSON.stringify([k.types, Object.keys(k.native), k.context, Object.keys(k.parameterizableTypes)]))
  }
  return h.digest('hex')
}

const CODE_CACHE_DELAY_MS = Number(process.env.MC_PROTOCOL_CODE_CACHE_MS ?? 15000)

function writeAtomic (file, data) {
  const tmp = `${file}.${process.pid}.tmp`
  writeFileSync(tmp, data)
  renameSync(tmp, file)
}

function store (file, c) {
  const body = parts.map(p => `${p}: ((native) => (${c[`${p}Compiler`].generate()})())(nativeOf.${p})`).join(',\n')
  mkdirSync(dir, { recursive: true })
  writeAtomic(file, `((nativeOf, PartialReadError) => {\nreturn {\n${body}\n}\n})\n`)
}

function load (file) {
  const cacheFile = file.replace(/\.js$/, '.cache')
  const cachedData = existsSync(cacheFile) ? readFileSync(cacheFile) : undefined
  const script = new vm.Script(readFileSync(file, 'utf8'), { filename: file, cachedData })
  if (!cachedData || script.cachedDataRejected) setTimeout(() => writeAtomic(cacheFile, script.createCachedData()), CODE_CACHE_DELAY_MS).unref()
  return script.runInThisContext()
}

export function install (enabled = process.env.MC_COMPILE_CACHE !== 'off') {
  if (!enabled || ProtoDefCompiler.prototype.compileProtoDefSync.cached) return
  const original = ProtoDefCompiler.prototype.compileProtoDefSync
  const { CompiledProtodef } = require('protodef/src/compiler.js')
  const { PartialReadError } = require('protodef/src/utils.js')
  function cached (options) {
    if (options?.printCode) return original.call(this, options)
    const file = path.join(dir, `${keyOf(this)}.js`)
    if (!existsSync(file)) store(file, this)
    const ctxs = load(file)({ sizeOf: this.sizeOfCompiler.native, write: this.writeCompiler.native, read: this.readCompiler.native }, PartialReadError)
    return new CompiledProtodef(ctxs.sizeOf, ctxs.write, ctxs.read)
  }
  cached.cached = true
  ProtoDefCompiler.prototype.compileProtoDefSync = cached
}

install()
