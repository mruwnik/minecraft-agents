// Why JavaScript: patches protodef (a CommonJS dependency) at its load boundary; must run before minecraft-protocol builds its serializers.
// Stores the generated protocol reader/writer source as a module under the compile-cache dir, so the next body start requires it
// (node's compile cache then covers it) instead of regenerating and eval-ing ~1 MB of code. Content-addressed, so a changed protocol
// or protodef version simply misses. MC_COMPILE_CACHE=off disables it.
import { createHash } from 'node:crypto'
import { createRequire } from 'node:module'
import { existsSync, mkdirSync, renameSync, writeFileSync } from 'node:fs'
import path from 'node:path'
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

function store (file, c) {
  const body = parts.map(p => `${p}: ((native) => (${c[`${p}Compiler`].generate()})())(nativeOf.${p})`).join(',\n')
  mkdirSync(dir, { recursive: true })
  const tmp = `${file}.${process.pid}.tmp`
  writeFileSync(tmp, `module.exports = (nativeOf, PartialReadError) => {\nreturn {\n${body}\n}\n}\n`)
  renameSync(tmp, file)
}

export function install (enabled = process.env.MC_COMPILE_CACHE !== 'off') {
  if (!enabled || ProtoDefCompiler.prototype.compileProtoDefSync.cached) return
  const original = ProtoDefCompiler.prototype.compileProtoDefSync
  const { CompiledProtodef } = require('protodef/src/compiler.js')
  const { PartialReadError } = require('protodef/src/utils.js')
  function cached (options) {
    if (options?.printCode) return original.call(this, options)
    const file = path.join(dir, `${keyOf(this)}.cjs`)
    if (!existsSync(file)) store(file, this)
    const ctxs = require(file)({ sizeOf: this.sizeOfCompiler.native, write: this.writeCompiler.native, read: this.readCompiler.native }, PartialReadError)
    return new CompiledProtodef(ctxs.sizeOf, ctxs.write, ctxs.read)
  }
  cached.cached = true
  ProtoDefCompiler.prototype.compileProtoDefSync = cached
}

install()
